/*
 * Copyright (c) 2023-2025 Atsushi Komaba
 *
 * This file is part of Syscraws.
 * Syscraws is free software: you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation, either version 3
 * of the License, or any later version.
 *
 * Syscraws is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Syscraws. If not, see <https://www.gnu.org/licenses/>.
 */

/*!
 * Defines the Abstract Syntax Tree (AST) and its parser.
 */

mod tests;
use super::CharsPeekable;
use crate::log::{Index, ParseError, Pos};
use enum_iterator::Sequence;

/**
 * The Abstract Syntax Tree (AST) for the entire file.
 */
pub struct File {
    /**
     * List of import statements in the file.
     */
    pub imports: Vec<WithExtraTokens<Import>>,
    /**
     * List of structure names defined in the file.
     */
    pub structure_names: Vec<StructureName>,
    /**
     * List of function names defined in the file.
     */
    pub function_names: Vec<FunctionName>,
    /**
     * Top-level statements in the file (includes function definitions).
     */
    pub top_level_statements: Vec<WithExtraTokens<TopLevelStatement>>,
}

/**
 * A parsed item that may contain extra tokens following its valid construct
 * on the same line.
 */
pub struct WithExtraTokens<T> {
    /**
     * The valid construct parsed successfully.
     */
    pub content: T,
    /**
     * [`Pos`] of extra tokens, if any. `None` if there are no extra tokens.
     */
    pub extra_tokens_pos: Option<Pos>,
}

/**
 * An import statement in the AST.
 */
pub struct Import {
    /**
     * [`Pos`] of the keyword `import`.
     */
    pub keyword_import_pos: Pos,
    /**
     * The target to import.
     */
    pub target: Option<TermWithPos>,
}

/**
 * A structure name in the AST.
 */
pub struct StructureName {
    /**
     * [`Pos`] of the keyword `struct`.
     */
    pub keyword_struct_pos: Pos,
    /**
     * The structure name and its [`Pos`].
     */
    pub name_and_pos: Option<(String, Pos)>,
}

/**
 * A function name in the AST.
 */
pub struct FunctionName {
    /**
     * [`Pos`] of the keyword `func` or `method`.
     */
    pub keyword_pos: Pos,
    /**
     * The function name and its [`Pos`].
     */
    pub name_and_pos: Option<(String, Pos)>,
    /**
     * whether the function is a method.
     */
    pub is_method: bool,
}

/**
 * A top-level statement in the AST.
 */
pub enum TopLevelStatement {
    /**
     * A structure definition.
     */
    StructureDefinition(StructureDefinition),
    /**
     * A function definition.
     */
    FunctionDefinition(FunctionDefinition),
    /**
     * A regular statement.
     */
    Statement(Statement),
}

/**
 * A structure definition in the AST.
 *
 * The structure name is stored in [`File::structure_names`], so it is not
 * included here.
 */
pub struct StructureDefinition {
    /**
     * List of type parameters.
     */
    pub ty_parameters: Option<Vec<ListElement>>,
    /**
     * [`Pos`] of extra tokens if any appear on the same line after the
     * keyword `struct`, optional struct name, and optional type parameter
     * list.
     */
    pub extra_tokens_pos: Option<Pos>,
    /**
     * List of fields of the structure.
     */
    pub fields: Vec<WithExtraTokens<TermWithPos>>,
}

/**
 * A function definition in the AST.
 *
 * The function name is stored in [`File::function_names`], so it is not
 * included here.
 */
pub struct FunctionDefinition {
    /**
     * List of type parameters.
     */
    pub ty_parameters: Option<Vec<ListElement>>,
    /**
     * List of parameters.
     */
    pub parameters: Result<Vec<ListElement>, Pos>,
    /**
     * Return type of the function.
     */
    pub return_ty: Option<ReturnTy>,
    /**
     * [`Pos`] of extra tokens if any appear on the same line after the
     * keyword `func`, optional function name, optional type parameter list,
     * optional parameter list, and optional return type.
     */
    pub extra_tokens_pos: Option<Pos>,
    /**
     * Body of the function.
     */
    pub body: Vec<WithExtraTokens<Statement>>,
}

/**
 * Return type of a function in the AST.
 */
pub struct ReturnTy {
    /**
     * [`Pos`] of `:`.
     */
    pub colon_pos: Pos,
    /**
     * The return type.
     */
    pub ty: Option<TermWithPos>,
}

/**
 * A statement in the AST.
 */
pub enum Statement {
    /**
     * Declaration of a variable.
     */
    VariableDeclaration {
        /**
         * Position of the keyword `var`.
         */
        keyword_var_pos: Pos,
        /**
         * The variable name, type (optional) and initial value (optional).
         */
        term: Option<TermWithPos>,
    },
    /**
     * A single expression.
     */
    Term(TermWithPos),
    /**
     * `while` statement.
     */
    While {
        /**
         * Position of the keyword `while`.
         */
        keyword_while_pos: Pos,
        /**
         * The condition.
         */
        condition: Option<TermWithPos>,
        /**
         * [`Pos`] of extra tokens if any appear on the same line after
         * `while` and optional condition.
         */
        extra_tokens_pos: Option<Pos>,
        /**
         * The body.
         */
        do_block: Vec<WithExtraTokens<Statement>>,
    },
    /**
     * `if` statement.
     */
    If {
        /**
         * Position of the keyword `if`.
         */
        keyword_if_pos: Pos,
        /**
         * The condition.
         */
        condition: Option<TermWithPos>,
        /**
         * [`Pos`] of extra tokens if any appear on the same line after `if`
         * and optional condition.
         */
        extra_tokens_pos: Option<Pos>,
        then_block: Vec<WithExtraTokens<Statement>>,
        else_block: Option<ElseBlock>,
    },
    Break,
    Continue,
}

/**
 * `else` block of an if statement.
 */
pub struct ElseBlock {
    /**
     * Position of the keyword `else`.
     */
    pub keyword_else_pos: Pos,
    /**
     * [`Pos`] of extra tokens if any appear on the same line after `else`.
     */
    pub extra_tokens_pos: Option<Pos>,
    pub block: Vec<WithExtraTokens<Statement>>,
}

/**
 * Pair of a [`Term`] and its [`Pos`].
 */
#[cfg_attr(test, derive(Debug, PartialEq, Eq))]
pub struct TermWithPos {
    pub term: Term,
    pub pos: Pos,
}

/**
 * A term in the AST, representing an expression, a type, or an import name.
 */
#[cfg_attr(test, derive(Debug, PartialEq, Eq))]
pub enum Term {
    /**
     * A numeric literal, either integer or floating-point number.
     */
    NumericLiteral(String),
    /**
     * A string literal.
     */
    StringLiteral(Vec<StringLiteralComponent>),
    /**
     * The integer type (`int`)
     */
    IntegerTy,
    /**
     * The floating-point type (`float`)
     */
    FloatTy,
    /**
     * The identity function (`_`)
     */
    Identity,
    /**
     * An identifier.
     */
    Identifier(String),
    /**
     * A term followed by `.` and field name.
     */
    FieldByName {
        term_left: Box<TermWithPos>,
        name: String,
    },
    /**
     * A term followed by `.` and field number.
     */
    FieldByNumber {
        term_left: Box<TermWithPos>,
        number: String,
    },
    /**
     * A term followed by `:` and another term.
     */
    TypeAnnotation {
        term_left: Box<TermWithPos>,
        colon_pos: Pos,
        term_right: Option<Box<TermWithPos>>,
    },
    /**
     * Unary operation.
     */
    UnaryOperation {
        operator_name: &'static str,
        operator_pos: Pos,
        operand: Option<Box<TermWithPos>>,
    },
    /**
     * Binary operation.
     */
    BinaryOperation {
        left_operand: Option<Box<TermWithPos>>,
        operator_name: &'static str,
        operator_pos: Pos,
        right_operand: Option<Box<TermWithPos>>,
    },
    /**
     * Assignment.
     */
    Assignment {
        left_hand_side: Option<Box<TermWithPos>>,
        operator_name: &'static str,
        operator_pos: Pos,
        right_hand_side: Option<Box<TermWithPos>>,
    },
    Conjunction {
        conditions: Vec<Option<TermWithPos>>,
        operators_pos: Vec<Pos>,
    },
    Disjunction {
        conditions: Vec<Option<TermWithPos>>,
        operators_pos: Vec<Pos>,
    },
    Parenthesized {
        inner: Box<TermWithPos>,
    },
    Tuple {
        elements: Vec<ListElement>,
    },
    FunctionCall {
        function: Box<TermWithPos>,
        arguments: Vec<ListElement>,
    },
    TypeParameters {
        term_left: Box<TermWithPos>,
        parameters: Vec<ListElement>,
    },
    ReturnType {
        arrow_pos: Pos,
        parameters: Box<TermWithPos>,
        return_ty: Option<Box<TermWithPos>>,
    },
}

/**
 * A component of a string literal in the AST.
 */
#[cfg_attr(test, derive(Debug, PartialEq, Eq))]
pub enum StringLiteralComponent {
    String(String),
    PlaceHolder {
        format: String,
        value: Option<TermWithPos>,
    },
}

/**
 * An element of a list in the AST.
 */
#[cfg_attr(test, derive(Debug, PartialEq, Eq))]
pub enum ListElement {
    NonEmpty(TermWithPos),
    Empty { comma_pos: Pos },
}

/**
 * Parses an entire file.
 */
pub fn parse_file(
    chars_peekable: &mut CharsPeekable,
    file_index: usize,
) -> Result<File, ParseError> {
    let mut parser = Parser::new(chars_peekable, file_index)?;
    let mut file = File {
        imports: Vec::new(),
        structure_names: Vec::new(),
        function_names: Vec::new(),
        top_level_statements: Vec::new(),
    };
    while let Some(item_start_token) = &mut parser.current.token {
        if let Token::KeywordImport = item_start_token {
            let import = parser.parse_import()?;
            file.imports.push(WithExtraTokens {
                content: import,
                extra_tokens_pos: parser.consume_line()?,
            });
        } else if let Token::KeywordStruct = item_start_token {
            let (name, definition) = parser.parse_structure_definition()?;
            file.structure_names.push(name);
            file.top_level_statements.push(WithExtraTokens {
                content: TopLevelStatement::StructureDefinition(definition),
                extra_tokens_pos: parser.consume_line()?,
            });
        } else if let Token::KeywordFunc | Token::KeywordMethod = item_start_token {
            let (name, definition) = parser.parse_function_definition()?;
            file.function_names.push(name);
            file.top_level_statements.push(WithExtraTokens {
                content: TopLevelStatement::FunctionDefinition(definition),
                extra_tokens_pos: parser.consume_line()?,
            });
        } else if let Some(statement) = parser.parse_statement(&mut Vec::new())? {
            file.top_level_statements.push(WithExtraTokens {
                content: TopLevelStatement::Statement(statement),
                extra_tokens_pos: parser.consume_line()?,
            });
        } else {
            return Err(ParseError::UnexpectedToken(parser.current_pos()));
        }
    }
    Ok(file)
}

/**
 * The parser used in [`parse_file`].
 */
struct Parser<'str, 'iter> {
    /**
     * iterator over characters.
     */
    iter: &'iter mut CharsPeekable<'str>,
    /**
     * Information on the current token.
     */
    current: TokenInfo,
    /**
     * End index of the previous token.
     */
    prev_end: Index,
    /**
     * The pre-order file index.
     */
    file_index: usize,
}

impl<'str, 'iter> Parser<'str, 'iter> {
    /**
     * Creates a new [`Parser`] from the given [`CharsPeekable`].
     *
     * It calls [`read_token`] once and sets [`Self::current`] to point to
     * the first token.
     */
    fn new(
        iter: &'iter mut CharsPeekable<'str>,
        file_index: usize,
    ) -> Result<Parser<'str, 'iter>, ParseError> {
        let first_token = read_token(iter, true, file_index)?;
        Ok(Parser {
            iter,
            current: first_token,
            prev_end: Index { line: 0, column: 0 },
            file_index,
        })
    }
}

/**
 * Information on a token.
 */
struct TokenInfo {
    /**
     * Token.
     */
    token: Option<Token>,
    /**
     * Start index of the token.
     */
    start: Index,
    /**
     * Whether there is a line break between this token and the previous
     * one.
     */
    is_on_new_line: bool,
}

/**
 * A token.
 */
#[cfg_attr(test, derive(Debug, PartialEq, Eq))]
enum Token {
    Digits(String),
    StringLiteral(Vec<StringLiteralComponent>),
    KeywordImport,
    KeywordExport,
    KeywordStruct,
    KeywordFunc,
    KeywordMethod,
    KeywordIf,
    KeywordElse,
    KeywordWhile,
    KeywordBreak,
    KeywordContinue,
    KeywordReturn,
    KeywordEnd,
    KeywordVar,
    KeywordInt,
    KeywordFloat,
    Underscore,
    Identifier(String),
    Plus,
    PlusEqual,
    Hyphen,
    HyphenEqual,
    HyphenGreater,
    Asterisk,
    AsteriskEqual,
    Slash,
    SlashEqual,
    Percent,
    PercentEqual,
    Equal,
    DoubleEqual,
    EqualGreater,
    Exclamation,
    ExclamationEqual,
    Greater,
    GreaterEqual,
    DoubleGreater,
    DoubleGreaterEqual,
    Less,
    LessEqual,
    DoubleLess,
    DoubleLessEqual,
    Ampersand,
    AmpersandEqual,
    DoubleAmpersand,
    Bar,
    BarEqual,
    DoubleBar,
    Circumflex,
    CircumflexEqual,
    Dot,
    Colon,
    Semicolon,
    Comma,
    Question,
    Tilde,
    Dollar,
    OpeningParenthesis,
    ClosingParenthesis,
    OpeningBracket,
    ClosingBracket,
    OpeningBrace,
    ClosingBrace,
}

impl Parser<'_, '_> {
    /**
     * Parses an import statement.
     */
    fn parse_import(&mut self) -> Result<Import, ParseError> {
        let keyword_import_pos = self.current_pos();
        self.consume_token()?;

        // The target to import should immediately follow the keyword `import`, without
        // a line break.
        let target = if self.current.is_on_new_line {
            None
        } else {
            self.parse_factor(false)?
        };

        Ok(Import {
            keyword_import_pos,
            target,
        })
    }

    /**
     * Parses a structure definition.
     */
    fn parse_structure_definition(
        &mut self,
    ) -> Result<(StructureName, StructureDefinition), ParseError> {
        let keyword_struct_pos = self.current_pos();
        self.consume_token()?;

        // The structure name should immediately follow `struct`, without a line break.
        let name_and_pos = if self.current.is_on_new_line {
            None
        } else if let Some(name) = &mut self.current.token {
            match name {
                Token::Identifier(name) => {
                    let name = std::mem::take(name);
                    let pos = self.current_pos();
                    self.consume_token()?;
                    Some((name, pos))
                }
                _ => {
                    return Err(ParseError::UnexpectedTokenAfterKeywordStruct {
                        unexpected_token_pos: self.current_pos(),
                        keyword_struct_pos,
                    })
                }
            }
        } else {
            None
        };

        let ty_parameters = if self.current.is_on_new_line {
            None
        } else if let Some(Token::OpeningBracket) = self.current.token {
            let opening_bracket_pos = self.current_pos();
            self.consume_token()?;

            let (ty_parameters, _) = self.parse_list_elements_and_trailing_comma()?;
            match self.current.token {
                Some(Token::ClosingBracket) => self.consume_token()?,
                Some(_) => {
                    return Err(ParseError::UnexpectedTokenInBrackets {
                        unexpected_token_pos: self.current_pos(),
                        opening_bracket_pos,
                    })
                }
                None => {
                    return Err(ParseError::UnclosedBracket {
                        opening_bracket_pos,
                    });
                }
            }
            Some(ty_parameters)
        } else {
            None
        };

        let extra_tokens_pos = self.consume_line()?;

        let mut fields = Vec::new();
        loop {
            if let Some(Token::KeywordEnd) = self.current.token {
                self.consume_token()?;
                break;
            } else if let Some(field) = self.parse_factor(false)? {
                fields.push(WithExtraTokens {
                    content: field,
                    extra_tokens_pos: self.consume_line()?,
                });
            } else {
                return Err(ParseError::UnclosedBlock {
                    pos: self.current_pos(),
                    starts_pos: vec![keyword_struct_pos.clone()],
                });
            }
        }

        Ok((
            StructureName {
                name_and_pos,
                keyword_struct_pos,
            },
            StructureDefinition {
                ty_parameters,
                fields,
                extra_tokens_pos,
            },
        ))
    }

    /**
     * Parses a function definition.
     */
    fn parse_function_definition(
        &mut self,
    ) -> Result<(FunctionName, FunctionDefinition), ParseError> {
        let is_method = matches!(self.current.token, Some(Token::KeywordMethod));
        let keyword_pos = self.current_pos();
        self.consume_token()?;

        // The function name should immediately follow `func`, without a line break.
        let name_and_pos = if self.current.is_on_new_line {
            None
        } else if let Some(name) = &mut self.current.token {
            match name {
                Token::Identifier(name) => {
                    let name = std::mem::take(name);
                    let pos = self.current_pos();
                    self.consume_token()?;
                    Some((name, pos))
                }
                _ => {
                    return Err(ParseError::UnexpectedTokenAfterKeywordFuncOrMethod {
                        unexpected_token_pos: self.current_pos(),
                        keyword_pos,
                    })
                }
            }
        } else {
            None
        };

        // Generic parameters list can follow.
        let ty_parameters = if self.current.is_on_new_line {
            None
        } else if let Some(Token::OpeningBracket) = self.current.token {
            let opening_bracket_pos = self.current_pos();
            self.consume_token()?;

            let (ty_parameters, _) = self.parse_list_elements_and_trailing_comma()?;
            match self.current.token {
                Some(Token::ClosingBracket) => self.consume_token()?,
                Some(_) => {
                    return Err(ParseError::UnexpectedTokenInBrackets {
                        unexpected_token_pos: self.current_pos(),
                        opening_bracket_pos,
                    })
                }
                None => {
                    return Err(ParseError::UnclosedBracket {
                        opening_bracket_pos,
                    });
                }
            }
            Some(ty_parameters)
        } else {
            None
        };

        // parameters list follows.
        let parameters = if self.current.is_on_new_line {
            None
        } else if let Some(Token::OpeningParenthesis) = self.current.token {
            let opening_parenthesis_pos = self.current_pos();
            self.consume_token()?;

            let mut parameters = Vec::new();
            loop {
                let parameter = self.parse_assign(true)?;
                match self.current.token {
                    Some(Token::ClosingParenthesis) => {
                        self.consume_token()?;
                        if let Some(element) = parameter {
                            parameters.push(ListElement::NonEmpty(element));
                        }
                        break;
                    }
                    Some(Token::Comma) => {
                        let comma_pos = self.current_pos();
                        self.consume_token()?;
                        if let Some(element) = parameter {
                            parameters.push(ListElement::NonEmpty(element));
                        } else {
                            parameters.push(ListElement::Empty { comma_pos })
                        }
                    }
                    Some(_) => {
                        return Err(ParseError::UnexpectedTokenInParentheses {
                            unexpected_token_pos: self.current_pos(),
                            opening_parenthesis_pos,
                        });
                    }
                    None => {
                        return Err(ParseError::UnclosedParenthesis {
                            opening_parenthesis_pos,
                        });
                    }
                }
            }
            Some(parameters)
        } else {
            None
        };
        let parameters = parameters.ok_or_else(|| self.range_from(keyword_pos.start));

        // The return type can be written after `:`.
        let return_ty = if let Some(Token::Colon) = self.current.token {
            let arrow_pos = self.current_pos();
            self.consume_token()?;
            Some(ReturnTy {
                colon_pos: arrow_pos,
                ty: self.parse_disjunction(false)?,
            })
        } else {
            None
        };

        let extra_tokens_pos = self.consume_line()?;

        // The function body follows.
        let mut starts_pos = vec![keyword_pos.clone()];
        let body = self.parse_block(&mut starts_pos)?;
        if !matches!(self.current.token, Some(Token::KeywordEnd)) {
            return Err(ParseError::UnclosedBlock {
                pos: self.current_pos(),
                starts_pos,
            });
        }
        self.consume_token()?;

        Ok((
            FunctionName {
                keyword_pos,
                name_and_pos,
                is_method,
            },
            FunctionDefinition {
                parameters,
                ty_parameters,
                return_ty,
                body,
                extra_tokens_pos,
            },
        ))
    }

    /**
     * Parses a block consisting of zero or more statements and a keyword
     * `end`.
     */
    fn parse_block(
        &mut self,
        starts_pos: &mut Vec<Pos>,
    ) -> Result<Vec<WithExtraTokens<Statement>>, ParseError> {
        let mut body = Vec::new();
        loop {
            match self.parse_statement(starts_pos)? {
                Some(statement) => body.push(WithExtraTokens {
                    content: statement,
                    extra_tokens_pos: self.consume_line()?,
                }),
                None => return Ok(body),
            }
        }
    }

    /**
     * Parses a [`Statement`].
     */
    fn parse_statement(
        &mut self,
        starts_pos: &mut Vec<Pos>,
    ) -> Result<Option<Statement>, ParseError> {
        if let Some(Token::KeywordVar) = self.current.token {
            self.parse_variable_declaration().map(Option::Some)
        } else if let Some(Token::KeywordIf) = self.current.token {
            self.parse_if_statement(starts_pos).map(Option::Some)
        } else if let Some(Token::KeywordWhile) = self.current.token {
            self.parse_while_statement(starts_pos).map(Option::Some)
        } else if let Some(Token::KeywordBreak) = self.current.token {
            self.consume_token()?;
            Ok(Some(Statement::Break))
        } else if let Some(Token::KeywordContinue) = self.current.token {
            Ok(Some(Statement::Continue))
        } else if let Some(term) = self.parse_assign(false)? {
            Ok(Some(Statement::Term(term)))
        } else {
            Ok(None)
        }
    }

    /**
     * Parses a [`Statement::VariableDeclaration`].
     */
    fn parse_variable_declaration(&mut self) -> Result<Statement, ParseError> {
        let keyword_var_pos = self.current_pos();
        self.consume_token()?;
        let term = self.parse_assign(false)?;
        Ok(Statement::VariableDeclaration {
            keyword_var_pos,
            term,
        })
    }

    fn parse_if_statement(&mut self, starts_pos: &mut Vec<Pos>) -> Result<Statement, ParseError> {
        let keyword_if_pos = self.current_pos();
        self.consume_token()?;

        // The condition should immediately follow `if`, without line break.
        let condition = if self.current.is_on_new_line {
            None
        } else {
            self.parse_disjunction(false)?
        };

        let extra_tokens_pos = self.consume_line()?;

        starts_pos.push(keyword_if_pos.clone());
        let then_block = self.parse_block(starts_pos)?;
        let else_block = match self.current.token {
            Some(Token::KeywordEnd) => {
                self.consume_token()?;
                starts_pos.pop();
                None
            }
            Some(Token::KeywordElse) => Some(self.parse_else_block(starts_pos)?),
            _ => {
                return Err(ParseError::UnclosedBlock {
                    pos: self.current_pos(),
                    starts_pos: std::mem::take(starts_pos),
                })
            }
        };

        Ok(Statement::If {
            keyword_if_pos,
            extra_tokens_pos,
            condition,
            then_block,
            else_block,
        })
    }

    fn parse_else_block(&mut self, starts_pos: &mut Vec<Pos>) -> Result<ElseBlock, ParseError> {
        let keyword_else_pos = self.current_pos();
        self.consume_token()?;
        if !self.current.is_on_new_line {
            if let Some(Token::KeywordIf) = self.current.token {
                let statement = self.parse_if_statement(starts_pos)?;
                let extra_tokens_pos = self.consume_line()?;
                return Ok(ElseBlock {
                    keyword_else_pos,
                    extra_tokens_pos: None,
                    block: vec![WithExtraTokens {
                        content: statement,
                        extra_tokens_pos,
                    }],
                });
            }
        }
        let extra_tokens_pos = self.consume_line()?;
        let block = self.parse_block(starts_pos)?;
        if !matches!(self.current.token, Some(Token::KeywordEnd)) {
            return Err(ParseError::UnclosedBlock {
                pos: self.current_pos(),
                starts_pos: std::mem::take(starts_pos),
            });
        }
        self.consume_token()?;
        starts_pos.pop();
        Ok(ElseBlock {
            keyword_else_pos,
            extra_tokens_pos,
            block,
        })
    }

    /**
     * Parses a while statement ([`Statement::While`]).
     */
    fn parse_while_statement(
        &mut self,
        starts_pos: &mut Vec<Pos>,
    ) -> Result<Statement, ParseError> {
        let keyword_while_pos = self.current_pos();
        self.consume_token()?;

        // The condition should immediately follow `while`, without line break.
        let condition = if self.current.is_on_new_line {
            None
        } else {
            self.parse_disjunction(false)?
        };

        // A line break is required right after the condition.
        let extra_tokens_pos = self.consume_line()?;

        starts_pos.push(keyword_while_pos.clone());
        let do_block = self.parse_block(starts_pos)?;
        if !matches!(self.current.token, Some(Token::KeywordEnd)) {
            return Err(ParseError::UnclosedBlock {
                pos: self.current_pos(),
                starts_pos: std::mem::take(starts_pos),
            });
        }
        self.consume_token()?;
        starts_pos.pop();

        Ok(Statement::While {
            keyword_while_pos,
            condition,
            extra_tokens_pos,
            do_block,
        })
    }

    /**
     * Consumes all remaining tokens on the current line.
     */
    fn consume_line(&mut self) -> Result<Option<Pos>, ParseError> {
        let start = self.current.start;
        let mut consumed = false;
        while self.current.token.is_some() && !self.current.is_on_new_line {
            self.consume_token()?;
            consumed = true;
        }
        Ok(consumed.then(|| self.range_from(start)))
    }

    /**
     * Parses an assignment expression.
     */
    fn parse_assign(&mut self, allow_line_break: bool) -> Result<Option<TermWithPos>, ParseError> {
        let start = self.current.start;
        let left_hand_side = self.parse_disjunction(allow_line_break)?;
        if let Some(operator_name) = self.current.token.as_ref().and_then(assignment_operator) {
            let operator_pos = self.current_pos();
            self.consume_token()?;
            let right_hand_side = self.parse_assign(allow_line_break)?;
            Ok(Some(TermWithPos {
                pos: self.range_from(start),
                term: Term::Assignment {
                    operator_name,
                    operator_pos,
                    left_hand_side: left_hand_side.map(Box::new),
                    right_hand_side: right_hand_side.map(Box::new),
                },
            }))
        } else {
            Ok(left_hand_side)
        }
    }

    fn parse_disjunction(
        &mut self,
        allow_line_break: bool,
    ) -> Result<Option<TermWithPos>, ParseError> {
        let start = self.current.start;
        let term = self.parse_conjunction(allow_line_break)?;
        if let Some(Token::DoubleBar) = self.current.token {
            let mut conditions = vec![term];
            let mut operators_pos = Vec::new();
            while let Some(Token::DoubleBar) = self.current.token {
                operators_pos.push(self.current_pos());
                self.consume_token()?;
                conditions.push(self.parse_conjunction(allow_line_break)?);
            }
            Ok(Some(TermWithPos {
                term: Term::Disjunction {
                    conditions,
                    operators_pos,
                },
                pos: self.range_from(start),
            }))
        } else {
            return Ok(term);
        }
    }

    fn parse_conjunction(
        &mut self,
        allow_line_break: bool,
    ) -> Result<Option<TermWithPos>, ParseError> {
        let start = self.current.start;
        let term = self.parse_binary_operation(allow_line_break)?;
        if let Some(Token::DoubleAmpersand) = self.current.token {
            let mut conditions = vec![term];
            let mut operators_pos = Vec::new();
            while let Some(Token::DoubleAmpersand) = self.current.token {
                operators_pos.push(self.current_pos());
                self.consume_token()?;
                conditions.push(self.parse_binary_operation(allow_line_break)?);
            }
            Ok(Some(TermWithPos {
                term: Term::Conjunction {
                    conditions,
                    operators_pos,
                },
                pos: self.range_from(start),
            }))
        } else {
            return Ok(term);
        }
    }

    fn parse_binary_operation(
        &mut self,
        allow_line_break: bool,
    ) -> Result<Option<TermWithPos>, ParseError> {
        self.parse_binary_operation_rec(allow_line_break, Precedence::first())
    }

    fn parse_binary_operation_rec(
        &mut self,
        allow_line_break: bool,
        precedence: Option<Precedence>,
    ) -> Result<Option<TermWithPos>, ParseError> {
        let Some(precedence) = precedence else {
            return self.parse_factor(allow_line_break);
        };
        let start = self.current.start;
        let mut left_operand =
            self.parse_binary_operation_rec(allow_line_break, precedence.next())?;
        while allow_line_break || !self.current.is_on_new_line {
            let Some(ref token) = self.current.token else {
                break;
            };
            if let Some(operator_name) = infix_operator(token, precedence) {
                let operator_pos = self.current_pos();
                self.consume_token()?;
                let right_operand =
                    self.parse_binary_operation_rec(allow_line_break, precedence.next())?;
                left_operand = Some(TermWithPos {
                    term: Term::BinaryOperation {
                        left_operand: left_operand.map(Box::new),
                        operator_name,
                        operator_pos,
                        right_operand: right_operand.map(Box::new),
                    },
                    pos: self.range_from(start),
                });
            } else {
                break;
            }
        }
        Ok(left_operand)
    }

    fn parse_factor(&mut self, allow_line_break: bool) -> Result<Option<TermWithPos>, ParseError> {
        let start = self.current.start;
        let mut factor = match self.parse_atom(allow_line_break)? {
            Some(factor) => factor,
            None => return Ok(None),
        };
        while let Some(ref token) = self.current.token {
            if let Token::Dot = token {
                let dot_pos = self.current_pos();
                self.consume_token()?;
                match self.current.token {
                    Some(Token::Identifier(ref mut name)) => {
                        let name = std::mem::take(name);
                        self.consume_token()?;
                        factor = TermWithPos {
                            term: Term::FieldByName {
                                term_left: Box::new(factor),
                                name,
                            },
                            pos: self.range_from(start),
                        };
                    }
                    Some(Token::Digits(ref mut number)) => {
                        let number = std::mem::take(number);
                        self.consume_token()?;
                        factor = TermWithPos {
                            term: Term::FieldByNumber {
                                term_left: Box::new(factor),
                                number,
                            },
                            pos: self.range_from(start),
                        };
                    }
                    Some(_) => {
                        return Err(ParseError::UnexpectedTokenAfterDot {
                            unexpected_token_pos: self.current_pos(),
                            dot_pos,
                        })
                    }
                    None => return Err(ParseError::MissingFieldAfterDot { dot_pos }),
                }
            } else if let Token::Colon = token {
                let colon_pos = self.current_pos();
                self.consume_token()?;
                let opt_term_right = self.parse_factor(allow_line_break)?;
                factor = TermWithPos {
                    term: Term::TypeAnnotation {
                        term_left: Box::new(factor),
                        colon_pos,
                        term_right: opt_term_right.map(Box::new),
                    },
                    pos: self.range_from(start),
                };
            } else if let Token::HyphenGreater = token {
                let arrow_pos = self.current_pos();
                self.consume_token()?;
                let opt_ret = self.parse_factor(allow_line_break)?;
                factor = TermWithPos {
                    term: Term::ReturnType {
                        arrow_pos,
                        parameters: Box::new(factor),
                        return_ty: opt_ret.map(Box::new),
                    },
                    pos: self.range_from(start),
                }
            } else if !allow_line_break && self.current.is_on_new_line {
                break;
            } else if let Token::OpeningParenthesis = token {
                let opening_parenthesis_pos = self.current_pos();
                self.consume_token()?;
                let (elements, _) = self.parse_list_elements_and_trailing_comma()?;
                match self.current.token {
                    Some(Token::ClosingParenthesis) => self.consume_token()?,
                    Some(_) => {
                        return Err(ParseError::UnexpectedTokenInParentheses {
                            unexpected_token_pos: self.current_pos(),
                            opening_parenthesis_pos,
                        })
                    }
                    None => {
                        return Err(ParseError::UnclosedParenthesis {
                            opening_parenthesis_pos,
                        })
                    }
                }
                factor = TermWithPos {
                    term: Term::FunctionCall {
                        function: Box::new(factor),
                        arguments: elements,
                    },
                    pos: self.range_from(start),
                };
            } else if let Token::OpeningBracket = token {
                let opening_bracket_pos = self.current_pos();
                self.consume_token()?;
                let (elements, _) = self.parse_list_elements_and_trailing_comma()?;
                match self.current.token {
                    Some(Token::ClosingBracket) => self.consume_token()?,
                    Some(_) => {
                        return Err(ParseError::UnexpectedTokenInBrackets {
                            unexpected_token_pos: self.current_pos(),
                            opening_bracket_pos,
                        })
                    }
                    None => {
                        return Err(ParseError::UnclosedBracket {
                            opening_bracket_pos,
                        });
                    }
                }
                factor = TermWithPos {
                    term: Term::TypeParameters {
                        term_left: Box::new(factor),
                        parameters: elements,
                    },
                    pos: self.range_from(start),
                };
            } else {
                break;
            }
        }
        Ok(Some(factor))
    }

    fn parse_atom(&mut self, allow_line_break: bool) -> Result<Option<TermWithPos>, ParseError> {
        let Some(first_token) = &mut self.current.token else {
            return Ok(None);
        };
        let start = self.current.start;
        let term = if let Token::Underscore = first_token {
            Term::Identity
        } else if let Token::Identifier(name) = first_token {
            let name = std::mem::take(name);
            self.consume_token()?;
            Term::Identifier(name)
        } else if let Token::StringLiteral(components) = first_token {
            let components = std::mem::take(components);
            self.consume_token()?;
            Term::StringLiteral(components)
        } else if let Token::Digits(value) = first_token {
            let mut value = std::mem::take(value);
            self.consume_token()?;
            if self.current.start == self.prev_end {
                if let Some(Token::Dot) = self.current.token {
                    let number_pos = self.range_from(start);
                    self.consume_token()?;
                    if let Some(Token::Identifier(ref mut name)) = self.current.token {
                        let number = TermWithPos {
                            term: Term::NumericLiteral(value),
                            pos: number_pos,
                        };
                        let name = std::mem::take(name);
                        self.consume_token()?;
                        return Ok(Some(TermWithPos {
                            term: Term::FieldByName {
                                term_left: Box::new(number),
                                name,
                            },
                            pos: self.range_from(start),
                        }));
                    } else {
                        value.push('.');
                        if self.current.start == self.prev_end {
                            if let Some(Token::Digits(ref decimal_part)) = self.current.token {
                                value.push_str(decimal_part);
                                self.consume_token()?;
                            }
                        }
                    }
                }
            }
            Term::NumericLiteral(value)
        } else if let Token::Dot = first_token {
            let dot_pos = self.current_pos();
            self.consume_token()?;
            if self.current.start == self.prev_end {
                if let Some(Token::Digits(ref value)) = self.current.token {
                    let value = format!(".{value}");
                    self.consume_token()?;
                    Term::NumericLiteral(value)
                } else {
                    return Err(ParseError::UnexpectedToken(dot_pos));
                }
            } else {
                return Err(ParseError::UnexpectedToken(dot_pos));
            }
        } else if let Token::KeywordInt = first_token {
            self.consume_token()?;
            Term::IntegerTy
        } else if let Token::KeywordFloat = first_token {
            self.consume_token()?;
            Term::FloatTy
        } else if let Token::OpeningParenthesis = first_token {
            let opening_parenthesis_pos = self.current_pos();
            self.consume_token()?;
            let (elements, has_trailing_comma) = self.parse_list_elements_and_trailing_comma()?;
            match self.current.token {
                Some(Token::ClosingParenthesis) => self.consume_token()?,
                Some(_) => {
                    return Err(ParseError::UnexpectedTokenInParentheses {
                        unexpected_token_pos: self.current_pos(),
                        opening_parenthesis_pos,
                    })
                }
                None => {
                    return Err(ParseError::UnclosedParenthesis {
                        opening_parenthesis_pos,
                    })
                }
            }
            if elements.len() == 1 && !has_trailing_comma {
                match elements.into_iter().next().unwrap() {
                    ListElement::NonEmpty(element) => Term::Parenthesized {
                        inner: Box::new(element),
                    },
                    ListElement::Empty { .. } => unreachable!(),
                }
            } else {
                Term::Tuple { elements }
            }
        } else if let Some(operator_name) = prefix_operator(&first_token) {
            let operator_pos = self.current_pos();
            self.consume_token()?;
            let opt_operand = self.parse_factor(allow_line_break)?;
            Term::UnaryOperation {
                operand: opt_operand.map(Box::new),
                operator_name,
                operator_pos,
            }
        } else {
            return Ok(None);
        };
        Ok(Some(TermWithPos {
            term,
            pos: self.range_from(start),
        }))
    }

    fn parse_list_elements_and_trailing_comma(
        &mut self,
    ) -> Result<(Vec<ListElement>, bool), ParseError> {
        let mut elements = Vec::new();
        loop {
            let element = self.parse_assign(true)?;
            if let Some(Token::Comma) = self.current.token {
                if let Some(element) = element {
                    elements.push(ListElement::NonEmpty(element));
                } else {
                    elements.push(ListElement::Empty {
                        comma_pos: self.current_pos(),
                    })
                }
                self.consume_token()?;
            } else {
                let has_trailing_comma = match element {
                    Some(element) => {
                        elements.push(ListElement::NonEmpty(element));
                        false
                    }
                    None => true,
                };
                return Ok((elements, has_trailing_comma));
            }
        }
    }
}

fn prefix_operator(token: &Token) -> Option<&'static str> {
    match token {
        Token::Plus => Some("plus"),
        Token::Hyphen => Some("minus"),
        Token::Slash => Some("reciprocal"),
        Token::Exclamation => Some("logical_not"),
        Token::Tilde => Some("bitwise_not"),
        _ => None,
    }
}

/**
 * Precedence of binary operators.
 */
#[derive(Clone, Copy, Sequence)]
enum Precedence {
    LogicalOr,
    LogicalAnd,
    Equality,
    Inequality,
    BitOr,
    BitXor,
    BitAnd,
    BitShift,
    AddSub,
    MulDivRem,
    TimeShift,
}

fn infix_operator(token: &Token, precedence: Precedence) -> Option<&'static str> {
    match (token, precedence) {
        (Token::Asterisk, Precedence::MulDivRem) => Some("mul"),
        (Token::Slash, Precedence::MulDivRem) => Some("div"),
        (Token::Percent, Precedence::MulDivRem) => Some("rem"),
        (Token::Plus, Precedence::AddSub) => Some("add"),
        (Token::Hyphen, Precedence::AddSub) => Some("sub"),
        (Token::DoubleGreater, Precedence::BitShift) => Some("right_shift"),
        (Token::DoubleLess, Precedence::BitShift) => Some("left_shift"),
        (Token::Ampersand, Precedence::BitAnd) => Some("bitwise_and"),
        (Token::Circumflex, Precedence::BitXor) => Some("bitwise_xor"),
        (Token::Bar, Precedence::BitOr) => Some("bitwise_or"),
        (Token::Greater, Precedence::Inequality) => Some("greater"),
        (Token::GreaterEqual, Precedence::Inequality) => Some("greater_or_equal"),
        (Token::Less, Precedence::Inequality) => Some("less"),
        (Token::LessEqual, Precedence::Inequality) => Some("less_or_equal"),
        (Token::DoubleEqual, Precedence::Equality) => Some("equal"),
        (Token::ExclamationEqual, Precedence::Equality) => Some("not_equal"),
        _ => None,
    }
}

fn assignment_operator(token: &Token) -> Option<&'static str> {
    match token {
        Token::Equal => Some("assign"),
        Token::PlusEqual => Some("add_assign"),
        Token::HyphenEqual => Some("sub_assign"),
        Token::AsteriskEqual => Some("mul_assign"),
        Token::SlashEqual => Some("div_assign"),
        Token::PercentEqual => Some("rem_assign"),
        Token::DoubleGreaterEqual => Some("right_shift_assign"),
        Token::DoubleLessEqual => Some("left_shift_assign"),
        Token::AmpersandEqual => Some("bitwise_and_assign"),
        Token::CircumflexEqual => Some("bitwise_xor_assign"),
        Token::BarEqual => Some("bitwise_or_assign"),
        _ => None,
    }
}

impl Parser<'_, '_> {
    /**
     * A shorthand to get the [`Pos`] of the current token.
     */
    fn current_pos(&self) -> Pos {
        Pos {
            file: self.file_index,
            start: self.current.start,
            end: self.iter.index(),
        }
    }
    /**
     * A shorthand to get the range from the given `start` to
     * [`Self::prev_end`].
     */
    fn range_from(&self, start: Index) -> Pos {
        Pos {
            file: self.file_index,
            start,
            end: self.prev_end,
        }
    }
    /**
     * A shorthand to call [`read_token`] and update [`Self::prev_end`] and
     * [`Self::current`].
     */
    fn consume_token(&mut self) -> Result<(), ParseError> {
        self.prev_end = self.iter.index();
        self.current = read_token(&mut self.iter, false, self.file_index)?;
        Ok(())
    }
}

/**
 * Reads a token.
 *
 * # Errors
 * - [`ParseError::UnexpectedCharacter`]: The first non-whitespace character
 *   is invalid as the beginning of a token.
 * - [`ParseError::UnterminatedStringLiteral`]: EOF is reached while reading
 *   a string literal.
 * - [`ParseError::InvalidEscapeSequence`]: Invalid character after a
 *   backslash `\` in a string literal.
 * - [`ParseError::UnexpectedTokenInStringLiteral`]: Unexpected token while
 *   reading a placeholder `${` ... `}` in a string literal.
 * - [`ParseError::InvalidBlockComment`]: `is_on_new_line` is `false` when a
 *   block comment starts.
 */
fn read_token(
    iter: &mut CharsPeekable,
    mut is_on_new_line: bool,
    file_index: usize,
) -> Result<TokenInfo, ParseError> {
    let (start_index, first_ch) = loop {
        let Some(ch) = iter.peek() else {
            return Ok(TokenInfo {
                token: None,
                start: iter.index(),
                is_on_new_line,
            });
        };
        if ch.is_ascii_whitespace() {
            if ch == '\n' {
                is_on_new_line = true
            }
            iter.consume();
        } else {
            break (iter.index(), ch);
        }
    };
    iter.consume();
    let token = match first_ch {
        '0'..='9' => {
            let mut value = first_ch.to_string();
            let mut after_e = false;
            while let Some(ch) = iter.peek() {
                after_e = match ch {
                    'e' | 'E' => true,
                    '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' => false,
                    '+' | '-' if after_e => false,
                    _ => break,
                };
                if ch != '_' {
                    value.push(ch);
                }
                iter.consume();
            }
            Token::Digits(value)
        }
        '"' => {
            let mut components = Vec::new();
            let mut string = String::new();
            loop {
                let Some(ch1) = iter.peek() else {
                    return Err(ParseError::UnterminatedStringLiteral(Pos {
                        file: file_index,
                        start: start_index,
                        end: iter.index(),
                    }));
                };
                let index1 = iter.index();
                iter.consume();
                match ch1 {
                    '$' => {
                        let index2 = iter.index();
                        if !string.is_empty() {
                            components
                                .push(StringLiteralComponent::String(std::mem::take(&mut string)));
                        }
                        // Since the usage of format strings is undecided, the current
                        // implementation is kept simple for now.
                        let mut format = String::new();
                        loop {
                            let Some(ch2) = iter.peek() else {
                                return Err(ParseError::UnterminatedStringLiteral(Pos {
                                    file: file_index,
                                    start: start_index,
                                    end: iter.index(),
                                }));
                            };
                            iter.consume();
                            match ch2 {
                                '"' => todo!(),
                                '{' => break,
                                ch => format.push(ch),
                            }
                        }
                        let mut parser = {
                            let start = iter.index();
                            let first_token = read_token(iter, false, file_index)?;
                            Parser {
                                iter,
                                current: first_token,
                                prev_end: start,
                                file_index,
                            }
                        };
                        let value = parser.parse_disjunction(true)?;
                        match parser.current.token {
                            Some(Token::ClosingBrace) => {
                                components
                                    .push(StringLiteralComponent::PlaceHolder { format, value });
                            }
                            Some(_) => {
                                return Err(ParseError::UnexpectedTokenInStringLiteral {
                                    unexpected_token_pos: parser.current_pos(),
                                    dollar_pos: Pos {
                                        file: file_index,
                                        start: index1,
                                        end: iter.index(),
                                    },
                                });
                            }
                            None => {
                                return Err(ParseError::UnterminatedStringLiteral(Pos {
                                    file: file_index,
                                    start: start_index,
                                    end: iter.index(),
                                }));
                            }
                        }
                    }
                    '\\' => {
                        let Some(ch) = iter.peek() else {
                            return Err(ParseError::UnterminatedStringLiteral(Pos {
                                file: file_index,
                                start: start_index,
                                end: iter.index(),
                            }));
                        };
                        iter.consume();
                        string.push(match ch {
                            'n' => '\n',
                            'r' => '\r',
                            't' => '\t',
                            '"' => '\"',
                            '\\' => '\\',
                            '0' => '\0',
                            '\'' => '\'',
                            _ => {
                                return Err(ParseError::InvalidEscapeSequence(Pos {
                                    file: file_index,
                                    start: index1,
                                    end: iter.index(),
                                }))
                            }
                        });
                    }
                    '"' => {
                        if !string.is_empty() {
                            components
                                .push(StringLiteralComponent::String(std::mem::take(&mut string)));
                        }
                        break Token::StringLiteral(components);
                    }
                    ch => string.push(ch),
                }
            }
        }
        _ if first_ch == '_' || unicode_ident::is_xid_start(first_ch) => {
            let mut name = first_ch.to_string();
            while let Some(ch) = iter.peek() {
                if unicode_ident::is_xid_continue(ch) {
                    name.push(ch);
                    iter.consume();
                } else {
                    break;
                }
            }
            match name.as_str() {
                "import" => Token::KeywordImport,
                "export" => Token::KeywordExport,
                "struct" => Token::KeywordStruct,
                "func" => Token::KeywordFunc,
                "method" => Token::KeywordMethod,
                "if" => Token::KeywordIf,
                "else" => Token::KeywordElse,
                "while" => Token::KeywordWhile,
                "break" => Token::KeywordBreak,
                "continue" => Token::KeywordContinue,
                "return" => Token::KeywordReturn,
                "end" => Token::KeywordEnd,
                "var" => Token::KeywordVar,
                "int" => Token::KeywordInt,
                "float" => Token::KeywordFloat,
                "_" => Token::Underscore,
                _ => Token::Identifier(name),
            }
        }
        '+' => {
            if iter.consume_if('=') {
                Token::PlusEqual
            } else {
                Token::Plus
            }
        }
        '-' => {
            if iter.consume_if('-') {
                skip_line_comment(iter);
                return read_token(iter, true, file_index);
            } else if iter.consume_if('=') {
                Token::HyphenEqual
            } else if iter.consume_if('>') {
                Token::HyphenGreater
            } else {
                Token::Hyphen
            }
        }
        '*' => {
            if iter.consume_if('=') {
                Token::AsteriskEqual
            } else {
                Token::Asterisk
            }
        }
        '/' => {
            if iter.consume_if('-') {
                skip_block_comment(iter, '/', '-', '-', '/', start_index, file_index)?;
                return read_token(iter, is_on_new_line, file_index);
            } else if iter.consume_if('/') {
                if !is_on_new_line {
                    return Err(ParseError::InvalidBlockComment {
                        start_pos: Pos {
                            file: file_index,
                            start: start_index,
                            end: iter.index(),
                        },
                    });
                }
                skip_block_comment(iter, '/', '/', '\\', '\\', start_index, file_index)?;
                skip_line_comment(iter);
                return read_token(iter, true, file_index);
            } else if iter.consume_if('=') {
                Token::SlashEqual
            } else {
                Token::Slash
            }
        }
        '%' => {
            if iter.consume_if('=') {
                Token::PercentEqual
            } else {
                Token::Percent
            }
        }
        '=' => {
            if iter.consume_if('=') {
                Token::DoubleEqual
            } else if iter.consume_if('>') {
                Token::EqualGreater
            } else {
                Token::Equal
            }
        }
        '!' => {
            if iter.consume_if('=') {
                Token::ExclamationEqual
            } else {
                Token::Exclamation
            }
        }
        '>' => {
            if iter.consume_if('>') {
                if iter.consume_if('=') {
                    Token::DoubleGreaterEqual
                } else {
                    Token::DoubleGreater
                }
            } else if iter.consume_if('=') {
                Token::GreaterEqual
            } else {
                Token::Greater
            }
        }
        '<' => {
            if iter.consume_if('<') {
                if iter.consume_if('=') {
                    Token::DoubleLessEqual
                } else {
                    Token::DoubleLess
                }
            } else if iter.consume_if('=') {
                Token::LessEqual
            } else {
                Token::Less
            }
        }
        '&' => {
            if iter.consume_if('&') {
                Token::DoubleAmpersand
            } else if iter.consume_if('=') {
                Token::AmpersandEqual
            } else {
                Token::Ampersand
            }
        }
        '|' => {
            if iter.consume_if('|') {
                Token::DoubleBar
            } else if iter.consume_if('=') {
                Token::BarEqual
            } else {
                Token::Bar
            }
        }
        '^' => {
            if iter.consume_if('=') {
                Token::CircumflexEqual
            } else {
                Token::Circumflex
            }
        }
        ':' => Token::Colon,
        ';' => Token::Semicolon,
        ',' => Token::Comma,
        '?' => Token::Question,
        '~' => Token::Tilde,
        '(' => Token::OpeningParenthesis,
        ')' => Token::ClosingParenthesis,
        '[' => Token::OpeningBracket,
        ']' => Token::ClosingBracket,
        '{' => Token::OpeningBrace,
        '}' => Token::ClosingBrace,
        '.' => Token::Dot,
        '$' => Token::Dollar,
        _ => {
            return Err(ParseError::UnexpectedCharacter(Pos {
                file: file_index,
                start: start_index,
                end: iter.index(),
            }))
        }
    };
    Ok(TokenInfo {
        token: Some(token),
        start: start_index,
        is_on_new_line,
    })
}

/**
 * Skips until the end of line.
 */
fn skip_line_comment(iter: &mut CharsPeekable) {
    loop {
        let ch = iter.peek();
        iter.consume();
        if let None | Some('\n') = ch {
            break;
        }
    }
}

/**
 * Skips over a block comment.
 *
 * A block comment starts with two consecutive characters `start0` and
 * `start1`, and ends with two consecutive characters `end0` and `end1`.
 * Block comments can be nested.
 *
 * # Errors
 * - [`ParseError::UnterminatedComment`]: EOF is reached before a matching
 *   end sequence is found.
 */
fn skip_block_comment(
    iter: &mut CharsPeekable,
    start0: char,
    start1: char,
    end0: char,
    end1: char,
    start_index: Index,
    file_index: usize,
) -> Result<(), ParseError> {
    let mut start_indices = vec![start_index];
    loop {
        let Some(ch) = iter.peek() else {
            return Err(ParseError::UnterminatedComment(
                start_indices
                    .into_iter()
                    .map(|start_index| Pos {
                        file: file_index,
                        start: start_index,
                        end: iter.index(),
                    })
                    .collect(),
            ));
        };
        let index = iter.index();
        iter.consume();
        if ch == start0 && iter.consume_if(start1) {
            start_indices.push(index);
        } else if ch == end0 && iter.consume_if(end1) {
            start_indices.pop();
            if start_indices.is_empty() {
                return Ok(());
            }
        }
    }
}
