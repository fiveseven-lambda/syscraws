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
 * Defines the Abstract Syntax Tree (AST).
 */

use crate::log::Pos;

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
    Return {
        value: Option<TermWithPos>,
    },
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
#[derive(Debug, PartialEq, Eq)]
pub struct TermWithPos {
    pub term: Term,
    pub pos: Pos,
}

/**
 * A term in the AST, representing an expression, a type, or an import name.
 */
#[derive(Debug, PartialEq, Eq)]
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
#[derive(Debug, PartialEq, Eq)]
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
#[derive(Debug, PartialEq, Eq)]
pub enum ListElement {
    NonEmpty(TermWithPos),
    Empty { comma_pos: Pos },
}
