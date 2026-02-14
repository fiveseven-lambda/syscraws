/*
 * Copyright (c) 2023-2026 Atsushi Komaba
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

use std::fmt::{self, Display, Formatter};
use std::io::Write;
use std::ops::Range;
use std::path::{Path, PathBuf};

pub struct Logger {
    pub writer: Box<dyn Write>,
    pub num_errors: usize,
    pub files: Vec<File>,
}

impl Logger {
    pub fn new(writer: Box<dyn Write>) -> Logger {
        Logger {
            writer,
            num_errors: 0,
            files: Vec::new(),
        }
    }

    /**
     * Called by [`frontend::read_input`](crate::frontend::read_input).
     */
    pub fn root_file_not_found(&mut self, path: &Path, err: std::io::Error) {
        writeln!(
            self.writer,
            "ERROR: File `{}` not found. {}",
            path.display(),
            err
        )
        .unwrap();
    }

    /**
     * Called by [`frontend::read_input`](crate::frontend::read_input).
     */
    pub fn cannot_read_root_file(&mut self, path: &Path, err: std::io::Error) {
        writeln!(
            self.writer,
            "ERROR: Cannot read file `{}`. {}",
            path.display(),
            err
        )
        .unwrap();
        self.num_errors += 1;
    }

    /**
     * Prints a final message before exiting.
     */
    pub fn aborting(&mut self) {
        writeln!(
            self.writer,
            "Aborting due to {} previous {}.",
            self.num_errors,
            if self.num_errors == 1 {
                "error"
            } else {
                "errors"
            }
        )
        .unwrap();
    }
}

pub struct File {
    pub path: PathBuf,
    pub content: String,
    pub lines: Vec<Range<usize>>,
}

impl Logger {
    pub fn quote(
        &mut self,
        Pos {
            file: file_index,
            start,
            end,
        }: Pos,
    ) {
        let file = &self.files[file_index];
        match end.line - start.line {
            0 => {
                let line = &file.content[file.lines[start.line].clone()];
                writeln!(
                    self.writer,
                    "L{}: {} !-> {} <-! {}",
                    start.line + 1,
                    &line[..start.column],
                    &line[start.column..end.column],
                    &line[end.column..],
                )
                .unwrap();
            }
            1 => {
                let start_line = &file.content[file.lines[start.line].clone()];
                let end_line = &file.content[file.lines[end.line].clone()];
                writeln!(
                    self.writer,
                    "L{}: {} !-> {}",
                    start.line + 1,
                    &start_line[..start.column],
                    &start_line[start.column..],
                )
                .unwrap();
                writeln!(
                    self.writer,
                    "L{}: {} <-! {}",
                    end.line + 1,
                    &end_line[..end.column],
                    &end_line[end.column..],
                )
                .unwrap();
            }
            2 => {
                let start_line = &file.content[file.lines[start.line].clone()];
                let mid_line = &file.content[file.lines[start.line + 1].clone()];
                let end_line = &file.content[file.lines[end.line].clone()];
                writeln!(
                    self.writer,
                    "L{}: {} !-> {}",
                    start.line + 1,
                    &start_line[..start.column],
                    &start_line[start.column..],
                )
                .unwrap();
                writeln!(self.writer, "L{}: {}", start.line + 2, mid_line).unwrap();
                writeln!(
                    self.writer,
                    "L{}: {} <-! {}",
                    end.line + 1,
                    &end_line[..end.column],
                    &end_line[end.column..],
                )
                .unwrap();
            }
            num_lines => {
                let start_line = &file.content[file.lines[start.line].clone()];
                let end_line = &file.content[file.lines[end.line].clone()];
                writeln!(
                    self.writer,
                    "L{}: {} !-> {}",
                    start.line + 1,
                    &start_line[..start.column],
                    &start_line[start.column..],
                )
                .unwrap();
                writeln!(self.writer, "({} lines)", num_lines - 1).unwrap();
                writeln!(
                    self.writer,
                    "L{}: {} <-! {}",
                    end.line + 1,
                    &end_line[..end.column],
                    &end_line[end.column..],
                )
                .unwrap();
            }
        }
        writeln!(self.writer).unwrap();
    }
}

#[derive(Debug)]
pub enum ParseError {
    /// Returned by [`read_token`](../frontend/ast/fn.read_token.html).
    UnexpectedCharacter(Pos),
    UnterminatedComment(Pos),
    /// Returned by [`read_token`](../frontend/ast/fn.read_token.html).
    UnterminatedStringLiteral(Pos),
    /// Returned by [`read_token`](../frontend/ast/fn.read_token.html).
    InvalidEscapeSequence(Pos),
    /// Returned by [`read_token`](../frontend/ast/fn.read_token.html).
    UnexpectedTokenInStringLiteral {
        unexpected_token_pos: Pos,
        dollar_pos: Pos,
    },
    /// Returned by [`read_token`](../frontend/ast/fn.read_token.html).
    InvalidBlockComment {
        start_pos: Pos,
    },
    UnexpectedToken(Pos),
    UnexpectedTokenAfterKeywordFuncOrMethod {
        unexpected_token_pos: Pos,
        keyword_pos: Pos,
    },
    UnexpectedTokenAfterKeywordStruct {
        unexpected_token_pos: Pos,
        keyword_struct_pos: Pos,
    },
    UnclosedBlock {
        pos: Pos,
        starts_pos: Vec<Pos>,
    },
    UnexpectedTokenAfterDot {
        unexpected_token_pos: Pos,
        dot_pos: Pos,
    },
    MissingFieldAfterDot {
        dot_pos: Pos,
    },
    UnexpectedTokenInParentheses {
        unexpected_token_pos: Pos,
        opening_parenthesis_pos: Pos,
    },
    UnclosedParenthesis {
        opening_parenthesis_pos: Pos,
    },
    UnexpectedTokenInBrackets {
        unexpected_token_pos: Pos,
        opening_bracket_pos: Pos,
    },
    UnclosedBracket {
        opening_bracket_pos: Pos,
    },
}

impl Logger {
    pub fn parse_error(&mut self, err: ParseError) {
        match err {
            ParseError::UnexpectedCharacter(pos) => {
                writeln!(self.writer, "{}", self.files[pos.file].path.display()).unwrap();
                writeln!(self.writer, "Unexpected character at {pos}.").unwrap();
                self.quote(pos);
            }
            ParseError::UnterminatedStringLiteral(pos) => {
                writeln!(self.writer, "{}", self.files[pos.file].path.display()).unwrap();
                writeln!(self.writer, "Unterminated string literal started at {pos}.").unwrap();
                self.quote(pos);
            }
            ParseError::InvalidEscapeSequence(pos) => {
                writeln!(self.writer, "{}", self.files[pos.file].path.display()).unwrap();
                writeln!(self.writer, "Invalid escape squence at {pos}.").unwrap();
                self.quote(pos);
            }
            ParseError::UnexpectedTokenInStringLiteral {
                unexpected_token_pos,
                dollar_pos,
            } => {
                writeln!(
                    self.writer,
                    "{}",
                    self.files[unexpected_token_pos.file].path.display()
                )
                .unwrap();
                writeln!(self.writer, "Unexpected token at {unexpected_token_pos}.").unwrap();
                self.quote(unexpected_token_pos);
                writeln!(
                    self.writer,
                    "Note: A placeholder in string literal started at {dollar_pos}."
                )
                .unwrap();
                self.quote(dollar_pos);
            }
            ParseError::UnterminatedComment(comment_pos) => {
                writeln!(
                    self.writer,
                    "{}",
                    self.files[comment_pos.file].path.display()
                )
                .unwrap();
                writeln!(self.writer, "Unterminated comment at {comment_pos}:").unwrap();
                self.quote(comment_pos);
            }
            ParseError::InvalidBlockComment { start_pos } => {
                writeln!(self.writer, "{}", self.files[start_pos.file].path.display()).unwrap();
                writeln!(
                    self.writer,
                    "A block comment must start at the beginning of the line, allowing only \
                     leading whitespaces."
                )
                .unwrap();
                self.quote(start_pos);
            }
            ParseError::UnexpectedToken(unexpected_token_pos) => {
                writeln!(self.writer, "Unexpected token at {}.", unexpected_token_pos).unwrap();
                self.quote(unexpected_token_pos);
            }
            ParseError::UnexpectedTokenAfterKeywordStruct {
                unexpected_token_pos,
                keyword_struct_pos,
            } => {
                writeln!(self.writer, "Unexpected token at {}.", unexpected_token_pos).unwrap();
                self.quote(unexpected_token_pos);
                writeln!(
                    self.writer,
                    "Expected an identifier after `struct` at {}.",
                    keyword_struct_pos
                )
                .unwrap();
                self.quote(keyword_struct_pos);
            }
            ParseError::UnexpectedTokenAfterKeywordFuncOrMethod {
                unexpected_token_pos,
                keyword_pos,
            } => {
                writeln!(self.writer, "Unexpected token at {}.", unexpected_token_pos).unwrap();
                self.quote(unexpected_token_pos);
                writeln!(
                    self.writer,
                    "Expected an identifier after keyword at {}.",
                    keyword_pos
                )
                .unwrap();
                self.quote(keyword_pos);
            }
            ParseError::UnclosedBlock { pos, starts_pos } => {
                writeln!(self.writer, "Blocks are unclosed at {}.", pos).unwrap();
                self.quote(pos);
                writeln!(self.writer, "Blocks opened at:").unwrap();
                for start_pos in &starts_pos {
                    self.quote(start_pos.clone());
                }
            }
            ParseError::MissingFieldAfterDot { dot_pos } => {
                writeln!(
                    self.writer,
                    "Missing field name or number after `.` at {dot_pos}."
                )
                .unwrap();
                self.quote(dot_pos);
            }
            ParseError::UnexpectedTokenAfterDot {
                unexpected_token_pos,
                dot_pos,
            } => {
                writeln!(self.writer, "Unexpected token at {}.", unexpected_token_pos).unwrap();
                self.quote(unexpected_token_pos);
                writeln!(
                    self.writer,
                    "Note: expected a field name or number after `.` at {dot_pos}."
                )
                .unwrap();
                self.quote(dot_pos);
            }
            ParseError::UnexpectedTokenInParentheses {
                unexpected_token_pos,
                opening_parenthesis_pos,
            } => {
                writeln!(self.writer, "Unexpected token at {}.", unexpected_token_pos).unwrap();
                self.quote(unexpected_token_pos);
                writeln!(
                    self.writer,
                    "Note: opening parenthesis at {}.",
                    opening_parenthesis_pos
                )
                .unwrap();
                self.quote(opening_parenthesis_pos);
            }
            ParseError::UnclosedParenthesis {
                opening_parenthesis_pos,
            } => {
                writeln!(
                    self.writer,
                    "Unclosed parenthesis opened at {}.",
                    opening_parenthesis_pos
                )
                .unwrap();
                self.quote(opening_parenthesis_pos);
            }
            ParseError::UnexpectedTokenInBrackets {
                unexpected_token_pos,
                opening_bracket_pos,
            } => {
                writeln!(self.writer, "Unexpected token at {}.", unexpected_token_pos).unwrap();
                self.quote(unexpected_token_pos);
                writeln!(
                    self.writer,
                    "Note: opening bracket at {}.",
                    opening_bracket_pos
                )
                .unwrap();
                self.quote(opening_bracket_pos);
            }
            ParseError::UnclosedBracket {
                opening_bracket_pos,
            } => {
                writeln!(
                    self.writer,
                    "Unclosed bracket opened at {}.",
                    opening_bracket_pos
                )
                .unwrap();
                self.quote(opening_bracket_pos);
            }
        }
        self.num_errors += 1;
    }

    pub fn extra_tokens(&mut self, pos: Pos) {
        writeln!(self.writer, "Extra tokens at {}.", pos).unwrap();
        self.quote(pos);
        self.num_errors += 1;
    }

    pub fn expected_lvalue(&mut self, pos: Pos) {
        writeln!(self.writer, "Expression at {} is not a lvalue", pos).unwrap();
        self.quote(pos);
        self.num_errors += 1;
    }

    pub fn cannot_parse_integer(&mut self, pos: Pos, err: std::num::ParseIntError) {
        writeln!(
            self.writer,
            "Cannot parse integer literal at {}: {}",
            pos, err
        )
        .unwrap();
        self.quote(pos);
        self.num_errors += 1;
    }

    pub fn cannot_parse_float(&mut self, pos: Pos, err: std::num::ParseFloatError) {
        writeln!(
            self.writer,
            "Cannot parse float literal at {}: {}",
            pos, err
        )
        .unwrap();
        self.quote(pos);
        self.num_errors += 1;
    }

    pub fn duplicate_definition(&mut self, pos: Pos, prev_pos: Pos) {
        writeln!(self.writer, "Duplicate definition at {}.", pos).unwrap();
        self.quote(pos);
        writeln!(self.writer, "Previously defined at {}.", prev_pos).unwrap();
        self.quote(prev_pos);
        self.num_errors += 1;
    }

    pub fn missing_import_target(&mut self, keyword_import_pos: Pos) {
        writeln!(
            self.writer,
            "Missing import target after keyword `import` at {keyword_import_pos}"
        )
        .unwrap();
        self.quote(keyword_import_pos);
        self.num_errors += 1;
    }

    pub fn invalid_import_target(&mut self, pos: Pos) {
        writeln!(self.writer, "Invalid import target at {pos}").unwrap();
        self.quote(pos);
        self.num_errors += 1;
    }

    pub fn placeholder_in_import_path(&mut self, pos: Pos) {
        writeln!(self.writer, "Import path at {pos} contains a placeholder").unwrap();
        self.quote(pos);
        self.num_errors += 1;
    }

    pub fn empty_argument(&mut self, comma_pos: Pos) {
        writeln!(self.writer, "Empty argument before comma at {comma_pos}").unwrap();
        self.quote(comma_pos);
        self.num_errors += 1;
    }

    pub fn cannot_read_file(&mut self, pos: Pos, path: &Path, err: std::io::Error) {
        writeln!(
            self.writer,
            "Cannot read file `{}`. {}",
            path.display(),
            err
        )
        .unwrap();
        self.quote(pos);
        self.num_errors += 1;
    }

    pub fn circular_imports(&mut self, pos: Pos, path: &Path) {
        writeln!(self.writer, "Circular imports of `{}`.", path.display()).unwrap();
        self.quote(pos);
        self.num_errors += 1;
    }

    pub fn missing_structure_name(&mut self, keyword_struct_pos: Pos) {
        writeln!(
            self.writer,
            "Missing structure name after `struct` at {}.",
            keyword_struct_pos
        )
        .unwrap();
        self.quote(keyword_struct_pos);
        self.num_errors += 1;
    }

    pub fn missing_function_name(&mut self, keyword_pos: Pos) {
        writeln!(
            self.writer,
            "Missing function name after keyword at {}.",
            keyword_pos
        )
        .unwrap();
        self.quote(keyword_pos);
        self.num_errors += 1;
    }

    pub fn empty_ty_parameter(&mut self, comma_pos: Pos) {
        writeln!(
            self.writer,
            "Empty type parameter before comma at {comma_pos}"
        )
        .unwrap();
        self.quote(comma_pos);
        self.num_errors += 1;
    }

    pub fn invalid_ty_parameter(&mut self, pos: Pos) {
        writeln!(self.writer, "Invalid type parameter at {}.", pos).unwrap();
        self.quote(pos);
        self.num_errors += 1;
    }

    pub fn empty_parameter(&mut self, comma_pos: Pos) {
        writeln!(self.writer, "Empty parameter before comma at {comma_pos}").unwrap();
        self.quote(comma_pos);
        self.num_errors += 1;
    }

    pub fn invalid_parameter(&mut self, pos: Pos) {
        writeln!(self.writer, "Invalid parameter at {}.", pos).unwrap();
        self.quote(pos);
        self.num_errors += 1;
    }

    pub fn invalid_structure_field(&mut self, pos: Pos) {
        writeln!(self.writer, "Invalid structure field at {}.", pos).unwrap();
        self.quote(pos);
        self.num_errors += 1;
    }

    pub fn missing_ty(&mut self, colon_pos: Pos) {
        writeln!(self.writer, "Missing type after colon at {}.", colon_pos).unwrap();
        self.quote(colon_pos);
        self.num_errors += 1;
    }

    pub fn missing_parameter_list(&mut self, signature_pos: Pos) {
        writeln!(
            self.writer,
            "Missing parameter list in function signature at {signature_pos}"
        )
        .unwrap();
        self.quote(signature_pos);
        self.num_errors += 1;
    }

    pub fn missing_variable_name(&mut self, keyword_var_pos: Pos) {
        writeln!(
            self.writer,
            "Missing variable name after keyword `var` at {}.",
            keyword_var_pos
        )
        .unwrap();
        self.quote(keyword_var_pos);
    }

    pub fn invalid_variable_name(&mut self, pos: Pos) {
        writeln!(self.writer, "Invalid variable name at {}.", pos).unwrap();
        self.quote(pos);
    }

    pub fn expected_expression(&mut self, pos: Pos) {
        writeln!(self.writer, "Expected an expression at {pos}").unwrap();
        self.quote(pos);
        self.num_errors += 1;
    }

    pub fn expected_ty(&mut self, pos: Pos) {
        writeln!(self.writer, "Expected a type at {pos}").unwrap();
        self.quote(pos);
        self.num_errors += 1;
    }

    pub fn expected_function(&mut self, pos: Pos) {
        writeln!(self.writer, "The expression at {pos} is not a function").unwrap();
        self.quote(pos);
        self.num_errors += 1;
    }

    pub fn missing_if_condition(&mut self, keyword_if_pos: Pos) {
        writeln!(
            self.writer,
            "Missing condition after keyword `if` at {keyword_if_pos}"
        )
        .unwrap();
        self.quote(keyword_if_pos);
        self.num_errors += 1;
    }

    pub fn missing_while_condition(&mut self, keyword_while_pos: Pos) {
        writeln!(
            self.writer,
            "Missing condition after keyword `while` at {keyword_while_pos}"
        )
        .unwrap();
        self.quote(keyword_while_pos);
        self.num_errors += 1;
    }

    pub fn undefined_variable(&mut self, pos: Pos) {
        writeln!(self.writer, "Undefined variable at {pos}").unwrap();
        self.quote(pos);
        self.num_errors += 1;
    }

    pub fn undefined_item(&mut self, name: &str, pos: Pos, file_index: usize) {
        writeln!(
            self.writer,
            "`{}` is not defined in {}",
            name,
            self.files[file_index].path.display()
        )
        .unwrap();
        self.quote(pos);
        self.num_errors += 1;
    }

    pub fn empty_left_operand(&mut self, operator_pos: Pos) {
        writeln!(
            self.writer,
            "Empty left operand before operator at {operator_pos}"
        )
        .unwrap();
        self.quote(operator_pos);
        self.num_errors += 1;
    }

    pub fn empty_right_operand(&mut self, operator_pos: Pos) {
        writeln!(
            self.writer,
            "Empty right operand after operator at {operator_pos}"
        )
        .unwrap();
        self.quote(operator_pos);
        self.num_errors += 1;
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Pos {
    pub file: usize,
    pub start: Index,
    pub end: Index,
}

impl Display for Pos {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.start.fmt_start(f)?;
        write!(f, "-")?;
        self.end.fmt_end(f)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Index {
    pub line: usize,
    pub column: usize,
}

impl Index {
    fn fmt_start(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.line + 1, self.column + 1)
    }
    fn fmt_end(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.line + 1, self.column)
    }
}

impl Display for Index {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.fmt_start(f)
    }
}
