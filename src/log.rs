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

use std::fmt::{self, Display, Formatter};
use std::io::Write;
use std::ops::Range;
use std::path::{Path, PathBuf};

pub struct Logger {
    pub num_errors: usize,
    pub err: Box<dyn Write>,
}

impl Logger {
    pub fn new(err: Box<dyn Write>) -> Logger {
        Logger { num_errors: 0, err }
    }

    /**
     * Called by [`frontend::read_input`](crate::frontend::read_input).
     */
    pub fn root_file_not_found(&mut self, path: &Path, err: std::io::Error) {
        writeln!(
            self.err,
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
            self.err,
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
            self.err,
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
        files: &[File],
    ) {
        let file = &files[file_index];
        match end.line - start.line {
            0 => {
                let line = &file.content[file.lines[start.line].clone()];
                writeln!(
                    self.err,
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
                    self.err,
                    "L{}: {} !-> {}",
                    start.line + 1,
                    &start_line[..start.column],
                    &start_line[start.column..],
                )
                .unwrap();
                writeln!(
                    self.err,
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
                    self.err,
                    "L{}: {} !-> {}",
                    start.line + 1,
                    &start_line[..start.column],
                    &start_line[start.column..],
                )
                .unwrap();
                writeln!(self.err, "L{}: {}", start.line + 2, mid_line).unwrap();
                writeln!(
                    self.err,
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
                    self.err,
                    "L{}: {} !-> {}",
                    start.line + 1,
                    &start_line[..start.column],
                    &start_line[start.column..],
                )
                .unwrap();
                writeln!(self.err, "({} lines)", num_lines - 1).unwrap();
                writeln!(
                    self.err,
                    "L{}: {} <-! {}",
                    end.line + 1,
                    &end_line[..end.column],
                    &end_line[end.column..],
                )
                .unwrap();
            }
        }
        writeln!(self.err).unwrap();
    }
}

#[derive(Debug)]
pub enum ParseError {
    /// Returned by [`read_token`](../frontend/ast/fn.read_token.html).
    UnexpectedCharacter(Pos),
    /// Returned by
    /// [`skip_block_comment`](../frontend/ast/fn.skip_block_comment.html).
    UnterminatedComment(Vec<Pos>),
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
    pub fn parse_error(&mut self, err: ParseError, files: &[File]) {
        match err {
            ParseError::UnexpectedCharacter(pos) => {
                writeln!(self.err, "{}", files[pos.file].path.display()).unwrap();
                writeln!(self.err, "Unexpected character at {pos}.").unwrap();
                self.quote(pos, files);
            }
            ParseError::UnterminatedStringLiteral(pos) => {
                writeln!(self.err, "{}", files[pos.file].path.display()).unwrap();
                writeln!(self.err, "Unterminated string literal started at {pos}.").unwrap();
                self.quote(pos, files);
            }
            ParseError::InvalidEscapeSequence(pos) => {
                writeln!(self.err, "{}", files[pos.file].path.display()).unwrap();
                writeln!(self.err, "Invalid escape squence at {pos}.").unwrap();
                self.quote(pos, files);
            }
            ParseError::UnexpectedTokenInStringLiteral {
                unexpected_token_pos,
                dollar_pos,
            } => {
                writeln!(
                    self.err,
                    "{}",
                    files[unexpected_token_pos.file].path.display()
                )
                .unwrap();
                writeln!(self.err, "Unexpected token at {unexpected_token_pos}.").unwrap();
                self.quote(unexpected_token_pos, files);
                writeln!(
                    self.err,
                    "Note: A placeholder in string literal started at {dollar_pos}."
                )
                .unwrap();
                self.quote(dollar_pos, files);
            }
            ParseError::UnterminatedComment(comments_pos) => {
                writeln!(self.err, "{}", files[comments_pos[0].file].path.display()).unwrap();
                for pos in comments_pos {
                    writeln!(self.err, "Unterminated comment at {pos}:").unwrap();
                    self.quote(pos, files);
                }
            }
            ParseError::InvalidBlockComment { start_pos } => {
                writeln!(self.err, "{}", files[start_pos.file].path.display()).unwrap();
                writeln!(
                    self.err,
                    "A block comment must start at the beginning of the line, allowing only \
                     leading whitespaces."
                )
                .unwrap();
                self.quote(start_pos, files);
            }
            ParseError::UnexpectedToken(unexpected_token_pos) => {
                writeln!(self.err, "Unexpected token at {}.", unexpected_token_pos).unwrap();
                self.quote(unexpected_token_pos, files);
            }
            ParseError::UnexpectedTokenAfterKeywordStruct {
                unexpected_token_pos,
                keyword_struct_pos,
            } => {
                writeln!(self.err, "Unexpected token at {}.", unexpected_token_pos).unwrap();
                self.quote(unexpected_token_pos, files);
                writeln!(
                    self.err,
                    "Expected an identifier after `struct` at {}.",
                    keyword_struct_pos
                )
                .unwrap();
                self.quote(keyword_struct_pos, files);
            }
            ParseError::UnexpectedTokenAfterKeywordFuncOrMethod {
                unexpected_token_pos,
                keyword_pos,
            } => {
                writeln!(self.err, "Unexpected token at {}.", unexpected_token_pos).unwrap();
                self.quote(unexpected_token_pos, files);
                writeln!(
                    self.err,
                    "Expected an identifier after keyword at {}.",
                    keyword_pos
                )
                .unwrap();
                self.quote(keyword_pos, files);
            }
            ParseError::UnclosedBlock { pos, starts_pos } => {
                writeln!(self.err, "Blocks are unclosed at {}.", pos).unwrap();
                self.quote(pos, files);
                writeln!(self.err, "Blocks opened at:").unwrap();
                for start_pos in &starts_pos {
                    self.quote(start_pos.clone(), files);
                }
            }
            ParseError::MissingFieldAfterDot { dot_pos } => {
                writeln!(
                    self.err,
                    "Missing field name or number after `.` at {dot_pos}."
                )
                .unwrap();
                self.quote(dot_pos, files);
            }
            ParseError::UnexpectedTokenAfterDot {
                unexpected_token_pos,
                dot_pos,
            } => {
                writeln!(self.err, "Unexpected token at {}.", unexpected_token_pos).unwrap();
                self.quote(unexpected_token_pos, files);
                writeln!(
                    self.err,
                    "Note: expected a field name or number after `.` at {dot_pos}."
                )
                .unwrap();
                self.quote(dot_pos, files);
            }
            ParseError::UnexpectedTokenInParentheses {
                unexpected_token_pos,
                opening_parenthesis_pos,
            } => {
                writeln!(self.err, "Unexpected token at {}.", unexpected_token_pos).unwrap();
                self.quote(unexpected_token_pos, files);
                writeln!(
                    self.err,
                    "Note: opening parenthesis at {}.",
                    opening_parenthesis_pos
                )
                .unwrap();
                self.quote(opening_parenthesis_pos, files);
            }
            ParseError::UnclosedParenthesis {
                opening_parenthesis_pos,
            } => {
                writeln!(
                    self.err,
                    "Unclosed parenthesis opened at {}.",
                    opening_parenthesis_pos
                )
                .unwrap();
                self.quote(opening_parenthesis_pos, files);
            }
            ParseError::UnexpectedTokenInBrackets {
                unexpected_token_pos,
                opening_bracket_pos,
            } => {
                writeln!(self.err, "Unexpected token at {}.", unexpected_token_pos).unwrap();
                self.quote(unexpected_token_pos, files);
                writeln!(
                    self.err,
                    "Note: opening bracket at {}.",
                    opening_bracket_pos
                )
                .unwrap();
                self.quote(opening_bracket_pos, files);
            }
            ParseError::UnclosedBracket {
                opening_bracket_pos,
            } => {
                writeln!(
                    self.err,
                    "Unclosed bracket opened at {}.",
                    opening_bracket_pos
                )
                .unwrap();
                self.quote(opening_bracket_pos, files);
            }
        }
        self.num_errors += 1;
    }

    pub fn extra_tokens(&mut self, pos: Pos, files: &[File]) {
        writeln!(self.err, "Extra tokens at {}.", pos).unwrap();
        self.quote(pos, files);
        self.num_errors += 1;
    }

    pub fn not_lvalue(&mut self, pos: Pos, files: &[File]) {
        writeln!(self.err, "Expression at {} is not an lvalue", pos).unwrap();
        self.quote(pos, files);
        self.num_errors += 1;
    }

    pub fn cannot_parse_integer(&mut self, pos: Pos, err: std::num::ParseIntError, files: &[File]) {
        writeln!(self.err, "Cannot parse integer literal at {}: {}", pos, err).unwrap();
        self.quote(pos, files);
        self.num_errors += 1;
    }

    pub fn cannot_parse_float(&mut self, pos: Pos, err: std::num::ParseFloatError, files: &[File]) {
        writeln!(self.err, "Cannot parse float literal at {}: {}", pos, err).unwrap();
        self.quote(pos, files);
        self.num_errors += 1;
    }

    pub fn duplicate_definition(&mut self, pos: Pos, prev_pos: Pos, files: &[File]) {
        writeln!(self.err, "Duplicate definition at {}.", pos).unwrap();
        self.quote(pos, files);
        writeln!(self.err, "Previously defined at {}.", prev_pos).unwrap();
        self.quote(prev_pos, files);
        self.num_errors += 1;
    }

    pub fn missing_import_target(&mut self, keyword_import_pos: Pos, files: &[File]) {
        writeln!(
            self.err,
            "Missing import target after keyword `import` at {keyword_import_pos}"
        )
        .unwrap();
        self.quote(keyword_import_pos, files);
        self.num_errors += 1;
    }

    pub fn invalid_import_target(&mut self, pos: Pos, files: &[File]) {
        writeln!(self.err, "Invalid import target at {pos}").unwrap();
        self.quote(pos, files);
        self.num_errors += 1;
    }

    pub fn placeholder_in_import_path(&mut self, pos: Pos, files: &[File]) {
        writeln!(self.err, "Import path at {pos} contains a placeholder").unwrap();
        self.quote(pos, files);
        self.num_errors += 1;
    }

    pub fn empty_argument(&mut self, comma_pos: Pos, files: &[File]) {
        writeln!(self.err, "Empty argument before comma at {comma_pos}").unwrap();
        self.quote(comma_pos, files);
        self.num_errors += 1;
    }

    pub fn cannot_read_file(&mut self, pos: Pos, path: &Path, err: std::io::Error, files: &[File]) {
        writeln!(self.err, "Cannot read file `{}`. {}", path.display(), err).unwrap();
        self.quote(pos, files);
        self.num_errors += 1;
    }

    pub fn circular_imports(&mut self, pos: Pos, path: &Path, files: &[File]) {
        writeln!(self.err, "Circular imports of `{}`.", path.display()).unwrap();
        self.quote(pos, files);
        self.num_errors += 1;
    }

    pub fn missing_structure_name(&mut self, keyword_struct_pos: Pos, files: &[File]) {
        writeln!(
            self.err,
            "Missing structure name after `struct` at {}.",
            keyword_struct_pos
        )
        .unwrap();
        self.quote(keyword_struct_pos, files);
        self.num_errors += 1;
    }

    pub fn missing_function_name(&mut self, keyword_pos: Pos, files: &[File]) {
        writeln!(
            self.err,
            "Missing function name after keyword at {}.",
            keyword_pos
        )
        .unwrap();
        self.quote(keyword_pos, files);
        self.num_errors += 1;
    }

    pub fn empty_ty_parameter(&mut self, comma_pos: Pos, files: &[File]) {
        writeln!(self.err, "Empty type parameter before comma at {comma_pos}").unwrap();
        self.quote(comma_pos, files);
        self.num_errors += 1;
    }

    pub fn invalid_ty_parameter(&mut self, pos: Pos, files: &[File]) {
        writeln!(self.err, "Invalid type parameter at {}.", pos).unwrap();
        self.quote(pos, files);
        self.num_errors += 1;
    }

    pub fn empty_parameter(&mut self, comma_pos: Pos, files: &[File]) {
        writeln!(self.err, "Empty parameter before comma at {comma_pos}").unwrap();
        self.quote(comma_pos, files);
        self.num_errors += 1;
    }

    pub fn invalid_parameter(&mut self, pos: Pos, files: &[File]) {
        writeln!(self.err, "Invalid parameter at {}.", pos).unwrap();
        self.quote(pos, files);
        self.num_errors += 1;
    }

    pub fn invalid_structure_field(&mut self, pos: Pos, files: &[File]) {
        writeln!(self.err, "Invalid structure field at {}.", pos).unwrap();
        self.quote(pos, files);
        self.num_errors += 1;
    }

    pub fn missing_ty(&mut self, colon_pos: Pos, files: &[File]) {
        writeln!(self.err, "Missing type after colon at {}.", colon_pos).unwrap();
        self.quote(colon_pos, files);
        self.num_errors += 1;
    }

    pub fn missing_parameter_list(&mut self, files: &[File]) {
        writeln!(self.err, "Missing parameter list").unwrap();
        self.num_errors += 1;
    }

    pub fn missing_variable_name(&mut self, keyword_var_pos: Pos, files: &[File]) {
        writeln!(
            self.err,
            "Missing variable name after keyword `var` at {}.",
            keyword_var_pos
        )
        .unwrap();
        self.quote(keyword_var_pos, files);
    }

    pub fn invalid_variable_name(&mut self, pos: Pos, files: &[File]) {
        writeln!(self.err, "Invalid variable name at {}.", pos).unwrap();
        self.quote(pos, files);
    }

    pub fn not_expression(&mut self, pos: Pos, files: &[File]) {
        writeln!(self.err, "Expected an expression at {pos}").unwrap();
        self.quote(pos, files);
        self.num_errors += 1;
    }

    pub fn not_ty(&mut self, pos: Pos, files: &[File]) {
        writeln!(self.err, "Expected a type at {pos}").unwrap();
        self.quote(pos, files);
        self.num_errors += 1;
    }

    pub fn not_function(&mut self, pos: Pos, files: &[File]) {
        writeln!(self.err, "The expression at {pos} is not a function").unwrap();
        self.quote(pos, files);
        self.num_errors += 1;
    }

    pub fn missing_if_condition(&mut self, keyword_if_pos: Pos, files: &[File]) {
        writeln!(
            self.err,
            "Missing condition after keyword `if` at {keyword_if_pos}"
        )
        .unwrap();
        self.quote(keyword_if_pos, files);
        self.num_errors += 1;
    }

    pub fn missing_while_condition(&mut self, keyword_while_pos: Pos, files: &[File]) {
        writeln!(
            self.err,
            "Missing condition after keyword `while` at {keyword_while_pos}"
        )
        .unwrap();
        self.quote(keyword_while_pos, files);
        self.num_errors += 1;
    }

    pub fn undefined_variable(&mut self, pos: Pos, files: &[File]) {
        writeln!(self.err, "Undefined variable at {pos}").unwrap();
        self.quote(pos, files);
        self.num_errors += 1;
    }

    pub fn undefined_item(&mut self, name: &str, pos: Pos, file_index: usize, files: &[File]) {
        writeln!(
            self.err,
            "`{}` is not defined in {}",
            name,
            files[file_index].path.display()
        )
        .unwrap();
        self.quote(pos, files);
        self.num_errors += 1;
    }

    pub fn empty_left_operand(&mut self, operator_pos: Pos, files: &[File]) {
        writeln!(
            self.err,
            "Empty left operand before operator at {operator_pos}"
        )
        .unwrap();
        self.quote(operator_pos, files);
        self.num_errors += 1;
    }

    pub fn empty_right_operand(&mut self, operator_pos: Pos, files: &[File]) {
        writeln!(
            self.err,
            "Empty right operand after operator at {operator_pos}"
        )
        .unwrap();
        self.quote(operator_pos, files);
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
