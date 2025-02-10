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
use std::ops::Range;
use std::path::{Path, PathBuf};

/**
 * Called by [`frontend::read_input`](crate::frontend::read_input).
 */
pub fn root_file_not_found(path: &Path, err: std::io::Error) {
    eprintln!("ERROR: File `{}` not found. {}", path.display(), err);
}

/**
 * Called by [`frontend::read_input`](crate::frontend::read_input).
 */
pub fn cannot_read_root_file(path: &Path, err: std::io::Error) {
    eprintln!("ERROR: Cannot read file `{}`. {}", path.display(), err);
}

/**
 * Prints a final message before exiting.
 */
pub fn aborting(num_errors: u32) {
    eprintln!("Aborting due to {num_errors} previous errors.");
}

pub fn missing_import_name(keyword_import_pos: Pos, file: &File) {}
pub fn invalid_import_target(pos: Pos, file: &File) {}

pub fn circular_imports(pos: Pos, file: &File) {
    eprintln!("ERROR: Circular imports at {pos}.");
    file.quote_pos(pos);
}

pub fn file_not_found(path: &Path, file: &File, err: std::io::Error) {
    eprintln!("ERROR: Cannot read file `{}`. {}", path.display(), err);
}
pub fn cannot_read_file(path: &Path, file: &File, err: std::io::Error) {
    eprintln!("ERROR: Cannot read file `{}`. {}", path.display(), err);
}

pub fn undefined_variable(pos: Pos, file: &File) {
    eprintln!("ERROR: Undefined variable at {pos}.");
    file.quote_pos(pos);
}

pub fn empty_element(comma_pos: Pos, file: &File) {}
pub struct File {
    pub path: PathBuf,
    pub content: String,
    pub lines: Vec<Range<usize>>,
}

impl File {
    fn quote_line(&self, line: usize) {
        eprintln!("{}", self.path.display());
        eprintln!(
            "L{}: !-> {}",
            line + 1,
            &self.content[self.lines[line].clone()]
        );
    }
    fn quote_index(&self, Index { line, column }: Index) {
        eprintln!("{}", self.path.display());
        let start_line = &self.content[self.lines[line].clone()];
        eprintln!(
            "L{}: {} !-> {}",
            line + 1,
            &start_line[..column],
            &start_line[column..],
        );
    }
    fn quote_pos(&self, Pos { start, end }: Pos) {
        eprintln!("{}", self.path.display());
        match end.line - start.line {
            0 => {
                let line = &self.content[self.lines[start.line].clone()];
                eprintln!(
                    "L{}: {} !-> {} <-! {}",
                    start.line + 1,
                    &line[..start.column],
                    &line[start.column..end.column],
                    &line[end.column..],
                );
            }
            1 => {
                let start_line = &self.content[self.lines[start.line].clone()];
                let end_line = &self.content[self.lines[end.line].clone()];
                eprintln!(
                    "L{}: {} !-> {}",
                    start.line + 1,
                    &start_line[..start.column],
                    &start_line[start.column..],
                );
                eprintln!(
                    "L{}: {} <-! {}",
                    end.line + 1,
                    &end_line[..end.column],
                    &end_line[end.column..],
                );
            }
            2 => {
                let start_line = &self.content[self.lines[start.line].clone()];
                let mid_line = &self.content[self.lines[start.line + 1].clone()];
                let end_line = &self.content[self.lines[end.line].clone()];
                eprintln!(
                    "L{}: {} !-> {}",
                    start.line + 1,
                    &start_line[..start.column],
                    &start_line[start.column..],
                );
                eprintln!("L{}: {}", start.line + 2, mid_line);
                eprintln!(
                    "L{}: {} <-! {}",
                    end.line + 1,
                    &end_line[..end.column],
                    &end_line[end.column..],
                );
            }
            num_lines => {
                let start_line = &self.content[self.lines[start.line].clone()];
                let end_line = &self.content[self.lines[end.line].clone()];
                eprintln!(
                    "L{}: {} !-> {}",
                    start.line + 1,
                    &start_line[..start.column],
                    &start_line[start.column..],
                );
                eprintln!("({} lines)", num_lines - 1);
                eprintln!(
                    "L{}: {} <-! {}",
                    end.line + 1,
                    &end_line[..end.column],
                    &end_line[end.column..],
                );
            }
        }
    }
}

#[derive(Debug)]
pub enum ParseError {
    /// Returned by [`read_token`](../frontend/ast/fn.read_token.html).
    UnexpectedCharacter(Index),
    /// Returned by
    /// [`skip_block_comment`](../frontend/ast/fn.skip_block_comment.html).
    UnterminatedComment {
        start_indices: Vec<Index>,
    },
    /// Returned by [`read_token`](../frontend/ast/fn.read_token.html).
    UnterminatedStringLiteral {
        start_index: Index,
    },
    /// Returned by [`read_token`](../frontend/ast/fn.read_token.html).
    InvalidEscapeSequence {
        backslash_index: Index,
    },
    /// Returned by [`read_token`](../frontend/ast/fn.read_token.html).
    UnexpectedTokenInStringLiteral {
        unexpected_token_pos: Pos,
        dollar_index: Index,
    },
    /// Returned by [`read_token`](../frontend/ast/fn.read_token.html).
    InvalidBlockComment {
        start_index: Index,
    },
    UnexpectedToken(Pos),
    UnexpectedTokenAfterKeywordFunc {
        unexpected_token_pos: Pos,
        keyword_func_pos: Pos,
    },
    /// Returned by [`parse_block`](../frontend/ast/fn.parse_block.html).
    UnclosedBlock {
        start_line_indices: Vec<usize>,
    },
    /// Returned by [`parse_block`](../frontend/ast/fn.parse_block.html).
    UnexpectedTokenInBlock {
        unexpected_token_pos: Pos,
        start_line_indices: Vec<usize>,
    },
    ExtraTokenAfterLine {
        extra_token_pos: Pos,
        line_pos: Pos,
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

impl ParseError {
    pub fn eprint(self, file: &File) {
        match self {
            ParseError::UnexpectedCharacter(index) => {
                eprintln!("Unexpected character at {}.", index);
                file.quote_index(index);
            }
            ParseError::UnterminatedStringLiteral { start_index } => {
                eprintln!("Unterminated string literal started at {start_index}.");
                file.quote_index(start_index);
            }
            ParseError::InvalidEscapeSequence { backslash_index } => {
                eprintln!("Invalid escape squence at {backslash_index}.");
                file.quote_index(backslash_index);
            }
            ParseError::UnexpectedTokenInStringLiteral {
                unexpected_token_pos,
                dollar_index,
            } => {
                eprintln!("Unexpected token at {unexpected_token_pos}.");
                file.quote_pos(unexpected_token_pos);
                eprintln!("Note: A placeholder in string literal started at {dollar_index}.");
                file.quote_index(dollar_index);
            }
            ParseError::UnterminatedComment {
                start_indices: starts_index,
            } => {
                eprintln!("Unterminated comment started at:");
                for start_index in starts_index {
                    file.quote_index(start_index);
                }
            }
            ParseError::InvalidBlockComment { start_index } => {
                eprintln!(
                    "A block comment must start at the beginning of the line, allowing only \
                     leading whitespaces."
                );
                file.quote_index(start_index);
            }
            ParseError::UnexpectedToken(unexpected_token_pos) => {
                eprintln!("Unexpected token at {}.", unexpected_token_pos);
                file.quote_pos(unexpected_token_pos);
            }
            ParseError::UnexpectedTokenAfterKeywordFunc {
                unexpected_token_pos,
                keyword_func_pos,
            } => {
                eprintln!("Unexpected token at {}.", unexpected_token_pos);
                file.quote_pos(unexpected_token_pos);
                eprintln!();
                eprintln!(
                    "Expected an identifier after `func` at {}.",
                    keyword_func_pos
                );
                file.quote_pos(keyword_func_pos);
                eprintln!();
            }
            ParseError::ExtraTokenAfterLine {
                extra_token_pos,
                line_pos: _,
            } => {
                eprintln!("An extra token at {}.", extra_token_pos);
                file.quote_pos(extra_token_pos);
                eprintln!();
            }
            ParseError::UnclosedBlock { start_line_indices } => {
                eprintln!("Unexpected end of file. Blocks opened at:");
                for &line_index in &start_line_indices {
                    file.quote_line(line_index);
                    eprintln!();
                }
            }
            ParseError::UnexpectedTokenInBlock {
                unexpected_token_pos,
                start_line_indices,
            } => {
                eprintln!("Unexpected token at {}.", unexpected_token_pos);
                file.quote_pos(unexpected_token_pos);
                eprintln!();
                eprintln!("Blocks opened at:");
                for &line_index in &start_line_indices {
                    file.quote_line(line_index);
                    eprintln!();
                }
            }
            _ => eprintln!("{:?}", self),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Pos {
    pub start: Index,
    pub end: Index,
}

impl Pos {
    pub fn line(&self) -> usize {
        self.start.line
    }
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
