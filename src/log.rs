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

pub fn circular_imports(pos: Pos, file: &File) {
    eprintln!("ERROR: Circular imports at {pos}.");
}

pub fn cannot_read_file(path: &Path, file: &File, err: std::io::Error) {
    eprintln!("ERROR: Cannot read file `{}`. {}", path.display(), err);
}

pub fn undefined_variable(pos: Pos, file: &File) {
    eprintln!("ERROR: Undefined variable at {pos}.");
    file.quote_pos(pos);
}

pub struct File {
    pub path: PathBuf,
    pub content: String,
    pub lines: Vec<Range<usize>>,
}

impl File {
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
        if start.line == end.line {
            let line = &self.content[self.lines[start.line].clone()];
            eprintln!(
                "L{}: {} !-> {} <-! {}",
                start.line + 1,
                &line[..start.column],
                &line[start.column..end.column],
                &line[end.column..],
            );
        } else {
            let start_line = &self.content[self.lines[start.line].clone()];
            eprintln!(
                "L{}: {} !-> {}",
                start.line + 1,
                &start_line[..start.column],
                &start_line[start.column..],
            );
            let end_line = &self.content[self.lines[end.line].clone()];
            eprintln!(
                "L{}: {} <-! {}",
                end.line + 1,
                &end_line[..end.column],
                &end_line[end.column..],
            );
        }
    }
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedCharacter(Index),
    UnterminatedComment {
        starts_index: Vec<Index>,
    },
    UnterminatedStringLiteral {
        start_index: Index,
    },
    InvalidEscapeSequence {
        backslash_index: Index,
    },
    UnmatchedClosingBraceInStringLiteral {
        closing_brace_index: Index,
        start_index: Index,
    },
    UnexpectedToken(Pos),
    MissingFunctionName {
        keyword_func_pos: Pos,
    },
    InvalidBlockComment {
        start_index: Index,
    },
    UnexpectedTokenAfterKeywordFunc {
        unexpected_token_pos: Pos,
        keyword_func_pos: Pos,
    },
    UnclosedBlock {
        start_line_indices: Vec<usize>,
    },
    UnexpectedTokenInBlock {
        unexpected_token_pos: Pos,
        start_line_indices: Vec<usize>,
    },
    MissingConditionAfterKeywordWhile {
        keyword_while_pos: Pos,
    },
    UnexpectedTokenAfterKeywordWhile {
        unexpected_token_pos: Pos,
        keyword_while_pos: Pos,
    },
    UnexpectedTokenAfterStatement {
        unexpected_token_pos: Pos,
        stmt_pos: Pos,
    },
    UnexpectedTokenAfterWhileCondition {
        unexpected_token_pos: Pos,
        condition_pos: Pos,
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
            ParseError::UnterminatedComment { starts_index } => {
                eprintln!("Unterminated comment");
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
