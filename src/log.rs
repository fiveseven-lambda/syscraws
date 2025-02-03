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

pub fn root_file_not_found(path: &Path, err: std::io::Error) {
    eprintln!("ERROR: File `{}` not found. {}", path.display(), err);
}

pub fn cannot_read_root_file(path: &Path, err: std::io::Error) {
    eprintln!("ERROR: Cannot read file `{}`. {}", path.display(), err);
}

pub fn abort(num_errors: u32) {
    eprintln!("Aborting due to {num_errors} previous errors.");
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
    MissingMethodName {
        dot_pos: Pos,
    },
    MissingFieldName {
        dot_pos: Pos,
    },
    EmptyFunc {
        keyword_func_pos: Pos,
    },
    MissingImportName {
        keyword_import_pos: Pos,
    },
    InvalidImportPath {
        term_pos: Pos,
    },
    CircularImports {
        path: PathBuf,
    },
    MissingImportPath {
        opening_parenthesis_pos: Pos,
        closing_parenthesis_pos: Pos,
    },
    CannotReadImportedFile {
        path: PathBuf,
        err: std::io::Error,
    },
    MissingStructName {
        keyword_struct_pos: Pos,
    },
    MissingFunctionParameters {
        keyword_func_pos: Pos,
    },
    InvalidBlockComment {
        start_index: Index,
    },
    DuplicateDefinition,
    UnexpectedTokenAfterKeywordImport {
        unexpected_token_pos: Pos,
        keyword_import_pos: Pos,
    },
    UnexpectedTokenAfterImportName {
        unexpected_token_pos: Pos,
        import_name_pos: Pos,
    },
    UnexpectedTokenAfterKeywordFunc {
        unexpected_token_pos: Pos,
        keyword_func_pos: Pos,
    },
    UnexpectedTokenAfterFunctionName {
        unexpected_token_pos: Pos,
        function_name_pos: Pos,
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
    MissingVariableName {
        keyword_var_pos: Pos,
    },
}

impl ParseError {
    pub fn eprint(&self, path: &Path, text: &str, lines: &[Range<usize>]) {
        match self {
            ParseError::UnexpectedToken(unexpected_token_pos) => {
                eprintln!("Unexpected token at {}", unexpected_token_pos);
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
