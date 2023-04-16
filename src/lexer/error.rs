/*
 * Copyright (c) 2023 Atsushi Komaba
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

#[derive(Debug)]
pub enum Error {
    UnexpectedCharacter(usize),
    UnterminatedComment(Vec<usize>),
    UnterminatedStringLiteral(usize),
    InvalidEscapeSequence(usize),
}

impl Error {
    pub fn eprint(&self, input: &str) {
        use crate::lines::Lines;
        let lines = Lines::new(input);
        match *self {
            Error::UnexpectedCharacter(pos) => {
                eprintln!("Unexpected character at {:?}", lines.line_column(pos));
                lines.eprint_pos(pos);
            }
            Error::UnterminatedComment(ref pos_comments) => {
                eprintln!("Unterminated comment");
                for &pos in pos_comments {
                    lines.eprint_pos(pos);
                }
            }
            Error::UnterminatedStringLiteral(pos) => {
                eprintln!(
                    "Unterminated string literal started at {:?}",
                    lines.line_column(pos)
                );
                lines.eprint_pos(pos);
            }
            Error::InvalidEscapeSequence(pos) => {
                eprintln!("Invalid escape sequence at {:?}", lines.line_column(pos));
                lines.eprint_pos(pos);
            }
        }
    }
}
