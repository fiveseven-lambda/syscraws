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

use crate::token::Token;

pub struct TokenSeq<'t> {
    tokens: &'t [Token],
    prev_end: usize,
}

impl<'t> TokenSeq<'t> {
    pub fn new(tokens: &'t [Token]) -> TokenSeq<'t> {
        TokenSeq {
            tokens,
            prev_end: 0,
        }
    }
    pub fn peek(&self) -> Option<Token> {
        self.tokens.first().copied()
    }
    pub fn consume(&mut self) {
        let (head, tail) = self.tokens.split_first().unwrap();
        self.prev_end = head.end;
        self.tokens = tail;
    }
    pub fn next_start(&self) -> Option<usize> {
        self.peek().map(|Token { start, .. }| start)
    }
    pub fn prev_end(&self) -> usize {
        self.prev_end
    }
}
