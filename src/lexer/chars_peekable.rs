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

use std::str::CharIndices;

pub struct CharsPeekable<'s> {
    pub input: &'s str,
    chars: CharIndices<'s>,
    peeked: Option<(usize, char)>,
}

impl<'s> CharsPeekable<'s> {
    pub fn new(input: &'s str) -> CharsPeekable<'s> {
        let mut chars = input.char_indices();
        let peeked = chars.next();
        CharsPeekable {
            input,
            chars,
            peeked,
        }
    }
    pub fn peek(&mut self) -> Option<char> {
        self.peeked.map(|(_, ch)| ch)
    }
    pub fn next_if(&mut self, pred: impl FnOnce(char) -> bool) -> Option<char> {
        self.peek().and_then(|ch| {
            pred(ch).then(|| {
                self.peeked = self.chars.next();
                ch
            })
        })
    }
    pub fn next(&mut self) -> Option<char> {
        self.next_if(|_| true)
    }
    pub fn consume_if(&mut self, pred: impl FnOnce(char) -> bool) -> bool {
        self.next_if(pred).is_some()
    }
    pub fn consume_if_eq(&mut self, expected: char) -> bool {
        self.consume_if(|ch| ch == expected)
    }
    pub fn consume_while(&mut self, mut pred: impl FnMut(char) -> bool) {
        while self.consume_if(&mut pred) {}
    }
    pub fn offset(&mut self) -> usize {
        match self.peeked {
            Some((offset, _)) => offset,
            None => self.input.len(),
        }
    }
}
