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
    chars: CharIndices<'s>,
    len: usize,
    peeked: Option<Option<(usize, char)>>,
}

impl<'s> CharsPeekable<'s> {
    pub fn new(s: &'s str) -> CharsPeekable<'s> {
        CharsPeekable {
            chars: s.char_indices(),
            len: s.len(),
            peeked: None,
        }
    }
    pub fn peek(&mut self) -> Option<char> {
        self.peeked
            .get_or_insert_with(|| self.chars.next())
            .map(|(_, ch)| ch)
    }
    pub fn next_if(&mut self, pred: impl FnOnce(char) -> bool) -> Option<char> {
        self.peek().and_then(|ch| {
            pred(ch).then(|| {
                self.peeked = None;
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
        match *self.peeked.get_or_insert_with(|| self.chars.next()) {
            Some((offset, _)) => offset,
            None => self.len,
        }
    }
}
