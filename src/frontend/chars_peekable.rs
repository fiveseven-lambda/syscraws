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
 * Defines [`CharsPeekable`], used in the parser to iterate over the
 * characters of an input string.
 */

use crate::log::Index;
use std::ops::Range;
use std::str::CharIndices;

/**
 * A structure used in the parser to iterate over the characters of an input
 * string.
 *
 * The next character can be peeked using the [`peek`](Self::peek) method.
 * It also tracks the byte ranges of each line, which can be retrieved using
 * the [`lines`](Self::lines) method.
 *
 * # TODO
 * Handle `\r\n` on Windows.
 */
pub struct CharsPeekable<'input> {
    /**
     * An iterator over the input string, providing both characters and
     * their byte positions from the start.
     */
    iter: CharIndices<'input>,
    /**
     * The next character to be consumed. `None` if EOF has been reached.
     */
    current_char: Option<char>,
    /**
     * The byte position of the [`current_char`](Self::current_char) from
     * the start of the input string.
     */
    current_index: usize,
    /**
     * The byte position of the start of the line containing the
     * [`current_char`](Self::current_char).
     */
    current_line_start: usize,
    /**
     * The byte ranges of each line in the input string.
     */
    lines: Vec<Range<usize>>,
}

impl<'input> CharsPeekable<'input> {
    /**
     * Creates a new [`CharsPeekable`] instance from the given input string.
     */
    pub fn new(input: &'input str) -> Self {
        let mut iter = input.char_indices();
        let first_char = iter.next().map(|(_, ch)| ch);
        Self {
            iter,
            current_char: first_char,
            current_index: 0,
            current_line_start: 0,
            lines: Vec::new(),
        }
    }
    /**
     * Returns the next character without consuming it, or `None` if at EOF.
     */
    pub fn peek(&self) -> Option<char> {
        self.current_char
    }
    /**
     * Returns the line and column numbers of the next character. If at EOF,
     * it returns the index of where the next character would be,
     * assuming the file ends with or without a newline.
     */
    pub fn index(&self) -> Index {
        Index {
            line: self.lines.len(),
            column: self.current_index - self.current_line_start,
        }
    }
    /**
     * Consumes the next character, advancing the iterator.
     */
    pub fn consume(&mut self) {
        let next_index = self.iter.offset();
        let next_char = self.iter.next().map(|(_, ch)| ch);
        if self.current_char == Some('\n') {
            self.lines.push(self.current_line_start..self.current_index);
            self.current_line_start = next_index;
        }
        self.current_index = next_index;
        self.current_char = next_char;
    }
    /**
     * Consumes the next character only if it matches the expected
     * character. If the next character is `expected`, it is consumed and
     * `true` is returned. Otherwise, the character is not consumed and
     * `false` is returned.
     */
    pub fn consume_if(&mut self, expected: char) -> bool {
        let ret = self.peek() == Some(expected);
        if ret {
            self.consume();
        }
        ret
    }
    /**
     * Returns the byte ranges of each line in the input string.
     * For each line, the range is represented by the byte positions
     * of the first character of the line and the newline character (`\n`)
     * at the end. If the file ends without a newline, the end of the last
     * range will be the EOF byte position (i.e., the total byte length of
     * the input string).
     *
     * Any line and column numbers previously obtained through the
     * [`peek`](Self::peek) method (even if at EOF) will correspond to a
     * valid UTF-8 boundary within the ranges returned by this method.
     */
    pub fn lines(mut self) -> Vec<Range<usize>> {
        self.lines.push(self.current_line_start..self.current_index);
        self.lines
    }
}
