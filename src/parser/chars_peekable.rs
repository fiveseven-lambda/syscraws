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

//! [`CharsPeekable`] を定義する．[`Lexer`](super::lexer::Lexer) はこれを通して文字列を走査する．

use std::str::CharIndices;

/**
 * [`CharsPeekable::peek()`] と [`CharsPeekable::offset()`] を併せ持つ．
 */
pub struct CharsPeekable<'s> {
    pub input: &'s str,
    chars: CharIndices<'s>,
    peeked: Option<(usize, char)>,
}

impl<'s> CharsPeekable<'s> {
    /**
     * 文字列 `input` を受け取り，`input` を走査する [`CharsPeekable`] を作る．
     *
     * 実はこの時点で 1 文字先読みし，`peeked` に格納している．
     */
    pub fn new(input: &'s str) -> CharsPeekable<'s> {
        let mut chars = input.char_indices();
        let peeked = chars.next();
        CharsPeekable {
            input,
            chars,
            peeked,
        }
    }
    /**
     * 消費せずに，次の文字を先読みする．
     */
    pub fn peek(&mut self) -> Option<char> {
        self.peeked.map(|(_, ch)| ch)
    }
    /**
     * 条件が真なら 1 文字読み進める．
     */
    pub fn next_if(&mut self, pred: impl FnOnce(char) -> bool) -> Option<char> {
        self.peek().and_then(|ch| {
            pred(ch).then(|| {
                self.peeked = self.chars.next();
                ch
            })
        })
    }
    /**
     * 1 文字読み進める．
     */
    pub fn next(&mut self) -> Option<char> {
        self.next_if(|_| true)
    }
    /**
     * 条件が真なら，1 文字消費する（[`CharsPeekable::next_if`] して結果を捨てる）．
     */
    pub fn consume_if(&mut self, pred: impl FnOnce(char) -> bool) -> bool {
        self.next_if(pred).is_some()
    }
    /**
     * `expected` を 1 文字消費する（[`CharsPeekable::consume_if`] の条件が「`expected` と等しい」になったもの）．
     */
    pub fn consume_if_eq(&mut self, expected: char) -> bool {
        self.consume_if(|ch| ch == expected)
    }
    /**
     * 条件が真である限り，消費し続ける（[`CharsPeekable::consume_if`] を繰り返し呼ぶ）．
     */
    pub fn consume_while(&mut self, mut pred: impl FnMut(char) -> bool) {
        while self.consume_if(&mut pred) {}
    }
    /**
     * 次の文字が，文字列の先頭から何バイト目にあるか答える．
     */
    pub fn offset(&mut self) -> usize {
        match self.peeked {
            Some((offset, _)) => offset,
            None => self.input.len(),
        }
    }
}
