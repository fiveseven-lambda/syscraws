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

//! 字句解析を担う [`Lexer`] を定義する．

use super::chars_peekable::CharsPeekable;
use super::token::Token;
use super::Error;
use either::Either;
use std::ops::Range;

/**
 * ソースコードの文字列をトークンに分解する．
 *
 * 空白，コメントを読み飛ばす仕事も担っている．
 *
 * 基本的な使い方としては，以下の 2 つを繰り返す．
 * - public なフィールド [`Self::peeked`] を見る．`peek()` のようなメソッドは存在せず，何もしなくても `peeked` が常に次のトークンを指している．`Lexer` は `peeked` を所有しているので，`lexer: &mut Lexer` を持っていれば `lexer.peeked` は煮るなり焼くなりできる．
 * - [`Self::consume()`] でトークンを読み進める．これによって [`Self::peeked`] が更新される．
 */
pub struct Lexer<'id, 'chars> {
    chars: &'chars mut CharsPeekable<'id>,
    last_end: usize,
    next_start: usize,
    pub peeked: Option<Token<'id>>,
}

impl<'id, 'chars> Lexer<'id, 'chars> {
    pub fn new(chars: &'chars mut CharsPeekable<'id>) -> Result<Self, Error> {
        let mut lexer = Lexer {
            chars,
            last_end: 0,
            next_start: 0,
            peeked: None,
        };
        lexer.consume()?;
        Ok(lexer)
    }
    /**
     * [`Self::peeked`] の先頭位置を返す．
     *
     * EOF に達して `peeked` が `None` なときは，ソースコード全体の長さを返す．
     */
    pub fn next_start(&self) -> usize {
        self.next_start
    }
    /**
     * [`Lexer`] は，最後に consume したトークン（`Self::peeked` の 1 個前のトークン）の末尾位置 `last_end` を保存している．本メソッドは，引数として受け取った `start` と保存していた `last_end` を合わせ，[`Range`] にして返す．
     */
    pub fn range_from(&self, start: usize) -> Range<usize> {
        start..self.last_end
    }
    /**
     * 次のトークンを読み，[`Self::peeked`] に格納する．
     */
    pub fn consume(&mut self) -> Result<(), Error> {
        self.last_end = self.chars.offset();
        self.peeked = self.next_token()?;
        Ok(())
    }
    /**
     * [`Self::peeked`] が条件 `pred` を満たせば，次のトークンを読み進める．さもなくば，エラーを返す．
     */
    pub fn consume_if_or_err(
        &mut self,
        pred: impl FnOnce(&Token) -> bool,
        reason_pos: Range<usize>,
    ) -> Result<(), Error> {
        match self.peeked {
            Some(ref token) if pred(token) => self.consume(),
            Some(_) => {
                let start = self.next_start();
                self.consume()?;
                Err(Error::UnexpectedTokenAfter {
                    error_pos: self.range_from(start),
                    reason_pos,
                })
            }
            None => Err(Error::EOFAfter { reason_pos }),
        }
    }
    /**
     * トークンを 1 つ読み進める．
     *
     * 確か，[`Self::consume()`] と統合できない理由があったと思う．
     */
    fn next_token(&mut self) -> Result<Option<Token<'id>>, Error> {
        self.chars.consume_while(|ch| ch.is_ascii_whitespace());
        let start = self.chars.offset();
        self.next_start = start;
        let Some(first_ch) = self.chars.next() else {
            return Ok(None);
        };
        let token = match first_ch {
            'a'..='z' | 'A'..='Z' | '_' => {
                self.chars
                    .consume_while(|ch| matches!(ch, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_'));
                let end = self.chars.offset();
                let name = unsafe { self.chars.input.get_unchecked(start..end) };
                match name {
                    "if" => Token::KeywordIf,
                    "else" => Token::KeywordElse,
                    "while" => Token::KeywordWhile,
                    "return" => Token::KeywordReturn,
                    _ => Token::Identifier(name),
                }
            }
            '0'..='9' => {
                self.read_number();
                let end = self.chars.offset();
                Token::Number(unsafe { self.chars.input.get_unchecked(start..end) })
            }
            '.' => {
                if self.chars.consume_if(|ch| ch.is_ascii_digit()) {
                    self.read_number();
                    let end = self.chars.offset();
                    Token::Number(unsafe { self.chars.input.get_unchecked(start..end) })
                } else {
                    Token::Dot
                }
            }
            '+' => {
                if self.chars.consume_if_eq('+') {
                    Token::DoublePlus
                } else if self.chars.consume_if_eq('=') {
                    Token::PlusEqual
                } else {
                    Token::Plus
                }
            }
            '-' => {
                if self.chars.consume_if_eq('-') {
                    Token::DoubleHyphen
                } else if self.chars.consume_if_eq('=') {
                    Token::HyphenEqual
                } else if self.chars.consume_if_eq('>') {
                    Token::HyphenGreater
                } else {
                    Token::Hyphen
                }
            }
            '*' => {
                if self.chars.consume_if_eq('=') {
                    Token::AsteriskEqual
                } else {
                    Token::Asterisk
                }
            }
            '/' => {
                if self.chars.consume_if_eq('/') {
                    self.chars.consume_while(|ch| ch != '\n');
                    return self.next_token();
                } else if self.chars.consume_if_eq('*') {
                    let mut comments = vec![start];
                    while !comments.is_empty() {
                        let pos = self.chars.offset();
                        match self.chars.next() {
                            Some('/') if self.chars.consume_if_eq('*') => comments.push(pos),
                            Some('*') if self.chars.consume_if_eq('/') => {
                                comments.pop();
                            }
                            Some(_) => {}
                            None => return Err(Error::UnterminatedComment(comments)),
                        }
                    }
                    return self.next_token();
                } else if self.chars.consume_if_eq('=') {
                    Token::SlashEqual
                } else {
                    Token::Slash
                }
            }
            '%' => {
                if self.chars.consume_if_eq('=') {
                    Token::PercentEqual
                } else {
                    Token::Percent
                }
            }
            '=' => {
                if self.chars.consume_if_eq('=') {
                    Token::DoubleEqual
                } else if self.chars.consume_if_eq('>') {
                    Token::EqualGreater
                } else {
                    Token::Equal
                }
            }
            '!' => {
                if self.chars.consume_if_eq('=') {
                    Token::ExclamationEqual
                } else {
                    Token::Exclamation
                }
            }
            '>' => {
                if self.chars.consume_if_eq('>') {
                    if self.chars.consume_if_eq('>') {
                        if self.chars.consume_if_eq('=') {
                            Token::TripleGreaterEqual
                        } else {
                            Token::TripleGreater
                        }
                    } else if self.chars.consume_if_eq('=') {
                        Token::DoubleGreaterEqual
                    } else {
                        Token::DoubleGreater
                    }
                } else if self.chars.consume_if_eq('=') {
                    Token::GreaterEqual
                } else {
                    Token::Greater
                }
            }
            '<' => {
                if self.chars.consume_if_eq('<') {
                    if self.chars.consume_if_eq('<') {
                        if self.chars.consume_if_eq('=') {
                            Token::TripleLessEqual
                        } else {
                            Token::TripleLess
                        }
                    } else if self.chars.consume_if_eq('=') {
                        Token::DoubleLessEqual
                    } else {
                        Token::DoubleLess
                    }
                } else if self.chars.consume_if_eq('=') {
                    Token::LessEqual
                } else {
                    Token::Less
                }
            }
            '&' => {
                if self.chars.consume_if_eq('&') {
                    Token::DoubleAmpersand
                } else if self.chars.consume_if_eq('=') {
                    Token::AmpersandEqual
                } else {
                    Token::Ampersand
                }
            }
            '|' => {
                if self.chars.consume_if_eq('|') {
                    Token::DoubleBar
                } else if self.chars.consume_if_eq('=') {
                    Token::BarEqual
                } else {
                    Token::Bar
                }
            }
            '^' => {
                if self.chars.consume_if_eq('=') {
                    Token::CircumflexEqual
                } else {
                    Token::Circumflex
                }
            }
            ':' => Token::Colon,
            ';' => Token::Semicolon,
            ',' => Token::Comma,
            '?' => Token::Question,
            '#' => Token::Hash,
            '~' => Token::Tilde,
            '(' => Token::OpeningParenthesis,
            ')' => Token::ClosingParenthesis,
            '[' => Token::OpeningBracket,
            ']' => Token::ClosingBracket,
            '{' => Token::OpeningBrace,
            '}' => Token::ClosingBrace,
            '"' => {
                let mut components = Vec::new();
                let mut string = String::new();
                #[derive(PartialEq, Eq)]
                enum Action {
                    Border,
                    Char,
                    Expr,
                }
                let mut prev_action = Action::Border;
                loop {
                    let Some(mut ch) = self.chars.next() else {
                        return Err(Error::UnterminatedStringLiteral(start));
                    };
                    let action = match ch {
                        '"' => Action::Border,
                        '{' => Action::Expr,
                        '\\' => {
                            ch = match self.chars.next() {
                                Some('n') => '\n',
                                Some('r') => '\r',
                                Some('t') => '\t',
                                Some('"') => '\"',
                                Some('\\') => '\\',
                                Some('0') => '\0',
                                Some('\'') => '\'',
                                _ => return Err(Error::InvalidEscapeSequence(start)),
                            };
                            Action::Char
                        }
                        _ => Action::Char,
                    };
                    if action == Action::Char {
                        string.push(ch);
                    } else if prev_action == Action::Char {
                        components.push(Either::Left(std::mem::take(&mut string)))
                    }
                    if action == Action::Expr {
                        let mut lexer = Lexer::new(self.chars)?;
                        let term = super::parse_assign(&mut lexer);
                        assert!(matches!(lexer.peeked, Some(Token::ClosingBrace)));
                        components.push(Either::Right(term.unwrap()));
                    } else if action == Action::Border {
                        break;
                    }
                    prev_action = action;
                }
                Token::String(components)
            }
            _ => return Err(Error::UnexpectedCharacter(start)),
        };
        Ok(Some(token))
    }

    fn read_number(&mut self) {
        let mut after_e = false;
        self.chars.consume_while(|ch| {
            after_e = match ch {
                'e' | 'E' => true,
                '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' | '.' => false,
                '+' | '-' if after_e => false,
                _ => return false,
            };
            true
        });
    }
}
