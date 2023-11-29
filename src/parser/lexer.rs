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

use super::chars_peekable::CharsPeekable;
use super::token::Token;
use super::Error;
use crate::range::Range;
use either::Either;

pub struct Lexer<'id, 'chars> {
    chars: &'chars mut CharsPeekable<'id>,
    last_end: usize,
    next_start: usize,
    pub peeked: Option<Token<'id>>,
}

impl<'id, 'chars> Lexer<'id, 'chars> {
    pub fn new(chars: &'chars mut CharsPeekable<'id>) -> Result<Lexer<'id, 'chars>, Error> {
        let mut lexer = Lexer {
            chars,
            last_end: 0,
            next_start: 0,
            peeked: None,
        };
        lexer.next_token()?;
        Ok(lexer)
    }
    pub fn next_start(&self) -> usize {
        self.next_start
    }
    pub fn range_from(&self, start: usize) -> Range {
        Range {
            start,
            end: self.last_end,
        }
    }
    pub fn consume(&mut self) -> Result<(), Error> {
        self.next_token()
    }
    pub fn consume_if_or_err(
        &mut self,
        pred: impl FnOnce(&Token) -> bool,
        reason_pos: Range,
    ) -> Result<(), Error> {
        match self.peeked {
            Some(ref token) if pred(token) => {
                self.consume()?;
                Ok(())
            }
            Some(_) => Err(Error::UnexpectedTokenAfter {
                error_pos: self.range_from(self.next_start),
                reason_pos,
            }),
            None => Err(Error::EOFAfter { reason_pos }),
        }
    }
    // 非公開にしたい
    pub fn next_token(&mut self) -> Result<(), Error> {
        self.last_end = self.chars.offset();
        self.chars.consume_while(|ch| ch.is_ascii_whitespace());
        let start = self.chars.offset();
        self.next_start = start;
        let Some(first_ch) = self.chars.next() else {
            self.peeked = None;
            return Ok(());
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
        self.peeked = Some(token);
        Ok(())
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
