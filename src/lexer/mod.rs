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

mod chars_peekable;
#[cfg(test)]
mod test;

use crate::token::{Token, TokenKind};
use chars_peekable::CharsPeekable;

pub fn tokenize(input: &str) -> Result<Vec<Token>, Error> {
    let mut chars = CharsPeekable::new(input);
    let mut tokens = Vec::new();
    loop {
        chars.consume_while(|ch| ch.is_ascii_whitespace());
        let start = chars.offset();
        let Some(ch) = chars.next() else {
            return Ok(tokens);
        };
        let token_kind = match ch {
            'a'..='z' | 'A'..='Z' | '_' => {
                chars.consume_while(|ch| matches!(ch, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_'));
                match unsafe { input.get_unchecked(start..chars.offset()) } {
                    "if" => TokenKind::KeywordIf,
                    "else" => TokenKind::KeywordElse,
                    "while" => TokenKind::KeywordWhile,
                    "return" => TokenKind::KeywordReturn,
                    _ => TokenKind::Identifier,
                }
            }
            '0'..='9' => {
                read_number(&mut chars);
                TokenKind::Number
            }
            '.' => {
                if chars.consume_if(|ch| ch.is_ascii_digit()) {
                    read_number(&mut chars);
                    TokenKind::Number
                } else {
                    TokenKind::Dot
                }
            }
            '+' => {
                if chars.consume_if_eq('+') {
                    TokenKind::DoublePlus
                } else if chars.consume_if_eq('=') {
                    TokenKind::PlusEqual
                } else {
                    TokenKind::Plus
                }
            }
            '-' => {
                if chars.consume_if_eq('-') {
                    TokenKind::DoubleMinus
                } else if chars.consume_if_eq('=') {
                    TokenKind::MinusEqual
                } else {
                    TokenKind::Minus
                }
            }
            '*' => {
                if chars.consume_if_eq('=') {
                    TokenKind::AsteriskEqual
                } else {
                    TokenKind::Asterisk
                }
            }
            '/' => {
                if chars.consume_if_eq('/') {
                    chars.consume_while(|ch| ch != '\n');
                    continue;
                } else if chars.consume_if_eq('*') {
                    let mut comments = vec![start];
                    while !comments.is_empty() {
                        let pos = chars.offset();
                        match chars.next() {
                            Some('/') if chars.consume_if_eq('*') => comments.push(pos),
                            Some('*') if chars.consume_if_eq('/') => {
                                comments.pop();
                            }
                            Some(_) => {}
                            None => return Err(Error::UnterminatedComment(comments)),
                        }
                    }
                    continue;
                } else if chars.consume_if_eq('=') {
                    TokenKind::SlashEqual
                } else {
                    TokenKind::Slash
                }
            }
            '%' => {
                if chars.consume_if_eq('=') {
                    TokenKind::PercentEqual
                } else {
                    TokenKind::Percent
                }
            }
            '=' => {
                if chars.consume_if_eq('=') {
                    TokenKind::DoubleEqual
                } else if chars.consume_if_eq('>') {
                    TokenKind::RightArrow
                } else {
                    TokenKind::Equal
                }
            }
            '!' => {
                if chars.consume_if_eq('=') {
                    TokenKind::ExclamationEqual
                } else {
                    TokenKind::Exclamation
                }
            }
            '>' => {
                if chars.consume_if_eq('>') {
                    if chars.consume_if_eq('>') {
                        if chars.consume_if_eq('=') {
                            TokenKind::TripleGreaterEqual
                        } else {
                            TokenKind::TripleGreater
                        }
                    } else if chars.consume_if_eq('=') {
                        TokenKind::DoubleGreaterEqual
                    } else {
                        TokenKind::DoubleGreater
                    }
                } else if chars.consume_if_eq('=') {
                    TokenKind::GreaterEqual
                } else {
                    TokenKind::Greater
                }
            }
            '<' => {
                if chars.consume_if_eq('<') {
                    if chars.consume_if_eq('<') {
                        if chars.consume_if_eq('=') {
                            TokenKind::TripleLessEqual
                        } else {
                            TokenKind::TripleLess
                        }
                    } else if chars.consume_if_eq('=') {
                        TokenKind::DoubleLessEqual
                    } else {
                        TokenKind::DoubleLess
                    }
                } else if chars.consume_if_eq('=') {
                    TokenKind::LessEqual
                } else {
                    TokenKind::Less
                }
            }
            '&' => {
                if chars.consume_if_eq('&') {
                    TokenKind::DoubleAmpersand
                } else if chars.consume_if_eq('=') {
                    TokenKind::AmpersandEqual
                } else {
                    TokenKind::Ampersand
                }
            }
            '|' => {
                if chars.consume_if_eq('|') {
                    TokenKind::DoubleBar
                } else if chars.consume_if_eq('=') {
                    TokenKind::BarEqual
                } else {
                    TokenKind::Bar
                }
            }
            '^' => {
                if chars.consume_if_eq('=') {
                    TokenKind::CircumflexEqual
                } else {
                    TokenKind::Circumflex
                }
            }
            ':' => TokenKind::Colon,
            ';' => TokenKind::Semicolon,
            ',' => TokenKind::Comma,
            '?' => TokenKind::Question,
            '#' => TokenKind::Hash,
            '~' => TokenKind::Tilde,
            '(' => TokenKind::OpeningParenthesis,
            ')' => TokenKind::ClosingParenthesis,
            '[' => TokenKind::OpeningBracket,
            ']' => TokenKind::ClosingBracket,
            '{' => TokenKind::OpeningBrace,
            '}' => TokenKind::ClosingBrace,
            '"' => {
                loop {
                    let pos = chars.offset();
                    match chars.next() {
                        Some('"') => break,
                        Some('\\') => match chars.next() {
                            Some('n' | 'r' | 't' | '"' | '\\' | '0' | '\'') => {}
                            Some('x') => {
                                for _ in 0..2 {
                                    if !chars.next().is_some_and(|ch| ch.is_ascii_digit()) {
                                        return Err(Error::InvalidEscapeSequence(pos));
                                    }
                                }
                            }
                            _ => return Err(Error::InvalidEscapeSequence(pos)),
                        },
                        Some(_) => {}
                        None => return Err(Error::UnterminatedStringLiteral(start)),
                    }
                }
                TokenKind::String
            }
            _ => return Err(Error::UnexpectedCharacter(start)),
        };
        let end = chars.offset();
        tokens.push(Token {
            token_kind,
            start,
            end,
        });
    }
}

fn read_number(chars: &mut CharsPeekable) {
    let mut after_e = false;
    chars.consume_while(|ch| {
        after_e = match ch {
            'e' | 'E' => true,
            '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' | '.' => false,
            '+' | '-' if after_e => false,
            _ => return false,
        };
        true
    });
}

#[derive(Debug)]
pub enum Error {
    UnexpectedCharacter(usize),
    UnterminatedComment(Vec<usize>),
    UnterminatedStringLiteral(usize),
    InvalidEscapeSequence(usize),
}
