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

use super::CharsPeekable;
use super::Error;
use either::Either;

use crate::pre_ast::PTerm;

pub enum Token<'id> {
    Identifier(&'id str),
    KeywordIf,
    KeywordElse,
    KeywordWhile,
    KeywordReturn,
    Number(&'id str),
    String(Vec<Either<String, Option<PTerm<'id>>>>),
    Plus,
    PlusEqual,
    DoublePlus,
    Hyphen,
    HyphenEqual,
    DoubleHyphen,
    HyphenGreater,
    Asterisk,
    AsteriskEqual,
    Slash,
    SlashEqual,
    Percent,
    PercentEqual,
    Equal,
    DoubleEqual,
    EqualGreater,
    Exclamation,
    ExclamationEqual,
    Greater,
    GreaterEqual,
    DoubleGreater,
    DoubleGreaterEqual,
    TripleGreater,
    TripleGreaterEqual,
    Less,
    LessEqual,
    DoubleLess,
    DoubleLessEqual,
    TripleLess,
    TripleLessEqual,
    Ampersand,
    AmpersandEqual,
    DoubleAmpersand,
    Bar,
    BarEqual,
    DoubleBar,
    Circumflex,
    CircumflexEqual,
    Dot,
    Colon,
    Semicolon,
    Comma,
    Question,
    Hash,
    Tilde,
    OpeningParenthesis,
    ClosingParenthesis,
    OpeningBracket,
    ClosingBracket,
    OpeningBrace,
    ClosingBrace,
}

pub fn next<'id>(chars: &mut CharsPeekable<'id>) -> Result<Option<Token<'id>>, Error> {
    chars.consume_while(|ch| ch.is_ascii_whitespace());
    let start = chars.offset();
    let Some(first_ch) = chars.next() else {
        return Ok(None);
    };
    let token = match first_ch {
        'a'..='z' | 'A'..='Z' | '_' => {
            chars.consume_while(|ch| matches!(ch, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_'));
            let end = chars.offset();
            Token::Identifier(unsafe { chars.input.get_unchecked(start..end) })
        }
        '0'..='9' => {
            read_number(chars);
            let end = chars.offset();
            Token::Number(unsafe { chars.input.get_unchecked(start..end) })
        }
        '.' => {
            if chars.consume_if(|ch| ch.is_ascii_digit()) {
                read_number(chars);
                let end = chars.offset();
                Token::Number(unsafe { chars.input.get_unchecked(start..end) })
            } else {
                Token::Dot
            }
        }
        '+' => {
            if chars.consume_if_eq('+') {
                Token::DoublePlus
            } else if chars.consume_if_eq('=') {
                Token::PlusEqual
            } else {
                Token::Plus
            }
        }
        '-' => {
            if chars.consume_if_eq('-') {
                Token::DoubleHyphen
            } else if chars.consume_if_eq('=') {
                Token::HyphenEqual
            } else if chars.consume_if_eq('>') {
                Token::HyphenGreater
            } else {
                Token::Hyphen
            }
        }
        '*' => {
            if chars.consume_if_eq('=') {
                Token::AsteriskEqual
            } else {
                Token::Asterisk
            }
        }
        '/' => {
            if chars.consume_if_eq('/') {
                chars.consume_while(|ch| ch != '\n');
                return next(chars);
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
                return next(chars);
            } else if chars.consume_if_eq('=') {
                Token::SlashEqual
            } else {
                Token::Slash
            }
        }
        '%' => {
            if chars.consume_if_eq('=') {
                Token::PercentEqual
            } else {
                Token::Percent
            }
        }
        '=' => {
            if chars.consume_if_eq('=') {
                Token::DoubleEqual
            } else if chars.consume_if_eq('>') {
                Token::EqualGreater
            } else {
                Token::Equal
            }
        }
        '!' => {
            if chars.consume_if_eq('=') {
                Token::ExclamationEqual
            } else {
                Token::Exclamation
            }
        }
        '>' => {
            if chars.consume_if_eq('>') {
                if chars.consume_if_eq('>') {
                    if chars.consume_if_eq('=') {
                        Token::TripleGreaterEqual
                    } else {
                        Token::TripleGreater
                    }
                } else if chars.consume_if_eq('=') {
                    Token::DoubleGreaterEqual
                } else {
                    Token::DoubleGreater
                }
            } else if chars.consume_if_eq('=') {
                Token::GreaterEqual
            } else {
                Token::Greater
            }
        }
        '<' => {
            if chars.consume_if_eq('<') {
                if chars.consume_if_eq('<') {
                    if chars.consume_if_eq('=') {
                        Token::TripleLessEqual
                    } else {
                        Token::TripleLess
                    }
                } else if chars.consume_if_eq('=') {
                    Token::DoubleLessEqual
                } else {
                    Token::DoubleLess
                }
            } else if chars.consume_if_eq('=') {
                Token::LessEqual
            } else {
                Token::Less
            }
        }
        '&' => {
            if chars.consume_if_eq('&') {
                Token::DoubleAmpersand
            } else if chars.consume_if_eq('=') {
                Token::AmpersandEqual
            } else {
                Token::Ampersand
            }
        }
        '|' => {
            if chars.consume_if_eq('|') {
                Token::DoubleBar
            } else if chars.consume_if_eq('=') {
                Token::BarEqual
            } else {
                Token::Bar
            }
        }
        '^' => {
            if chars.consume_if_eq('=') {
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
                let Some(mut ch) = chars.next() else {
                    return Err(Error::UnterminatedStringLiteral(start));
                };
                let action = match ch {
                    '"' => Action::Border,
                    '{' => Action::Expr,
                    '\\' => {
                        ch = match chars.next() {
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
                    let mut peeked = next(chars)?;
                    let term = crate::parser::parse_assign(chars, &mut peeked);
                    assert!(matches!(peeked, Some(Token::ClosingBrace)));
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
