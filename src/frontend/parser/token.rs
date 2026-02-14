/*
 * Copyright (c) 2023-2026 Atsushi Komaba
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
use super::ast;
use crate::log::{Index, ParseError, Pos};

/**
 * Information on a token.
 */
pub struct TokenInfo {
    /**
     * Token.
     */
    pub token: Option<Token>,
    /**
     * Start index of the token.
     */
    pub start: Index,
    /**
     * Whether there is a line break between this token and the previous
     * one.
     */
    pub is_on_new_line: bool,
}

/**
 * A token.
 */
#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Digits(String),
    StringLiteral(Vec<ast::StringLiteralComponent>),
    KeywordImport,
    KeywordExport,
    KeywordStruct,
    KeywordFunc,
    KeywordMethod,
    KeywordIf,
    KeywordElse,
    KeywordWhile,
    KeywordBreak,
    KeywordContinue,
    KeywordReturn,
    KeywordEnd,
    KeywordVar,
    KeywordInt,
    KeywordFloat,
    Underscore,
    Identifier(String),
    Plus,
    PlusEqual,
    Hyphen,
    HyphenEqual,
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
    Less,
    LessEqual,
    DoubleLess,
    DoubleLessEqual,
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
    Tilde,
    Dollar,
    OpeningParenthesis,
    ClosingParenthesis,
    OpeningBracket,
    ClosingBracket,
    OpeningBrace,
    ClosingBrace,
}

/**
 * Reads a token.
 *
 * # Errors
 * - [`ParseError::UnexpectedCharacter`]: The first non-whitespace character
 *   is invalid as the beginning of a token.
 * - [`ParseError::UnterminatedStringLiteral`]: EOF is reached while reading
 *   a string literal.
 * - [`ParseError::InvalidEscapeSequence`]: Invalid character after a
 *   backslash `\` in a string literal.
 * - [`ParseError::UnexpectedTokenInStringLiteral`]: Unexpected token while
 *   reading a placeholder `${` ... `}` in a string literal.
 * - [`ParseError::InvalidBlockComment`]: `is_on_new_line` is `false` when a
 *   block comment starts.
 */
pub fn read(
    iter: &mut CharsPeekable,
    mut is_on_new_line: bool,
    file_index: usize,
) -> Result<TokenInfo, ParseError> {
    let (start_index, first_ch) = loop {
        let Some(ch) = iter.peek() else {
            return Ok(TokenInfo {
                token: None,
                start: iter.index(),
                is_on_new_line,
            });
        };
        if ch.is_ascii_whitespace() {
            if ch == '\n' {
                is_on_new_line = true
            }
            iter.consume();
        } else {
            break (iter.index(), ch);
        }
    };
    iter.consume();
    let token = match first_ch {
        '0'..='9' => {
            let mut value = first_ch.to_string();
            let mut after_e = false;
            while let Some(ch) = iter.peek() {
                after_e = match ch {
                    'e' | 'E' => true,
                    '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' => false,
                    '+' | '-' if after_e => false,
                    _ => break,
                };
                if ch != '_' {
                    value.push(ch);
                }
                iter.consume();
            }
            Token::Digits(value)
        }
        '"' => {
            let mut components = Vec::new();
            let mut string = String::new();
            loop {
                let Some(ch1) = iter.peek() else {
                    return Err(ParseError::UnterminatedStringLiteral(Pos {
                        file: file_index,
                        start: start_index,
                        end: iter.index(),
                    }));
                };
                let index1 = iter.index();
                iter.consume();
                match ch1 {
                    '$' => {
                        let index2 = iter.index();
                        if !string.is_empty() {
                            components.push(ast::StringLiteralComponent::String(std::mem::take(
                                &mut string,
                            )));
                        }
                        // Since the usage of format strings is undecided, the current
                        // implementation is kept simple for now.
                        let mut format = String::new();
                        loop {
                            let Some(ch2) = iter.peek() else {
                                return Err(ParseError::UnterminatedStringLiteral(Pos {
                                    file: file_index,
                                    start: start_index,
                                    end: iter.index(),
                                }));
                            };
                            iter.consume();
                            match ch2 {
                                '"' => todo!(),
                                '{' => break,
                                ch => format.push(ch),
                            }
                        }
                        let mut parser = {
                            let start = iter.index();
                            let first_token = read(iter, false, file_index)?;
                            super::Parser {
                                iter,
                                current: first_token,
                                prev_end: start,
                                file_index,
                            }
                        };
                        let value = parser.parse_disjunction(true)?;
                        match parser.current.token {
                            Some(Token::ClosingBrace) => {
                                components.push(ast::StringLiteralComponent::PlaceHolder {
                                    format,
                                    value,
                                });
                            }
                            Some(_) => {
                                return Err(ParseError::UnexpectedTokenInStringLiteral {
                                    unexpected_token_pos: parser.current_pos(),
                                    dollar_pos: Pos {
                                        file: file_index,
                                        start: index1,
                                        end: iter.index(),
                                    },
                                });
                            }
                            None => {
                                return Err(ParseError::UnterminatedStringLiteral(Pos {
                                    file: file_index,
                                    start: start_index,
                                    end: iter.index(),
                                }));
                            }
                        }
                    }
                    '\\' => {
                        let Some(ch) = iter.peek() else {
                            return Err(ParseError::UnterminatedStringLiteral(Pos {
                                file: file_index,
                                start: start_index,
                                end: iter.index(),
                            }));
                        };
                        iter.consume();
                        string.push(match ch {
                            'n' => '\n',
                            'r' => '\r',
                            't' => '\t',
                            '"' => '\"',
                            '\\' => '\\',
                            '0' => '\0',
                            '\'' => '\'',
                            _ => {
                                return Err(ParseError::InvalidEscapeSequence(Pos {
                                    file: file_index,
                                    start: index1,
                                    end: iter.index(),
                                }));
                            }
                        });
                    }
                    '"' => {
                        if !string.is_empty() {
                            components.push(ast::StringLiteralComponent::String(std::mem::take(
                                &mut string,
                            )));
                        }
                        break Token::StringLiteral(components);
                    }
                    ch => string.push(ch),
                }
            }
        }
        _ if first_ch == '_' || unicode_ident::is_xid_start(first_ch) => {
            let mut name = first_ch.to_string();
            while let Some(ch) = iter.peek() {
                if unicode_ident::is_xid_continue(ch) {
                    name.push(ch);
                    iter.consume();
                } else {
                    break;
                }
            }
            match name.as_str() {
                "import" => Token::KeywordImport,
                "export" => Token::KeywordExport,
                "struct" => Token::KeywordStruct,
                "func" => Token::KeywordFunc,
                "method" => Token::KeywordMethod,
                "if" => Token::KeywordIf,
                "else" => Token::KeywordElse,
                "while" => Token::KeywordWhile,
                "break" => Token::KeywordBreak,
                "continue" => Token::KeywordContinue,
                "return" => Token::KeywordReturn,
                "end" => Token::KeywordEnd,
                "var" => Token::KeywordVar,
                "int" => Token::KeywordInt,
                "float" => Token::KeywordFloat,
                "_" => Token::Underscore,
                _ => Token::Identifier(name),
            }
        }
        '+' => {
            if iter.consume_if('=') {
                Token::PlusEqual
            } else {
                Token::Plus
            }
        }
        '-' => {
            if iter.consume_if('-') {
                skip_line_comment(iter);
                return read(iter, true, file_index);
            } else if iter.consume_if('=') {
                Token::HyphenEqual
            } else if iter.consume_if('>') {
                Token::HyphenGreater
            } else {
                Token::Hyphen
            }
        }
        '*' => {
            if iter.consume_if('=') {
                Token::AsteriskEqual
            } else {
                Token::Asterisk
            }
        }
        '/' => {
            if iter.consume_if('-') {
                let mut num_hyphens = 1;
                while let Some('-') = iter.peek() {
                    num_hyphens += 1;
                    iter.consume();
                }
                let mut hyphen_count = 0;
                loop {
                    match iter.peek() {
                        None => {
                            return Err(ParseError::UnterminatedComment(Pos {
                                file: file_index,
                                start: start_index,
                                end: iter.index(),
                            }));
                        }
                        Some('-') => hyphen_count += 1,
                        Some('/') if hyphen_count >= num_hyphens => break,
                        _ => hyphen_count = 0,
                    };
                    iter.consume();
                }
                iter.consume();
                return read(iter, is_on_new_line, file_index);
            } else if iter.consume_if('/') {
                if !is_on_new_line {
                    return Err(ParseError::InvalidBlockComment {
                        start_pos: Pos {
                            file: file_index,
                            start: start_index,
                            end: iter.index(),
                        },
                    });
                }
                let mut num_slashes = 2;
                while let Some('/') = iter.peek() {
                    num_slashes += 1;
                    iter.consume();
                }
                loop {
                    let Some(ch) = iter.peek() else {
                        return Err(ParseError::UnterminatedComment(Pos {
                            file: file_index,
                            start: start_index,
                            end: iter.index(),
                        }));
                    };
                    iter.consume();
                    if ch == '\n' {
                        while iter.peek().is_some_and(|ch| ch.is_ascii_whitespace()) {
                            iter.consume();
                        }
                        let mut backslash_count = 0;
                        while let Some('\\') = iter.peek() {
                            backslash_count += 1;
                            if backslash_count == num_slashes {
                                skip_line_comment(iter);
                                return read(iter, true, file_index);
                            }
                            iter.consume();
                        }
                    }
                }
            } else if iter.consume_if('=') {
                Token::SlashEqual
            } else {
                Token::Slash
            }
        }
        '%' => {
            if iter.consume_if('=') {
                Token::PercentEqual
            } else {
                Token::Percent
            }
        }
        '=' => {
            if iter.consume_if('=') {
                Token::DoubleEqual
            } else if iter.consume_if('>') {
                Token::EqualGreater
            } else {
                Token::Equal
            }
        }
        '!' => {
            if iter.consume_if('=') {
                Token::ExclamationEqual
            } else {
                Token::Exclamation
            }
        }
        '>' => {
            if iter.consume_if('>') {
                if iter.consume_if('=') {
                    Token::DoubleGreaterEqual
                } else {
                    Token::DoubleGreater
                }
            } else if iter.consume_if('=') {
                Token::GreaterEqual
            } else {
                Token::Greater
            }
        }
        '<' => {
            if iter.consume_if('<') {
                if iter.consume_if('=') {
                    Token::DoubleLessEqual
                } else {
                    Token::DoubleLess
                }
            } else if iter.consume_if('=') {
                Token::LessEqual
            } else {
                Token::Less
            }
        }
        '&' => {
            if iter.consume_if('&') {
                Token::DoubleAmpersand
            } else if iter.consume_if('=') {
                Token::AmpersandEqual
            } else {
                Token::Ampersand
            }
        }
        '|' => {
            if iter.consume_if('|') {
                Token::DoubleBar
            } else if iter.consume_if('=') {
                Token::BarEqual
            } else {
                Token::Bar
            }
        }
        '^' => {
            if iter.consume_if('=') {
                Token::CircumflexEqual
            } else {
                Token::Circumflex
            }
        }
        ':' => Token::Colon,
        ';' => Token::Semicolon,
        ',' => Token::Comma,
        '?' => Token::Question,
        '~' => Token::Tilde,
        '(' => Token::OpeningParenthesis,
        ')' => Token::ClosingParenthesis,
        '[' => Token::OpeningBracket,
        ']' => Token::ClosingBracket,
        '{' => Token::OpeningBrace,
        '}' => Token::ClosingBrace,
        '.' => Token::Dot,
        '$' => Token::Dollar,
        _ => {
            return Err(ParseError::UnexpectedCharacter(Pos {
                file: file_index,
                start: start_index,
                end: iter.index(),
            }));
        }
    };
    Ok(TokenInfo {
        token: Some(token),
        start: start_index,
        is_on_new_line,
    })
}

/**
 * Skips until the end of line.
 */
fn skip_line_comment(iter: &mut CharsPeekable) {
    loop {
        let ch = iter.peek();
        iter.consume();
        if let None | Some('\n') = ch {
            break;
        }
    }
}
