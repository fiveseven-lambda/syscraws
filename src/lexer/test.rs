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

use super::*;

#[test]
fn identifier() {
    let mut chars = CharsPeekable::new("abcdef _a__a_ _0123");
    assert!(matches!(
        next_token(&mut chars).unwrap(),
        Some(Token::Identifier("abcdef"))
    ));
    assert!(matches!(
        next_token(&mut chars).unwrap(),
        Some(Token::Identifier("_a__a_"))
    ));
    assert!(matches!(
        next_token(&mut chars).unwrap(),
        Some(Token::Identifier("_0123"))
    ));
    assert!(matches!(next_token(&mut chars).unwrap(), None));
}

#[test]
fn number() {
    let mut chars = CharsPeekable::new(
        " 105   10.5
          20.  .15",
    );
    assert!(matches!(
        next_token(&mut chars).unwrap(),
        Some(Token::Number("105"))
    ));
    assert!(matches!(
        next_token(&mut chars).unwrap(),
        Some(Token::Number("10.5"))
    ));
    assert!(matches!(
        next_token(&mut chars).unwrap(),
        Some(Token::Number("20."))
    ));
    assert!(matches!(
        next_token(&mut chars).unwrap(),
        Some(Token::Number(".15"))
    ));
}
#[test]
fn string() {}

#[test]
fn block_comment() {
    let mut chars = CharsPeekable::new("x/*comment*/y/*/comment*/z");
    assert!(matches!(
        next_token(&mut chars).unwrap(),
        Some(Token::Identifier("x"))
    ));
    assert!(matches!(
        next_token(&mut chars).unwrap(),
        Some(Token::Identifier("y"))
    ));
    assert!(matches!(
        next_token(&mut chars).unwrap(),
        Some(Token::Identifier("z"))
    ));
    let mut chars = CharsPeekable::new("/*/comment*/*/");
    assert!(matches!(
        next_token(&mut chars).unwrap(),
        Some(Token::Asterisk)
    ));
    assert!(matches!(
        next_token(&mut chars).unwrap(),
        Some(Token::Slash)
    ));
    let mut chars = CharsPeekable::new("a/*xxx/*xxx*/xxx*/b");
    assert!(matches!(
        next_token(&mut chars).unwrap(),
        Some(Token::Identifier("a"))
    ));
    assert!(matches!(
        next_token(&mut chars).unwrap(),
        Some(Token::Identifier("b"))
    ));
    let mut chars = CharsPeekable::new("/*");
    assert!(next_token(&mut chars).is_err());
    let mut chars = CharsPeekable::new("/*/");
    assert!(next_token(&mut chars).is_err());
}
