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

fn check(input: &str, expected: Vec<(TokenKind, &str)>) {
    let tokens = tokenize(input).unwrap();
    assert_eq!(tokens.len(), expected.len(), "different number of tokens");
    for (
        &Token {
            token_kind,
            start,
            end,
        },
        &(expected_token_kind, expected_token),
    ) in tokens.iter().zip(&expected)
    {
        assert_eq!(token_kind, expected_token_kind, "different kind");
        assert_eq!(&input[start..end], expected_token, "different token");
    }
}

#[test]
fn identifier() {
    check(
        "abcdef _a__a_ _0123",
        vec![
            (TokenKind::Identifier, "abcdef"),
            (TokenKind::Identifier, "_a__a_"),
            (TokenKind::Identifier, "_0123"),
        ],
    );
}

#[test]
fn number() {
    check(
        " 105   10.5
          20.  .15",
        vec![
            (TokenKind::Number, "105"),
            (TokenKind::Number, "10.5"),
            (TokenKind::Number, "20."),
            (TokenKind::Number, ".15"),
        ],
    );
}

#[test]
fn string() {
    check(
        r#"
            "abc"
            "abc\"def"
        "#,
        vec![
            (TokenKind::String, r#""abc""#),
            (TokenKind::String, r#""abc\"def""#),
        ],
    );
}

#[test]
fn block_comment() {
    check(
        "x/*comment*/y/*/comment*/z",
        vec![
            (TokenKind::Identifier, "x"),
            (TokenKind::Identifier, "y"),
            (TokenKind::Identifier, "z"),
        ],
    );
    check(
        "/*/comment*/*/",
        vec![(TokenKind::Asterisk, "*"), (TokenKind::Slash, "/")],
    );
    check(
        "a/*xxx/*xxx*/xxx*/b",
        vec![(TokenKind::Identifier, "a"), (TokenKind::Identifier, "b")],
    );
    assert!(tokenize("/*").is_err());
    assert!(tokenize("/*/").is_err());
}
