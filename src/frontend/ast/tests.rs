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

#![cfg(test)]

use super::*;

macro_rules! index {
    ($line:tt : $column:tt) => {
        Index {
            line: $line,
            column: $column,
        }
    };
}

#[test]
fn get_pos_with_space() {
    let input = " foo  bar  baz\n";
    let mut chars_peekable = CharsPeekable::new(&input);
    let mut parser = Parser::new(&mut chars_peekable).unwrap();

    for (prev, start, end) in [
        (index!(0:0), index!(0:1), index!(0:4)),
        (index!(0:4), index!(0:6), index!(0:9)),
        (index!(0:9), index!(0:11), index!(0:14)),
        (index!(0:14), index!(1:0), index!(1:0)),
    ] {
        assert_eq!(parser.prev_end, prev);
        assert_eq!(parser.current.start, start);
        assert_eq!(parser.iter.index(), end);
        parser.consume_token().unwrap();
    }
}

#[test]
fn get_pos_without_space() {
    let input = "foo+bar";
    let mut chars_peekable = CharsPeekable::new(&input);
    let mut parser = Parser::new(&mut chars_peekable).unwrap();

    for (prev, start, end) in [
        (index!(0:0), index!(0:0), index!(0:3)),
        (index!(0:3), index!(0:3), index!(0:4)),
        (index!(0:4), index!(0:4), index!(0:7)),
        (index!(0:7), index!(0:7), index!(0:7)),
    ] {
        assert_eq!(parser.prev_end, prev);
        assert_eq!(parser.current.start, start);
        assert_eq!(parser.iter.index(), end);
        parser.consume_token().unwrap();
    }
}

macro_rules! pos {
    ($start_line:tt : $start_column:tt - $end_line:tt : $end_column:tt) => {
        Pos {
            start: index!($start_line:$start_column),
            end: index!($end_line:$end_column),
        }
    };
}

#[test]
fn parse_numeric_literal() {
    for input in ["12", "1.2", "12.", ".12", "6.02e23", "6.02e+23", "1.6e-19"] {
        let mut chars_peekable = CharsPeekable::new(&input);
        let mut parser = Parser::new(&mut chars_peekable).unwrap();
        let factor = parser.parse_factor(false).unwrap().unwrap();
        assert_eq!(factor.term, Term::NumericLiteral(String::from(input)));
    }
}

#[test]
fn parse_string_literal() {
    let input = r#""foo$x{10}${ bar }baz""#;
    let mut chars_peekable = CharsPeekable::new(&input);
    let mut parser = Parser::new(&mut chars_peekable).unwrap();
    let factor = parser.parse_factor(false).unwrap().unwrap();
    let Term::StringLiteral(components) = factor.term else {
        panic!("Not a string literal");
    };
    assert_eq!(
        components[0],
        StringLiteralComponent::String(String::from("foo"))
    );
    match &components[1] {
        StringLiteralComponent::String(s) => panic!("{}", s),
        StringLiteralComponent::PlaceHolder { format, value } => {
            assert_eq!(format, "x");
            let value = value.as_ref().unwrap();
            assert_eq!(value.term, Term::NumericLiteral(String::from("10")));
            assert_eq!(value.pos, pos!(0:7-0:9));
        }
    }
    match &components[2] {
        StringLiteralComponent::String(s) => panic!("{}", s),
        StringLiteralComponent::PlaceHolder { format, value } => {
            assert_eq!(format, "");
            let value = value.as_ref().unwrap();
            assert_eq!(value.term, Term::Identifier(String::from("bar")));
            assert_eq!(value.pos, pos!(0:13-0:16));
        }
    }
    assert_eq!(
        components[3],
        StringLiteralComponent::String(String::from("baz"))
    );
}

#[test]
fn parse_identifier() {
    let input = "foo";
    let mut chars_peekable = CharsPeekable::new(&input);
    let mut parser = Parser::new(&mut chars_peekable).unwrap();
    let factor = parser.parse_factor(false).unwrap().unwrap();
    assert_eq!(factor.term, Term::Identifier(String::from("foo")));
    assert_eq!(factor.pos, pos!(0:0-0:3));
}

#[test]
fn parse_addition() {
    let input = "foo + bar";
    let mut chars_peekable = CharsPeekable::new(&input);
    let mut parser = Parser::new(&mut chars_peekable).unwrap();
    let factor = parser.parse_binary_operator(false).unwrap().unwrap();
    assert_eq!(factor.pos, pos!(0:0-0:9));
    let Term::BinaryOperation {
        left_operand,
        operator,
        right_operand,
    } = factor.term
    else {
        panic!("Not a binary operation");
    };
    let left_operand = left_operand.unwrap();
    assert_eq!(left_operand.term, Term::Identifier(String::from("foo")));
    assert_eq!(left_operand.pos, pos!(0:0-0:3));
    assert_eq!(operator.term, Term::MethodName(String::from("add")));
    assert_eq!(operator.pos, pos!(0:4-0:5));
    let right_operand = right_operand.unwrap();
    assert_eq!(right_operand.term, Term::Identifier(String::from("bar")));
    assert_eq!(right_operand.pos, pos!(0:6-0:9));
}
