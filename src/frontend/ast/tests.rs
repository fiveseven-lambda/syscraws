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
fn skip_comments() {
    for (is_on_new_line, input) in std::iter::repeat(true)
        .zip([
            "foo--comment\nbar",
            "foo/-comment-/\nbar",
            "foo\n/-comment-/bar",
            r"foo
            // comment
            |  comment
            \\ comment
            bar",
            r"foo
            ///  comment
            | // comment
            | |  comment
            | \\ comment
            \\\  comment
            bar",
            r"foo
            ////   comment
            | |    comment
            | \\// comment
            |   |  comment
            \\  \\ comment
            bar",
        ])
        .chain(std::iter::repeat(false).zip([
            "foo/-com-//-ment-/bar",
            "foo/-/-com-//-ment-/-/bar",
            "foo/-/comment-/bar",
            "foo/-/-/comment-/--/bar",
            "foo/-com//-ment-/-/bar",
            "foo/-com\nment-/bar",
        ]))
    {
        let mut chars_peekable = CharsPeekable::new(&input);
        let mut parser = Parser::new(&mut chars_peekable).unwrap();
        assert_eq!(
            parser.current.token,
            Some(Token::Identifier(String::from("foo")))
        );
        let foo_pos = parser.current_pos();
        parser.consume_token().unwrap();
        assert_eq!(
            parser.current.token,
            Some(Token::Identifier(String::from("bar")))
        );
        assert_eq!(parser.current.is_on_new_line, is_on_new_line);
        let bar_pos = parser.current_pos();
        parser.consume_token().unwrap();
        assert!(parser.current.token.is_none());
        let lines = chars_peekable.lines();
        assert_eq!(
            &input[lines[foo_pos.start.line].start + foo_pos.start.column
                ..lines[foo_pos.end.line].start + foo_pos.end.column],
            "foo"
        );
        assert_eq!(
            &input[lines[bar_pos.start.line].start + bar_pos.start.column
                ..lines[bar_pos.end.line].start + bar_pos.end.column],
            "bar"
        );
    }
    for input in ["foo//\\\\", "foo/-\n-/ //\\\\"] {
        let mut chars_peekable = CharsPeekable::new(&input);
        let mut parser = Parser::new(&mut chars_peekable).unwrap();
        assert!(parser.consume_token().is_err());
    }
}

#[test]
fn invalid_block_comments_in_string_literal() {
    let input = r#"
        "${ //
            | comment
            \\
        }"
    "#;
    let mut chars_peekable = CharsPeekable::new(input);
    assert!(Parser::new(&mut chars_peekable).is_err());
}

#[test]
fn block_comment_at_beginning() {
    let input = r"//
                  | comment
                  \\
    foo";
    let mut chars_peekable = CharsPeekable::new(input);
    let mut parser = Parser::new(&mut chars_peekable).unwrap();
    assert_eq!(
        parser.current.token,
        Some(Token::Identifier(String::from("foo")))
    );
    parser.consume_token().unwrap();
    assert!(parser.current.token.is_none());
}

#[test]
fn parse_numeric_literal() {
    for input in ["12", "1.2", "12.", ".12", "6.02e23", "6.02e+23", "1.6e-19"] {
        let mut chars_peekable = CharsPeekable::new(&input);
        let mut parser = Parser::new(&mut chars_peekable).unwrap();
        let factor = parser.parse_atom(false).unwrap().unwrap();
        assert_eq!(factor.pos, pos!(0:0-0:(input.len())));
        assert_eq!(factor.term, Term::NumericLiteral(String::from(input)));
    }
}

#[test]
fn parse_string_literal() {
    let input = r#""foo$x{10}${ bar }baz""#;
    let mut chars_peekable = CharsPeekable::new(&input);
    let mut parser = Parser::new(&mut chars_peekable).unwrap();
    let factor = parser.parse_atom(false).unwrap().unwrap();
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
    let factor = parser.parse_atom(false).unwrap().unwrap();
    assert_eq!(factor.term, Term::Identifier(String::from("foo")));
    assert_eq!(factor.pos, pos!(0:0-0:3));
}

#[test]
fn parse_field() {
    let input = "10.foo.20.bar";
    let mut chars_peekable = CharsPeekable::new(&input);
    let mut parser = Parser::new(&mut chars_peekable).unwrap();
    let term_10_foo_20_bar = parser.parse_factor(false).unwrap().unwrap();
    assert_eq!(term_10_foo_20_bar.pos, pos!(0:0-0:13));
    let Term::FieldByName {
        term_left: term_10_foo_20,
        name: field_bar,
    } = term_10_foo_20_bar.term
    else {
        panic!("Not a field by name");
    };
    assert_eq!(field_bar, "bar");
    assert_eq!(term_10_foo_20.pos, pos!(0:0-0:9));
    let Term::FieldByNumber {
        term_left: term_10_foo,
        number: field_20,
    } = term_10_foo_20.term
    else {
        panic!("Not a field by number");
    };
    assert_eq!(field_20, "20");
    assert_eq!(term_10_foo.pos, pos!(0:0-0:6));
    let Term::FieldByName {
        term_left: term_10,
        name: field_foo,
    } = term_10_foo.term
    else {
        panic!("Not a field by name");
    };
    assert_eq!(field_foo, "foo");
    assert_eq!(term_10.pos, pos!(0:0-0:2));
    assert_eq!(term_10.term, Term::NumericLiteral(String::from("10")));
}

#[test]
fn parse_addition() {
    let input = "foo + bar";
    let mut chars_peekable = CharsPeekable::new(&input);
    let mut parser = Parser::new(&mut chars_peekable).unwrap();
    let factor = parser.parse_binary_operation(false).unwrap().unwrap();
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

#[test]
fn parse_function_definition() {
    let input = "
    func foo(x: int, y: int): int
        x + y
    end
    ";
    let mut chars_peekable = CharsPeekable::new(&input);
    let mut parser = Parser::new(&mut chars_peekable).unwrap();
    let (name, definition) = parser.parse_function_definition().unwrap();
    assert_eq!(name.name, Some(String::from("foo")));
    for (parameter, expected_parameter_name) in
        definition.parameters.unwrap().iter().zip(["x", "y"])
    {
        match parameter {
            ListElement::Empty { comma_pos } => panic!("{comma_pos}"),
            ListElement::NonEmpty(parameter) => {
                let Term::TypeAnnotation {
                    term_left,
                    colon_pos: _,
                    term_right,
                } = &parameter.term
                else {
                    panic!("{}", parameter.pos);
                };
                let Term::Identifier(parameter_name) = &term_left.term else {
                    panic!("{}", term_left.pos);
                };
                assert_eq!(parameter_name, expected_parameter_name);
                assert_eq!(Term::IntegerTy, term_right.as_ref().unwrap().term);
            }
        }
    }
}
