#![cfg(test)]

use super::*;

#[test]
fn parse_identifier() {
    let input = "foo";
    let mut chars_peekable = CharsPeekable::new(&input);
    let mut parser = Parser::new(&mut chars_peekable).unwrap();
    let term = parser.parse_factor(false).unwrap().unwrap();
    let Term::Identifier(name) = term.term else {
        panic!();
    };
    assert_eq!(name, "foo");
}

#[test]
fn get_pos_with_space() {
    let input = " foo  bar  baz\n";
    let mut chars_peekable = CharsPeekable::new(&input);
    let mut parser = Parser::new(&mut chars_peekable).unwrap();

    for (prev, start, end) in [
        (
            Index { line: 0, column: 0 },
            Index { line: 0, column: 1 },
            Index { line: 0, column: 4 },
        ),
        (
            Index { line: 0, column: 4 },
            Index { line: 0, column: 6 },
            Index { line: 0, column: 9 },
        ),
        (
            Index { line: 0, column: 9 },
            Index {
                line: 0,
                column: 11,
            },
            Index {
                line: 0,
                column: 14,
            },
        ),
        (
            Index {
                line: 0,
                column: 14,
            },
            Index { line: 1, column: 0 },
            Index { line: 1, column: 0 },
        ),
    ] {
        assert_eq!(parser.prev_token_end, prev);
        assert_eq!(parser.current_token.start_index, start);
        assert_eq!(parser.iter.peek_index(), end);
        parser.consume_token().unwrap();
    }
}

#[test]
fn get_pos_without_space() {
    let input = "foo+bar";
    let mut chars_peekable = CharsPeekable::new(&input);
    let mut parser = Parser::new(&mut chars_peekable).unwrap();

    for (prev, start, end) in [
        (
            Index { line: 0, column: 0 },
            Index { line: 0, column: 0 },
            Index { line: 0, column: 3 },
        ),
        (
            Index { line: 0, column: 3 },
            Index { line: 0, column: 3 },
            Index { line: 0, column: 4 },
        ),
        (
            Index { line: 0, column: 4 },
            Index { line: 0, column: 4 },
            Index { line: 0, column: 7 },
        ),
        (
            Index { line: 0, column: 7 },
            Index { line: 0, column: 7 },
            Index { line: 0, column: 7 },
        ),
    ] {
        assert_eq!(parser.prev_token_end, prev);
        assert_eq!(parser.current_token.start_index, start);
        assert_eq!(parser.iter.peek_index(), end);
        parser.consume_token().unwrap();
    }
}
