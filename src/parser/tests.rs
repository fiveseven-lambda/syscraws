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

#![cfg(test)]

use super::*;
use crate::pre_ast;

macro_rules! match_stmt {
    ($stmt:expr => $pos:pat, $expected:pat) => {
        let PStmt {
            pos: $pos,
            stmt: $expected,
        } = $stmt
        else {
            $stmt._debug_print(0);
            panic!("Expected:\n{}", stringify!($expected));
        };
    };
}

macro_rules! match_term {
    ($term:expr => $pos:pat, $expected:pat) => {
        let PTerm {
            pos: $pos,
            term: $expected,
        } = $term
        else {
            $term._debug_print(0);
            panic!("Expected:\n{}", stringify!($expected));
        };
    };
}

#[test]
fn example() {
    let s = "abc;";
    let stmts = parse(&s).unwrap();
    if stmts.len() != 1 {
        pre_ast::_debug_print(&stmts);
        panic!();
    }
    assert_eq!(stmts.len(), 1);
    let mut iter = stmts.into_iter();
    let stmt = iter.next().unwrap();
    match_stmt!(stmt => stmt_pos, Stmt::Term(Some(term)));
    assert_eq!(&s[stmt_pos], "abc;");
    match_term!(term => term_pos, Term::Identifier("abc"));
    assert_eq!(&s[term_pos], "abc");
}

#[test]
fn arithmetic() {
    let s = "
        a + 100 - 5;
    ";
    let stmts = parse(&s).unwrap();
    if stmts.len() != 1 {
        pre_ast::_debug_print(&stmts);
        panic!();
    }
    assert_eq!(stmts.len(), 1);
    let mut iter = stmts.into_iter();
    let stmt = iter.next().unwrap();
    match_stmt!(stmt => stmt_pos, Stmt::Term(Some(term)));
    assert_eq!(&s[stmt_pos], "a + 100 - 5;");
    match_term!(
        term =>
        term_pos,
        Term::BinaryOperation {
            operator: Operator::Sub,
            operator_pos,
            opt_left_operand: Some(left),
            opt_right_operand: Some(right)
        }
    );
    assert_eq!(&s[term_pos], "a + 100 - 5");
    assert_eq!(&s[operator_pos], "-");
    {
        match_term!(
            *left =>
            left_pos,
            Term::BinaryOperation {
                operator: Operator::Add,
                operator_pos,
                opt_left_operand: Some(left),
                opt_right_operand: Some(right)
            }
        );
        assert_eq!(&s[left_pos], "a + 100");
        assert_eq!(&s[operator_pos], "+");
        match_term!(*left => left_pos, Term::Identifier("a"));
        assert_eq!(&s[left_pos], "a");
        match_term!(*right => right_pos, Term::Integer(100));
        assert_eq!(&s[right_pos], "100");
    }
    match_term!(*right => right_pos, Term::Integer(5));
    assert_eq!(&s[right_pos], "5");
}
