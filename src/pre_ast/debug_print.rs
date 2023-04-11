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

use super::{PStmt, PTerm, Stmt, Term};

pub fn _debug_print(stmts: &[PStmt]) {
    for stmt in stmts {
        stmt._debug_print(0);
    }
}

impl<'id> PStmt<'id> {
    fn _debug_print(&self, depth: usize) {
        let PStmt { pos, stmt } = self;
        let indent = "  ".repeat(depth);
        match stmt {
            Stmt::Term(Some(term)) => {
                eprintln!("{indent}{pos:?} Term statement");
                term._debug_print(depth + 1);
            }
            Stmt::Term(None) => {
                eprintln!("{indent}{pos:?} Term statement (empty)");
            }
            Stmt::Return(term) => {
                eprintln!("{indent}{pos:?} return statement");
                if let Some(term) = term {
                    term._debug_print(depth + 1);
                }
            }
            Stmt::If {
                cond,
                pos_if,
                stmt_then,
                pos_else,
                stmt_else,
            } => {
                eprintln!("{indent}{pos:?} If statement");
                eprintln!("{indent}  {pos_if:?} if");
                match cond {
                    Some(term) => term._debug_print(depth + 2),
                    None => eprintln!("{indent}    empty condition"),
                }
                eprintln!("{indent}  then");
                match stmt_then {
                    Some(stmt) => stmt._debug_print(depth + 2),
                    None => eprintln!("{indent}    empty statement"),
                }
                match pos_else {
                    Some(pos_else) => {
                        eprintln!("{indent}  {pos_else:?} else");
                        match stmt_else {
                            Some(stmt) => stmt._debug_print(depth + 2),
                            None => eprintln!("{indent}    empty statement"),
                        }
                    }
                    None => {}
                }
            }
            Stmt::While {
                cond,
                pos_while,
                stmt,
            } => {
                eprintln!("{indent}{pos:?} While statement");
                eprintln!("{indent}  {pos_while:?} while");
                match cond {
                    Some(term) => term._debug_print(depth + 2),
                    None => eprintln!("{indent}    empty condition"),
                }
                eprintln!("{indent}  do");
                match stmt {
                    Some(stmt) => stmt._debug_print(depth + 2),
                    None => eprintln!("{indent}    empty statement"),
                }
            }
            Stmt::Block { antecedent, stmts } => {
                eprintln!("{indent}{:?} block", self.pos);
                match antecedent {
                    Some(term) => term._debug_print(depth + 1),
                    None => eprintln!("{indent}  (No antecedent)"),
                }
                for stmt in stmts {
                    stmt._debug_print(depth + 1);
                }
            }
        }
    }
}

impl<'id> PTerm<'id> {
    fn _debug_print(&self, depth: usize) {
        let PTerm { pos, term } = self;
        let indent = "  ".repeat(depth);
        match term {
            Term::Identifier(name) => eprintln!("{indent}{pos:?} Identifier ({name})"),
            Term::Integer(value) => eprintln!("{indent}{pos:?} Integer ({value})"),
            Term::Float(value) => eprintln!("{indent}{pos:?} Float ({value})"),
            Term::String(value) => eprintln!("{indent}{pos:?} String ({value})"),
            Term::UnaryOperation {
                operator,
                pos_operator,
                operand,
            } => {
                eprintln!("{indent}{pos:?} Unary Operation");
                eprintln!("{indent}{pos_operator:?} {operator:?}");
                match operand {
                    Some(operand) => operand._debug_print(depth + 1),
                    None => eprintln!("{indent}  (operand is empty)"),
                }
            }
            Term::BinaryOperation {
                operator,
                pos_operator,
                left_operand,
                right_operand,
            } => {
                eprintln!("{indent}{pos:?} Binary Operation");
                eprintln!("{indent}{pos_operator:?} {operator:?}");
                match left_operand {
                    Some(term) => term._debug_print(depth + 1),
                    None => eprintln!("{indent}  (left operand is empty)"),
                }
                match right_operand {
                    Some(term) => term._debug_print(depth + 1),
                    None => eprintln!("{indent}  (right operand is empty)"),
                }
            }
            Term::TypeAnnotation {
                pos_colon,
                term,
                ty,
            } => {
                eprintln!("{indent}{pos:?} Type Annotation");
                eprintln!("{indent}{pos_colon:?} Colon");
                match term {
                    Some(term) => term._debug_print(depth + 1),
                    None => eprintln!("{indent}  (expr is empty)"),
                }
                match ty {
                    Some(ty) => ty._debug_print(depth + 1),
                    None => eprintln!("{indent}  (type is empty)"),
                }
            }
            Term::Parenthesized {
                antecedent,
                elements,
                has_trailing_comma,
            } => {
                eprintln!("{indent}{pos:?} Parenthesized Term");
                match antecedent {
                    Some(term) => term._debug_print(depth + 1),
                    None => eprintln!("{indent}  (No antecedent)"),
                }
                eprintln!(
                    "{indent}{} elements (trailing comma: {has_trailing_comma})",
                    elements.len()
                );
                for elem in elements {
                    match elem {
                        Ok(term) => term._debug_print(depth + 1),
                        Err(pos_comma) => {
                            eprintln!("{indent}  No element before comma at {pos_comma:?}")
                        }
                    }
                }
            }
            Term::Assignment {
                operator,
                pos_operator,
                left_hand_side,
                right_hand_side,
            } => {
                eprintln!("{indent}{pos:?} Assignment");
                eprintln!("{indent}{pos_operator:?} {operator:?}");
                match left_hand_side {
                    Some(term) => term._debug_print(depth + 1),
                    None => eprintln!("{indent}  (left hand side is empty)"),
                }
                match right_hand_side {
                    Some(term) => term._debug_print(depth + 1),
                    None => eprintln!("{indent}  (right hand side is empty)"),
                }
            }
        }
    }
}
