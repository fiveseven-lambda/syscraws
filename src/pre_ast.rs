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

use crate::range::Range;
use either::Either;
use enum_iterator::Sequence;

mod debug_print;
pub use debug_print::_debug_print;
pub mod translate;

#[derive(Debug, Sequence)]
pub enum Operator {
    Plus,
    Minus,
    Recip,
    LogicalNot,
    BitNot,
    PreInc,
    PreDec,
    PostInc,
    PostDec,
    // ForwardShift,
    // BackwardShift,
    Mul,
    Div,
    Rem,
    Add,
    Sub,
    RightShift,
    LeftShift,
    BitAnd,
    BitXor,
    BitOr,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Equal,
    NotEqual,
    // LogicalAnd,
    // LogicalOr,
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    RemAssign,
    RightShiftAssign,
    LeftShiftAssign,
    // ForwardShiftAssign,
    // BackwardShiftAssign,
    BitAndAssign,
    BitXorAssign,
    BitOrAssign,
}

pub enum Term<'id> {
    Identifier(&'id str),
    Integer(i32),
    Float(f64),
    String(Vec<Either<String, Option<PTerm<'id>>>>),
    UnaryOperation {
        operator: Operator,
        operator_pos: Range,
        opt_operand: Option<Box<PTerm<'id>>>,
    },
    BinaryOperation {
        operator: Operator,
        operator_pos: Range,
        opt_left_operand: Option<Box<PTerm<'id>>>,
        opt_right_operand: Option<Box<PTerm<'id>>>,
    },
    TypeAnnotation {
        colon_pos: Range,
        opt_term: Option<Box<PTerm<'id>>>,
        opt_ty: Option<Box<PTerm<'id>>>,
    },
    ReturnType {
        arrow_pos: Range,
        opt_term: Option<Box<PTerm<'id>>>,
        opt_ty: Option<Box<PTerm<'id>>>,
    },
    Parenthesized {
        opt_antecedent: Option<Box<PTerm<'id>>>,
        elements: Vec<Arg<'id>>,
        has_trailing_comma: bool,
    },
    Assignment {
        operator: Operator,
        operator_pos: Range,
        opt_left_hand_side: Option<Box<PTerm<'id>>>,
        opt_right_hand_side: Option<Box<PTerm<'id>>>,
    },
}

pub enum Arg<'id> {
    Term(PTerm<'id>),
    Empty { comma_pos: Range },
}

pub struct PTerm<'id> {
    pub pos: Range,
    pub term: Term<'id>,
}

pub enum Stmt<'id> {
    Term(Option<PTerm<'id>>),
    Return(Option<PTerm<'id>>),
    If {
        cond: Option<PTerm<'id>>,
        if_pos: Range,
        stmt_then: Option<Box<PStmt<'id>>>,
        else_pos: Option<Range>,
        stmt_else: Option<Box<PStmt<'id>>>,
    },
    While {
        cond: Option<PTerm<'id>>,
        while_pos: Range,
        stmt: Option<Box<PStmt<'id>>>,
    },
    Block {
        antecedent: Option<PTerm<'id>>,
        stmts: Vec<PStmt<'id>>,
    },
}

pub struct PStmt<'id> {
    pub pos: Range,
    pub stmt: Stmt<'id>,
}
