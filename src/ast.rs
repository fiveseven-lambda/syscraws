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
use num::BigInt;

pub enum Expr {
    Identifier(String),
    Decl(String, Option<Box<PExpr>>),
    Integer(BigInt),
    Float(f64),
    String(String),
    Operator(Operator),
    Call(Box<PExpr>, Vec<PExpr>),
}
pub struct PExpr {
    pos: Range,
    expr: Expr,
}
impl PExpr {
    pub fn new(pos: Range, expr: Expr) -> PExpr {
        PExpr { pos, expr }
    }
}

#[derive(Debug)]
pub enum Operator {
    Plus,
    Minus,
    LogicalNot,
    BitNot,
    PreInc,
    PreDec,
    PostInc,
    PostDec,
    ForwardShift,
    BackwardShift,
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
    LogicalAnd,
    LogicalOr,
    Cast,
    Assign,
    ForwardShiftAssign,
    BackwardShiftAssign,
    MulAssign,
    DivAssign,
    RemAssign,
    AddAssign,
    SubAssign,
    RightShiftAssign,
    LeftShiftAssign,
    BitAndAssign,
    BitXorAssign,
    BitOrAssign,
}

pub enum Stmt {
    Expr(Option<PExpr>),
    Return(Option<PExpr>),
    If(PExpr, Box<PStmt>, Option<Box<PStmt>>),
    While(PExpr, Box<PStmt>),
    Block(Vec<PStmt>),
    Def {
        name: String,
        args: Vec<String>,
        body: Vec<PStmt>,
    },
}

pub struct PStmt {
    pos: Range,
    stmt: Stmt,
}

impl PStmt {
    pub fn new(pos: Range, stmt: Stmt) -> PStmt {
        PStmt { pos, stmt }
    }
}

impl PExpr {
    pub fn debug_print(&self, depth: usize) {
        let PExpr { pos, expr } = self;
        let indent = "  ".repeat(depth);
        match expr {
            Expr::Identifier(id) => println!("{pos:?}{indent}identifier({id})"),
            Expr::Decl(id, expr) => {
                println!("{pos:?}{indent}declaration({id})");
                if let Some(expr) = expr {
                    expr.debug_print(depth + 1);
                }
            }
            Expr::Integer(value) => println!("{pos:?}{indent}integer({value})"),
            Expr::Float(value) => println!("{pos:?}{indent}float({value})"),
            Expr::String(value) => println!("{pos:?}{indent}string({value})"),
            Expr::Operator(operator) => println!("{pos:?}{indent}operator({operator:?})"),
            Expr::Call(func, args) => {
                println!("{pos:?}{indent}call");
                func.debug_print(depth + 1);
                for arg in args {
                    arg.debug_print(depth + 1);
                }
            }
        }
    }
}
impl PStmt {
    pub fn debug_print(&self, depth: usize) {
        let PStmt { pos, stmt } = self;
        let indent = "  ".repeat(depth);
        match stmt {
            Stmt::Expr(None) => {
                println!("{pos:?}{indent}expr statement (empty)");
            }
            Stmt::Expr(Some(expr)) => {
                println!("{pos:?}{indent}expr statement");
                expr.debug_print(depth + 1);
            }
            Stmt::Return(expr) => {
                println!("{pos:?}{indent}return statement");
                if let Some(expr) = expr {
                    expr.debug_print(depth + 1);
                }
            }
            Stmt::Block(stmts) => {
                println!("{pos:?}{indent}block statement");
                for stmt in stmts {
                    stmt.debug_print(depth + 1);
                }
            }
            Stmt::If(cond, stmt_then, stmt_else) => {
                println!("{pos:?}{indent}if statement");
                cond.debug_print(depth + 1);
                stmt_then.debug_print(depth + 1);
                if let Some(stmt_else) = stmt_else {
                    stmt_else.debug_print(depth + 1);
                }
            }
            Stmt::While(cond, stmt) => {
                println!("{pos:?}{indent}while statement");
                cond.debug_print(depth + 1);
                stmt.debug_print(depth + 1);
            }
            Stmt::Def { name, args, body } => {
                println!(
                    "{pos:?}{indent}function definition: {name}({})",
                    args.join(", ")
                );
                for stmt in body {
                    stmt.debug_print(depth + 1);
                }
            }
        }
    }
}
