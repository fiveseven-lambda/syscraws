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

//! 抽象構文木 AST を定義する．

use std::cell::OnceCell;

mod debug_print;
pub mod type_check;

pub struct Program {
    pub funcs: Vec<Vec<(FuncTy, Func)>>,
    pub defs: Vec<Block>,
    pub vars: Vec<Option<Ty>>,
}

#[derive(Clone)]
pub enum Func {
    Builtin(BuiltinFunc),
    Defined(usize),
}

#[derive(Clone)]
pub struct FuncTy {
    pub num_vars: usize,
    pub args: Vec<Ty>,
    pub ret: Ty,
}

#[derive(Clone)]
pub enum Expr {
    Variable(usize),
    Func(usize, OnceCell<usize>),
    Integer(i32),
    Float(f64),
    String(String),
    Call(Box<Expr>, Vec<Expr>),
}

#[derive(Clone)]
pub enum Ty {
    Var(usize),
    Const {
        kind: crate::ty::Kind,
        args: Vec<Ty>,
    },
}

#[derive(Clone)]
pub enum Stmt {
    Expr(Expr),
    Return(Option<Expr>),
    If(Expr, Block, Block),
    While(Expr, Block),
}
#[derive(Clone)]
pub struct Block {
    stmts: Vec<Stmt>,
    size: usize,
}

impl Block {
    pub fn new() -> Self {
        Block {
            stmts: Vec::new(),
            size: 0,
        }
    }
    pub fn add_stmt(&mut self, stmt: Stmt) {
        self.size += match &stmt {
            Stmt::Expr(_) => 1,
            Stmt::Return(_) => 1,
            Stmt::If(_, block_then, block_else) => 1 + block_then.size + block_else.size,
            Stmt::While(_, block) => 1 + block.size,
        };
        self.stmts.push(stmt);
    }
}

#[derive(Debug, Clone)]
pub enum BuiltinFunc {
    AddInt,
    AddFloat,
    Assign,
    New,
    Delete,
    Deref,
    ToString,
    Concat,
}
