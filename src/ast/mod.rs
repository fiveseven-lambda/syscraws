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

use std::env::vars;

use crate::{ir, ty};
use num::BigInt;
mod debug_print;

pub enum Expr {
    Variable(usize),
    Global(usize),
    Func(usize),
    Integer(BigInt),
    Float(f64),
    String(String),
    Call(Box<Expr>, Vec<Expr>),
}

pub enum Stmt {
    Empty,
    Expr(Expr),
    Return(Option<Expr>),
    If(Expr, Box<StmtWithSize>, Box<StmtWithSize>),
    While(Expr, Box<StmtWithSize>),
    Block(Vec<StmtWithSize>),
}
pub struct StmtWithSize {
    stmt: Stmt,
    size: usize,
}

impl StmtWithSize {
    pub fn new_empty() -> StmtWithSize {
        StmtWithSize {
            stmt: Stmt::Empty,
            size: 0,
        }
    }
    pub fn new_expr(expr: Expr) -> StmtWithSize {
        StmtWithSize {
            stmt: Stmt::Expr(expr),
            size: 1,
        }
    }
    pub fn new_return(expr: Option<Expr>) -> StmtWithSize {
        StmtWithSize {
            stmt: Stmt::Return(expr),
            size: 1,
        }
    }
    pub fn new_block(stmts: Vec<StmtWithSize>) -> StmtWithSize {
        let size: usize = stmts.iter().map(|StmtWithSize { size, .. }| size).sum();
        StmtWithSize {
            stmt: Stmt::Block(stmts),
            size,
        }
    }
    pub fn new_if(cond: Expr, stmt_true: StmtWithSize, stmt_false: StmtWithSize) -> StmtWithSize {
        let size = 1 + stmt_true.size + stmt_false.size;
        StmtWithSize {
            stmt: Stmt::If(cond, stmt_true.into(), stmt_false.into()),
            size,
        }
    }
    pub fn new_while(cond: Expr, stmt: StmtWithSize) -> StmtWithSize {
        let size = 1 + stmt.size;
        StmtWithSize {
            stmt: Stmt::While(cond, stmt.into()),
            size,
        }
    }
}

pub struct FuncDef {
    num_args: usize,
    tys: Vec<Option<ty::Ty>>,
    ret_ty: Option<ty::Ty>,
    body: Vec<StmtWithSize>,
    size: usize,
}

#[derive(Debug, Clone, Copy)]
pub enum Func {
    Builtin(ir::BuiltinFunc),
    UserDefined(usize),
}

impl FuncDef {
    pub fn new(
        num_args: usize,
        tys: Vec<Option<ty::Ty>>,
        ret_ty: Option<ty::Ty>,
        body: Vec<StmtWithSize>,
    ) -> FuncDef {
        let size: usize = body.iter().map(|StmtWithSize { size, .. }| size).sum();
        FuncDef {
            num_args,
            tys,
            ret_ty,
            body,
            size,
        }
    }
    pub fn get_ty(&self) -> ty::Func {
        let args = self
            .tys
            .iter()
            .take(self.num_args)
            .map(|ty| ty.clone().unwrap())
            .collect();
        let ret = self.ret_ty.clone().unwrap();
        ty::Func { args, ret }
    }
}

pub fn converter(from: &ty::Ty, to: &ty::Ty) -> Option<Vec<Func>> {
    match (from.kind, to.kind) {
        (t1, t2) if t1 == t2 => Some(Vec::new()),
        (ty::Kind::Reference, ty::Kind::Reference) => None,
        (ty::Kind::Reference, _) => converter(&from.args[0], to).map(|mut fns| {
            fns.push(Func::Builtin(ir::BuiltinFunc::Deref));
            fns
        }),
        (ty::Kind::Integer, ty::Kind::Float) => {
            Some(vec![Func::Builtin(ir::BuiltinFunc::IntegerToFloat)])
        }
        _ => None,
    }
}

impl Expr {
    pub fn translate(
        self,
        vars_ty: &[Option<ty::Ty>],
        overloads: &[Vec<Func>],
        funcs_ty: &[ty::Func],
    ) -> (ty::Ty, ir::Expr) {
        match self {
            Expr::Variable(id) => (
                ty::Ty::reference(vars_ty[id].clone().unwrap()),
                ir::Expr::Local(id),
            ),
            Expr::Integer(value) => (ty::Ty::integer(), ir::Expr::Imm(ir::Value::Integer(value))),
            Expr::Float(value) => (ty::Ty::float(), ir::Expr::Imm(ir::Value::Float(value))),
            Expr::String(value) => (ty::Ty::string(), ir::Expr::Imm(ir::Value::String(value))),
            Expr::Call(func, args) => match *func {
                Expr::Func(symbol_id) => {
                    let (args_ty, args_expr): (Vec<_>, Vec<_>) = args
                        .into_iter()
                        .map(|arg| arg.translate(vars_ty, overloads, funcs_ty))
                        .unzip();
                    let mut candidates = Vec::new();
                    'candidate: for &func in overloads[symbol_id].iter() {
                        let func_ty = match func {
                            Func::Builtin(func) => func.ty(),
                            Func::UserDefined(id) => funcs_ty[id].clone(),
                        };
                        if func_ty.args.len() != args_ty.len() {
                            continue 'candidate;
                        }
                        let mut converters = Vec::with_capacity(args_ty.len());
                        for (expected_ty, ty) in func_ty.args.iter().zip(&args_ty) {
                            match converter(ty, expected_ty) {
                                Some(funcs) => converters.push(funcs),
                                None => continue 'candidate,
                            }
                        }
                        candidates.push((func, func_ty.ret, converters))
                    }
                    if !candidates.is_empty() {
                        let (chosen, ret_ty, converters) = candidates.pop().unwrap();
                        let args = args_expr
                            .into_iter()
                            .zip(converters)
                            .map(|(arg, converters)| {
                                converters.into_iter().rev().fold(arg, |arg, converter| {
                                    let func = match converter {
                                        Func::Builtin(func) => func,
                                        _ => panic!(),
                                    };
                                    ir::Expr::Call(
                                        ir::Expr::Imm(ir::Value::BuiltinFunc(func)).into(),
                                        vec![arg],
                                    )
                                })
                            })
                            .collect();
                        let func = match chosen {
                            Func::Builtin(func) => ir::Value::BuiltinFunc(func),
                            Func::UserDefined(id) => ir::Value::UserDefinedFunc(id),
                        };
                        (ret_ty, ir::Expr::Call(ir::Expr::Imm(func).into(), args))
                    } else {
                        panic!("no candidates")
                    }
                }
                _ => todo!(),
            },
            _ => todo!(),
        }
    }
}

impl FuncDef {
    pub fn translate(self, overloads: &[Vec<Func>], funcs_ty: &[ty::Func]) -> ir::FuncDef {
        let mut target = Vec::with_capacity(self.size);
        for (i, stmt) in self.body.into_iter().enumerate() {
            match stmt.stmt {
                Stmt::Expr(expr) => {
                    target.push(ir::Stmt::Expr(
                        expr.translate(&self.tys, overloads, funcs_ty).1,
                        i + 1,
                    ));
                }
                _ => todo!(),
            }
        }
        ir::debug_print(&target);
        ir::FuncDef::new(self.tys.len(), target)
    }
}
