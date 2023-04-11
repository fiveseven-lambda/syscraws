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
    Expr(Expr),
    Return(Option<Expr>),
    If(Expr, Block, Block),
    While(Expr, Block),
}
pub struct Block {
    stmts: Vec<Stmt>,
    size: usize,
}

impl Block {
    pub fn new(stmts: Vec<Stmt>) -> Block {
        let size = stmts
            .iter()
            .map(|stmt| match stmt {
                Stmt::Expr(_) => 1,
                Stmt::Return(_) => 1,
                Stmt::If(_, block_then, block_else) => 1 + block_then.size + block_else.size,
                Stmt::While(_, block) => 1 + block.size,
            })
            .sum();
        Block { stmts, size }
    }
}

pub struct FuncDef {
    num_args: usize,
    tys: Vec<Option<ty::Ty>>,
    ret_ty: Option<ty::Ty>,
    body: Block,
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
        body: Block,
    ) -> FuncDef {
        FuncDef {
            num_args,
            tys,
            ret_ty,
            body,
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

struct Builder {
    stmts: Vec<ir::Stmt>,
}
impl Builder {
    fn new() -> Builder {
        Builder { stmts: Vec::new() }
    }
    fn add_stmt(&mut self, stmt: ir::Stmt) -> usize {
        let num = self.stmts.len();
        self.stmts.push(stmt);
        num
    }
    fn result(self) -> Vec<ir::Stmt> {
        self.stmts
    }
    fn add_stmts(
        &mut self,
        mut stmts: Vec<Stmt>,
        vars_ty: &[Option<ty::Ty>],
        overloads: &[Vec<Func>],
        funcs_ty: &[ty::Func],
        end: Option<usize>,
    ) -> Option<usize> {
        if let Some(stmt) = stmts.pop() {
            let cur = match stmt {
                Stmt::Expr(expr) => self.add_stmt(ir::Stmt::Expr(
                    expr.translate(vars_ty, overloads, funcs_ty).1,
                    end,
                )),
                Stmt::If(cond, block_true, block_false) => {
                    let next_true =
                        self.add_stmts(block_true.stmts, vars_ty, overloads, funcs_ty, end);
                    let next_false =
                        self.add_stmts(block_false.stmts, vars_ty, overloads, funcs_ty, end);
                    self.add_stmt(ir::Stmt::Branch(
                        cond.translate(vars_ty, overloads, funcs_ty).1,
                        next_true,
                        next_false,
                    ))
                }
                Stmt::While(cond, block) => {
                    let cur = self.stmts.len() + block.size;
                    let next = self.add_stmts(block.stmts, vars_ty, overloads, funcs_ty, Some(cur));
                    assert_eq!(cur, self.stmts.len());
                    self.add_stmt(ir::Stmt::Branch(
                        cond.translate(vars_ty, overloads, funcs_ty).1,
                        next,
                        end,
                    ))
                }
                Stmt::Return(expr) => self.add_stmt(ir::Stmt::Return(
                    expr.unwrap().translate(vars_ty, overloads, funcs_ty).1,
                )),
            };
            self.add_stmts(stmts, vars_ty, overloads, funcs_ty, Some(cur))
        } else {
            end
        }
    }
}

impl FuncDef {
    pub fn translate(self, overloads: &[Vec<Func>], funcs_ty: &[ty::Func]) -> ir::FuncDef {
        let mut builder = Builder::new();
        let entry = builder.add_stmts(self.body.stmts, &self.tys, overloads, funcs_ty, None);
        ir::FuncDef::new(self.tys.len(), builder.result(), entry)
    }
}
