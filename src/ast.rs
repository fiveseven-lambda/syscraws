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
    If(Expr, Vec<Stmt>, Vec<Stmt>),
    While(Expr, Vec<Stmt>),
}

pub struct FuncDef {
    num_args: usize,
    tys: Vec<Option<ty::Ty>>,
    body: Vec<Stmt>,
}

pub enum Func {
    Builtin(ir::BuiltinFunc),
    UserDefined(FuncDef),
}

impl FuncDef {
    pub fn new(num_args: usize, tys: Vec<Option<ty::Ty>>, body: Vec<Stmt>) -> FuncDef {
        FuncDef {
            num_args,
            tys,
            body,
        }
    }
}

pub fn translate(
    stmts: &mut impl Iterator<Item = Stmt>,
    funcs: &[Vec<(Option<ty::Func>, Func)>],
    tys: &[Option<ty::Ty>],
    target: &mut Vec<ir::Stmt>,
    next: Option<usize>,
) -> Option<usize> {
    match stmts.next() {
        Some(Stmt::Expr(expr)) => {
            let next = translate(stmts, funcs, tys, target, next);
            let this = target.len();
            target.push(ir::Stmt::Expr(expr.translate(funcs, tys).1, next));
            Some(this)
        }
        Some(Stmt::If(cond, stmts_then, stmts_else)) => {
            let next_then = translate(&mut stmts_then.into_iter(), funcs, tys, target, next);
            let next_else = translate(&mut stmts_else.into_iter(), funcs, tys, target, next);
            let this = target.len();
            target.push(ir::Stmt::Branch(
                cond.translate(funcs, tys).1,
                next_then,
                next_else,
            ));
            Some(this)
        }
        Some(_) => todo!(),
        None => next,
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
    fn translate(
        self,
        funcs: &[Vec<(Option<ty::Func>, Func)>],
        tys: &[Option<ty::Ty>],
    ) -> (ty::Ty, ir::Expr) {
        match self {
            Expr::Variable(id) => (
                ty::Ty::reference(tys[id].clone().unwrap()),
                ir::Expr::Local(id),
            ),
            Expr::Integer(value) => (ty::Ty::integer(), ir::Expr::Imm(ir::Value::Integer(value))),
            Expr::Float(value) => (ty::Ty::float(), ir::Expr::Imm(ir::Value::Float(value))),
            Expr::Call(func, args) => match *func {
                Expr::Func(id) => {
                    let (args_ty, args_expr): (Vec<_>, Vec<_>) = args
                        .into_iter()
                        .map(|arg| arg.translate(funcs, tys))
                        .unzip();
                    let mut candidates = Vec::new();
                    'candidate: for (i, (func_ty, _)) in funcs[id].iter().enumerate() {
                        let func_ty = func_ty.as_ref().unwrap();
                        let mut converters = Vec::with_capacity(args_ty.len());
                        for (expected_ty, ty) in func_ty.args.iter().zip(&args_ty) {
                            match converter(ty, expected_ty) {
                                Some(funcs) => converters.push(funcs),
                                None => continue 'candidate,
                            }
                        }
                        candidates.push((i, converters))
                    }
                    if !candidates.is_empty() {
                        let (chosen, converters) = candidates.pop().unwrap();
                        match funcs[id][chosen] {
                            (ref func_ty, Func::Builtin(func)) => {
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
                                (
                                    func_ty.as_ref().unwrap().ret.clone(),
                                    ir::Expr::Call(
                                        ir::Expr::Imm(ir::Value::BuiltinFunc(func)).into(),
                                        args,
                                    ),
                                )
                            }
                            _ => panic!(),
                        }
                    } else {
                        panic!("{} candidates", candidates.len());
                    }
                }
                _ => todo!(),
            },
            _ => todo!(),
        }
    }
}

impl Expr {
    pub fn debug_print(&self, depth: usize) {
        let indent = "  ".repeat(depth);
        match self {
            Expr::Variable(id) => println!("{indent}variable({id})"),
            Expr::Global(id) => println!("{indent}global({id})"),
            Expr::Func(id) => println!("{indent}func({id})"),
            Expr::Integer(value) => println!("{indent}integer({value})"),
            Expr::Float(value) => println!("{indent}float({value})"),
            Expr::String(value) => println!("{indent}string({value})"),
            Expr::Call(func, args) => {
                println!("{indent}call");
                func.debug_print(depth + 1);
                for arg in args {
                    arg.debug_print(depth + 1);
                }
            }
        }
    }
}
impl Stmt {
    pub fn debug_print(&self, depth: usize) {
        let indent = "  ".repeat(depth);
        match self {
            Stmt::Expr(expr) => {
                println!("{indent}expression statement");
                expr.debug_print(depth + 1);
            }
            Stmt::Return(expr) => {
                println!("{indent}return statement");
                if let Some(expr) = expr {
                    expr.debug_print(depth + 1);
                }
            }
            Stmt::If(cond, stmts_then, stmts_else) => {
                println!("{indent}if statement");
                cond.debug_print(depth + 1);
                println!("{indent}then");
                for stmt in stmts_then {
                    stmt.debug_print(depth + 1);
                }
                println!("{indent}else");
                for stmt in stmts_else {
                    stmt.debug_print(depth + 1);
                }
            }
            Stmt::While(cond, stmts) => {
                println!("{indent}while statement");
                cond.debug_print(depth + 1);
                println!("{indent}do");
                for stmt in stmts {
                    stmt.debug_print(depth + 1);
                }
            }
        }
    }
}
impl FuncDef {
    pub fn debug_print(&self) {
        println!("{} args, {} locals", self.num_args, self.tys.len());
        for stmt in &self.body {
            stmt.debug_print(0);
        }
    }
}
