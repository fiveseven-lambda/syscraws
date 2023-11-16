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

use std::collections::VecDeque;
use std::iter;

use crate::{ir, ty};
use either::Either;
mod debug_print;

pub enum Expr {
    Variable(usize),
    Global(usize),
    Func(usize),
    Integer(i32),
    Float(f64),
    String(Vec<Either<String, Expr>>),
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
    tys: Vec<Option<ty::Expr>>,
    ret_ty: Option<ty::Expr>,
    body: Block,
}

impl FuncDef {
    pub fn new(
        num_args: usize,
        tys: Vec<Option<ty::Expr>>,
        ret_ty: Option<ty::Expr>,
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
        ty::Func {
            num_vars: 0,
            args,
            ret,
        }
    }
}

pub fn converter(from: &ty::Ty, to: &ty::Ty) -> Option<Vec<ir::Func>> {
    match (from.kind, to.kind) {
        (t1, t2) if t1 == t2 => Some(Vec::new()),
        (ty::Kind::Reference, ty::Kind::Reference) => None,
        (ty::Kind::Reference, _) => converter(&from.args[0], to).map(|mut fns| {
            fns.push(ir::Func::Builtin(ir::BuiltinFunc::Deref));
            fns
        }),
        (ty::Kind::Integer, ty::Kind::Float) => {
            Some(vec![ir::Func::Builtin(ir::BuiltinFunc::IntegerToFloat)])
        }
        _ => None,
    }
}

impl Expr {
    pub fn translate(
        self,
        vars_ty: &[ty::Ty],
        overloads: &[Vec<ir::Func>],
        funcs_ty: &[ty::Func],
    ) -> (ty::Ty, ir::Expr) {
        match self {
            Expr::Variable(id) => (ty!(Reference, vars_ty[id].clone()), ir::Expr::Local(id)),
            Expr::Integer(value) => (ty!(Integer), ir::Expr::Imm(ir::Value::Integer(value))),
            Expr::Float(value) => (ty!(Float), ir::Expr::Imm(ir::Value::Float(value))),
            Expr::String(value) => todo!(),
            Expr::Call(func, args) => match *func {
                Expr::Func(symbol_id) => {
                    let args: Vec<_> = args
                        .into_iter()
                        .map(|expr| expr.translate(vars_ty, overloads, funcs_ty))
                        .collect();
                    let converters = [ir::BuiltinFunc::Deref, ir::BuiltinFunc::IntegerToFloat];
                    let candidates: Vec<_> = overloads[symbol_id]
                        .iter()
                        .flat_map(|&func| {
                            let func_ty = match func {
                                ir::Func::Builtin(func) => func.ty(),
                                ir::Func::UserDefined(id) => funcs_ty[id].clone(),
                            };
                            if func_ty.args.len() != args.len() {
                                return None;
                            }
                            let mut vars = vec![None; func_ty.num_vars];
                            let mut cost_sum = 0;
                            args.iter()
                                .zip(&func_ty.args)
                                .map(|((given, expr), expected)| {
                                    let (given_depth, given_inner) = given.pause();
                                    let (expected_depth, expected_inner) = expected.pause();
                                    let mut queue =
                                        VecDeque::from([(0, given_inner.clone(), expr.clone())]);
                                    while let Some((cost, ty, expr)) = queue.pop_front() {
                                        let mut vars_tmp = vars.clone();
                                        if expected_inner.identify(&ty, &mut vars_tmp) {
                                            vars = vars_tmp;
                                            cost_sum += cost;
                                            return Some((given_depth, expected_depth, expr));
                                        }
                                        for &converter in &converters {
                                            let converter_ty = converter.ty();
                                            let mut converter_vars =
                                                vec![None; converter_ty.num_vars];
                                            if converter_ty.args[0]
                                                .identify(&ty, &mut converter_vars)
                                            {
                                                let next_ty =
                                                    converter_ty.ret.subst(&converter_vars);
                                                let (c, next_expr) = app(
                                                    ir::Expr::Imm(ir::Value::Func(
                                                        ir::Func::Builtin(converter),
                                                    )),
                                                    vec![(given_depth, 0, expr.clone())],
                                                );
                                                queue.push_back((cost + c + 1, next_ty, next_expr));
                                            }
                                        }
                                    }
                                    None
                                })
                                .collect::<Option<Vec<_>>>()
                                .map(|args| {
                                    let depth = args
                                        .iter()
                                        .map(|&(given, expected, _)| given.saturating_sub(expected))
                                        .max()
                                        .unwrap_or(0);
                                    let ty = (0..depth)
                                        .fold(func_ty.ret.subst(&vars), |ty, _| ty!(Sound, ty));
                                    let (cost, args) =
                                        app(ir::Expr::Imm(ir::Value::Func(func)), args);
                                    (cost_sum + cost, ty, args)
                                })
                        })
                        .collect();
                    for (cost, ty, candidate) in &candidates {
                        println!("cost {cost}, {ty:?}");
                        candidate._debug_print(0);
                    }
                    if let Some((_, ty, expr)) =
                        candidates.into_iter().min_by_key(|&(cost, _, _)| cost)
                    {
                        (ty, expr)
                    } else {
                        panic!("no candidates");
                    }
                }
                _ => todo!(),
            },
            _ => todo!(),
        }
    }
}

fn app(func: ir::Expr, args: Vec<(usize, usize, ir::Expr)>) -> (usize, ir::Expr) {
    if args.iter().any(|(given, expected, _)| given > expected) {
        let (cost, expr) = app(
            ir::Expr::Imm(ir::Value::Func(ir::Func::Builtin(ir::BuiltinFunc::App))),
            iter::once((0, 0, func))
                .chain(
                    args.into_iter()
                        .map(|(given, expected, expr)| (given, expected + 1, expr)),
                )
                .collect(),
        );
        (cost + 1, expr)
    } else {
        let mut sum_cost = 0;
        let args = args
            .into_iter()
            .map(|(given, expected, expr)| {
                (given..expected).fold(expr, |expr, i| {
                    let (cost, expr) = app(
                        ir::Expr::Imm(ir::Value::Func(ir::Func::Builtin(ir::BuiltinFunc::Const))),
                        vec![(i, 0, expr)],
                    );
                    sum_cost += cost + 1;
                    expr
                })
            })
            .collect();
        (sum_cost, ir::Expr::Call(func.into(), args))
    }
}

struct Builder<'def> {
    stmts: Vec<ir::Stmt>,
    vars_ty: &'def [ty::Ty],
    ret_ty: ty::Ty,
    overloads: &'def [Vec<ir::Func>],
    funcs_ty: &'def [ty::Func],
}
impl<'def> Builder<'def> {
    fn new(
        vars_ty: &'def [ty::Ty],
        ret_ty: ty::Ty,
        overloads: &'def [Vec<ir::Func>],
        funcs_ty: &'def [ty::Func],
    ) -> Builder<'def> {
        Builder {
            stmts: Vec::new(),
            ret_ty,
            vars_ty,
            overloads,
            funcs_ty,
        }
    }
    fn add_stmt(&mut self, stmt: ir::Stmt) -> usize {
        let num = self.stmts.len();
        self.stmts.push(stmt);
        num
    }
    fn result(self) -> Vec<ir::Stmt> {
        self.stmts
    }
    fn add_stmts(&mut self, mut stmts: Vec<Stmt>, end: Option<usize>) -> Option<usize> {
        if let Some(stmt) = stmts.pop() {
            let cur = match stmt {
                Stmt::Expr(expr) => self.add_stmt(ir::Stmt::Expr(
                    expr.translate(self.vars_ty, self.overloads, self.funcs_ty)
                        .1,
                    end,
                )),
                Stmt::If(cond, block_true, block_false) => {
                    let next_true = self.add_stmts(block_true.stmts, end);
                    let next_false = self.add_stmts(block_false.stmts, end);
                    self.add_stmt(ir::Stmt::Branch(
                        cond.translate(self.vars_ty, self.overloads, self.funcs_ty)
                            .1,
                        next_true,
                        next_false,
                    ))
                }
                Stmt::While(cond, block) => {
                    let cur = self.stmts.len() + block.size;
                    let next = self.add_stmts(block.stmts, Some(cur));
                    assert_eq!(cur, self.stmts.len());
                    self.add_stmt(ir::Stmt::Branch(
                        cond.translate(self.vars_ty, self.overloads, self.funcs_ty)
                            .1,
                        next,
                        end,
                    ))
                }
                Stmt::Return(expr) => {
                    let converters = [ir::BuiltinFunc::Deref, ir::BuiltinFunc::IntegerToFloat];
                    let (ty, expr) =
                        expr.unwrap()
                            .translate(self.vars_ty, self.overloads, self.funcs_ty);
                    let (given_depth, given_inner) = ty.pause();
                    let (expected_depth, expected_inner) = self.ret_ty.pause();
                    assert!(given_depth <= expected_depth);
                    let mut queue = VecDeque::from([(given_inner.clone(), expr.clone())]);
                    loop {
                        let Some((ty, expr)) = queue.pop_front() else {
                            panic!();
                        };
                        if &ty == expected_inner {
                            break self.add_stmt(ir::Stmt::Return(
                                (given_depth..expected_depth).fold(expr, |expr, i| {
                                    app(
                                        ir::Expr::Imm(ir::Value::Func(ir::Func::Builtin(
                                            ir::BuiltinFunc::Const,
                                        ))),
                                        vec![(i, 0, expr)],
                                    )
                                    .1
                                }),
                            ));
                        }
                        for &converter in &converters {
                            let converter_ty = converter.ty();
                            let mut converter_vars = vec![None; converter_ty.num_vars];
                            if converter_ty.args[0].identify(&ty, &mut converter_vars) {
                                let next_ty = converter_ty.ret.subst(&converter_vars);
                                let next_expr = app(
                                    ir::Expr::Imm(ir::Value::Func(ir::Func::Builtin(converter))),
                                    vec![(given_depth, 0, expr.clone())],
                                )
                                .1;
                                queue.push_back((next_ty, next_expr));
                            }
                        }
                    }
                }
            };
            self.add_stmts(stmts, Some(cur))
        } else {
            end
        }
    }
}

impl FuncDef {
    pub fn translate(self, overloads: &[Vec<ir::Func>], funcs_ty: &[ty::Func]) -> ir::FuncDef {
        let vars_ty: Vec<_> = self
            .tys
            .iter()
            .map(|expr| expr.as_ref().unwrap().subst(&[]))
            .collect();
        let ret_ty = self.ret_ty.unwrap().subst(&[]);
        let mut builder = Builder::new(&vars_ty, ret_ty, overloads, funcs_ty);
        let entry = builder.add_stmts(self.body.stmts, None);
        ir::FuncDef::new(self.tys.len(), builder.result(), entry)
    }
}
