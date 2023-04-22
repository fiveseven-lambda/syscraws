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

use std::collections::{HashMap, VecDeque};

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
            Expr::String(value) => (ty!(String), ir::Expr::Imm(ir::Value::String(value))),
            Expr::Call(func, args) => match *func {
                Expr::Func(symbol_id) => {
                    let (args_ty, args_expr): (Vec<_>, Vec<_>) = args
                        .into_iter()
                        .map(|expr| expr.translate(vars_ty, overloads, funcs_ty))
                        .unzip();
                    let converters = [ir::BuiltinFunc::Deref, ir::BuiltinFunc::IntegerToFloat];
                    let candidates: Vec<_> = overloads[symbol_id]
                        .iter()
                        .enumerate()
                        .map(|(i, &func)| {
                            let func_ty = match func {
                                ir::Func::Builtin(func) => func.ty(),
                                ir::Func::UserDefined(id) => funcs_ty[id].clone(),
                            };
                            if func_ty.args.len() != args_ty.len() {
                                return None;
                            }
                            let mut vars = vec![None; func_ty.num_vars];
                            let paths: Option<Vec<_>> = args_ty
                                .iter()
                                .zip(&func_ty.args)
                                .map(|(given, expected)| {
                                    {
                                        let mut vars_tmp = Vec::new();
                                        if expected.identify(&given, &mut vars_tmp) {
                                            return Some(Vec::new());
                                        }
                                    }
                                    let mut prev = HashMap::new();
                                    prev.insert(given.clone(), None);
                                    let mut queue = VecDeque::from([given]);
                                    while let Some(ty) = queue.pop_front() {
                                        for converter in &converters {
                                            let ty::Func {
                                                num_vars,
                                                args,
                                                ret,
                                            } = converter.ty();
                                            let mut vars_converter = vec![None; num_vars];
                                            if args[0].identify(ty, &mut vars_converter) {
                                                let next = ret.subst(&vars_converter);
                                                let mut vars_tmp = vars.clone();
                                                if expected.identify(&next, &mut vars_tmp) {
                                                    let mut cur_ty = ty;
                                                    let mut path = vec![converter];
                                                    while let Some((c, prev_ty)) = prev[cur_ty] {
                                                        path.push(c);
                                                        cur_ty = prev_ty;
                                                    }
                                                    vars = vars_tmp;
                                                    return Some(path);
                                                } else {
                                                    prev.insert(next, Some((converter, ty)));
                                                }
                                            }
                                        }
                                    }
                                    None
                                })
                                .collect();
                            paths.map(|paths| {
                                let sum: usize = paths.iter().map(|path| path.len()).sum();
                                (i, sum, paths, func_ty.ret.subst(&vars))
                            })
                        })
                        .collect();
                    let mut shortest = None;
                    for (i, candidate) in candidates.iter().enumerate() {
                        if let Some((_, len, _, _)) = candidate {
                            match &mut shortest {
                                None => shortest = Some((len, i)),
                                Some(shortest) if shortest.0 >= len => *shortest = (len, i),
                                Some(_) => {}
                            }
                        }
                    }
                    if let Some((_, i)) = shortest {
                        let chosen = candidates[i].as_ref().unwrap();
                        let args: Vec<_> = args_expr
                            .into_iter()
                            .zip(&chosen.2)
                            .map(|(expr, converters)| {
                                converters.into_iter().fold(expr, |expr, &&converter| {
                                    ir::Expr::Call(
                                        ir::Expr::Imm(ir::Value::Func(ir::Func::Builtin(
                                            converter,
                                        )))
                                        .into(),
                                        vec![expr],
                                    )
                                })
                            })
                            .collect();
                        (
                            chosen.3.clone(),
                            ir::Expr::Call(
                                ir::Expr::Imm(ir::Value::Func(overloads[symbol_id][chosen.0]))
                                    .into(),
                                args,
                            ),
                        )
                    } else {
                        dbg!(args_ty);
                        dbg!(&overloads[symbol_id]);
                        dbg!(candidates);
                        panic!("no candidates");
                    }
                }
                _ => todo!(),
            },
            _ => todo!(),
        }
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
                    let (ty, expr) =
                        expr.unwrap()
                            .translate(self.vars_ty, self.overloads, self.funcs_ty);
                    let expr = converter(&ty, &self.ret_ty)
                        .unwrap()
                        .into_iter()
                        .rev()
                        .fold(expr, |expr, converter| {
                            ir::Expr::Call(
                                ir::Expr::Imm(ir::Value::Func(converter)).into(),
                                vec![expr],
                            )
                        });
                    self.add_stmt(ir::Stmt::Return(expr))
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
