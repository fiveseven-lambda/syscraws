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

use crate::{ir, ty};
use enum_iterator::Sequence;
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

#[derive(Debug, Clone, Sequence)]
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
    Builtin(unsafe fn(Vec<ir::Value>) -> ir::Value),
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

pub fn operators() -> Vec<Vec<(Option<ty::Func>, Func)>> {
    let mut ret: Vec<_> = (0..Operator::CARDINALITY).map(|_| Vec::new()).collect();
    ret[Operator::Add as usize].push((
        Some(ty::Func {
            args: vec![ty::Ty::integer(), ty::Ty::integer()],
            ret: ty::Ty::integer(),
        }),
        Func::Builtin(ir::add_integer),
    ));
    ret[Operator::Add as usize].push((
        Some(ty::Func {
            args: vec![ty::Ty::float(), ty::Ty::float()],
            ret: ty::Ty::float(),
        }),
        Func::Builtin(ir::add_float),
    ));
    ret[Operator::Assign as usize].push((
        Some(ty::Func {
            args: vec![ty::Ty::reference(ty::Ty::integer()), ty::Ty::integer()],
            ret: ty::Ty::reference(ty::Ty::integer()),
        }),
        Func::Builtin(ir::add_integer),
    ));
    ret
}

pub fn translate(
    stmts: Vec<Stmt>,
    funcs: &[Vec<(Option<ty::Func>, Func)>],
    tys: &[Option<ty::Ty>],
    target: &mut Vec<ir::Stmt>,
    next: Option<usize>,
) {
    for (i, stmt) in stmts.into_iter().enumerate() {
        match stmt {
            Stmt::Expr(expr) => {
                target.push(ir::Stmt::Expr(expr.translate(funcs, tys).1, Some(i + 1)));
            }
            _ => todo!(),
        }
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
                    for (func_ty, func) in &funcs[id] {
                        let func_ty = func_ty.as_ref().unwrap();
                        println!("{:?}", func_ty);

                        if func_ty
                            .args
                            .iter()
                            .zip(&args_ty)
                            .all(|(expected_ty, ty)| expected_ty == ty)
                        {
                            let Func::Builtin(func) = func else {
                                todo!();
                            };
                            return (
                                func_ty.ret.clone(),
                                ir::Expr::Call(
                                    ir::Expr::Imm(ir::Value::Func(*func)).into(),
                                    args_expr,
                                ),
                            );
                        }
                    }
                    panic!();
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
