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

use std::hint::unreachable_unchecked;

use num::BigInt;

#[derive(Debug)]
pub enum Value {
    Integer(BigInt),
    Address(usize, usize),
    Float(f64),
    Func(unsafe fn(Vec<Value>) -> Value),
}

impl Value {
    unsafe fn as_integer_unchecked(&self) -> &BigInt {
        match self {
            Value::Integer(value) => value,
            _ => unsafe { unreachable_unchecked() },
        }
    }
    unsafe fn as_float_unchecked(&self) -> f64 {
        match self {
            Value::Float(value) => *value,
            _ => unsafe { unreachable_unchecked() },
        }
    }
}

pub unsafe fn add_integer(args: Vec<Value>) -> Value {
    Value::Integer(args[0].as_integer_unchecked() + args[1].as_integer_unchecked())
}
pub unsafe fn add_float(args: Vec<Value>) -> Value {
    Value::Float(args[0].as_float_unchecked() + args[1].as_float_unchecked())
}

pub enum Expr {
    Imm(Value),
    Local(usize),
    Global(usize),
    Call(Box<Expr>, Vec<Expr>),
}

pub enum Stmt {
    Expr(Expr, Option<usize>),
    Branch(Expr, Option<usize>, Option<usize>),
}

impl Expr {
    fn debug_print(&self, depth: usize) {
        let indent = "  ".repeat(depth);
        match self {
            Expr::Imm(value) => println!("{indent}{value:?}"),
            Expr::Local(id) => println!("{indent}Local({id:?})"),
            Expr::Global(id) => println!("{indent}Global({id:?})"),
            Expr::Call(func, args) => {
                func.debug_print(depth);
                for arg in args {
                    arg.debug_print(depth + 1);
                }
            }
        }
    }
}

pub fn debug_print(stmts: &[Stmt]) {
    for (i, stmt) in stmts.iter().enumerate() {
        println!("[{i}]");
        match stmt {
            Stmt::Expr(expr, next) => {
                expr.debug_print(0);
                println!("-> {next:?}");
            }
            Stmt::Branch(expr, next_true, next_false) => todo!(),
        }
    }
}
