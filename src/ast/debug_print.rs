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

use super::{Block, Expr, Func, Stmt, Ty};

impl Debug for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Variable(id) => write!(f, "v{id}"),
            Expr::Func(id) => write!(f, "f{id}"),
            Expr::Integer(value) => write!(f, "{value}i"),
            Expr::Float(value) => write!(f, "{value}f"),
            Expr::String(value) => write!(f, "\"{value}\""),
            Expr::Call(func, args) => write!(
                f,
                "{func:?}({})",
                args.iter()
                    .map(|arg| format!("{arg:?}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

impl Expr {
    pub fn _debug_print(&self, depth: usize) {
        let indent = "  ".repeat(depth);
        match self {
            Expr::Variable(id) => println!("{indent}variable({id})"),
            Expr::Func(id) => println!("{indent}func({id})"),
            Expr::Integer(value) => println!("{indent}integer({value})"),
            Expr::Float(value) => println!("{indent}float({value})"),
            Expr::String(value) => eprintln!("{indent}string({value})"),
            Expr::Call(func, args) => {
                println!("{indent}call");
                func._debug_print(depth + 1);
                for arg in args {
                    arg._debug_print(depth + 1);
                }
            }
        }
    }
}
impl Stmt {
    pub fn _debug_print(&self, depth: usize) {
        let indent = "  ".repeat(depth);
        match self {
            Stmt::Expr(expr) => {
                println!("{indent}{expr:?}");
            }
            Stmt::Return(expr) => {
                println!("{indent}return {expr:?}");
            }
            Stmt::If(cond, stmts_then, stmts_else) => {
                println!("{indent}if {cond:?}");
                stmts_then._debug_print(depth);
                stmts_else._debug_print(depth);
            }
            Stmt::While(cond, stmts) => {
                println!("{indent}while {cond:?}");
                stmts._debug_print(depth);
            }
        }
    }
}
impl Block {
    pub fn _debug_print(&self, depth: usize) {
        let indent = "  ".repeat(depth);
        println!("{indent}block (size: {})", self.size);
        for stmt in &self.stmts {
            stmt._debug_print(depth + 1);
        }
    }
}
impl Func {
    pub fn _debug_print(&self) {
        match self {
            Func::Builtin(func) => eprintln!("Builtin({func:?})"),
            Func::Defined { body, args, ret_ty } => {
                eprintln!(
                    "({}) -> {ret_ty:?}",
                    args.iter()
                        .map(|arg| format!("v{arg}"))
                        .collect::<Vec<_>>()
                        .join(", ")
                );
                body._debug_print(0);
            }
        }
    }
}

use std::fmt::{self, Debug, Formatter};
impl Debug for Ty {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.kind)?;
        if !self.args.is_empty() {
            write!(
                f,
                "[{}]",
                self.args
                    .iter()
                    .map(|arg| format!("{arg:?}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            )?;
        }
        Ok(())
    }
}
