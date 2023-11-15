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

use either::Either;

use super::{Block, Expr, FuncDef, Stmt};

impl Expr {
    pub fn _debug_print(&self, depth: usize) {
        let indent = "  ".repeat(depth);
        match self {
            Expr::Variable(id) => println!("{indent}variable({id})"),
            Expr::Global(id) => println!("{indent}global({id})"),
            Expr::Func(id) => println!("{indent}func({id})"),
            Expr::Integer(value) => println!("{indent}integer({value})"),
            Expr::Float(value) => println!("{indent}float({value})"),
            Expr::String(components) => {
                eprintln!("{indent}string");
                for component in components {
                    match component {
                        Either::Left(string) => println!("{indent}string({string})"),
                        Either::Right(expr) => {
                            println!("{indent}expr");
                            expr._debug_print(depth + 1);
                        }
                    }
                }
            }
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
                println!("{indent}expression statement");
                expr._debug_print(depth + 1);
            }
            Stmt::Return(expr) => {
                println!("{indent}return statement");
                if let Some(expr) = expr {
                    expr._debug_print(depth + 1);
                }
            }
            Stmt::If(cond, stmts_then, stmts_else) => {
                println!("{indent}if statement");
                cond._debug_print(depth + 1);
                stmts_then._debug_print(depth);
                stmts_else._debug_print(depth);
            }
            Stmt::While(cond, stmts) => {
                println!("{indent}while statement");
                cond._debug_print(depth + 1);
                stmts._debug_print(depth);
            }
        }
    }
}
impl Block {
    pub fn _debug_print(&self, depth: usize) {
        let indent = "  ".repeat(depth);
        println!("{indent} block (size: {})", self.size);
        for stmt in &self.stmts {
            stmt._debug_print(depth + 1);
        }
    }
}
impl FuncDef {
    pub fn _debug_print(&self) {
        println!("{} args, {} locals", self.num_args, self.tys.len());
        for (i, ty) in self.tys.iter().enumerate() {
            println!("{i}: {ty:?}");
        }
        println!("{:?}", self.ret_ty);
        self.body._debug_print(0);
    }
}
