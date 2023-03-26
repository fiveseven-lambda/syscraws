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

use std::collections::HashMap;
use std::hint::unreachable_unchecked;

use num::BigInt;

#[derive(Clone)]
pub enum Value {
    Integer(BigInt),
    Boolean(bool),
    Address(Address),
    Float(f64),
    BuiltinFunc(BuiltinFunc),
    UserDefinedFunc(usize),
}

impl Value {
    unsafe fn into_integer_unchecked(self) -> BigInt {
        match self {
            Value::Integer(value) => value,
            _ => unsafe { unreachable_unchecked() },
        }
    }
    unsafe fn into_boolean_unchecked(self) -> bool {
        match self {
            Value::Boolean(value) => value,
            _ => unsafe { unreachable_unchecked() },
        }
    }
    unsafe fn into_float_unchecked(self) -> f64 {
        match self {
            Value::Float(value) => value,
            _ => unsafe { unreachable_unchecked() },
        }
    }
    unsafe fn into_address_unchecked(self) -> Address {
        match self {
            Value::Address(addr) => addr,
            _ => unsafe { unreachable_unchecked() },
        }
    }
}

pub unsafe fn add_integer(args: Vec<Value>, _: &mut Memory) -> Value {
    let mut args = args.into_iter();
    let first = args.next().unwrap_unchecked().into_integer_unchecked();
    let second = args.next().unwrap_unchecked().into_integer_unchecked();
    Value::Integer(first + second)
}
use num::ToPrimitive;
pub unsafe fn integer_to_float(args: Vec<Value>, _: &mut Memory) -> Value {
    let mut args = args.into_iter();
    let value = args.next().unwrap_unchecked().into_integer_unchecked();
    Value::Float(value.to_f64().unwrap_or(f64::INFINITY))
}
pub unsafe fn add_float(args: Vec<Value>, _: &mut Memory) -> Value {
    let mut args = args.into_iter();
    let first = args.next().unwrap_unchecked().into_float_unchecked();
    let second = args.next().unwrap_unchecked().into_float_unchecked();
    Value::Float(first + second)
}
pub unsafe fn deref(args: Vec<Value>, memory: &mut Memory) -> Value {
    let mut args = args.into_iter();
    let addr = args.next().unwrap_unchecked().into_address_unchecked();
    memory.get(addr)
}
pub unsafe fn assign(args: Vec<Value>, memory: &mut Memory) -> Value {
    let mut args = args.into_iter();
    let src = args.next().unwrap_unchecked();
    let dest = args.next().unwrap_unchecked().into_address_unchecked();
    *memory.get_mut(dest) = src.clone();
    src
}

pub type BuiltinFunc = unsafe fn(Vec<Value>, &mut Memory) -> Value;

pub enum Expr {
    Imm(Value),
    Local(usize),
    Call(Box<Expr>, Vec<Expr>),
}
impl Expr {
    unsafe fn eval(&self, memory: &mut Memory, stack_id: usize, funcs: &[Func]) -> Value {
        match *self {
            Expr::Imm(ref value) => value.clone(),
            Expr::Local(pos) => Value::Address((stack_id, pos)),
            Expr::Call(ref func, ref args) => match func.eval(memory, stack_id, funcs) {
                Value::BuiltinFunc(func) => {
                    let args = args
                        .iter()
                        .map(|expr| expr.eval(memory, stack_id, funcs))
                        .collect();
                    func(args, memory)
                }
                Value::UserDefinedFunc(id) => todo!(),
                _ => unsafe { unreachable_unchecked() },
            },
        }
    }
}

pub enum Stmt {
    Return(Expr),
    Expr(Expr, Option<usize>),
    Branch(Expr, Option<usize>, Option<usize>),
}

pub struct Func {
    num_locals: usize,
    stmts: Vec<Stmt>,
}

impl Func {
    unsafe fn run(&self, memory: &mut Memory, funcs: &[Func]) -> Option<Value> {
        let stack_id = memory.num_stack;
        memory.num_stack += 1;
        memory.stacks.insert(stack_id, Vec::new());
        let mut counter = Some(0);
        let ret = loop {
            counter = match counter {
                Some(counter) => match self.stmts[counter] {
                    Stmt::Expr(ref expr, next) => {
                        expr.eval(memory, stack_id, funcs);
                        next
                    }
                    Stmt::Return(ref expr) => {
                        break Some(expr.eval(memory, stack_id, funcs));
                    }
                    Stmt::Branch(ref cond, next_true, next_false) => {
                        if cond.eval(memory, stack_id, funcs).into_boolean_unchecked() {
                            next_true
                        } else {
                            next_false
                        }
                    }
                },
                None => break None,
            };
        };
        memory.stacks.remove(&stack_id);
        ret
    }
}

pub struct Memory {
    num_stack: usize,
    stacks: HashMap<usize, Vec<Option<Value>>>,
}

type Address = (usize, usize);

impl Memory {
    fn get(&self, (stack, pos): Address) -> Value {
        self.stacks[&stack][pos].clone().unwrap()
    }
    fn get_mut(&mut self, (stack, pos): Address) -> &mut Value {
        self.stacks.get_mut(&stack).unwrap()[pos].as_mut().unwrap()
    }
}

use std::fmt;
impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(value) => write!(f, "{value}"),
            Value::Boolean(value) => write!(f, "{value}"),
            Value::Address((stack, pos)) => write!(f, "{stack}:{pos}"),
            Value::Float(value) => write!(f, "{value}"),
            Value::BuiltinFunc(ptr) => write!(f, "builtin func {ptr:p}"),
            Value::UserDefinedFunc(id) => write!(f, "func {id}"),
        }
    }
}
impl Expr {
    fn debug_print(&self, depth: usize) {
        let indent = "  ".repeat(depth);
        match self {
            Expr::Imm(value) => println!("{indent}{value:?}"),
            Expr::Local(id) => println!("{indent}Local({id:?})"),
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
            Stmt::Return(expr) => {
                println!("return");
                expr.debug_print(0);
            }
            Stmt::Expr(expr, next) => {
                expr.debug_print(0);
                println!("-> {next:?}");
            }
            Stmt::Branch(expr, next_true, next_false) => todo!(),
        }
    }
}
