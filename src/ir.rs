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
    Void,
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
            _ => panic!(),
        }
    }
    unsafe fn into_boolean_unchecked(self) -> bool {
        match self {
            Value::Boolean(value) => value,
            _ => panic!(),
        }
    }
    unsafe fn into_float_unchecked(self) -> f64 {
        match self {
            Value::Float(value) => value,
            _ => panic!(),
        }
    }
    unsafe fn into_address_unchecked(self) -> Address {
        match self {
            Value::Address(addr) => addr,
            _ => panic!(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum BuiltinFunc {
    AddInteger,
    SubInteger,
    MulInteger,
    DivInteger,
    RemInteger,
    PrintInteger,
    AddFloat,
    SubFloat,
    MulFloat,
    DivFloat,
    RemFloat,
    PrintFloat,
    EqualInteger,
    NotEqualInteger,
    GreaterInteger,
    GreaterEqualInteger,
    LessInteger,
    LessEqualInteger,
    IntegerToFloat,
    PrintBoolean,
    Assign,
    Deref,
}
impl BuiltinFunc {
    unsafe fn call(self, args: Vec<Value>, memory: &mut Memory) -> Value {
        let mut args = args.into_iter();
        let mut arg = || args.next().unwrap_unchecked();
        match self {
            BuiltinFunc::AddInteger
            | BuiltinFunc::SubInteger
            | BuiltinFunc::MulInteger
            | BuiltinFunc::DivInteger
            | BuiltinFunc::RemInteger => {
                let first = arg().into_integer_unchecked();
                let second = arg().into_integer_unchecked();
                Value::Integer(match self {
                    BuiltinFunc::AddInteger => first + second,
                    BuiltinFunc::SubInteger => first - second,
                    BuiltinFunc::MulInteger => first * second,
                    BuiltinFunc::DivInteger => first / second,
                    BuiltinFunc::RemInteger => first % second,
                    _ => unreachable_unchecked(),
                })
            }
            BuiltinFunc::EqualInteger
            | BuiltinFunc::NotEqualInteger
            | BuiltinFunc::GreaterInteger
            | BuiltinFunc::GreaterEqualInteger
            | BuiltinFunc::LessInteger
            | BuiltinFunc::LessEqualInteger => {
                let first = arg().into_integer_unchecked();
                let second = arg().into_integer_unchecked();
                Value::Boolean(match self {
                    BuiltinFunc::EqualInteger => first == second,
                    BuiltinFunc::NotEqualInteger => first != second,
                    BuiltinFunc::GreaterInteger => first > second,
                    BuiltinFunc::GreaterEqualInteger => first >= second,
                    BuiltinFunc::LessInteger => first < second,
                    BuiltinFunc::LessEqualInteger => first <= second,
                    _ => unreachable_unchecked(),
                })
            }
            BuiltinFunc::PrintInteger => {
                let value = arg().into_integer_unchecked();
                println!("{value}");
                Value::Void
            }
            BuiltinFunc::AddFloat
            | BuiltinFunc::SubFloat
            | BuiltinFunc::MulFloat
            | BuiltinFunc::DivFloat
            | BuiltinFunc::RemFloat => {
                let first = arg().into_float_unchecked();
                let second = arg().into_float_unchecked();
                Value::Float(match self {
                    BuiltinFunc::AddFloat => first + second,
                    BuiltinFunc::SubFloat => first - second,
                    BuiltinFunc::MulFloat => first * second,
                    BuiltinFunc::DivFloat => first / second,
                    BuiltinFunc::RemFloat => first % second,
                    _ => unreachable_unchecked(),
                })
            }
            BuiltinFunc::PrintFloat => {
                let value = arg().into_float_unchecked();
                println!("{value}");
                Value::Void
            }
            BuiltinFunc::IntegerToFloat => {
                use num::ToPrimitive;
                let value = arg().into_integer_unchecked();
                Value::Float(value.to_f64().unwrap())
            }
            BuiltinFunc::PrintBoolean => {
                let value = arg().into_boolean_unchecked();
                println!("{value}");
                Value::Void
            }
            BuiltinFunc::Assign => {
                let src = arg();
                let dest = arg().into_address_unchecked();
                memory.set(src.clone(), dest);
                src
            }
            BuiltinFunc::Deref => {
                let ptr = arg().into_address_unchecked();
                memory.get(ptr)
            }
        }
    }
}

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
                    func.call(args, memory)
                }
                Value::UserDefinedFunc(id) => todo!(),
                _ => unsafe { unreachable_unchecked() },
            },
        }
    }
}

pub enum Stmt {
    Return(Expr),
    Expr(Expr, usize),
    Branch(Expr, usize, usize),
}

pub struct Func {
    num_locals: usize,
    stmts: Vec<Stmt>,
}

impl Func {
    pub fn new(num_locals: usize, stmts: Vec<Stmt>) -> Func {
        Func { num_locals, stmts }
    }
    pub unsafe fn run(&self, memory: &mut Memory, funcs: &[Func]) -> Option<Value> {
        let stack_id = memory.num_stack;
        memory.num_stack += 1;
        memory.stacks.insert(stack_id, vec![None; self.num_locals]);
        let mut counter = 0;
        while let Some(stmt) = self.stmts.get(counter) {
            counter = match stmt {
                Stmt::Expr(expr, next) => {
                    expr.eval(memory, stack_id, funcs);
                    *next
                }
                Stmt::Return(expr) => {
                    break;
                }
                Stmt::Branch(cond, next_true, next_false) => {
                    if cond.eval(memory, stack_id, funcs).into_boolean_unchecked() {
                        *next_true
                    } else {
                        *next_false
                    }
                }
            }
        }
        memory.stacks.remove(&stack_id);
        None
    }
}

#[derive(Debug)]
pub struct Memory {
    num_stack: usize,
    stacks: HashMap<usize, Vec<Option<Value>>>,
}

type Address = (usize, usize);

impl Memory {
    pub fn new() -> Memory {
        Memory {
            num_stack: 0,
            stacks: HashMap::new(),
        }
    }
    fn get(&self, (stack, pos): Address) -> Value {
        self.stacks[&stack][pos].clone().unwrap()
    }
    fn set(&mut self, value: Value, (stack, pos): Address) {
        self.stacks.get_mut(&stack).unwrap()[pos] = Some(value)
    }
}

use std::fmt;
impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Void => write!(f, "void"),
            Value::Integer(value) => write!(f, "{value}"),
            Value::Boolean(value) => write!(f, "{value}"),
            Value::Address((stack, pos)) => write!(f, "{stack}:{pos}"),
            Value::Float(value) => write!(f, "{value}"),
            Value::BuiltinFunc(ptr) => write!(f, "{ptr:?}"),
            Value::UserDefinedFunc(id) => write!(f, "func #{id}"),
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
            Stmt::Branch(cond, next_true, next_false) => {
                cond.debug_print(0);
                println!("true -> {next_true:?}");
                println!("false -> {next_false:?}");
            }
        }
    }
}
