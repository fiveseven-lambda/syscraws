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

use crate::ty;
use num::BigInt;

#[derive(Clone)]
pub enum Value {
    Void,
    Integer(BigInt),
    Boolean(bool),
    Address(Address),
    Float(f64),
    String(String),
    Func(Func),
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
    unsafe fn into_string_unchecked(self) -> String {
        match self {
            Value::String(value) => value,
            _ => panic!(),
        }
    }
    unsafe fn into_address_unchecked(self) -> Address {
        match self {
            Value::Address(addr) => addr,
            _ => panic!(),
        }
    }
    unsafe fn into_func_unchecked(self) -> Func {
        match self {
            Value::Func(func) => func,
            _ => panic!(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Func {
    Builtin(BuiltinFunc),
    UserDefined(usize),
}

#[derive(Clone, Copy, Debug)]
pub enum BuiltinFunc {
    PlusInteger,
    PlusFloat,
    MinusInteger,
    MinusFloat,
    RecipFloat,
    NotBoolean,
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
    PrintString,
    AssignInteger,
    AssignFloat,
    Deref,
}
impl BuiltinFunc {
    pub fn ty(self) -> ty::Func {
        match self {
            BuiltinFunc::PlusInteger | BuiltinFunc::MinusInteger => ty::Func {
                args: vec![ty::Ty::integer()],
                ret: ty::Ty::integer(),
            },
            BuiltinFunc::PlusFloat | BuiltinFunc::MinusFloat | BuiltinFunc::RecipFloat => {
                ty::Func {
                    args: vec![ty::Ty::float()],
                    ret: ty::Ty::float(),
                }
            }
            BuiltinFunc::NotBoolean => ty::Func {
                args: vec![ty::Ty::boolean()],
                ret: ty::Ty::boolean(),
            },
            BuiltinFunc::AddInteger
            | BuiltinFunc::SubInteger
            | BuiltinFunc::MulInteger
            | BuiltinFunc::DivInteger
            | BuiltinFunc::RemInteger => ty::Func {
                args: vec![ty::Ty::integer(), ty::Ty::integer()],
                ret: ty::Ty::integer(),
            },
            BuiltinFunc::IntegerToFloat => ty::Func {
                args: vec![ty::Ty::integer()],
                ret: ty::Ty::float(),
            },
            BuiltinFunc::EqualInteger
            | BuiltinFunc::NotEqualInteger
            | BuiltinFunc::GreaterInteger
            | BuiltinFunc::GreaterEqualInteger
            | BuiltinFunc::LessInteger
            | BuiltinFunc::LessEqualInteger => ty::Func {
                args: vec![ty::Ty::integer(), ty::Ty::integer()],
                ret: ty::Ty::boolean(),
            },
            BuiltinFunc::AddFloat
            | BuiltinFunc::SubFloat
            | BuiltinFunc::MulFloat
            | BuiltinFunc::DivFloat
            | BuiltinFunc::RemFloat => ty::Func {
                args: vec![ty::Ty::float(), ty::Ty::float()],
                ret: ty::Ty::float(),
            },
            BuiltinFunc::AssignInteger => ty::Func {
                args: vec![ty::Ty::integer(), ty::Ty::reference(ty::Ty::integer())],
                ret: ty::Ty::integer(),
            },
            BuiltinFunc::AssignFloat => ty::Func {
                args: vec![ty::Ty::float(), ty::Ty::reference(ty::Ty::float())],
                ret: ty::Ty::float(),
            },
            BuiltinFunc::PrintInteger => ty::Func {
                args: vec![ty::Ty::integer()],
                ret: ty::Ty::tuple(vec![]),
            },
            BuiltinFunc::PrintFloat => ty::Func {
                args: vec![ty::Ty::float()],
                ret: ty::Ty::tuple(vec![]),
            },
            BuiltinFunc::PrintBoolean => ty::Func {
                args: vec![ty::Ty::boolean()],
                ret: ty::Ty::tuple(vec![]),
            },
            BuiltinFunc::PrintString => ty::Func {
                args: vec![ty::Ty::string()],
                ret: ty::Ty::tuple(vec![]),
            },
            BuiltinFunc::Deref => todo!(),
        }
    }
    unsafe fn call(self, args: Vec<Value>, memory: &mut Memory) -> Value {
        let mut args = args.into_iter();
        let mut arg = || args.next().unwrap_unchecked();
        match self {
            BuiltinFunc::PlusInteger | BuiltinFunc::MinusInteger => {
                let value = arg().into_integer_unchecked();
                Value::Integer(match self {
                    BuiltinFunc::PlusInteger => value,
                    BuiltinFunc::MinusInteger => -value,
                    _ => unreachable_unchecked(),
                })
            }
            BuiltinFunc::PlusFloat | BuiltinFunc::MinusFloat | BuiltinFunc::RecipFloat => {
                let value = arg().into_float_unchecked();
                Value::Float(match self {
                    BuiltinFunc::PlusFloat => value,
                    BuiltinFunc::MinusFloat => -value,
                    BuiltinFunc::RecipFloat => 1. / value,
                    _ => unreachable_unchecked(),
                })
            }
            BuiltinFunc::NotBoolean => {
                let value = arg().into_boolean_unchecked();
                Value::Boolean(!value)
            }
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
            BuiltinFunc::PrintString => {
                let value = arg().into_string_unchecked();
                println!("{value}");
                Value::Void
            }
            BuiltinFunc::AssignInteger | BuiltinFunc::AssignFloat => {
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
    unsafe fn eval(&self, memory: &mut Memory, stack_id: usize, funcs: &[FuncDef]) -> Value {
        match *self {
            Expr::Imm(ref value) => value.clone(),
            Expr::Local(pos) => Value::Address((stack_id, pos)),
            Expr::Call(ref func, ref args) => {
                let func = func.eval(memory, stack_id, funcs);
                let args_iter = args.iter().map(|expr| expr.eval(memory, stack_id, funcs));
                match func.into_func_unchecked() {
                    Func::Builtin(func) => func.call(args_iter.collect(), memory),
                    Func::UserDefined(id) => funcs[id]
                        .run(args_iter.map(Option::Some).collect(), memory, funcs)
                        .unwrap_or(Value::Void),
                }
            }
        }
    }
}

pub enum Stmt {
    Return(Expr),
    Expr(Expr, Option<usize>),
    Branch(Expr, Option<usize>, Option<usize>),
}

pub struct FuncDef {
    num_locals: usize,
    stmts: Vec<Stmt>,
    entry: Option<usize>,
}

impl FuncDef {
    pub fn new(num_locals: usize, stmts: Vec<Stmt>, entry: Option<usize>) -> FuncDef {
        FuncDef {
            num_locals,
            stmts,
            entry,
        }
    }
    pub unsafe fn run(
        &self,
        args: Vec<Option<Value>>,
        memory: &mut Memory,
        funcs: &[FuncDef],
    ) -> Option<Value> {
        let stack_id = memory.num_stack;
        memory.num_stack += 1;
        let mut stack = args;
        stack.resize(self.num_locals, None);
        memory.stacks.insert(stack_id, stack);
        let mut counter = self.entry;
        let mut ret = None;
        while let Some(i) = counter {
            counter = match &self.stmts[i] {
                Stmt::Expr(expr, next) => {
                    expr.eval(memory, stack_id, funcs);
                    *next
                }
                Stmt::Return(expr) => {
                    ret = Some(expr.eval(memory, stack_id, funcs));
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
        ret
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
            Value::String(value) => write!(f, "{value:?}"),
            Value::Func(func) => write!(f, "func {func:?}"),
        }
    }
}
impl Expr {
    fn _debug_print(&self, depth: usize) {
        let indent = "  ".repeat(depth);
        match self {
            Expr::Imm(value) => println!("{indent}{value:?}"),
            Expr::Local(id) => println!("{indent}Local({id:?})"),
            Expr::Call(func, args) => {
                func._debug_print(depth);
                for arg in args {
                    arg._debug_print(depth + 1);
                }
            }
        }
    }
}

pub fn _debug_print(stmts: &[Stmt]) {
    for (i, stmt) in stmts.iter().enumerate() {
        println!("[{i}]");
        match stmt {
            Stmt::Return(expr) => {
                println!("return");
                expr._debug_print(0);
            }
            Stmt::Expr(expr, next) => {
                expr._debug_print(0);
                println!("-> {next:?}");
            }
            Stmt::Branch(cond, next_true, next_false) => {
                cond._debug_print(0);
                println!("true -> {next_true:?}");
                println!("false -> {next_false:?}");
            }
        }
    }
}
