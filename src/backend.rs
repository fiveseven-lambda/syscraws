/*
 * Copyright (c) 2023-2026 Atsushi Komaba
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

/*!
 * Receives intermediate representation ([`ir`]) and executes it.
 */

use crate::{ffi, ir};

mod tests;
mod ty;

use serde::Serialize;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ffi::CString;
use std::rc::Rc;

fn translate_ty(ir_ty: &ir::Ty, ty_parameters: &[Rc<ty::Ty>]) -> Rc<ty::Ty> {
    match ir_ty {
        ir::Ty::Constructor(constructor) => Rc::new(ty::Ty::Constructor(constructor.clone())),
        ir::Ty::Parameter(index) => ty_parameters[*index].clone(),
        ir::Ty::Application {
            constructor,
            arguments,
        } => Rc::new(ty::Ty::Application {
            constructor: translate_ty(constructor, ty_parameters),
            arguments: translate_tys(arguments, ty_parameters),
        }),
    }
}

fn translate_tys(ir_tys: &[ir::Ty], ty_parameters: &[Rc<ty::Ty>]) -> Rc<ty::Ty> {
    return ir_tys.iter().rfold(Rc::new(ty::Ty::Nil), |tail, ir_head| {
        Rc::new(ty::Ty::Cons {
            head: translate_ty(ir_head, ty_parameters),
            tail,
        })
    });
}

pub fn translate(ir_program: ir::Program) -> Result<unsafe extern "C" fn() -> u8, ()> {
    let global_variables_ty: Vec<_> = (0..ir_program.num_global_variables)
        .map(|_| Rc::new(ty::Ty::Var(RefCell::new(ty::Var::Unassigned(0)))))
        .collect();
    for (_, function_definition) in ir_program.function_definitions.iter().enumerate() {
        let local_variables_ty: Vec<_> = (0..function_definition.num_local_variables)
            .map(|_| Rc::new(ty::Ty::Var(RefCell::new(ty::Var::Unassigned(0)))))
            .collect();
        let (mut candidates, candidate_values): (Vec<Vec<Candidate>>, Vec<Vec<Expression>>) =
            function_definition
                .function_uses
                .iter()
                .map(|function_use| {
                    function_use
                        .candidates
                        .iter()
                        .enumerate()
                        .map(|(index, ir_function)| {
                            let (ty, value) =
                                translate_function(ir_function, &ir_program.function_tys);
                            (
                                Candidate {
                                    index,
                                    ty,
                                    unifications: ty::Unifications::new(),
                                },
                                value,
                            )
                        })
                        .unzip()
                })
                .unzip();
        let mut function_use_indices = Vec::new();
        for call in &function_definition.calls {
            let function_use_index = match call.function {
                ir::Expression::FunctionUse(function_use_index) => function_use_index,
                ir::Expression::Call(call_index) => function_use_indices[call_index],
                _ => todo!(),
            };
            function_use_indices.push(function_use_index);
            let num_arguments = call.arguments.len();
            let argument_tys: Vec<_> = call
                .arguments
                .iter()
                .map(|arg| match *arg {
                    ir::Expression::Integer(_) => {
                        Rc::new(ty::Ty::Constructor(ir::TyConstructor::Integer))
                    }
                    ir::Expression::Float(_) => {
                        Rc::new(ty::Ty::Constructor(ir::TyConstructor::Float))
                    }
                    ir::Expression::Call(call_index) => {
                        let function_use_index = function_use_indices[call_index];
                        if candidates[function_use_index].len() > 1 {
                            todo!();
                        } else if let Some(Candidate { ty, .. }) =
                            candidates[function_use_index].first()
                        {
                            ty.clone()
                        } else {
                            todo!();
                        }
                    }
                    ir::Expression::Variable(storage, index) => match storage {
                        ir::Storage::Global => global_variables_ty[index].clone(),
                        ir::Storage::Local => local_variables_ty[index].clone(),
                    },
                    _ => todo!(),
                })
                .collect();
            let mut argument_ty_vars = HashMap::from([(None, 0)]);
            let argument_orders: Vec<_> = argument_tys
                .iter()
                .map(|argument_ty| {
                    let (var, order) = argument_ty.extract_function_ty();
                    let next_index = argument_ty_vars.len();
                    let index = *argument_ty_vars.entry(var).or_insert(next_index);
                    (index, order)
                })
                .collect();
            candidates[function_use_index] = std::mem::take(&mut candidates[function_use_index])
                .into_iter()
                .flat_map(|candidate| {
                    let mut unifications = candidate.unifications.undo();
                    let return_ty = Rc::new(ty::Ty::Var(RefCell::new(ty::Var::Unassigned(0))));
                    let parameter_tys: Vec<_> = (0..num_arguments)
                        .map(|_| Rc::new(ty::Ty::Var(RefCell::new(ty::Var::Unassigned(0)))))
                        .collect();
                    if !unifications.unify(
                        &candidate.ty,
                        &Rc::new(ty::Ty::Application {
                            constructor: Rc::new(ty::Ty::Constructor(ir::TyConstructor::Function)),
                            arguments: Rc::new(ty::Ty::Cons {
                                head: return_ty.clone(),
                                tail: parameter_tys.iter().rev().fold(
                                    Rc::new(ty::Ty::Nil),
                                    |tail, parameter_ty| {
                                        Rc::new(ty::Ty::Cons {
                                            head: parameter_ty.clone(),
                                            tail,
                                        })
                                    },
                                ),
                            }),
                        }),
                    ) {
                        unifications.undo();
                        return None;
                    }
                    let mut ty_vars = argument_ty_vars.clone();
                    let mut inequalities = Vec::new();
                    let mut diff_sum = 0;
                    for (&(argument_index, argument_order), parameter_ty) in
                        argument_orders.iter().zip(&parameter_tys)
                    {
                        let (parameter_var, parameter_order) = parameter_ty.extract_function_ty();
                        let next_index = ty_vars.len();
                        let parameter_index = *ty_vars.entry(parameter_var).or_insert(next_index);
                        let diff = argument_order - parameter_order;
                        diff_sum += diff.abs();
                        inequalities.push((argument_index, parameter_index, diff));
                    }
                    let num_ty_vars = ty_vars.len();
                    let Some(mut orders) = get_orders(&inequalities, num_ty_vars, diff_sum) else {
                        unifications.undo();
                        return None;
                    };
                    let mut min_order = -1;
                    let mut max_order = diff_sum;
                    while max_order - min_order > 1 {
                        let mid_order = (min_order + max_order) / 2;
                        match get_orders(&inequalities, num_ty_vars, mid_order) {
                            Some(new_orders) => {
                                orders = new_orders;
                                max_order = mid_order;
                            }
                            None => min_order = mid_order,
                        }
                    }
                    let extra_calls: Vec<_> = (0..max_order)
                        .map(|_| Rc::new(ty::Ty::Var(RefCell::new(ty::Var::Unassigned(0)))))
                        .collect();
                    for i in 0..num_arguments {
                        unifications.unify(
                            &argument_tys[i],
                            &extra_calls.iter().take(orders[i] as usize).fold(
                                parameter_tys[i].clone(),
                                |ty, extra_call| {
                                    Rc::new(ty::Ty::Application {
                                        constructor: Rc::new(ty::Ty::Constructor(
                                            ir::TyConstructor::Function,
                                        )),
                                        arguments: Rc::new(ty::Ty::Cons {
                                            head: ty,
                                            tail: extra_call.clone(),
                                        }),
                                    })
                                },
                            ),
                        );
                    }
                    let return_ty = extra_calls.iter().fold(return_ty, |ty, extra_call| {
                        Rc::new(ty::Ty::Application {
                            constructor: Rc::new(ty::Ty::Constructor(ir::TyConstructor::Function)),
                            arguments: Rc::new(ty::Ty::Cons {
                                head: ty,
                                tail: extra_call.clone(),
                            }),
                        })
                    });
                    Some(Candidate {
                        index: candidate.index,
                        ty: return_ty,
                        unifications: unifications.undo(),
                    })
                })
                .collect();
            if candidates[function_use_index].len() <= 1 {
                if let Some(candidate) = std::mem::take(&mut candidates[function_use_index])
                    .into_iter()
                    .next()
                {
                    candidates[function_use_index] = vec![Candidate {
                        index: candidate.index,
                        ty: candidate.ty,
                        unifications: ty::Unifications::new(),
                    }];
                    candidate.unifications.undo();
                }
            }
        }
    }
    todo!();
}

struct Candidate {
    index: usize,
    ty: Rc<ty::Ty>,
    unifications: ty::Unifications,
}

fn get_orders(
    inequalities: &[(usize, usize, i32)],
    num_ty_vars: usize,
    max_order: i32,
) -> Option<Vec<i32>> {
    let mut orders = vec![None; num_ty_vars];
    orders[0] = Some(0);
    for _ in 0..num_ty_vars {
        let mut updated = false;
        for &(arg, param, diff) in inequalities {
            if let Some(arg_order) = orders[arg] {
                if orders[param].is_none_or(|param_order| param_order > arg_order + diff) {
                    orders[param] = Some(arg_order + diff);
                    updated = true;
                }
            }
            if let Some(param_order) = orders[param] {
                if orders[arg].is_none_or(|arg_order| arg_order > param_order + max_order - diff) {
                    orders[arg] = Some(param_order + max_order - diff);
                    updated = true;
                }
            }
        }
        if !updated {
            return inequalities
                .iter()
                .map(|&(arg, param, diff)| Some(orders[arg]? - orders[param]? + diff))
                .collect();
        }
    }
    None
}

fn translate_function(
    ir_function: &ir::Function,
    ir_functions_ty: &[ir::FunctionTy],
) -> (Rc<ty::Ty>, Expression) {
    match *ir_function {
        ir::Function::UserDefined(index) => {
            let ir_function_ty = &ir_functions_ty[index];
            let ty_parameters: Vec<_> = (0..ir_function_ty.num_ty_parameters)
                .map(|_| Rc::new(ty::Ty::Var(RefCell::new(ty::Var::Unassigned(0)))))
                .collect();
            (
                Rc::new(ty::Ty::Application {
                    constructor: Rc::new(ty::Ty::Constructor(ir::TyConstructor::Function)),
                    arguments: Rc::new(ty::Ty::Cons {
                        head: translate_ty(&ir_function_ty.return_ty, &ty_parameters),
                        tail: translate_tys(&ir_function_ty.parameter_tys, &ty_parameters),
                    }),
                }),
                Expression::Function {
                    index,
                    ty_parameters,
                },
            )
        }
        ir::Function::AddInteger => (
            Rc::new(ty::Ty::Application {
                constructor: Rc::new(ty::Ty::Constructor(ir::TyConstructor::Function)),
                arguments: Rc::new(ty::Ty::Cons {
                    head: Rc::new(ty::Ty::Constructor(ir::TyConstructor::Integer)),
                    tail: Rc::new(ty::Ty::Cons {
                        head: Rc::new(ty::Ty::Constructor(ir::TyConstructor::Integer)),
                        tail: Rc::new(ty::Ty::Cons {
                            head: Rc::new(ty::Ty::Constructor(ir::TyConstructor::Integer)),
                            tail: Rc::new(ty::Ty::Nil),
                        }),
                    }),
                }),
            }),
            Expression::AddInteger,
        ),
        ir::Function::Dereference => {
            let ty = Rc::new(ty::Ty::Var(RefCell::new(ty::Var::Unassigned(0))));
            (
                Rc::new(ty::Ty::Application {
                    constructor: Rc::new(ty::Ty::Constructor(ir::TyConstructor::Function)),
                    arguments: Rc::new(ty::Ty::Cons {
                        head: ty.clone(),
                        tail: Rc::new(ty::Ty::Cons {
                            head: Rc::new(ty::Ty::Application {
                                constructor: Rc::new(ty::Ty::Constructor(
                                    ir::TyConstructor::Reference,
                                )),
                                arguments: Rc::new(ty::Ty::Cons {
                                    head: ty.clone(),
                                    tail: Rc::new(ty::Ty::Nil),
                                }),
                            }),
                            tail: Rc::new(ty::Ty::Nil),
                        }),
                    }),
                }),
                Expression::Dereference(ty),
            )
        }
        ir::Function::Identity => {
            let ty = Rc::new(ty::Ty::Var(RefCell::new(ty::Var::Unassigned(0))));
            (
                Rc::new(ty::Ty::Application {
                    constructor: Rc::new(ty::Ty::Constructor(ir::TyConstructor::Function)),
                    arguments: Rc::new(ty::Ty::Cons {
                        head: ty.clone(),
                        tail: Rc::new(ty::Ty::Cons {
                            head: ty.clone(),
                            tail: Rc::new(ty::Ty::Nil),
                        }),
                    }),
                }),
                Expression::Identity(ty),
            )
        }
        ir::Function::Delete => {
            let ty = Rc::new(ty::Ty::Var(RefCell::new(ty::Var::Unassigned(0))));
            (
                Rc::new(ty::Ty::Application {
                    constructor: Rc::new(ty::Ty::Constructor(ir::TyConstructor::Function)),
                    arguments: Rc::new(ty::Ty::Cons {
                        head: Rc::new(ty::Ty::Application {
                            constructor: Rc::new(ty::Ty::Constructor(ir::TyConstructor::Tuple)),
                            arguments: Rc::new(ty::Ty::Nil),
                        }),
                        tail: Rc::new(ty::Ty::Cons {
                            head: ty.clone(),
                            tail: Rc::new(ty::Ty::Nil),
                        }),
                    }),
                }),
                Expression::Delete(ty),
            )
        }
        ir::Function::Assign => {
            let ty = Rc::new(ty::Ty::Var(RefCell::new(ty::Var::Unassigned(0))));
            (
                Rc::new(ty::Ty::Application {
                    constructor: Rc::new(ty::Ty::Constructor(ir::TyConstructor::Function)),
                    arguments: Rc::new(ty::Ty::Cons {
                        head: Rc::new(ty::Ty::Application {
                            constructor: Rc::new(ty::Ty::Constructor(ir::TyConstructor::Tuple)),
                            arguments: Rc::new(ty::Ty::Nil),
                        }),
                        tail: Rc::new(ty::Ty::Cons {
                            head: Rc::new(ty::Ty::Application {
                                constructor: Rc::new(ty::Ty::Constructor(
                                    ir::TyConstructor::Reference,
                                )),
                                arguments: Rc::new(ty::Ty::Cons {
                                    head: ty.clone(),
                                    tail: Rc::new(ty::Ty::Nil),
                                }),
                            }),
                            tail: Rc::new(ty::Ty::Cons {
                                head: ty.clone(),
                                tail: Rc::new(ty::Ty::Nil),
                            }),
                        }),
                    }),
                }),
                Expression::Assign(ty),
            )
        }
        ir::Function::Print => (
            Rc::new(ty::Ty::Application {
                constructor: Rc::new(ty::Ty::Constructor(ir::TyConstructor::Function)),
                arguments: Rc::new(ty::Ty::Cons {
                    head: Rc::new(ty::Ty::Application {
                        constructor: Rc::new(ty::Ty::Constructor(ir::TyConstructor::Tuple)),
                        arguments: Rc::new(ty::Ty::Nil),
                    }),
                    tail: Rc::new(ty::Ty::Cons {
                        head: Rc::new(ty::Ty::Var(RefCell::new(ty::Var::Unassigned(0)))),
                        tail: Rc::new(ty::Ty::Nil),
                    }),
                }),
            }),
            Expression::Print,
        ),
        _ => todo!(),
    }
}

fn translate_expression(
    expression: &ir::Expression,
    function_uses: &[ir::FunctionUse],
    function_use_values: &[Expression],
    function_use_orders: &[Vec<Vec<i32>>],
) -> Expression {
    match *expression {
        ir::Expression::Integer(value) => Expression::Integer(value),
        ir::Expression::Float(value) => Expression::Float(value),
        ir::Expression::String(ref value) => Expression::String(value.clone()),
        ir::Expression::Variable(storage, index) => Expression::Variable(storage, index),
        _ => todo!(),
    }
}

#[derive(Serialize)]
struct Block {
    expressions: Vec<Expression>,
    next: Next,
}

impl Block {
    unsafe fn codegen(&self) {
        for expression in &self.expressions {
            unsafe { expression.codegen() };
        }
        match self.next {
            Next::Return(ref value) => {
                unsafe { ffi::create_return(value.codegen()) };
            }
            _ => todo!(),
        }
    }
}

#[derive(Serialize)]
enum Next {
    Jump(Option<usize>),
    Br(Expression, Option<usize>, Option<usize>),
    Return(Expression),
}

#[derive(Clone, Serialize)]
enum Expression {
    Integer(i32),
    Float(f64),
    String(String),
    Function {
        index: usize,
        ty_parameters: Vec<Rc<ty::Ty>>,
    },
    Variable(ir::Storage, usize),
    AddInteger,
    Dereference(Rc<ty::Ty>),
    Identity(Rc<ty::Ty>),
    Delete(Rc<ty::Ty>),
    Assign(Rc<ty::Ty>),
    Print,
    Compile {
        expression: Box<Expression>,
        parameters_ty: Vec<Rc<ty::Ty>>,
        return_ty: Rc<ty::Ty>,
    },
    Call {
        function: Box<Expression>,
        arguments: Vec<Expression>,
    },
}

impl Expression {
    unsafe fn codegen(&self) -> *mut ffi::Value {
        match *self {
            Expression::Integer(value) => unsafe { ffi::create_integer(value) },
            Expression::Variable(storage, index) => unsafe { ffi::create_integer(0) },
            Expression::Call {
                ref function,
                ref arguments,
            } => match arguments.len() {
                _ => todo!(),
            },
            _ => todo!(),
        }
    }
}
