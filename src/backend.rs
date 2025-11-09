/*
 * Copyright (c) 2023-2025 Atsushi Komaba
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

use crate::ir;

mod tests;
mod ty;

#[cfg(test)]
use serde::Serialize;
use std::cell::RefCell;
use std::collections::HashMap;
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
    let local_variables_ty: Vec<Vec<_>> = ir_program
        .num_local_variables
        .iter()
        .map(|&num_local_variables| {
            (0..num_local_variables)
                .map(|_| Rc::new(ty::Ty::Var(RefCell::new(ty::Var::Unassigned(0)))))
                .collect()
        })
        .collect();
    let mut function_use_tys: Vec<Rc<ty::Ty>> = Vec::new();
    let mut function_use_orders = Vec::new();
    for function_use in &ir_program.function_uses {
        let mut candidates: Vec<_> = function_use
            .candidates
            .iter()
            .map(|ir_function| {
                let (ty, _) = translate_function(ir_function, &ir_program.function_tys);
                Some(Candidate {
                    ty,
                    orders: Vec::new(),
                    unifications: ty::Unifications::new(),
                })
            })
            .collect();
        for call in &function_use.calls {
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
                    ir::Expression::FunctionUse(index) => function_use_tys[index].clone(),
                    ir::Expression::Variable(storage, index) => match storage {
                        ir::Storage::Global => global_variables_ty[index].clone(),
                        ir::Storage::Local(function_index) => {
                            local_variables_ty[function_index][index].clone()
                        }
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
            candidates = candidates
                .into_iter()
                .map(|candidate| {
                    let Some(candidate) = candidate else {
                        return None;
                    };
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
                    let mut min_order = 0;
                    let mut max_order = diff_sum;
                    let mut orders = None;
                    while max_order - min_order > 1 {
                        let mid_order = (min_order + max_order) / 2;
                        match get_orders(&inequalities, num_ty_vars, mid_order) {
                            Some(ords) => {
                                orders = Some(ords);
                                max_order = mid_order;
                            }
                            None => {
                                min_order = mid_order;
                            }
                        }
                    }
                    let Some(orders) = orders else {
                        unifications.undo();
                        return None;
                    };
                    // TODO: unify types here
                    let mut candidate = Candidate {
                        ty: return_ty,
                        orders: candidate.orders,
                        unifications: unifications.undo(),
                    };
                    candidate.orders.push(orders);
                    Some(candidate)
                })
                .collect();
        }
        let passed_candidates: Vec<_> = candidates.into_iter().flatten().collect();
        if passed_candidates.len() > 1 {
            todo!("Error not implemented: ambiguous function call");
        } else if let Some(candidate) = passed_candidates.into_iter().next() {
            function_use_tys.push(candidate.ty);
            function_use_orders.push(candidate.orders);
        } else {
            todo!("Error not implemented: no matching function found");
        }
    }
    todo!();
}

struct Candidate {
    ty: Rc<ty::Ty>,
    orders: Vec<Vec<i32>>,
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
            todo!();
        }
        if !updated {
            return orders.into_iter().collect();
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

#[cfg_attr(test, derive(Serialize))]
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
    App {
        function: Box<Expression>,
        arguments: Vec<Expression>,
    },
}
