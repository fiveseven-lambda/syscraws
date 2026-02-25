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
        let (candidate_tys, mut candidates): (Vec<Tys>, Vec<Vec<_>>) = function_definition
            .function_uses
            .iter()
            .enumerate()
            .map(|(function_use_index, function_use)| {
                let (tys, candidates) = function_use
                    .candidates
                    .iter()
                    .enumerate()
                    .map(|(index, candidate)| {
                        let ty = get_function_ty(candidate, &ir_program.function_tys);
                        (
                            Some(ty),
                            Candidate {
                                index,
                                unifications: ty::Unifications::new(),
                            },
                        )
                    })
                    .unzip();
                (
                    Tys {
                        function_use_index,
                        tys,
                    },
                    candidates,
                )
            })
            .unzip();
        let mut call_tys: Vec<Tys> = Vec::new();
        for call in &function_definition.calls {
            let &Tys {
                function_use_index,
                tys: ref callee_tys,
            } = match call.function {
                ir::Expression::Call(index) => &call_tys[index],
                ir::Expression::FunctionUse(index) => &candidate_tys[index],
                _ => todo!(),
            };
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
                    ir::Expression::String(_) => {
                        Rc::new(ty::Ty::Constructor(ir::TyConstructor::String))
                    }
                    ir::Expression::Call(call_index) => {
                        let argument_ty = &call_tys[call_index];
                        let argument_candidates = &candidates[argument_ty.function_use_index];
                        if argument_candidates.len() > 1 {
                            todo!();
                        } else if let Some(candidate) = argument_candidates.first() {
                            argument_ty.tys[candidate.index].clone().unwrap()
                        } else {
                            todo!();
                        }
                    }
                    ir::Expression::FunctionUse(argument_function_use_index) => {
                        todo!();
                    }
                    ir::Expression::Variable(storage, index) => match storage {
                        ir::Storage::Global => global_variables_ty[index].clone(),
                        ir::Storage::Local => local_variables_ty[index].clone(),
                    },
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
            let mut return_tys: Vec<Option<Rc<ty::Ty>>> = function_definition.function_uses
                [function_use_index]
                .candidates
                .iter()
                .map(|_| None)
                .collect();
            let old_candidates = &mut candidates[function_use_index];
            let resolved = old_candidates.len() <= 1;
            let new_candidates: Vec<_> = std::mem::take(old_candidates)
                .into_iter()
                .flat_map(
                    |Candidate {
                         index,
                         unifications,
                     }| {
                        let mut unifications = if resolved {
                            unifications
                        } else {
                            unifications.undo()
                        };
                        let callee_ty = callee_tys[index].as_ref().unwrap();
                        let return_ty = Rc::new(ty::Ty::Var(RefCell::new(ty::Var::Unassigned(0))));
                        let parameter_tys: Vec<_> = (0..num_arguments)
                            .map(|_| Rc::new(ty::Ty::Var(RefCell::new(ty::Var::Unassigned(0)))))
                            .collect();
                        if !unifications.unify(
                            &callee_ty,
                            &Rc::new(ty::Ty::Application {
                                constructor: Rc::new(ty::Ty::Constructor(
                                    ir::TyConstructor::Function,
                                )),
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
                            let (parameter_var, parameter_order) =
                                parameter_ty.extract_function_ty();
                            let next_index = ty_vars.len();
                            let parameter_index =
                                *ty_vars.entry(parameter_var).or_insert(next_index);
                            let diff = argument_order - parameter_order;
                            diff_sum += diff.abs();
                            inequalities.push((argument_index, parameter_index, diff));
                        }
                        let num_ty_vars = ty_vars.len();
                        let Some(mut orders) = get_orders(&inequalities, num_ty_vars, diff_sum)
                        else {
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
                        return_tys[index] =
                            Some(extra_calls.iter().fold(return_ty, |ty, extra_call| {
                                Rc::new(ty::Ty::Application {
                                    constructor: Rc::new(ty::Ty::Constructor(
                                        ir::TyConstructor::Function,
                                    )),
                                    arguments: Rc::new(ty::Ty::Cons {
                                        head: ty,
                                        tail: extra_call.clone(),
                                    }),
                                })
                            }));
                        Some(Candidate {
                            index,
                            unifications: if resolved {
                                unifications
                            } else {
                                unifications.undo()
                            },
                        })
                    },
                )
                .collect();
            if !resolved && new_candidates.len() <= 1 {
                if let Some(Candidate {
                    index,
                    unifications,
                }) = new_candidates.into_iter().next()
                {
                    *old_candidates = vec![Candidate {
                        index,
                        unifications: unifications.undo(),
                    }];
                }
            } else {
                *old_candidates = new_candidates;
            }
            call_tys.push(Tys {
                function_use_index,
                tys: return_tys,
            });
        }
    }
    unsafe { ffi::initialize_jit() };
    let num_definitions = ir_program.function_definitions.len();
    for (function_index, function_definition) in
        ir_program.function_definitions.into_iter().enumerate()
    {
        let function_name = CString::new(format!("{}", function_index)).unwrap();
        unsafe {
            let function_type = ffi::get_function_type(false, ffi::get_integer_type(), 0);
            ffi::add_function(
                function_name.as_ptr(),
                function_type,
                function_definition.blocks.len(),
            );
        }
        for (block_index, block) in function_definition.blocks.into_iter().enumerate() {
            unsafe {
                ffi::set_insert_point(block_index);
            }
            match block.next {
                ir::Next::Return(expression) => {
                    let value = match expression {
                        ir::Expression::Integer(value) => unsafe { ffi::create_integer(value) },
                        _ => todo!(),
                    };
                    unsafe {
                        ffi::create_return(value);
                    }
                }
                _ => todo!(),
            }
        }
        if function_index == num_definitions - 1 {
            let pointer = unsafe { ffi::compile_function(function_name.as_ptr()) };
            return Ok(pointer);
        }
    }
    Err(())
}

struct Tys {
    function_use_index: usize,
    tys: Vec<Option<Rc<ty::Ty>>>,
}

struct Candidate {
    index: usize,
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

fn get_function_ty(ir_function: &ir::Function, ir_functions_ty: &[ir::FunctionTy]) -> Rc<ty::Ty> {
    match *ir_function {
        ir::Function::UserDefined(index) => {
            let ir_function_ty = &ir_functions_ty[index];
            let ty_parameters: Vec<_> = (0..ir_function_ty.num_ty_parameters)
                .map(|_| Rc::new(ty::Ty::Var(RefCell::new(ty::Var::Unassigned(0)))))
                .collect();
            Rc::new(ty::Ty::Application {
                constructor: Rc::new(ty::Ty::Constructor(ir::TyConstructor::Function)),
                arguments: Rc::new(ty::Ty::Cons {
                    head: translate_ty(&ir_function_ty.return_ty, &ty_parameters),
                    tail: translate_tys(&ir_function_ty.parameter_tys, &ty_parameters),
                }),
            })
        }
        ir::Function::AddInteger => Rc::new(ty::Ty::Application {
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
        ir::Function::Dereference => {
            let ty = Rc::new(ty::Ty::Var(RefCell::new(ty::Var::Unassigned(0))));
            Rc::new(ty::Ty::Application {
                constructor: Rc::new(ty::Ty::Constructor(ir::TyConstructor::Function)),
                arguments: Rc::new(ty::Ty::Cons {
                    head: ty.clone(),
                    tail: Rc::new(ty::Ty::Cons {
                        head: Rc::new(ty::Ty::Application {
                            constructor: Rc::new(ty::Ty::Constructor(ir::TyConstructor::Reference)),
                            arguments: Rc::new(ty::Ty::Cons {
                                head: ty.clone(),
                                tail: Rc::new(ty::Ty::Nil),
                            }),
                        }),
                        tail: Rc::new(ty::Ty::Nil),
                    }),
                }),
            })
        }
        ir::Function::Identity => {
            let ty = Rc::new(ty::Ty::Var(RefCell::new(ty::Var::Unassigned(0))));
            Rc::new(ty::Ty::Application {
                constructor: Rc::new(ty::Ty::Constructor(ir::TyConstructor::Function)),
                arguments: Rc::new(ty::Ty::Cons {
                    head: ty.clone(),
                    tail: Rc::new(ty::Ty::Cons {
                        head: ty.clone(),
                        tail: Rc::new(ty::Ty::Nil),
                    }),
                }),
            })
        }
        ir::Function::Delete => {
            let ty = Rc::new(ty::Ty::Var(RefCell::new(ty::Var::Unassigned(0))));
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
            })
        }
        ir::Function::Assign => {
            let ty = Rc::new(ty::Ty::Var(RefCell::new(ty::Var::Unassigned(0))));
            Rc::new(ty::Ty::Application {
                constructor: Rc::new(ty::Ty::Constructor(ir::TyConstructor::Function)),
                arguments: Rc::new(ty::Ty::Cons {
                    head: Rc::new(ty::Ty::Application {
                        constructor: Rc::new(ty::Ty::Constructor(ir::TyConstructor::Tuple)),
                        arguments: Rc::new(ty::Ty::Nil),
                    }),
                    tail: Rc::new(ty::Ty::Cons {
                        head: Rc::new(ty::Ty::Application {
                            constructor: Rc::new(ty::Ty::Constructor(ir::TyConstructor::Reference)),
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
            })
        }
        ir::Function::Print => Rc::new(ty::Ty::Application {
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
        _ => todo!(),
    }
}
