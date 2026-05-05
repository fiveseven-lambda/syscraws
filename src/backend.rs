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
mod unify;

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
    let mut function_definitions = Vec::new();
    let instances = HashMap::from([(
        ir::Class::Add,
        [
            (
                ir::Ty::Constructor(ir::TyConstructor::AddInteger),
                vec![
                    ir::Ty::Constructor(ir::TyConstructor::Integer),
                    ir::Ty::Constructor(ir::TyConstructor::Integer),
                ],
            ),
            (
                ir::Ty::Constructor(ir::TyConstructor::AddFloat),
                vec![
                    ir::Ty::Constructor(ir::TyConstructor::Float),
                    ir::Ty::Constructor(ir::TyConstructor::Float),
                ],
            ),
        ],
    )]);
    for function_definition in &ir_program.function_definitions {
        let local_variables_ty: Vec<_> = (0..function_definition.num_local_variables)
            .map(|_| Rc::new(ty::Ty::Var(RefCell::new(ty::Var::Unassigned(0)))))
            .collect();
        let mut function_uses: Vec<FunctionUse> = function_definition
            .function_uses
            .iter()
            .map(|function_use| FunctionUse::Unresolved(function_use.candidates.clone()))
            .collect();
        let mut calls: Vec<Option<Call>> = function_definition.calls.iter().map(|_| None).collect();
        loop {
            let mut updated = false;
            for function_use_index in 0..function_definition.function_uses.len() {
                let candidates = match function_uses[function_use_index] {
                    FunctionUse::Unresolved(ref mut candidates) => std::mem::take(candidates),
                    FunctionUse::Resolved(_, _) => continue,
                };
                let mut new_candidates = Vec::new();
                let mut updated_calls = Vec::new();
                let mut substitutions = Vec::new();
                for candidate in candidates {
                    let candidate_ty = get_function_ty(&candidate, &ir_program.function_tys);
                    function_uses[function_use_index] =
                        FunctionUse::Resolved(candidate.clone(), candidate_ty.clone());
                    if let ir::Function::Method(class, method_index) = &candidate {
                        let instance = Rc::new(ty::Ty::Var(RefCell::new(ty::Var::Unassigned(0))));
                        let ty_arguments =
                            vec![Rc::new(ty::Ty::Var(RefCell::new(ty::Var::Unassigned(0))))];
                        let relations = vec![(instance, class.clone(), ty_arguments)];
                        search_instance(&relations);
                    } else {
                        let mut unifier = unify::Unifier::new();
                        let mut updated_call_indices = Vec::new();
                        if translate_calls(
                            function_definition.function_uses[function_use_index].used_by,
                            &mut unifier,
                            &mut updated_call_indices,
                            &function_definition.calls,
                            &function_uses,
                            &mut calls,
                            &local_variables_ty,
                            &global_variables_ty,
                            &ir_program.function_tys,
                        ) {
                            substitutions.extend(unifier.rollback());
                            for call_index in updated_call_indices.into_iter().rev() {
                                let call = std::mem::take(&mut calls[call_index]);
                                updated_calls.push((call_index, call));
                            }
                            new_candidates.push((candidate, candidate_ty));
                        } else {
                            for _ in unifier.rollback() {}
                            for call_index in updated_call_indices.into_iter().rev() {
                                calls[call_index] = None;
                            }
                        }
                    }
                }
                if new_candidates.len() > 2 {
                    function_uses[function_use_index] = FunctionUse::Unresolved(
                        new_candidates
                            .into_iter()
                            .map(|(candidate, _)| candidate)
                            .collect(),
                    );
                } else if let Some((candidate, ty)) = new_candidates.into_iter().next() {
                    for substitution in substitutions.into_iter().rev() {
                        substitution.commit();
                    }
                    for (call_index, call) in updated_calls.into_iter().rev() {
                        calls[call_index] = call;
                    }
                    function_uses[function_use_index] = FunctionUse::Resolved(candidate, ty);
                    updated = true;
                } else {
                    panic!();
                }
            }
            if !updated {
                break;
            }
        }
        function_definitions.push((function_uses, calls));
    }
    unsafe { ffi::initialize_jit() };
    let num_definitions = ir_program.function_definitions.len();
    for (function_index, (ir_function_definition, function_definition)) in ir_program
        .function_definitions
        .into_iter()
        .zip(function_definitions)
        .enumerate()
    {
        let function_name = CString::new(format!("{}", function_index)).unwrap();
        unsafe {
            let function_type = ffi::get_function_type(false, ffi::get_integer_type(), 0);
            ffi::add_function(
                function_name.as_ptr(),
                function_type,
                ir_function_definition.blocks.len(),
            );
        }
        for (block_index, block) in ir_function_definition.blocks.into_iter().enumerate() {
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

fn search_instance(relations: &[(Rc<ty::Ty>, ir::Class, Vec<Rc<ty::Ty>>)]) {}

fn get_expression_ty(
    expression: &ir::Expression,
    function_uses: &[FunctionUse],
    calls: &[Option<Call>],
    local_variables_ty: &[Rc<ty::Ty>],
    global_variables_ty: &[Rc<ty::Ty>],
    ir_function_tys: &[ir::FunctionTy],
) -> Option<Rc<ty::Ty>> {
    match *expression {
        ir::Expression::Integer(_) => {
            Some(Rc::new(ty::Ty::Constructor(ir::TyConstructor::Integer)))
        }
        ir::Expression::Float(_) => Some(Rc::new(ty::Ty::Constructor(ir::TyConstructor::Integer))),
        ir::Expression::String(_) => Some(Rc::new(ty::Ty::Constructor(ir::TyConstructor::String))),
        ir::Expression::FunctionUse(argument_function_use_index) => {
            match &function_uses[argument_function_use_index] {
                FunctionUse::Resolved(_, ty) => Some(ty.clone()),
                FunctionUse::Unresolved(_) => None,
            }
        }
        ir::Expression::Call(argument_call_index) => match &calls[argument_call_index] {
            Some(Call { ty, .. }) => Some(ty.clone()),
            None => None,
        },
        ir::Expression::Variable(storage, index) => match storage {
            ir::Storage::Global => Some(global_variables_ty[index].clone()),
            ir::Storage::Local => Some(local_variables_ty[index].clone()),
        },
    }
}

#[derive(serde::Serialize)]
enum FunctionUse {
    Resolved(ir::Function, Rc<ty::Ty>),
    Unresolved(Vec<ir::Function>),
}

#[derive(serde::Serialize)]
struct Call {
    ty: Rc<ty::Ty>,
    extra_calls: Vec<Rc<ty::Ty>>,
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

fn translate_calls(
    call_index: Option<usize>,
    unifications: &mut unify::Unifier,
    updated_call_indices: &mut Vec<usize>,
    ir_calls: &[ir::Call],
    function_uses: &[FunctionUse],
    calls: &mut [Option<Call>],
    local_variables_ty: &[Rc<ty::Ty>],
    global_variables_ty: &[Rc<ty::Ty>],
    ir_function_tys: &[ir::FunctionTy],
) -> bool {
    let Some(call_index) = call_index else {
        return true;
    };
    let call = &ir_calls[call_index];
    let num_arguments = call.arguments.len();
    let Some(argument_tys): Option<Vec<_>> = call
        .arguments
        .iter()
        .map(|argument| {
            get_expression_ty(
                argument,
                function_uses,
                calls,
                local_variables_ty,
                global_variables_ty,
                ir_function_tys,
            )
        })
        .collect()
    else {
        return true;
    };
    let Some(function_ty) = get_expression_ty(
        &call.function,
        function_uses,
        calls,
        local_variables_ty,
        global_variables_ty,
        ir_function_tys,
    ) else {
        return true;
    };
    let return_ty = Rc::new(ty::Ty::Var(RefCell::new(ty::Var::Unassigned(0))));
    let parameter_tys: Vec<_> = (0..num_arguments)
        .map(|_| Rc::new(ty::Ty::Var(RefCell::new(ty::Var::Unassigned(0)))))
        .collect();
    if !unifications.unify(
        &function_ty,
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
        return false;
    }
    let mut ty_vars = HashMap::from([(None, 0)]);
    let argument_orders: Vec<_> = argument_tys
        .iter()
        .map(|argument_ty| {
            let (var, order) = argument_ty.extract_function_ty();
            let next_index = ty_vars.len();
            let index = *ty_vars.entry(var).or_insert(next_index);
            (index, order)
        })
        .collect();
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
        return false;
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
                        constructor: Rc::new(ty::Ty::Constructor(ir::TyConstructor::Function)),
                        arguments: Rc::new(ty::Ty::Cons {
                            head: ty,
                            tail: extra_call.clone(),
                        }),
                    })
                },
            ),
        );
    }
    let call_ty = extra_calls.iter().fold(return_ty, |ty, extra_call| {
        Rc::new(ty::Ty::Application {
            constructor: Rc::new(ty::Ty::Constructor(ir::TyConstructor::Function)),
            arguments: Rc::new(ty::Ty::Cons {
                head: ty,
                tail: extra_call.clone(),
            }),
        })
    });
    calls[call_index] = Some(Call {
        ty: call_ty.clone(),
        extra_calls,
    });
    updated_call_indices.push(call_index);
    translate_calls(
        call.used_by,
        unifications,
        updated_call_indices,
        ir_calls,
        function_uses,
        calls,
        local_variables_ty,
        global_variables_ty,
        ir_function_tys,
    )
}

fn get_function_ty(function: &ir::Function, ir_function_tys: &[ir::FunctionTy]) -> Rc<ty::Ty> {
    match *function {
        ir::Function::UserDefined(function_index) => {
            let ir_function_ty = &ir_function_tys[function_index];
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
        ir::Function::Dereference => {
            let target_ty = Rc::new(ty::Ty::Var(RefCell::new(ty::Var::Unassigned(0))));
            Rc::new(ty::Ty::Application {
                constructor: Rc::new(ty::Ty::Constructor(ir::TyConstructor::Function)),
                arguments: Rc::new(ty::Ty::Cons {
                    head: target_ty.clone(),
                    tail: Rc::new(ty::Ty::Cons {
                        head: Rc::new(ty::Ty::Application {
                            constructor: Rc::new(ty::Ty::Constructor(ir::TyConstructor::Reference)),
                            arguments: Rc::new(ty::Ty::Cons {
                                head: target_ty.clone(),
                                tail: Rc::new(ty::Ty::Nil),
                            }),
                        }),
                        tail: Rc::new(ty::Ty::Nil),
                    }),
                }),
            })
        }
        _ => todo!(),
    }
}
