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

use crate::ir;
use serde::ser::{Serialize, SerializeMap, SerializeStructVariant, Serializer};
use std::cell::RefCell;
use std::collections::HashMap;
use std::ptr;
use std::rc::Rc;
mod unify;

pub enum Ty {
    Constructor(Constructor),
    Parameter(usize),
    Application {
        constructor: Rc<Ty>,
        arguments: Rc<Ty>,
    },
    Nil,
    Cons {
        head: Rc<Ty>,
        tail: Rc<Ty>,
    },
    Var(RefCell<Var>),
}

#[derive(PartialEq, Eq, serde::Serialize)]
pub enum Constructor {
    Integer,
    Float,
    String,
    Function,
    Reference,
    Tuple,
}

impl Serialize for Ty {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        match self {
            Ty::Constructor(constructor) => {
                serializer.serialize_newtype_variant("Ty", 0, "Constructor", constructor)
            }
            Ty::Parameter(index) => {
                serializer.serialize_newtype_variant("Ty", 1, "Parameter", index)
            }
            Ty::Application {
                constructor,
                arguments,
            } => {
                let mut state = serializer.serialize_struct_variant("Ty", 2, "Application", 2)?;
                state.serialize_field("constructor", constructor)?;
                state.serialize_field("arguments", arguments)?;
                state.end()
            }
            Ty::Nil => serializer.serialize_unit_variant("Ty", 3, "Nil"),
            Ty::Cons { head, tail } => {
                let mut state = serializer.serialize_struct_variant("Ty", 4, "Cons", 2)?;
                state.serialize_field("head", head)?;
                state.serialize_field("tail", tail)?;
                state.end()
            }
            Ty::Var(var) => var.serialize(serializer),
        }
    }
}

#[derive(Clone)]
pub enum Var {
    Assigned(Rc<Ty>),
    Unassigned(u32),
}

impl Serialize for Var {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        match self {
            Var::Assigned(ty) => ty.serialize(serializer),
            Var::Unassigned(rank) => {
                let mut map = serializer.serialize_map(Some(1))?;
                map.serialize_entry("addr", &format!("{:p}", self))?;
                map.serialize_entry("rank", rank)?;
                map.end()
            }
        }
    }
}

impl Ty {
    pub fn contains(self: &Rc<Ty>, var: &RefCell<Var>) -> bool {
        match self.as_ref() {
            Ty::Constructor(_) => false,
            Ty::Parameter(_) => false,
            Ty::Application {
                constructor,
                arguments,
            } => constructor.contains(var) || arguments.contains(var),
            Ty::Nil => false,
            Ty::Cons { head, tail } => head.contains(var) || tail.contains(var),
            Ty::Var(self_var) => match *self_var.borrow() {
                Var::Assigned(ref ty) => ty.contains(var),
                Var::Unassigned(_) => ptr::eq(self_var, var),
            },
        }
    }

    pub fn extract_function_ty(self: &Rc<Ty>) -> (Option<*const Var>, i32) {
        match self.as_ref() {
            Ty::Application {
                constructor,
                arguments,
            } => match constructor.as_ref() {
                Ty::Constructor(Constructor::Function) => match arguments.as_ref() {
                    Ty::Cons { head, tail: _ } => {
                        let (ty, depth) = head.extract_function_ty();
                        (ty, depth + 1)
                    }
                    _ => panic!(),
                },
                _ => (None, 0),
            },
            Ty::Var(var) => match *var.borrow() {
                Var::Assigned(ref ty) => ty.extract_function_ty(),
                Var::Unassigned(_) => (Some(var.as_ptr()), 0),
            },
            _ => (None, 0),
        }
    }
}

fn translate_ty(
    ir_ty: &ir::Constant,
    ty_parameters: &mut Vec<Vec<Rc<Ty>>>,
    ty_variables: &[Rc<Ty>],
) -> Rc<Ty> {
    match *ir_ty {
        ir::Constant::Integer => Rc::new(Ty::Constructor(Constructor::Integer)),
        ir::Constant::Float => Rc::new(Ty::Constructor(Constructor::Float)),
        ir::Constant::Function => Rc::new(Ty::Constructor(Constructor::Function)),
        ir::Constant::Parameter(i, j) => ty_parameters[i][j].clone(),
        ir::Constant::Variable(index) => ty_variables[index].clone(),
        ir::Constant::Application(ref constructor, ref arguments) => Rc::new(Ty::Application {
            constructor: translate_ty(constructor, ty_parameters, ty_variables),
            arguments: translate_tys(arguments, ty_parameters, ty_variables),
        }),
        _ => todo!(),
    }
}

fn translate_tys(
    ir_tys: &[ir::Constant],
    ty_parameters: &mut Vec<Vec<Rc<Ty>>>,
    ty_variables: &[Rc<Ty>],
) -> Rc<Ty> {
    return ir_tys.iter().rfold(Rc::new(Ty::Nil), |tail, ir_head| {
        Rc::new(Ty::Cons {
            head: translate_ty(ir_head, ty_parameters, ty_variables),
            tail,
        })
    });
}

pub fn check(ir_program: &ir::Program) -> Result<Vec<TyInfo>, ()> {
    let ty_variables: Vec<_> = (0..ir_program.num_ty_variables)
        .map(|_| Rc::new(Ty::Var(RefCell::new(Var::Unassigned(0)))))
        .collect();
    let mut ty_info = Vec::new();
    for function_definition in &ir_program.function_definitions {
        let local_variables_ty: Vec<_> = function_definition
            .local_variable_tys
            .iter()
            .map(|local_variable_ty| {
                translate_ty(local_variable_ty, &mut Vec::new(), &ty_variables)
            })
            .collect();
        let mut function_use_tys: Vec<FunctionUseTy> = function_definition
            .function_uses
            .iter()
            .map(|function_use| FunctionUseTy::Unresolved(function_use.candidates.clone()))
            .collect();
        let mut call_tys: Vec<Option<CallTy>> =
            function_definition.calls.iter().map(|_| None).collect();
        loop {
            let mut updated = false;
            for function_use_index in 0..function_definition.function_uses.len() {
                let candidates = match function_use_tys[function_use_index] {
                    FunctionUseTy::Unresolved(ref mut candidates) => std::mem::take(candidates),
                    FunctionUseTy::Resolved(_, _) => continue,
                };
                let mut new_candidates = Vec::new();
                let mut updated_calls = Vec::new();
                let mut substitutions = Vec::new();
                for candidate in candidates {
                    let candidate_ty = get_function_ty(
                        &candidate,
                        &ir_program.function_tys,
                        &mut Vec::new(),
                        &ty_variables,
                    );
                    function_use_tys[function_use_index] =
                        FunctionUseTy::Resolved(candidate.clone(), candidate_ty.clone());
                    let mut unifier = unify::Unifier::new();
                    let mut updated_call_indices = Vec::new();
                    if translate_calls(
                        function_definition.function_uses[function_use_index].used_by,
                        &mut unifier,
                        &mut updated_call_indices,
                        &function_definition.calls,
                        &function_use_tys,
                        &mut call_tys,
                        &local_variables_ty,
                        &ty_variables,
                        &ir_program.function_tys,
                    ) {
                        substitutions.extend(unifier.rollback());
                        for call_index in updated_call_indices.into_iter().rev() {
                            let call = std::mem::take(&mut call_tys[call_index]);
                            updated_calls.push((call_index, call));
                        }
                        new_candidates.push((candidate, candidate_ty));
                    } else {
                        for _ in unifier.rollback() {}
                        for call_index in updated_call_indices.into_iter().rev() {
                            call_tys[call_index] = None;
                        }
                    }
                }
                if new_candidates.len() > 2 {
                    function_use_tys[function_use_index] = FunctionUseTy::Unresolved(
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
                        call_tys[call_index] = call;
                    }
                    function_use_tys[function_use_index] = FunctionUseTy::Resolved(candidate, ty);
                    updated = true;
                } else {
                    panic!();
                }
            }
            if !updated {
                break;
            }
        }
        ty_info.push(TyInfo {
            function_use_tys,
            call_tys,
        });
    }
    Ok(ty_info)
}

fn get_expression_ty(
    expression: &ir::Expression,
    function_uses: &[FunctionUseTy],
    calls: &[Option<CallTy>],
    local_variables_ty: &[Rc<Ty>],
    global_variables_ty: &[Rc<Ty>],
    ir_function_tys: &[ir::Constant],
) -> Option<Rc<Ty>> {
    match *expression {
        ir::Expression::Integer(_) => Some(Rc::new(Ty::Constructor(Constructor::Integer))),
        ir::Expression::Float(_) => Some(Rc::new(Ty::Constructor(Constructor::Float))),
        ir::Expression::String(_) => Some(Rc::new(Ty::Constructor(Constructor::String))),
        ir::Expression::FunctionUse(argument_function_use_index) => {
            match &function_uses[argument_function_use_index] {
                FunctionUseTy::Resolved(_, ty) => Some(ty.clone()),
                FunctionUseTy::Unresolved(_) => None,
            }
        }
        ir::Expression::Call(argument_call_index) => match &calls[argument_call_index] {
            Some(CallTy { ty, .. }) => Some(ty.clone()),
            None => None,
        },
        ir::Expression::Variable(storage, index) => match storage {
            ir::Storage::Global => Some(global_variables_ty[index].clone()),
            ir::Storage::Local => Some(local_variables_ty[index].clone()),
        },
    }
}

pub struct TyInfo {
    function_use_tys: Vec<FunctionUseTy>,
    call_tys: Vec<Option<CallTy>>,
}

#[derive(serde::Serialize)]
pub enum FunctionUseTy {
    Resolved(ir::Constant, Rc<Ty>),
    Unresolved(Vec<ir::Constant>),
}

#[derive(serde::Serialize)]
pub struct CallTy {
    pub ty: Rc<Ty>,
    pub extra_calls: Vec<Rc<Ty>>,
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
    function_uses: &[FunctionUseTy],
    calls: &mut [Option<CallTy>],
    local_variables_ty: &[Rc<Ty>],
    global_variables_ty: &[Rc<Ty>],
    ir_function_tys: &[ir::Constant],
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
    let return_ty = Rc::new(Ty::Var(RefCell::new(Var::Unassigned(0))));
    let parameter_tys: Vec<_> = (0..num_arguments)
        .map(|_| Rc::new(Ty::Var(RefCell::new(Var::Unassigned(0)))))
        .collect();
    if !unifications.unify(
        &function_ty,
        &Rc::new(Ty::Application {
            constructor: Rc::new(Ty::Constructor(Constructor::Function)),
            arguments: Rc::new(Ty::Cons {
                head: return_ty.clone(),
                tail: parameter_tys
                    .iter()
                    .rev()
                    .fold(Rc::new(Ty::Nil), |tail, parameter_ty| {
                        Rc::new(Ty::Cons {
                            head: parameter_ty.clone(),
                            tail,
                        })
                    }),
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
        .map(|_| Rc::new(Ty::Var(RefCell::new(Var::Unassigned(0)))))
        .collect();
    for i in 0..num_arguments {
        unifications.unify(
            &argument_tys[i],
            &extra_calls.iter().take(orders[i] as usize).fold(
                parameter_tys[i].clone(),
                |ty, extra_call| {
                    Rc::new(Ty::Application {
                        constructor: Rc::new(Ty::Constructor(Constructor::Function)),
                        arguments: Rc::new(Ty::Cons {
                            head: ty,
                            tail: extra_call.clone(),
                        }),
                    })
                },
            ),
        );
    }
    let call_ty = extra_calls.iter().fold(return_ty, |ty, extra_call| {
        Rc::new(Ty::Application {
            constructor: Rc::new(Ty::Constructor(Constructor::Function)),
            arguments: Rc::new(Ty::Cons {
                head: ty,
                tail: extra_call.clone(),
            }),
        })
    });
    calls[call_index] = Some(CallTy {
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

fn get_function_ty(
    function: &ir::Constant,
    ir_function_tys: &[ir::Constant],
    parameters: &mut Vec<Vec<Rc<Ty>>>,
    variables: &[Rc<Ty>],
) -> Rc<Ty> {
    match *function {
        ir::Constant::Application(ref function, ref arguments) => {
            let new_parameters = arguments
                .iter()
                .map(|argument| translate_ty(argument, parameters, variables))
                .collect();
            parameters.push(new_parameters);
            get_function_ty(function, ir_function_tys, parameters, variables)
        }
        ir::Constant::AddInteger => Rc::new(Ty::Application {
            constructor: Rc::new(Ty::Constructor(Constructor::Function)),
            arguments: Rc::new(Ty::Cons {
                head: Rc::new(Ty::Constructor(Constructor::Integer)),
                tail: Rc::new(Ty::Cons {
                    head: Rc::new(Ty::Constructor(Constructor::Integer)),
                    tail: Rc::new(Ty::Cons {
                        head: Rc::new(Ty::Constructor(Constructor::Integer)),
                        tail: Rc::new(Ty::Nil),
                    }),
                }),
            }),
        }),
        ir::Constant::DeleteInteger => Rc::new(Ty::Application {
            constructor: Rc::new(Ty::Constructor(Constructor::Function)),
            arguments: Rc::new(Ty::Cons {
                head: Rc::new(Ty::Application {
                    constructor: Rc::new(Ty::Constructor(Constructor::Tuple)),
                    arguments: Rc::new(Ty::Nil),
                }),
                tail: Rc::new(Ty::Cons {
                    head: Rc::new(Ty::Constructor(Constructor::Integer)),
                    tail: Rc::new(Ty::Nil),
                }),
            }),
        }),
        ir::Constant::Dereference => Rc::new(Ty::Application {
            constructor: Rc::new(Ty::Constructor(Constructor::Function)),
            arguments: Rc::new(Ty::Cons {
                head: parameters.last().unwrap()[0].clone(),
                tail: Rc::new(Ty::Cons {
                    head: Rc::new(Ty::Application {
                        constructor: Rc::new(Ty::Constructor(Constructor::Reference)),
                        arguments: Rc::new(Ty::Cons {
                            head: parameters.last().unwrap()[0].clone(),
                            tail: Rc::new(Ty::Nil),
                        }),
                    }),
                    tail: Rc::new(Ty::Nil),
                }),
            }),
        }),
        ir::Constant::UserDefinedFunction(index) => {
            translate_ty(&ir_function_tys[index], parameters, variables)
        }
        _ => todo!(),
    }
}
