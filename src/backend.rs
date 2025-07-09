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

use crate::{ffi, ir};

mod tests;
mod ty;

#[cfg(test)]
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
        .map(|_| Rc::new(ty::Ty::Var(Rc::new(RefCell::new(ty::Var::Unassigned(0))))))
        .collect();
    let mut program = Program {
        function_definitions: Vec::new(),
    };
    for definition in ir_program.function_definitions {
        let mut body_rev = Vec::new();

        let local_variables_ty: Vec<_> = (0..definition.num_local_variables)
            .map(|_| Rc::new(ty::Ty::Var(Rc::new(RefCell::new(ty::Var::Unassigned(0))))))
            .collect();

        translate_block(
            &definition.body,
            &mut body_rev,
            None,
            &local_variables_ty,
            &global_variables_ty,
            &ir_program.functions_ty,
        );

        program
            .function_definitions
            .push(FunctionDefinition { body_rev });
    }
    unsafe { ffi::initialize_jit() };
    let context = unsafe { ffi::create_context() };
    let num_definitions = program.function_definitions.len();
    for (function_index, definition) in program.function_definitions.into_iter().enumerate() {
        let function_name = CString::new(format!("{}", function_index)).unwrap();
        unsafe {
            let function_type = ffi::get_function_type(false, ffi::get_integer_type(), 0);
            ffi::add_function(
                context,
                function_name.as_ptr(),
                function_type,
                definition.body_rev.len(),
            );
        }
        for (block_index, block) in definition.body_rev.into_iter().rev().enumerate() {
            unsafe {
                ffi::set_insert_point(context, block_index);
                block.codegen(context);
            }
        }
        if function_index == num_definitions - 1 {
            let pointer = unsafe { ffi::compile_function(context, function_name.as_ptr()) };
            unsafe {
                ffi::delete_context(context);
            }
            return Ok(pointer);
        }
    }
    Err(())
}

fn translate_block(
    block: &ir::Block,
    blocks: &mut Vec<Block>,
    mut next: Option<usize>,
    local_variables_ty: &[Rc<ty::Ty>],
    global_variables_ty: &[Rc<ty::Ty>],
    ir_functions_ty: &[ir::FunctionTy],
) -> Option<usize> {
    for statement in block.statements.iter().rev() {
        match statement {
            ir::Statement::Expressions(ir_expressions) => {
                let index = blocks.len();
                blocks.push(Block {
                    expressions: ir_expressions
                        .iter()
                        .map(|ir_expression| {
                            translate_expression(
                                ir_expression,
                                local_variables_ty,
                                global_variables_ty,
                                ir_functions_ty,
                            )
                            .1
                        })
                        .collect(),
                    next: Next::Jump(next),
                });
                next = Some(index);
            }
            ir::Statement::If {
                antecedents: ir_antecedents,
                condition: ir_condition,
                then_block,
                else_block,
            } => {
                let else_index = translate_block(
                    else_block,
                    blocks,
                    next,
                    local_variables_ty,
                    global_variables_ty,
                    ir_functions_ty,
                );
                let then_index = translate_block(
                    then_block,
                    blocks,
                    next,
                    local_variables_ty,
                    global_variables_ty,
                    ir_functions_ty,
                );
                let (condition_ty, condition) = translate_expression(
                    ir_condition,
                    local_variables_ty,
                    global_variables_ty,
                    ir_functions_ty,
                );
                let condition_index = blocks.len();
                blocks.push(Block {
                    expressions: ir_antecedents
                        .iter()
                        .map(|ir_expression| {
                            translate_expression(
                                ir_expression,
                                local_variables_ty,
                                global_variables_ty,
                                ir_functions_ty,
                            )
                            .1
                        })
                        .collect(),
                    next: Next::Br(condition, then_index, else_index),
                });
                next = Some(condition_index);
            }
            ir::Statement::While {
                condition: ir_condition,
                do_block,
            } => {
                let condition_index = blocks.len() + do_block.size;
                let do_index = translate_block(
                    do_block,
                    blocks,
                    Some(condition_index),
                    local_variables_ty,
                    global_variables_ty,
                    ir_functions_ty,
                );
                assert_eq!(blocks.len(), condition_index);
                let (condition_ty, condition) = translate_expression(
                    ir_condition,
                    local_variables_ty,
                    global_variables_ty,
                    ir_functions_ty,
                );
                blocks.push(Block {
                    expressions: Vec::new(),
                    next: Next::Br(condition, do_index, next),
                });
                next = Some(condition_index);
            }
            ir::Statement::Return {
                antecedents: ir_antecedents,
                value: ir_value,
            } => {
                let (value_ty, value) = translate_expression(
                    ir_value,
                    local_variables_ty,
                    global_variables_ty,
                    ir_functions_ty,
                );
                blocks.push(Block {
                    expressions: ir_antecedents
                        .iter()
                        .map(|ir_expression| {
                            translate_expression(
                                ir_expression,
                                local_variables_ty,
                                global_variables_ty,
                                ir_functions_ty,
                            )
                            .1
                        })
                        .collect(),
                    next: Next::Return(value),
                });
            }
            _ => todo!(),
        }
    }
    next
}

fn translate_function(
    ir_function: &ir::Function,
    ir_functions_ty: &[ir::FunctionTy],
) -> (Rc<ty::Ty>, Expression) {
    match *ir_function {
        ir::Function::UserDefined(index) => {
            let ir_function_ty = &ir_functions_ty[index];
            let ty_parameters: Vec<_> = (0..ir_function_ty.num_ty_parameters)
                .map(|_| Rc::new(ty::Ty::Var(Rc::new(RefCell::new(ty::Var::Unassigned(0))))))
                .collect();
            (
                Rc::new(ty::Ty::Application {
                    constructor: Rc::new(ty::Ty::Constructor(ir::TyConstructor::Function)),
                    arguments: Rc::new(ty::Ty::Cons {
                        head: translate_ty(&ir_function_ty.return_ty, &ty_parameters),
                        tail: translate_tys(&ir_function_ty.parameters_ty, &ty_parameters),
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
            let ty = Rc::new(ty::Ty::Var(Rc::new(RefCell::new(ty::Var::Unassigned(0)))));
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
            let ty = Rc::new(ty::Ty::Var(Rc::new(RefCell::new(ty::Var::Unassigned(0)))));
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
            let ty = Rc::new(ty::Ty::Var(Rc::new(RefCell::new(ty::Var::Unassigned(0)))));
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
            let ty = Rc::new(ty::Ty::Var(Rc::new(RefCell::new(ty::Var::Unassigned(0)))));
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
                        head: Rc::new(ty::Ty::Var(Rc::new(RefCell::new(ty::Var::Unassigned(0))))),
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
    local_variables_ty: &[Rc<ty::Ty>],
    global_variables_ty: &[Rc<ty::Ty>],
    ir_functions_ty: &[ir::FunctionTy],
) -> (Rc<ty::Ty>, Expression) {
    match expression {
        ir::Expression::Integer(value) => (
            Rc::new(ty::Ty::Constructor(ir::TyConstructor::Integer)),
            Expression::Integer(*value),
        ),
        ir::Expression::Float(value) => (
            Rc::new(ty::Ty::Constructor(ir::TyConstructor::Float)),
            Expression::Float(*value),
        ),
        ir::Expression::String(value) => (
            Rc::new(ty::Ty::Constructor(ir::TyConstructor::String)),
            Expression::String(value.clone()),
        ),
        ir::Expression::Function { candidates, calls } => {
            let candidates: Vec<_> = candidates
                .into_iter()
                .filter_map(|ir_function| {
                    let mut unifications = ty::Unifications::new();
                    let ret = calls.iter().try_fold(
                        translate_function(&ir_function, ir_functions_ty),
                        |function, call| {
                            translate_call(
                                function,
                                call,
                                &mut unifications,
                                local_variables_ty,
                                global_variables_ty,
                                ir_functions_ty,
                            )
                        },
                    );
                    let undo = unifications.undo();
                    ret.map(|ret| (ret, undo))
                })
                .collect();
            if candidates.len() == 1 {
                let (ret, undo) = candidates.into_iter().next().unwrap();
                undo.undo();
                ret
            } else {
                todo!("{}", candidates.len());
            }
        }
        ir::Expression::Variable(storage, index) => {
            let ty = match storage {
                ir::Storage::Local => local_variables_ty[*index].clone(),
                ir::Storage::Global => global_variables_ty[*index].clone(),
            };
            (
                Rc::new(ty::Ty::Application {
                    constructor: Rc::new(ty::Ty::Constructor(ir::TyConstructor::Reference)),
                    arguments: Rc::new(ty::Ty::Cons {
                        head: ty,
                        tail: Rc::new(ty::Ty::Nil),
                    }),
                }),
                Expression::Variable(*storage, *index),
            )
        }
    }
}

fn translate_call(
    (function_ty, function): (Rc<ty::Ty>, Expression),
    call: &ir::Call,
    unifications: &mut ty::Unifications,
    local_variables_ty: &[Rc<ty::Ty>],
    global_variables_ty: &[Rc<ty::Ty>],
    ir_functions_ty: &[ir::FunctionTy],
) -> Option<(Rc<ty::Ty>, Expression)> {
    let ty::Ty::Application {
        constructor,
        arguments: return_and_parameters_ty,
    } = function_ty.as_ref()
    else {
        return None;
    };
    let ty::Ty::Constructor(ir::TyConstructor::Function) = constructor.as_ref() else {
        return None;
    };
    let ty::Ty::Cons {
        head: return_ty,
        tail: parameters_ty,
    } = return_and_parameters_ty.as_ref()
    else {
        return None;
    };
    let parameters_ty = {
        let mut parameters_ty_ref: &ty::Ty = parameters_ty;
        let mut parameters_ty_vec = Vec::new();
        while let ty::Ty::Cons { head, tail } = parameters_ty_ref {
            parameters_ty_vec.push(head.clone());
            parameters_ty_ref = tail;
        }
        parameters_ty_vec
    };
    if call.arguments.len() != parameters_ty.len() {
        return None;
    }
    let arguments: Vec<_> = call
        .arguments
        .iter()
        .map(|argument| {
            translate_expression(
                &argument,
                local_variables_ty,
                global_variables_ty,
                ir_functions_ty,
            )
        })
        .collect();
    let mut min_order = 0;
    let mut max_order = 0;
    let mut inequalities = Vec::new();

    let mut ty_variable_indices = HashMap::new();
    for ((argument_ty, _), parameter_ty) in arguments.iter().zip(&parameters_ty) {
        let (argument_return_ty, argument_depth) = argument_ty.extract_function_ty();
        let argument_ret = match argument_return_ty {
            Some(ptr) => {
                let next_index = ty_variable_indices.len() + 1;
                *ty_variable_indices.entry(ptr).or_insert(next_index)
            }
            None => 0,
        };
        let (parameter_return_ty, parameter_depth) = parameter_ty.extract_function_ty();
        let parameter_ret = match parameter_return_ty {
            Some(ptr) => {
                let next_index = ty_variable_indices.len() + 1;
                *ty_variable_indices.entry(ptr).or_insert(next_index)
            }
            None => 0,
        };
        let gap = argument_depth - parameter_depth;
        inequalities.push(Inequality {
            actual_argument: argument_ret,
            formal_parameter: parameter_ret,
            gap,
        });
        max_order += gap.abs();
    }
    let num_ty_variables = ty_variable_indices.len();
    drop(ty_variable_indices);
    let Some(mut depths) = get_depths(&inequalities, num_ty_variables, max_order) else {
        return None;
    };
    while min_order < max_order - 1 {
        let mid_order = (min_order + max_order) / 2;
        match get_depths(&inequalities, num_ty_variables, mid_order) {
            Some(new_depths) => {
                max_order = mid_order;
                depths = new_depths;
            }
            None => {
                min_order = mid_order;
            }
        }
    }

    let nums_extra_calls: Option<Vec<_>> = inequalities
        .into_iter()
        .map(
            |Inequality {
                 actual_argument,
                 formal_parameter,
                 gap,
             }| {
                let argument_depth = depths[actual_argument];
                let parameter_depth = depths[formal_parameter];
                (gap + argument_depth - parameter_depth).try_into().ok()
            },
        )
        .collect();
    let Some(nums_extra_calls) = nums_extra_calls else {
        return None;
    };
    let extra_calls: Vec<_> = (0..max_order)
        .map(|_| Rc::new(ty::Ty::Var(Rc::new(RefCell::new(ty::Var::Unassigned(0))))))
        .collect();
    for (((argument_ty, _), parameter_ty), num_extra_calls) in
        arguments.iter().zip(&parameters_ty).zip(nums_extra_calls)
    {
        if !unifications.unify(
            &extra_calls[..num_extra_calls].iter().fold(
                parameter_ty.clone(),
                |parameter_ty, extra_call| {
                    Rc::new(ty::Ty::Application {
                        constructor: Rc::new(ty::Ty::Constructor(ir::TyConstructor::Function)),
                        arguments: Rc::new(ty::Ty::Cons {
                            head: parameter_ty,
                            tail: extra_call.clone(),
                        }),
                    })
                },
            ),
            argument_ty,
        ) {
            return None;
        }
    }

    match max_order {
        0 => Some((
            return_ty.clone(),
            Expression::App {
                function: Box::new(Expression::Compile {
                    expression: Box::new(function),
                    parameters_ty: parameters_ty,
                    return_ty: return_ty.clone(),
                }),
                arguments: arguments
                    .into_iter()
                    .map(|(_, argument)| argument)
                    .collect(),
            },
        )),
        1 => Some((
            Rc::new(ty::Ty::Application {
                constructor: Rc::new(ty::Ty::Constructor(ir::TyConstructor::Function)),
                arguments: Rc::new(ty::Ty::Cons {
                    head: return_ty.clone(),
                    tail: extra_calls[0].clone(),
                }),
            }),
            Expression::App {
                function: Box::new(function),
                arguments: arguments
                    .into_iter()
                    .map(|(_, argument)| argument)
                    .collect(),
            },
        )),
        _ => todo!(),
    }
}

#[derive(Debug)]
struct Inequality {
    actual_argument: usize,
    formal_parameter: usize,
    gap: i32,
}

fn get_depths(inequalities: &[Inequality], num_vars: usize, max_order: i32) -> Option<Vec<i32>> {
    let mut depths = vec![None; num_vars + 1];
    depths[0] = Some(0);
    for _ in 0..=num_vars {
        let mut updated = false;
        for &Inequality {
            actual_argument,
            formal_parameter,
            gap,
        } in inequalities
        {
            match (depths[actual_argument], depths[formal_parameter]) {
                (None, None) => {}
                (Some(argument_depth), None) => {
                    depths[formal_parameter] = Some(argument_depth + gap);
                    updated = true;
                }
                (None, Some(parameter_depth)) => {
                    depths[actual_argument] = Some(parameter_depth - gap + max_order);
                    updated = true;
                }
                (Some(argument_depth), Some(parameter_depth)) => {
                    if parameter_depth < argument_depth + gap {
                        depths[formal_parameter] = Some(argument_depth + gap);
                        updated = true;
                    }
                    if argument_depth < parameter_depth - gap + max_order {
                        depths[actual_argument] = Some(parameter_depth - gap + max_order);
                        updated = true;
                    }
                }
            }
        }
        if !updated {
            return Some(depths.into_iter().map(Option::unwrap).collect());
        }
    }
    None
}

#[cfg_attr(test, derive(Serialize))]
struct Program {
    function_definitions: Vec<FunctionDefinition>,
}

#[cfg_attr(test, derive(Serialize))]
struct FunctionDefinition {
    body_rev: Vec<Block>,
}

#[cfg_attr(test, derive(Serialize))]
struct Block {
    expressions: Vec<Expression>,
    next: Next,
}

impl Block {
    unsafe fn codegen(&self, context: *mut ffi::Context) {
        for expression in &self.expressions {
            unsafe { ffi::add_expression(context, expression.codegen()) };
        }
        match self.next {
            Next::Return(ref value) => {
                unsafe { ffi::add_return(context, value.codegen()) };
            }
            _ => todo!(),
        }
    }
}

#[cfg_attr(test, derive(Serialize))]
enum Next {
    Jump(Option<usize>),
    Br(Expression, Option<usize>, Option<usize>),
    Return(Expression),
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

impl Expression {
    unsafe fn codegen(&self) -> *const ffi::Expression {
        match *self {
            Expression::Integer(value) => unsafe { ffi::create_integer(value) },
            Expression::Variable(storage, index) => unsafe { ffi::create_integer(0) },
            Expression::App {
                ref function,
                ref arguments,
            } => match arguments.len() {
                0 => unsafe { ffi::create_app(function.codegen(), 0) },
                1 => unsafe { ffi::create_app(function.codegen(), 1, arguments[0].codegen()) },
                _ => todo!(),
            },
            _ => todo!(),
        }
    }
}
