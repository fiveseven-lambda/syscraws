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
            arguments: arguments
                .iter()
                .map(|argument| translate_ty(argument, ty_parameters))
                .collect(),
        }),
    }
}

pub fn translate(ir_program: ir::Program) {
    for definition in ir_program.function_definitions {
        let variables_ty: Vec<_> = (0..definition.num_local_variables)
            .map(|_| Rc::new(ty::Ty::Var(Rc::new(RefCell::new(ty::Var::Unassigned(0))))))
            .collect();
        let mut num_blocks = 0;
        translate_block(
            &definition.body,
            &mut num_blocks,
            &variables_ty,
            &ir_program.functions_ty,
        );
    }
}

fn translate_block(
    block: &ir::Block,
    num_blocks: &mut usize,
    variables_ty: &[Rc<ty::Ty>],
    functions_ty: &[ir::FunctionTy],
) {
    for statement in &block.statements {
        translate_statement(statement, num_blocks, variables_ty, functions_ty);
    }
}

fn translate_statement(
    statement: &ir::Statement,
    num_blocks: &mut usize,
    variables_ty: &[Rc<ty::Ty>],
    functions_ty: &[ir::FunctionTy],
) {
    match statement {
        ir::Statement::Expr(exprs) => {
            for expr in exprs {
                translate_expression(expr, variables_ty, functions_ty);
            }
            println!("{num_blocks}: Jump");
            *num_blocks += 1;
        }
        ir::Statement::If {
            antecedents,
            condition,
            then_block,
            else_block,
        } => {
            for expr in antecedents {
                translate_expression(expr, variables_ty, functions_ty);
            }
            translate_expression(condition, variables_ty, functions_ty);
            println!("{num_blocks}: Br");
            *num_blocks += 1;
            translate_block(then_block, num_blocks, variables_ty, functions_ty);
            translate_block(else_block, num_blocks, variables_ty, functions_ty);
        }
        ir::Statement::While {
            condition,
            do_block,
        } => {
            translate_expression(condition, variables_ty, functions_ty);
            println!("{num_blocks}: Br");
            *num_blocks += 1;
            translate_block(do_block, num_blocks, variables_ty, functions_ty);
        }
        ir::Statement::Break(exprs) => {
            for expr in exprs {
                translate_expression(expr, variables_ty, functions_ty);
            }
            println!("{num_blocks}: break");
        }
        ir::Statement::Continue(exprs) => {
            for expr in exprs {
                translate_expression(expr, variables_ty, functions_ty);
            }
            println!("{num_blocks}: continue");
        }
    }
}

fn translate_function(
    ir_function: &ir::Function,
    ir_functions_ty: &[ir::FunctionTy],
) -> (Rc<ty::Ty>, Expression) {
    match ir_function {
        ir::Function::UserDefined(index) => {
            let ir_function_ty = &ir_functions_ty[*index];
            let ty_parameters: Vec<_> = (0..ir_function_ty.num_ty_parameters)
                .map(|_| Rc::new(ty::Ty::Var(Rc::new(RefCell::new(ty::Var::Unassigned(0))))))
                .collect();
            let mut return_and_parameters_ty =
                Vec::with_capacity(1 + ir_function_ty.parameters_ty.len());
            return_and_parameters_ty.push(translate_ty(&ir_function_ty.return_ty, &ty_parameters));
            return_and_parameters_ty.extend(
                ir_function_ty
                    .parameters_ty
                    .iter()
                    .map(|parameter_ty| translate_ty(&parameter_ty, &ty_parameters)),
            );
            let function_ty = Rc::new(ty::Ty::Application {
                constructor: Rc::new(ty::Ty::Constructor(ir::TyConstructor::Function)),
                arguments: return_and_parameters_ty,
            });
            let function = Expression::Function(*index);
            (function_ty, function)
        }
        _ => todo!(),
    }
}

fn translate_expression(
    expression: &ir::Expression,
    variables_ty: &[Rc<ty::Ty>],
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
                    calls.iter().try_fold(
                        translate_function(&ir_function, ir_functions_ty),
                        |(function_ty, function), ir::Call { arguments }| {
                            let ty::Ty::Application {
                                constructor,
                                arguments: return_and_parameters_ty,
                            } = function_ty.as_ref()
                            else {
                                return None;
                            };
                            let ty::Ty::Constructor(ir::TyConstructor::Function) =
                                constructor.as_ref()
                            else {
                                return None;
                            };
                            let return_ty = &return_and_parameters_ty[0];
                            let ty::Ty::List(parameters_ty) = return_and_parameters_ty[1].as_ref()
                            else {
                                return None;
                            };
                            if arguments.len() != parameters_ty.len() {
                                return None;
                            }
                            let arguments: Vec<_> = arguments
                                .iter()
                                .map(|argument| {
                                    translate_expression(&argument, variables_ty, ir_functions_ty)
                                })
                                .collect();
                            let mut depths = HashMap::new();
                            depths.insert(None, Some(0));
                            let mut inequalities = Vec::new();
                            for ((argument_ty, _), parameter_ty) in
                                arguments.iter().zip(parameters_ty)
                            {
                                let (argument_return_ty, argument_depth) =
                                    argument_ty.extract_function_ty();
                                let (parameter_return_ty, parameter_depth) =
                                    parameter_ty.extract_function_ty();
                                if let Some(ref argument_return_ty) = argument_return_ty {
                                    depths.insert(Some(Rc::as_ptr(argument_return_ty)), None);
                                }
                                if let Some(ref parameter_return_ty) = parameter_return_ty {
                                    depths.insert(Some(Rc::as_ptr(parameter_return_ty)), None);
                                }
                                inequalities.push((
                                    argument_return_ty,
                                    parameter_return_ty,
                                    argument_depth - parameter_depth,
                                ));
                            }
                            for i in 0..depths.len() {
                                let mut updated = false;
                                for (argument, parameter, diff) in &inequalities {
                                    let Some(argument_depth) =
                                        depths[&argument.as_ref().map(Rc::as_ptr)]
                                    else {
                                        continue;
                                    };
                                    let new_parameter_depth = argument_depth + diff;
                                    let parameter_depth = depths
                                        .get_mut(&parameter.as_ref().map(Rc::as_ptr))
                                        .unwrap();
                                    if parameter_depth
                                        .is_none_or(|depth| depth > new_parameter_depth)
                                    {
                                        *parameter_depth = Some(new_parameter_depth);
                                        updated = true;
                                    }
                                }
                                if i == depths.len() - 1 && updated {
                                    return None;
                                }
                            }
                            let nums_extra_calls: Option<Vec<_>> = inequalities
                                .iter()
                                .map(|(argument_ty, parameter_ty, diff)| {
                                    let Some(argument_depth) =
                                        depths[&argument_ty.as_ref().map(Rc::as_ptr)]
                                    else {
                                        return None;
                                    };
                                    let Some(parameter_depth) =
                                        depths[&parameter_ty.as_ref().map(Rc::as_ptr)]
                                    else {
                                        return None;
                                    };
                                    (diff + argument_depth - parameter_depth).try_into().ok()
                                })
                                .collect();
                            let Some(nums_extra_calls) = nums_extra_calls else {
                                return None;
                            };
                            let extra_calls: Vec<_> =
                                (0..nums_extra_calls.iter().cloned().max().unwrap_or(0))
                                    .map(|_| {
                                        Rc::new(ty::Ty::Var(Rc::new(RefCell::new(
                                            ty::Var::Unassigned(0),
                                        ))))
                                    })
                                    .collect();
                            for (((argument_ty, _), parameter_ty), num_extra_calls) in
                                arguments.iter().zip(parameters_ty).zip(nums_extra_calls)
                            {
                                if !unifications.unify(
                                    &extra_calls[..num_extra_calls].iter().fold(
                                        parameter_ty.clone(),
                                        |parameter_ty, extra_call| {
                                            Rc::new(ty::Ty::Application {
                                                constructor: Rc::new(ty::Ty::Constructor(
                                                    ir::TyConstructor::Function,
                                                )),
                                                arguments: vec![parameter_ty, extra_call.clone()],
                                            })
                                        },
                                    ),
                                    argument_ty,
                                ) {
                                    return None;
                                }
                            }

                            Some((
                                return_ty.clone(),
                                Expression::Call {
                                    function: Box::new(function),
                                    arguments: arguments
                                        .into_iter()
                                        .map(|(_, argument)| argument)
                                        .collect(),
                                },
                            ))
                        },
                    )
                })
                .collect();
            if candidates.len() == 1 {
                candidates.into_iter().next().unwrap()
            } else {
                todo!();
            }
        }
        _ => todo!(),
    }
}

enum Expression {
    Integer(i32),
    Float(f64),
    String(String),
    Function(usize),
    AddInteger,
    Call {
        function: Box<Expression>,
        arguments: Vec<Expression>,
    },
}
