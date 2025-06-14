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

pub fn translate(ir_program: ir::Program) -> Result<unsafe fn() -> u8, ()> {
    let mut program = Program {
        function_definitions: Vec::new(),
    };
    for definition in ir_program.function_definitions {
        let mut body = Vec::new();

        let variables_ty: Vec<_> = (0..definition.num_local_variables)
            .map(|_| Rc::new(ty::Ty::Var(Rc::new(RefCell::new(ty::Var::Unassigned(0))))))
            .collect();

        translate_block(
            &definition.body,
            &mut body,
            definition.body.size,
            &variables_ty,
            &ir_program.functions_ty,
        );

        program
            .function_definitions
            .push(FunctionDefinition { body });
    }
    todo!();
}

fn translate_block(
    block: &ir::Block,
    blocks: &mut Vec<Block>,
    next: usize,
    variables_ty: &[Rc<ty::Ty>],
    ir_functions_ty: &[ir::FunctionTy],
) {
    for (index, statement) in block.statements.iter().enumerate() {
        match statement {
            ir::Statement::Expr(expressions) => {
                blocks.push(Block {
                    expressions: expressions
                        .iter()
                        .map(|expression| {
                            translate_expression(expression, variables_ty, ir_functions_ty).1
                        })
                        .collect(),
                    next: Next::Jump(if index == block.statements.len() - 1 {
                        next
                    } else {
                        blocks.len() + 1
                    }),
                });
            }
            ir::Statement::If {
                antecedents,
                condition,
                then_block,
                else_block,
            } => {
                let then_index = blocks.len() + 1;
                let else_index = then_index + then_block.size;
                let mut expressions = Vec::new();
                for antecedent in antecedents {
                    expressions
                        .push(translate_expression(antecedent, variables_ty, ir_functions_ty).1);
                }
                let (condition_ty, condition) =
                    translate_expression(condition, variables_ty, ir_functions_ty);
                blocks.push(Block {
                    expressions,
                    next: Next::Br(condition, then_index, else_index),
                });
                let next_index = if index == block.statements.len() - 1 {
                    next
                } else {
                    else_index + else_block.size
                };
                translate_block(
                    then_block,
                    blocks,
                    next_index,
                    variables_ty,
                    ir_functions_ty,
                );
                translate_block(
                    else_block,
                    blocks,
                    next_index,
                    variables_ty,
                    ir_functions_ty,
                );
            }
            ir::Statement::While {
                condition,
                do_block,
            } => {
                let condition_index = blocks.len();
                let do_index = condition_index + 1;
                let next_index = if index == block.statements.len() - 1 {
                    next
                } else {
                    do_index + do_block.size
                };
                let (condition_ty, condition) =
                    translate_expression(condition, variables_ty, ir_functions_ty);
                blocks.push(Block {
                    expressions: Vec::new(),
                    next: Next::Br(condition, do_index, next_index),
                });
                translate_block(
                    do_block,
                    blocks,
                    condition_index,
                    variables_ty,
                    ir_functions_ty,
                );
            }
            _ => todo!(),
        }
    }
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
                Expression::Identity(ty),
            )
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
                    let ret = calls.iter().try_fold(
                        translate_function(&ir_function, ir_functions_ty),
                        |function, call| {
                            translate_call(
                                function,
                                call,
                                &mut unifications,
                                variables_ty,
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
        ir::Expression::Variable(storage, index) => (
            Rc::new(ty::Ty::Application {
                constructor: Rc::new(ty::Ty::Constructor(ir::TyConstructor::Reference)),
                arguments: Rc::new(ty::Ty::Cons {
                    head: variables_ty[*index].clone(),
                    tail: Rc::new(ty::Ty::Nil),
                }),
            }),
            Expression::Variable(*storage, *index),
        ),
    }
}

fn translate_call(
    (function_ty, function): (Rc<ty::Ty>, Expression),
    call: &ir::Call,
    unifications: &mut ty::Unifications,
    variables_ty: &[Rc<ty::Ty>],
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
        .map(|argument| translate_expression(&argument, variables_ty, ir_functions_ty))
        .collect();
    let mut min_order = 0;
    let mut max_order = 0;
    let mut inequalities = Vec::new();
    for ((argument_ty, _), parameter_ty) in arguments.iter().zip(&parameters_ty) {
        let (argument_return_ty, argument_depth) = argument_ty.extract_function_ty();
        let (parameter_return_ty, parameter_depth) = parameter_ty.extract_function_ty();
        let gap = argument_depth - parameter_depth;
        inequalities.push((argument_return_ty, parameter_return_ty, gap));
        max_order += gap.abs();
    }
    let get_depths = |order| {
        let mut depths = HashMap::new();
        depths.insert(None, Some(0));
        for (argument_return_ty, parameter_return_ty, _) in &inequalities {
            if let Some(argument_return_ty) = argument_return_ty {
                depths.insert(Some(Rc::as_ptr(argument_return_ty)), None);
            }
            if let Some(parameter_return_ty) = parameter_return_ty {
                depths.insert(Some(Rc::as_ptr(parameter_return_ty)), None);
            }
        }
        for _ in 0..depths.len() {
            let mut updated = false;
            for (argument, parameter, diff) in &inequalities {
                let Some(argument_depth) = depths[&argument.as_ref().map(Rc::as_ptr)] else {
                    continue;
                };
                let new_parameter_depth = argument_depth + diff + order;
                let parameter_depth = depths.get_mut(&parameter.as_ref().map(Rc::as_ptr)).unwrap();
                if parameter_depth.is_none_or(|depth| depth > new_parameter_depth) {
                    *parameter_depth = Some(new_parameter_depth);
                    updated = true;
                }
            }
            if !updated {
                return Some(depths);
            }
        }
        None
    };
    let Some(mut depths) = get_depths(max_order) else {
        return None;
    };
    while min_order < max_order - 1 {
        let mid_order = (min_order + max_order) / 2;
        match get_depths(mid_order) {
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
        .iter()
        .map(|(argument_ty, parameter_ty, diff)| {
            let Some(argument_depth) = depths[&argument_ty.as_ref().map(Rc::as_ptr)] else {
                return None;
            };
            let Some(parameter_depth) = depths[&parameter_ty.as_ref().map(Rc::as_ptr)] else {
                return None;
            };
            (diff + argument_depth - parameter_depth).try_into().ok()
        })
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
}

struct Program {
    function_definitions: Vec<FunctionDefinition>,
}

struct FunctionDefinition {
    body: Vec<Block>,
}

struct Block {
    expressions: Vec<Expression>,
    next: Next,
}

enum Next {
    Jump(usize),
    Br(Expression, usize, usize),
}

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
    Delete,
    Call {
        function: Box<Expression>,
        arguments: Vec<Expression>,
    },
}
