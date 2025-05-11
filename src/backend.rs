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

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone)]
struct Ty {
    inner: Rc<RefCell<TyInner>>,
}

impl std::hash::Hash for Ty {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.inner).hash(state);
    }
}

impl std::cmp::PartialEq for Ty {
    fn eq(&self, other: &Self) -> bool {
        Rc::as_ptr(&self.inner) == Rc::as_ptr(&other.inner)
    }
}

impl std::cmp::Eq for Ty {}

enum TyInner {
    Constructor(ir::TyConstructor),
    Parameter(usize),
    Application { constructor: Ty, arguments: Vec<Ty> },
    List(Vec<Ty>),
    Undetermined,
    SameAs(Ty),
}

impl Ty {
    fn contains(&self, other: &Ty) -> bool {
        if Rc::ptr_eq(&self.inner, &other.inner) {
            return true;
        }
        match &*self.inner.borrow() {
            TyInner::Constructor(_) => false,
            TyInner::Parameter(_) => false,
            TyInner::Application {
                constructor,
                arguments,
            } => {
                constructor.contains(other)
                    || arguments.iter().any(|argument| argument.contains(other))
            }
            TyInner::List(elements) => elements.iter().any(|element| element.contains(other)),
            TyInner::Undetermined => false,
            TyInner::SameAs(this) => this.contains(other),
        }
    }
}

struct Unifications {
    history: Vec<(Ty, Ty)>,
}

impl Unifications {
    fn unify(&mut self, left: &Ty, right: &Ty) -> bool {
        let left_binding = left.inner.borrow();
        let right_binding = right.inner.borrow();
        match (&*left_binding, &*right_binding) {
            (TyInner::SameAs(left), _) => {
                drop(right_binding);
                self.unify(left, right)
            }
            (_, TyInner::SameAs(right)) => {
                drop(left_binding);
                self.unify(left, right)
            }
            (TyInner::Undetermined, _) => {
                if right.contains(left) {
                    return false;
                }
                drop(left_binding);
                self.history.push((left.clone(), right.clone()));
                *left.inner.borrow_mut() = TyInner::SameAs(right.clone());
                true
            }
            (_, TyInner::Undetermined) => {
                if left.contains(right) {
                    return false;
                }
                drop(right_binding);
                self.history.push((right.clone(), left.clone()));
                *right.inner.borrow_mut() = TyInner::SameAs(left.clone());
                true
            }
            (TyInner::Constructor(left_constructor), TyInner::Constructor(right_constructor)) => {
                left_constructor == right_constructor
            }
            (TyInner::Parameter(left_index), TyInner::Parameter(right_index)) => {
                left_index == right_index
            }
            (
                TyInner::Application {
                    constructor: left_constructor,
                    arguments: left_arguments,
                },
                TyInner::Application {
                    constructor: right_constructor,
                    arguments: right_arguments,
                },
            ) => {
                left_arguments.len() == right_arguments.len()
                    && self.unify(left_constructor, right_constructor)
                    && left_arguments.iter().zip(right_arguments).all(
                        |(self_element, other_element)| self.unify(self_element, other_element),
                    )
            }
            (TyInner::List(self_elements), TyInner::List(other_elements)) => {
                self_elements.len() == other_elements.len()
                    && self_elements.iter().zip(other_elements).all(
                        |(self_element, other_element)| self.unify(self_element, other_element),
                    )
            }
            _ => false,
        }
    }
}

impl Ty {
    fn extract_function_ty(&self) -> (Option<Ty>, i32) {
        match *self.inner.borrow() {
            TyInner::Application {
                ref constructor,
                ref arguments,
            } => match *constructor.inner.borrow() {
                TyInner::Constructor(ir::TyConstructor::Function) => {
                    let (ty, depth) = arguments[0].extract_function_ty();
                    (ty, depth + 1)
                }
                _ => (None, 0),
            },
            TyInner::Parameter(_) => (None, 0),
            TyInner::Constructor(_) => todo!("Runtime error"),
            TyInner::List(_) => todo!("Runtime error"),
            TyInner::Undetermined => (Some(self.clone()), 0),
            TyInner::SameAs(ref this) => this.extract_function_ty(),
        }
    }
}

impl Unifications {
    fn undo(&self) {
        for (ty, _) in &self.history {
            *ty.inner.borrow_mut() = TyInner::Undetermined
        }
    }
    fn redo(&self) {
        for (left, right) in &self.history {
            *left.inner.borrow_mut() = TyInner::SameAs(right.clone())
        }
    }
}

impl ir::FunctionTy {
    fn build(&self) -> Ty {
        let ty_parameters: Vec<_> = (0..self.num_ty_parameters)
            .map(|_| Ty {
                inner: Rc::new(RefCell::new(TyInner::Undetermined)),
            })
            .collect();
        let return_ty = self.return_ty.build(&ty_parameters);
        let parameters_ty = Ty {
            inner: Rc::new(RefCell::new(TyInner::List(
                self.parameters_ty
                    .iter()
                    .map(|ty| ty.build(&ty_parameters))
                    .collect(),
            ))),
        };
        Ty {
            inner: Rc::new(RefCell::new(TyInner::Application {
                constructor: Ty {
                    inner: Rc::new(RefCell::new(TyInner::Constructor(
                        ir::TyConstructor::Function,
                    ))),
                },
                arguments: vec![return_ty, parameters_ty],
            })),
        }
    }
}

impl ir::Ty {
    fn build(&self, parameters: &[Ty]) -> Ty {
        match *self {
            ir::Ty::Constructor(ref constructor) => Ty {
                inner: Rc::new(RefCell::new(TyInner::Constructor(constructor.clone()))),
            },
            ir::Ty::Application {
                ref constructor,
                ref arguments,
            } => Ty {
                inner: Rc::new(RefCell::new(TyInner::Application {
                    constructor: constructor.build(parameters),
                    arguments: arguments.iter().map(|ty| ty.build(parameters)).collect(),
                })),
            },
            ir::Ty::Parameter(index) => parameters[index].clone(),
        }
    }
}

impl ir::Program {
    pub fn translate(self) {
        for definition in self.function_definitions {
            let variables_ty: Vec<_> = (0..definition.num_local_variables)
                .map(|_| Ty {
                    inner: Rc::new(RefCell::new(TyInner::Undetermined)),
                })
                .collect();
            let mut num_blocks = 0;
            translate_block(
                definition.body,
                &mut num_blocks,
                &variables_ty,
                &self.functions_ty,
            );
        }
    }
}

fn translate_block(
    block: ir::Block,
    num_blocks: &mut usize,
    variables_ty: &[Ty],
    functions_ty: &[ir::FunctionTy],
) {
    for statement in block.statements {
        translate_statement(statement, num_blocks, variables_ty, functions_ty);
    }
}

fn translate_statement(
    statement: ir::Statement,
    num_blocks: &mut usize,
    variables_ty: &[Ty],
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

fn get_function_ty(function: &ir::Function, functions_ty: &[ir::FunctionTy]) -> Ty {
    match *function {
        ir::Function::UserDefined(index) => functions_ty[index].build(),
        ir::Function::DeleteInteger => Ty {
            inner: Rc::new(RefCell::new(TyInner::Application {
                constructor: Ty {
                    inner: Rc::new(RefCell::new(TyInner::Constructor(
                        ir::TyConstructor::Function,
                    ))),
                },
                arguments: vec![
                    Ty {
                        inner: Rc::new(RefCell::new(TyInner::Constructor(
                            ir::TyConstructor::Integer,
                        ))),
                    },
                    Ty {
                        inner: Rc::new(RefCell::new(TyInner::Application {
                            constructor: Ty {
                                inner: Rc::new(RefCell::new(TyInner::Constructor(
                                    ir::TyConstructor::Tuple,
                                ))),
                            },
                            arguments: vec![Ty {
                                inner: Rc::new(RefCell::new(TyInner::List(Vec::new()))),
                            }],
                        })),
                    },
                ],
            })),
        },
        _ => todo!(),
    }
}

fn translate_expression(
    expr: ir::Expression,
    variables_ty: &[Ty],
    functions_ty: &[ir::FunctionTy],
) -> (Ty, Expression) {
    match expr {
        ir::Expression::Integer(value) => (
            Ty {
                inner: Rc::new(RefCell::new(TyInner::Constructor(
                    ir::TyConstructor::Integer,
                ))),
            },
            Expression::Integer(value),
        ),
        ir::Expression::Float(value) => (
            Ty {
                inner: Rc::new(RefCell::new(TyInner::Constructor(ir::TyConstructor::Float))),
            },
            Expression::Float(value),
        ),
        ir::Expression::String(value) => todo!(),
        ir::Expression::Function { candidates, calls } => {
            let calls: Vec<Vec<_>> = calls
                .into_iter()
                .map(|ir::Call { arguments }| {
                    arguments
                        .into_iter()
                        .map(|argument| translate_expression(argument, variables_ty, functions_ty))
                        .collect()
                })
                .collect();
            eprintln!("{}", candidates.len());
            let candidates: Vec<_> = candidates
                .iter()
                .filter_map(|candidate| {
                    let mut unifications = Unifications {
                        history: Vec::new(),
                    };
                    let ret_ty = calls.iter().try_fold(
                        get_function_ty(candidate, functions_ty),
                        |function_ty, arguments| {
                            let TyInner::Application {
                                ref constructor,
                                arguments: ref return_and_parameters_ty,
                            } = *function_ty.inner.borrow()
                            else {
                                return None;
                            };
                            let TyInner::Constructor(ir::TyConstructor::Function) =
                                *constructor.inner.borrow()
                            else {
                                return None;
                            };
                            let return_ty = &return_and_parameters_ty[0];
                            let TyInner::List(ref parameters_ty) =
                                *return_and_parameters_ty[1].inner.borrow()
                            else {
                                return None;
                            };
                            if arguments.len() != parameters_ty.len() {
                                return None;
                            }
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
                                    depths.insert(Some(argument_return_ty.clone()), None);
                                }
                                if let Some(ref parameter_return_ty) = parameter_return_ty {
                                    depths.insert(Some(parameter_return_ty.clone()), None);
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
                                    let Some(argument_depth) = depths[argument] else {
                                        continue;
                                    };
                                    let new_parameter_depth = argument_depth + diff;
                                    let parameter_depth = depths.get_mut(parameter).unwrap();
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
                                    let Some(argument_depth) = depths[argument_ty] else {
                                        return None;
                                    };
                                    let Some(parameter_depth) = depths[parameter_ty] else {
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
                                    .map(|_| Ty {
                                        inner: Rc::new(RefCell::new(TyInner::Undetermined)),
                                    })
                                    .collect();
                            for (((argument_ty, _), parameter_ty), num_extra_calls) in
                                arguments.iter().zip(parameters_ty).zip(nums_extra_calls)
                            {
                                if !unifications.unify(
                                    &extra_calls[..num_extra_calls].iter().fold(
                                        parameter_ty.clone(),
                                        |parameter_ty, extra_call| Ty {
                                            inner: Rc::new(RefCell::new(TyInner::Application {
                                                constructor: Ty {
                                                    inner: Rc::new(RefCell::new(
                                                        TyInner::Constructor(
                                                            ir::TyConstructor::Function,
                                                        ),
                                                    )),
                                                },
                                                arguments: vec![parameter_ty, extra_call.clone()],
                                            })),
                                        },
                                    ),
                                    argument_ty,
                                ) {
                                    return None;
                                }
                            }

                            Some(return_ty.clone())
                        },
                    );
                    unifications.undo();
                    ret_ty.map(|ret_ty| (ret_ty, unifications))
                })
                .collect();
            eprintln!("{} candidates found", candidates.len());
            match &candidates[..] {
                [(ty, unifications)] => {
                    unifications.redo();
                    (ty.clone(), Expression::Integer(0))
                }
                _ => todo!(),
            }
        }
        ir::Expression::Variable(storage, index) => {
            todo!();
        }
    }
}

enum Expression {
    Integer(i32),
    Float(f64),
    Variable(usize),
}
