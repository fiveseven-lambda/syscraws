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

mod ir;
mod tests;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Definitions {
    pub structures: Vec<(TyKind, Structure)>,
    pub functions_ty: Vec<FunctionTy>,
    pub function_definitions: Vec<FunctionDefinition>,
    pub num_global_variables: usize,
}

pub struct Structure {
    pub num_ty_parameters: usize,
    pub fields_ty: Vec<TyBuilder>,
}

pub struct FunctionTy {
    pub num_ty_parameters: usize,
    pub parameters_ty: Vec<TyBuilder>,
    pub return_ty: TyBuilder,
}

#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Function {
    IAdd,
    Deref,
    Identity,
    IAssign,
    Delete,
    UserDefined(usize),
    Field {
        structure_index: usize,
        field_index: usize,
    },
    FieldRef {
        structure_index: usize,
        field_index: usize,
    },
}

pub struct FunctionDefinition {
    pub num_local_variables: usize,
    pub body: Block,
}

#[derive(Clone)]
pub enum TyBuilder {
    Constructor(TyConstructor),
    Parameter(usize),
    Application {
        constructor: Box<TyBuilder>,
        arguments: Vec<TyBuilder>,
    },
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum TyConstructor {
    Integer,
    Float,
    Reference,
    Tuple,
    Function,
    Structure(usize),
}

pub enum TyKind {
    Ty,
    Abstraction {
        parameters: TyListKind,
        ret: Box<TyKind>,
    },
}

pub enum TyListKind {
    Nil,
    Cons(Box<TyKind>, Box<TyListKind>),
    Rest,
}

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
    Constructor(TyConstructor),
    Parameter(usize),
    Application { constructor: Ty, arguments: Ty },
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
            } => constructor.contains(other) || arguments.contains(other),
            TyInner::List(elements) => elements.iter().any(|element| element.contains(other)),
            TyInner::Undetermined => false,
            TyInner::SameAs(this) => this.contains(other),
        }
    }

    fn unify(&self, other: &Ty, history: &mut Vec<Ty>) -> bool {
        let self_binding = self.inner.borrow();
        let other_binding = other.inner.borrow();
        match (&*self_binding, &*other_binding) {
            (TyInner::SameAs(self_), _) => {
                drop(other_binding);
                self_.unify(other, history)
            }
            (_, TyInner::SameAs(other_)) => {
                drop(self_binding);
                self.unify(other_, history)
            }
            (TyInner::Undetermined, _) => {
                if other.contains(self) {
                    return false;
                }
                drop(self_binding);
                history.push(self.clone());
                *self.inner.borrow_mut() = TyInner::SameAs(other.clone());
                true
            }
            (_, TyInner::Undetermined) => {
                if self.contains(other) {
                    return false;
                }
                drop(other_binding);
                history.push(other.clone());
                *other.inner.borrow_mut() = TyInner::SameAs(self.clone());
                true
            }
            (TyInner::Constructor(self_constructor), TyInner::Constructor(other_constructor)) => {
                self_constructor == other_constructor
            }
            (TyInner::Parameter(self_index), TyInner::Parameter(other_index)) => {
                self_index == other_index
            }
            (
                TyInner::Application {
                    constructor: self_constructor,
                    arguments: self_arguments,
                },
                TyInner::Application {
                    constructor: other_constructor,
                    arguments: other_arguments,
                },
            ) => {
                self_constructor.unify(other_constructor, history)
                    && self_arguments.unify(other_arguments, history)
            }
            (TyInner::List(self_elements), TyInner::List(other_elements)) => {
                self_elements.len() == other_elements.len()
                    && self_elements.iter().zip(other_elements).all(
                        |(self_element, other_element)| self_element.unify(other_element, history),
                    )
            }
            _ => false,
        }
    }

    fn extract_function_ty(&self) -> (Option<Ty>, i32) {
        match *self.inner.borrow() {
            TyInner::Application {
                ref constructor,
                ref arguments,
            } => match *constructor.inner.borrow() {
                TyInner::Constructor(TyConstructor::Function) => match *arguments.inner.borrow() {
                    TyInner::List(ref elements) => {
                        let mut ret = elements[0].extract_function_ty();
                        ret.1 += 1;
                        ret
                    }
                    _ => todo!("Error"),
                },
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

fn rollback(history: &[Ty]) {
    for ty in history {
        *ty.inner.borrow_mut() = TyInner::Undetermined
    }
}

pub struct Block {
    pub statements: Vec<Statement>,
    pub size: usize,
}

pub enum Statement {
    Expr(Vec<Expression>),
    If {
        antecedents: Vec<Expression>,
        condition: Expression,
        then_block: Block,
        else_block: Block,
    },
    While {
        condition: Expression,
        do_block: Block,
    },
    Break(Vec<Expression>),
    Continue(Vec<Expression>),
}

pub enum Expression {
    Integer(i32),
    Float(f64),
    Variable(LocalOrGlobal, usize),
    Function {
        candidates: Vec<Function>,
        calls: Vec<Call>,
    },
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum LocalOrGlobal {
    Local,
    Global,
}

pub struct Call {
    pub arguments: Vec<Expression>,
}

impl FunctionTy {
    fn build(&self) -> Ty {
        let ty_parameters: Vec<_> = (0..self.num_ty_parameters)
            .map(|_| Ty {
                inner: Rc::new(RefCell::new(TyInner::Undetermined)),
            })
            .collect();
        let return_and_parameters_ty = std::iter::once(&self.return_ty)
            .chain(&self.parameters_ty)
            .map(|ty| ty.build(&ty_parameters))
            .collect();
        Ty {
            inner: Rc::new(RefCell::new(TyInner::Application {
                constructor: Ty {
                    inner: Rc::new(RefCell::new(TyInner::Constructor(TyConstructor::Function))),
                },
                arguments: Ty {
                    inner: Rc::new(RefCell::new(TyInner::List(return_and_parameters_ty))),
                },
            })),
        }
    }
}

impl TyBuilder {
    fn build(&self, parameters: &[Ty]) -> Ty {
        match *self {
            TyBuilder::Constructor(ref constructor) => Ty {
                inner: Rc::new(RefCell::new(TyInner::Constructor(constructor.clone()))),
            },
            TyBuilder::Application {
                ref constructor,
                ref arguments,
            } => Ty {
                inner: Rc::new(RefCell::new(TyInner::Application {
                    constructor: constructor.build(parameters),
                    arguments: Ty {
                        inner: Rc::new(RefCell::new(TyInner::List(
                            arguments.iter().map(|ty| ty.build(parameters)).collect(),
                        ))),
                    },
                })),
            },
            TyBuilder::Parameter(index) => parameters[index].clone(),
        }
    }
}

impl Definitions {
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
    block: Block,
    num_blocks: &mut usize,
    variables_ty: &[Ty],
    functions_ty: &[FunctionTy],
) {
    for statement in block.statements {
        translate_statement(statement, num_blocks, variables_ty, functions_ty);
    }
}

fn translate_statement(
    statement: Statement,
    num_blocks: &mut usize,
    variables_ty: &[Ty],
    functions_ty: &[FunctionTy],
) {
    match statement {
        Statement::Expr(exprs) => {
            for expr in exprs {
                translate_expression(expr, variables_ty, functions_ty);
            }
            println!("{num_blocks}: Jump");
            *num_blocks += 1;
        }
        Statement::If {
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
        Statement::While {
            condition,
            do_block,
        } => {
            translate_expression(condition, variables_ty, functions_ty);
            println!("{num_blocks}: Br");
            *num_blocks += 1;
            translate_block(do_block, num_blocks, variables_ty, functions_ty);
        }
        Statement::Break(exprs) => {
            for expr in exprs {
                translate_expression(expr, variables_ty, functions_ty);
            }
            println!("{num_blocks}: break");
        }
        Statement::Continue(exprs) => {
            for expr in exprs {
                translate_expression(expr, variables_ty, functions_ty);
            }
            println!("{num_blocks}: continue");
        }
    }
}

fn get_function_ty(function: &Function, functions_ty: &[FunctionTy]) -> Ty {
    match *function {
        Function::UserDefined(index) => functions_ty[index].build(),
        Function::Delete => Ty {
            inner: Rc::new(RefCell::new(TyInner::Application {
                constructor: Ty {
                    inner: Rc::new(RefCell::new(TyInner::Constructor(TyConstructor::Function))),
                },
                arguments: Ty {
                    inner: Rc::new(RefCell::new(TyInner::List(vec![
                        Ty {
                            inner: Rc::new(RefCell::new(TyInner::Application {
                                constructor: Ty {
                                    inner: Rc::new(RefCell::new(TyInner::Constructor(
                                        TyConstructor::Tuple,
                                    ))),
                                },
                                arguments: Ty {
                                    inner: Rc::new(RefCell::new(TyInner::List(Vec::new()))),
                                },
                            })),
                        },
                        Ty {
                            inner: Rc::new(RefCell::new(TyInner::Undetermined)),
                        },
                    ]))),
                },
            })),
        },
        _ => todo!(),
    }
}

fn translate_expression(
    expr: Expression,
    variables_ty: &[Ty],
    functions_ty: &[FunctionTy],
) -> (Ty, ir::Expression) {
    match expr {
        Expression::Integer(value) => (
            Ty {
                inner: Rc::new(RefCell::new(TyInner::Constructor(TyConstructor::Integer))),
            },
            ir::Expression::Integer(value),
        ),
        Expression::Float(value) => (
            Ty {
                inner: Rc::new(RefCell::new(TyInner::Constructor(TyConstructor::Float))),
            },
            ir::Expression::Float(value),
        ),
        Expression::Function { candidates, calls } => {
            let calls: Vec<Vec<_>> = calls
                .into_iter()
                .map(|Call { arguments }| {
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
                    let mut history = Vec::new();
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
                            let TyInner::Constructor(TyConstructor::Function) =
                                *constructor.inner.borrow()
                            else {
                                return None;
                            };
                            let TyInner::List(ref return_and_parameters_ty) =
                                *return_and_parameters_ty.inner.borrow()
                            else {
                                return None;
                            };
                            let return_ty = &return_and_parameters_ty[0];
                            let parameters_ty = &return_and_parameters_ty[1..];
                            if arguments.len() != parameters_ty.len() {
                                return None;
                            }
                            let mut depths = HashMap::new();
                            let mut inequalities = Vec::new();
                            for ((argument_ty, _), parameter_ty) in
                                arguments.iter().zip(parameters_ty)
                            {
                                let (argument_return_ty, argument_depth) =
                                    argument_ty.extract_function_ty();
                                let (parameter_return_ty, parameter_depth) =
                                    parameter_ty.extract_function_ty();
                                if let Some(ref argument_return_ty) = argument_return_ty {
                                    depths.insert(argument_return_ty.clone(), None);
                                }
                                if let Some(ref parameter_return_ty) = parameter_return_ty {
                                    depths.insert(parameter_return_ty.clone(), None);
                                }
                                inequalities.push((
                                    argument_return_ty,
                                    parameter_return_ty,
                                    argument_depth - parameter_depth,
                                ));
                            }
                            for _ in 0..depths.len() {
                                for (argument, parameter, diff) in &inequalities {
                                    let argument_depth = if let Some(argument) = argument {
                                        if let Some(argument_depth) = depths[argument] {
                                            argument_depth
                                        } else {
                                            continue;
                                        }
                                    } else {
                                        0
                                    };
                                    let Some(parameter) = parameter else {
                                        continue;
                                    };
                                    if depths[parameter]
                                        .is_none_or(|depth| depth > argument_depth + diff)
                                    {
                                        depths
                                            .insert(parameter.clone(), Some(argument_depth + diff));
                                    }
                                }
                            }
                            // 得られたdepthに基づいてunify
                            Some(return_ty.clone())
                        },
                    );
                    rollback(&history);
                    ret_ty
                })
                .collect();
            eprintln!("{} candidates found", candidates.len());
            match &candidates[..] {
                [ty] => (ty.clone(), ir::Expression::Integer(0)),
                _ => todo!(),
            }
        }
        Expression::Variable(local_or_global, index) => (
            variables_ty[index].clone(),
            ir::Expression::Variable(local_or_global, index),
        ),
    }
}
