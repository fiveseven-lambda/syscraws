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

use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub struct Definitions {
    pub tys_kind: HashMap<TyConstructor, TyKind>,
    pub structures: Vec<Structure>,
    pub functions: Vec<(FunctionTy, FunctionDefinition)>,
    pub num_global_variables: usize,
}

impl Definitions {
    pub fn builtin() -> Definitions {
        Definitions {
            tys_kind: HashMap::from([
                (TyConstructor::Integer, TyKind::Ty),
                (TyConstructor::Float, TyKind::Ty),
                (
                    TyConstructor::Reference,
                    TyKind::Abstraction {
                        parameters: TyListKind::Cons(
                            Box::new(TyKind::Ty),
                            Box::new(TyListKind::Nil),
                        ),
                        ret: Box::new(TyKind::Ty),
                    },
                ),
                (
                    TyConstructor::Tuple,
                    TyKind::Abstraction {
                        parameters: TyListKind::Rest,
                        ret: Box::new(TyKind::Ty),
                    },
                ),
                (
                    TyConstructor::Function,
                    TyKind::Abstraction {
                        parameters: TyListKind::Cons(
                            Box::new(TyKind::Ty),
                            Box::new(TyListKind::Rest),
                        ),
                        ret: Box::new(TyKind::Ty),
                    },
                ),
            ]),
            structures: Vec::new(),
            functions: Vec::new(),
            num_global_variables: 0,
        }
    }
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

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Function {
    IAdd,
    Deref,
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

enum TyInner {
    Constructor(TyConstructor),
    Parameter(usize),
    Application { constructor: Ty, arguments: Ty },
    Nil,
    Cons(Ty, Ty),
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
            TyInner::Nil => false,
            TyInner::Cons(head, tail) => head.contains(other) || tail.contains(other),
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
            (TyInner::Nil, TyInner::Nil) => true,
            (TyInner::Cons(self_head, self_tail), TyInner::Cons(other_head, other_tail)) => {
                self_head.unify(other_head, history) && self_tail.unify(other_tail, history)
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
            _ => false,
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
    While(Expression, Block),
}

pub enum Expression {
    GlobalVariable(usize),
    LocalVariable(usize),
    Function {
        candidates: Vec<Function>,
        calls: Vec<Call>,
    },
}

fn translate_function() {}

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
        let mut arguments = Ty {
            inner: Rc::new(RefCell::new(TyInner::Nil)),
        };
        for ty in self.parameters_ty.iter().rev() {
            arguments = Ty {
                inner: Rc::new(RefCell::new(TyInner::Cons(
                    ty.build(&ty_parameters),
                    arguments,
                ))),
            };
        }
        let arguments = Ty {
            inner: Rc::new(RefCell::new(TyInner::Cons(
                self.return_ty.build(&ty_parameters),
                arguments,
            ))),
        };
        Ty {
            inner: Rc::new(RefCell::new(TyInner::Application {
                constructor: Ty {
                    inner: Rc::new(RefCell::new(TyInner::Constructor(TyConstructor::Function))),
                },
                arguments,
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
                    arguments: arguments.iter().rev().fold(
                        Ty {
                            inner: Rc::new(RefCell::new(TyInner::Nil)),
                        },
                        |tail, head| Ty {
                            inner: Rc::new(RefCell::new(TyInner::Cons(
                                head.build(parameters),
                                tail,
                            ))),
                        },
                    ),
                })),
            },
            TyBuilder::Parameter(index) => parameters[index].clone(),
        }
    }
}

fn get_function_ty(
    function: &Function,
    function_definition: &[(FunctionTy, FunctionDefinition)],
) -> Ty {
    match *function {
        Function::UserDefined(index) => function_definition[index].0.build(),
        _ => todo!(),
    }
}

fn get_ty(expression: &Expression, function_definition: &[(FunctionTy, FunctionDefinition)]) {
    match expression {
        Expression::Function { candidates, calls } => {
            for candidate in candidates {
                let ty = get_function_ty(candidate, function_definition);
                for call in calls {
                    match *ty.inner.borrow() {
                        TyInner::Application {
                            ref constructor,
                            ref arguments,
                        } => match *constructor.inner.borrow() {
                            TyInner::Constructor(TyConstructor::Function) => {}
                            _ => todo!(),
                        },
                        _ => todo!(),
                    }
                }
            }
        }
        _ => todo!(),
    }
}
