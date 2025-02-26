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
    pub functions_ty: HashMap<Function, FunctionTy>,
    pub functions: Vec<FunctionDefinition>,
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
            functions_ty: HashMap::from([]),
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
    pub body: Vec<Statement>,
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
    Application { constructor: Ty, arguments: TyList },
    Undetermined,
    SameAs(Ty),
}

#[derive(Clone)]
struct TyList {
    inner: Rc<RefCell<TyListInner>>,
}

enum TyListInner {
    Nil,
    Cons(Ty, TyList),
    Undetermined,
    SameAs(TyList),
}

enum TyOrTyList {
    Ty(Ty),
    TyList(TyList),
}

trait Contains<T> {
    fn contains(&self, other: &T) -> bool;
}

impl Contains<Ty> for Ty {
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
            TyInner::Undetermined => false,
            TyInner::SameAs(this) => this.contains(other),
        }
    }
}
impl Contains<TyList> for Ty {
    fn contains(&self, other: &TyList) -> bool {
        match &*self.inner.borrow() {
            TyInner::Constructor(_) => false,
            TyInner::Parameter(_) => false,
            TyInner::Application {
                constructor,
                arguments,
            } => constructor.contains(other) || arguments.contains(other),
            TyInner::Undetermined => false,
            TyInner::SameAs(this) => this.contains(other),
        }
    }
}

impl Contains<Ty> for TyList {
    fn contains(&self, other: &Ty) -> bool {
        match &*self.inner.borrow() {
            TyListInner::Nil => false,
            TyListInner::Cons(head, tail) => head.contains(other) || tail.contains(other),
            TyListInner::Undetermined => false,
            TyListInner::SameAs(this) => this.contains(other),
        }
    }
}
impl Contains<TyList> for TyList {
    fn contains(&self, other: &TyList) -> bool {
        if Rc::ptr_eq(&self.inner, &other.inner) {
            return true;
        }
        match &*self.inner.borrow() {
            TyListInner::Nil => false,
            TyListInner::Cons(head, tail) => head.contains(other) || tail.contains(other),
            TyListInner::Undetermined => false,
            TyListInner::SameAs(this) => this.contains(other),
        }
    }
}

trait Unify {
    fn unify(&self, other: &Self, history: &mut Vec<TyOrTyList>) -> bool;
}

impl Unify for Ty {
    fn unify(&self, other: &Ty, history: &mut Vec<TyOrTyList>) -> bool {
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
                history.push(TyOrTyList::Ty(self.clone()));
                *self.inner.borrow_mut() = TyInner::SameAs(other.clone());
                true
            }
            (_, TyInner::Undetermined) => {
                if self.contains(other) {
                    return false;
                }
                drop(other_binding);
                history.push(TyOrTyList::Ty(other.clone()));
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
            _ => false,
        }
    }
}

impl Unify for TyList {
    fn unify(&self, other: &TyList, history: &mut Vec<TyOrTyList>) -> bool {
        let self_binding = self.inner.borrow();
        let other_binding = other.inner.borrow();
        match (&*self_binding, &*other_binding) {
            (TyListInner::SameAs(self_), _) => {
                drop(other_binding);
                self_.unify(other, history)
            }
            (_, TyListInner::SameAs(other_)) => {
                drop(self_binding);
                self.unify(other_, history)
            }
            (TyListInner::Undetermined, _) => {
                if other.contains(self) {
                    return false;
                }
                drop(self_binding);
                history.push(TyOrTyList::TyList(self.clone()));
                *self.inner.borrow_mut() = TyListInner::SameAs(other.clone());
                true
            }
            (_, TyListInner::Undetermined) => {
                if self.contains(other) {
                    return false;
                }
                drop(other_binding);
                history.push(TyOrTyList::TyList(other.clone()));
                *other.inner.borrow_mut() = TyListInner::SameAs(self.clone());
                true
            }
            (TyListInner::Nil, TyListInner::Nil) => true,
            (
                TyListInner::Cons(self_head, self_tail),
                TyListInner::Cons(other_head, other_tail),
            ) => self_head.unify(other_head, history) && self_tail.unify(other_tail, history),
            _ => todo!(),
        }
    }
}

fn rollback(history: &[TyOrTyList]) {
    for ty_or_ty_list in history {
        match ty_or_ty_list {
            TyOrTyList::Ty(ty) => *ty.inner.borrow_mut() = TyInner::Undetermined,
            TyOrTyList::TyList(ty_list) => *ty_list.inner.borrow_mut() = TyListInner::Undetermined,
        }
    }
}

pub enum Statement {
    Empty,
    Expr(Expression),
    While(Expression, Vec<Statement>),
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
