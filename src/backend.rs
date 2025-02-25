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

struct TyList {
    inner: Rc<RefCell<TyListInner>>,
}

enum TyListInner {
    Nil,
    Cons(Ty, TyList),
    Undetermined,
    SameAs(TyList),
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
