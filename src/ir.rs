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
 * Defines the intermediate representation shared between
 * [`frontend`](crate::frontend) and [`backend`](crate::backend).
 */

#[cfg(test)]
use serde::Serialize;

#[cfg_attr(test, derive(Serialize))]
pub struct Program {
    pub structures: Vec<(TyKind, Structure)>,
    pub function_tys: Vec<FunctionTy>,
    pub function_definitions: Vec<FunctionDefinition>,
    pub num_global_variables: usize,
}

#[cfg_attr(test, derive(Serialize))]
pub struct Structure {
    pub num_ty_parameters: usize,
    pub field_tys: Vec<Ty>,
}

#[derive(Clone)]
#[cfg_attr(test, derive(Serialize))]
pub struct FunctionTy {
    pub num_ty_parameters: usize,
    pub parameter_tys: Vec<Ty>,
    pub return_ty: Ty,
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(test, derive(Serialize))]
pub enum Function {
    AddInteger,
    IntegerToString,
    Dereference,
    Identity,
    Assign,
    Delete,
    ConcatenateString,
    Print,
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

#[cfg_attr(test, derive(Serialize))]
pub struct FunctionDefinition {
    pub num_local_variables: usize,
    pub body: Block,
    pub overloads: Vec<Vec<Function>>,
}

#[derive(Clone)]
#[cfg_attr(test, derive(Serialize))]
pub enum Ty {
    Constructor(TyConstructor),
    Parameter(usize),
    Application {
        constructor: Box<Ty>,
        arguments: Vec<Ty>,
    },
}

#[derive(Clone, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(Serialize))]
pub enum TyConstructor {
    Integer,
    Float,
    String,
    Reference,
    Tuple,
    Function,
    Structure(usize),
}

#[cfg_attr(test, derive(Serialize))]
pub enum TyKind {
    Ty,
    Abstraction {
        parameters: TyListKind,
        ret: Box<TyKind>,
    },
}

#[cfg_attr(test, derive(Serialize))]
pub enum TyListKind {
    Nil,
    Cons(Box<TyKind>, Box<TyListKind>),
    Rest,
}

#[cfg_attr(test, derive(Serialize))]
pub struct Block {
    pub statements: Vec<Statement>,
    pub size: usize,
}

#[cfg_attr(test, derive(Serialize))]
pub enum Statement {
    Expressions(Vec<Expression>),
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
    Return {
        antecedents: Vec<Expression>,
        value: Expression,
    },
}

#[cfg_attr(test, derive(Serialize))]
pub enum Expression {
    Integer(i32),
    Float(f64),
    String(String),
    Variable(Storage, usize),
    Function {
        overload_index: usize,
        calls: Vec<Call>,
    },
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[cfg_attr(test, derive(Serialize))]
pub enum Storage {
    Global,
    Local,
}

#[cfg_attr(test, derive(Serialize))]
pub struct Call {
    pub arguments: Vec<Expression>,
}
