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

use std::collections::HashMap;

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
                        parameters: vec![TyKind::Ty],
                        ret: Box::new(TyKind::Ty),
                    },
                ),
                (
                    TyConstructor::Tuple,
                    TyKind::Abstraction {
                        parameters: vec![TyKind::List(Box::new(TyKind::Ty))],
                        ret: Box::new(TyKind::Ty),
                    },
                ),
                (
                    TyConstructor::Function,
                    TyKind::Abstraction {
                        parameters: vec![TyKind::List(Box::new(TyKind::Ty)), TyKind::Ty],
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
    pub fields_ty: Vec<Ty>,
}

pub struct FunctionTy {
    pub num_ty_parameters: usize,
    pub parameters_ty: Vec<Ty>,
    pub return_ty: Ty,
}

#[derive(PartialEq, Eq, Hash)]
pub enum Function {
    IAdd,
    Deref,
    UserDefined(usize),
}

pub struct FunctionDefinition {
    pub num_local_variables: usize,
    pub body: Vec<Statement>,
}

#[derive(Clone)]
pub enum Ty {
    Constructor(TyConstructor),
    Parameter(usize),
    Application {
        constructor: Box<Ty>,
        arguments: Vec<Ty>,
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

#[derive(Clone)]
pub enum TyKind {
    Ty,
    List(Box<TyKind>),
    Abstraction {
        parameters: Vec<TyKind>,
        ret: Box<TyKind>,
    },
}

pub enum Statement {
    Empty,
    Expr(Expression),
    While(Expression, Vec<Statement>),
}

#[derive(Clone)]
pub enum Expression {
    GlobalVariable(usize),
    LocalVariable(usize),
    Function {
        candidates: Vec<usize>,
        calls: Vec<Call>,
    },
}

#[derive(Clone)]
pub struct Call {
    pub arguments: Vec<Expression>,
}
