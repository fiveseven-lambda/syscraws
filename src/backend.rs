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

pub struct Definitions {
    pub structures: Vec<Structure>,
    pub functions: Vec<(FunctionTy, Function)>,
    pub function_definitions: Vec<FunctionDefinition>,
    pub num_global_variables: usize,
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
    Application {
        constructor: TyConstructor,
        arguments: Vec<Ty>,
    },
    Parameter(usize),
}

#[derive(Clone)]
pub enum TyConstructor {
    Integer,
    Float,
    Reference,
    UserDefined(usize),
}

pub enum Statement {
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
