/*
 * Copyright (c) 2023-2026 Atsushi Komaba
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

use serde::Serialize;

#[derive(Serialize)]
pub struct Program {
    pub structure_tys: Vec<Constant>,
    pub structure_definitions: Vec<StructureDefinition>,
    pub function_tys: Vec<Constant>,
    pub function_definitions: Vec<FunctionDefinition>,
    pub num_global_variables: usize,
}

#[derive(PartialEq, Eq, Clone, Serialize)]
pub enum Constant {
    Integer,
    Float,
    Ty,
    Structure(usize),
    Parameter(usize, usize),
    Product(Vec<Constant>, Box<Constant>),
    FunctionTy,
    Application(Box<Constant>, Vec<Constant>),
    Identity,
    Delete,
    Dereference,
    ConcatenateString,
    Function(usize),
}

#[derive(Serialize)]
pub struct StructureDefinition {
    pub field_tys: Vec<Constant>,
}

#[derive(Serialize)]
pub struct FunctionUse {
    pub candidates: Vec<Constant>,
    pub used_by: Option<usize>,
}

#[derive(Serialize)]
pub struct FunctionDefinition {
    pub num_local_variables: usize,
    pub function_uses: Vec<FunctionUse>,
    pub calls: Vec<Call>,
    pub blocks: Vec<Block>,
}

#[derive(Serialize)]
pub struct Block {
    pub call_bound: usize,
    pub next: Next,
}

#[derive(Serialize)]
pub enum Next {
    Jump(usize),
    Branch(Expression, usize, usize),
    Return(Expression),
}

#[derive(Serialize)]
pub enum Expression {
    Integer(i32),
    Float(f64),
    String(String),
    Variable(Storage, usize),
    FunctionUse(usize),
    Call(usize),
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Serialize)]
pub enum Storage {
    Global,
    Local,
}

#[derive(Serialize)]
pub struct Call {
    pub function: Expression,
    pub arguments: Vec<Expression>,
    pub used_by: Option<usize>,
}
