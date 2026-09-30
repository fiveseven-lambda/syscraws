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
}

#[derive(PartialEq, Eq, Clone, Serialize)]
pub enum Constant {
    Ty,
    Structure(usize),
    Parameter(usize, usize),
    Product(Vec<Constant>, Box<Constant>),
    Application(Box<Constant>, Vec<Constant>),
}

#[derive(Serialize)]
pub struct StructureDefinition {
    pub field_tys: Vec<Constant>,
}
