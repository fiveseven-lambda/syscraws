/*
 * Copyright (c) 2023 Atsushi Komaba
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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Kind {
    Integer,
    Float,
    Boolean,
    String,
    Reference,
    Function,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Ty {
    pub kind: Kind,
    pub args: Vec<Ty>,
}

impl Ty {
    pub fn integer() -> Ty {
        Ty {
            kind: Kind::Integer,
            args: vec![],
        }
    }
    pub fn float() -> Ty {
        Ty {
            kind: Kind::Float,
            args: vec![],
        }
    }
    pub fn boolean() -> Ty {
        Ty {
            kind: Kind::Boolean,
            args: vec![],
        }
    }
    pub fn string() -> Ty {
        Ty {
            kind: Kind::String,
            args: vec![],
        }
    }
    pub fn reference(arg: Ty) -> Ty {
        Ty {
            kind: Kind::Reference,
            args: vec![arg],
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Func {
    pub args: Vec<Ty>,
    pub ret: Ty,
}
