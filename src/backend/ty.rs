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

use serde::ser::{Serialize, SerializeMap, SerializeStructVariant, Serializer};
use std::cell::RefCell;
use std::ptr;
use std::rc::Rc;

use crate::ir;

pub enum Ty {
    Constructor(ir::TyConstructor),
    Parameter(usize),
    Application {
        constructor: Rc<Ty>,
        arguments: Rc<Ty>,
    },
    Nil,
    Cons {
        head: Rc<Ty>,
        tail: Rc<Ty>,
    },
    Var(RefCell<Var>),
}

impl Serialize for Ty {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        match self {
            Ty::Constructor(constructor) => {
                serializer.serialize_newtype_variant("Ty", 0, "Constructor", constructor)
            }
            Ty::Parameter(index) => {
                serializer.serialize_newtype_variant("Ty", 1, "Parameter", index)
            }
            Ty::Application {
                constructor,
                arguments,
            } => {
                let mut state = serializer.serialize_struct_variant("Ty", 2, "Application", 2)?;
                state.serialize_field("constructor", constructor)?;
                state.serialize_field("arguments", arguments)?;
                state.end()
            }
            Ty::Nil => serializer.serialize_unit_variant("Ty", 3, "Nil"),
            Ty::Cons { head, tail } => {
                let mut state = serializer.serialize_struct_variant("Ty", 4, "Cons", 2)?;
                state.serialize_field("head", head)?;
                state.serialize_field("tail", tail)?;
                state.end()
            }
            Ty::Var(var) => var.serialize(serializer),
        }
    }
}

#[derive(Clone)]
pub enum Var {
    Assigned(Rc<Ty>),
    Unassigned(u32),
}

impl Serialize for Var {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        match self {
            Var::Assigned(ty) => ty.serialize(serializer),
            Var::Unassigned(rank) => {
                let mut map = serializer.serialize_map(Some(1))?;
                map.serialize_entry("Unassigned", rank)?;
                map.end()
            }
        }
    }
}
impl Ty {
    pub fn contains(self: &Rc<Ty>, var: &RefCell<Var>) -> bool {
        match self.as_ref() {
            Ty::Constructor(_) => false,
            Ty::Parameter(_) => false,
            Ty::Application {
                constructor,
                arguments,
            } => constructor.contains(var) || arguments.contains(var),
            Ty::Nil => false,
            Ty::Cons { head, tail } => head.contains(var) || tail.contains(var),
            Ty::Var(self_var) => match *self_var.borrow() {
                Var::Assigned(ref ty) => ty.contains(var),
                Var::Unassigned(_) => ptr::eq(self_var, var),
            },
        }
    }

    pub fn extract_function_ty(self: &Rc<Ty>) -> (Option<*const Var>, i32) {
        match self.as_ref() {
            Ty::Application {
                constructor,
                arguments,
            } => match constructor.as_ref() {
                Ty::Constructor(ir::TyConstructor::Function) => match arguments.as_ref() {
                    Ty::Cons { head, tail: _ } => {
                        let (ty, depth) = head.extract_function_ty();
                        (ty, depth + 1)
                    }
                    _ => panic!(),
                },
                _ => (None, 0),
            },
            Ty::Var(var) => match *var.borrow() {
                Var::Assigned(ref ty) => ty.extract_function_ty(),
                Var::Unassigned(_) => (Some(var.as_ptr()), 0),
            },
            _ => (None, 0),
        }
    }
}
