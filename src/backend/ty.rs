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

#[cfg(test)]
use serde::ser::{Serialize, SerializeMap, SerializeStructVariant, Serializer};
use std::cell::RefCell;
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

#[cfg(test)]
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

#[cfg(test)]
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

struct Unification {
    ty: Rc<Ty>,
    old: Var,
}

pub struct Unifications(Vec<Unification>);

impl Unifications {
    pub fn new() -> Unifications {
        Unifications(Vec::new())
    }

    pub fn unify(&mut self, left: &Rc<Ty>, right: &Rc<Ty>) -> bool {
        match (left.as_ref(), right.as_ref()) {
            (Ty::Constructor(left_constructor), Ty::Constructor(right_constructor)) => {
                left_constructor == right_constructor
            }
            (Ty::Parameter(left_index), Ty::Parameter(right_index)) => left_index == right_index,
            (
                Ty::Application {
                    constructor: left_constructor,
                    arguments: left_arguments,
                },
                Ty::Application {
                    constructor: right_constructor,
                    arguments: right_arguments,
                },
            ) => {
                self.unify(left_constructor, right_constructor)
                    && self.unify(left_arguments, right_arguments)
            }
            (Ty::Nil, Ty::Nil) => true,
            (
                Ty::Cons {
                    head: left_head,
                    tail: left_tail,
                },
                Ty::Cons {
                    head: right_head,
                    tail: right_tail,
                },
            ) => self.unify(left_head, right_head) && self.unify(left_tail, right_tail),
            (Ty::Var(left_var), Ty::Var(right_var)) => {
                let left_rank = match *left_var.borrow() {
                    Var::Assigned(ref left) => return self.unify(left, right),
                    Var::Unassigned(left_rank) => left_rank,
                };
                let right_rank = match *right_var.borrow() {
                    Var::Assigned(ref right) => return self.unify(left, right),
                    Var::Unassigned(right_rank) => right_rank,
                };
                if left_var.as_ptr() != right_var.as_ptr() {
                    match left_rank.cmp(&right_rank) {
                        std::cmp::Ordering::Greater => {
                            self.0.push(Unification {
                                ty: right.clone(),
                                old: right_var.borrow().clone(),
                            });
                            *right_var.borrow_mut() = Var::Assigned(left.clone());
                        }
                        std::cmp::Ordering::Less => {
                            self.0.push(Unification {
                                ty: left.clone(),
                                old: left_var.borrow().clone(),
                            });
                            *left_var.borrow_mut() = Var::Assigned(right.clone());
                        }
                        std::cmp::Ordering::Equal => {
                            self.0.push(Unification {
                                ty: left.clone(),
                                old: left_var.borrow().clone(),
                            });
                            *left_var.borrow_mut() = Var::Unassigned(left_rank + 1);
                            self.0.push(Unification {
                                ty: right.clone(),
                                old: right_var.borrow().clone(),
                            });
                            *right_var.borrow_mut() = Var::Assigned(left.clone());
                        }
                    }
                }
                true
            }
            (Ty::Var(left_var), _) => {
                if let Var::Assigned(ref left) = *left_var.borrow() {
                    self.unify(left, right)
                } else if right.contains(left_var) {
                    false
                } else {
                    self.0.push(Unification {
                        ty: left.clone(),
                        old: left_var.borrow().clone(),
                    });
                    *left_var.borrow_mut() = Var::Assigned(right.clone());
                    true
                }
            }
            (_, Ty::Var(right_var)) => {
                if let Var::Assigned(ref right) = *right_var.borrow() {
                    self.unify(left, right)
                } else if left.contains(right_var) {
                    false
                } else {
                    self.0.push(Unification {
                        ty: right.clone(),
                        old: right_var.borrow().clone(),
                    });
                    *right_var.borrow_mut() = Var::Assigned(left.clone());
                    true
                }
            }
            _ => false,
        }
    }

    pub fn undo(self) -> Unifications {
        Unifications(
            self.0
                .into_iter()
                .rev()
                .map(|Unification { ty, old }| {
                    let Ty::Var(ref var) = *ty else {
                        unreachable!()
                    };
                    let old = var.replace(old);
                    Unification { ty, old }
                })
                .collect(),
        )
    }
}

impl Ty {
    fn contains(self: &Rc<Ty>, var: &RefCell<Var>) -> bool {
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
                Var::Unassigned(_) => self_var.as_ptr() == var.as_ptr(),
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
