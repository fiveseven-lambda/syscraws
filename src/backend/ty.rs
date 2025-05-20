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

use std::cell::RefCell;
use std::rc::Rc;

use crate::ir;

pub enum Ty {
    Constructor(ir::TyConstructor),
    Parameter(usize),
    Application {
        constructor: Rc<Ty>,
        arguments: Vec<Rc<Ty>>,
    },
    List(Vec<Rc<Ty>>),
    Var(Rc<RefCell<Var>>),
}

#[derive(Clone)]
pub enum Var {
    Assigned(Rc<Ty>),
    Unassigned(u32),
}

struct Unification {
    var: Rc<RefCell<Var>>,
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
                    && left_arguments.iter().zip(right_arguments).all(
                        |(left_argument, right_argument)| self.unify(left_argument, right_argument),
                    )
            }
            (Ty::List(self_elements), Ty::List(other_elements)) => {
                self_elements.len() == other_elements.len()
                    && self_elements.iter().zip(other_elements).all(
                        |(self_element, other_element)| self.unify(self_element, other_element),
                    )
            }
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
                            self.bind(right_var, Var::Assigned(left.clone()));
                        }
                        std::cmp::Ordering::Less => {
                            self.bind(left_var, Var::Assigned(right.clone()));
                        }
                        std::cmp::Ordering::Equal => {
                            self.bind(left_var, Var::Unassigned(left_rank + 1));
                            self.bind(right_var, Var::Assigned(left.clone()));
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
                    self.bind(left_var, Var::Assigned(right.clone()));
                    true
                }
            }
            (_, Ty::Var(right_var)) => {
                if let Var::Assigned(ref right) = *right_var.borrow() {
                    self.unify(left, right)
                } else if left.contains(right_var) {
                    false
                } else {
                    self.bind(right_var, Var::Assigned(left.clone()));
                    true
                }
            }
            _ => false,
        }
    }

    fn bind(&mut self, left: &Rc<RefCell<Var>>, right: Var) {
        self.0.push(Unification {
            var: left.clone(),
            old: left.borrow().clone(),
        });
        *left.borrow_mut() = right;
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
            } => {
                constructor.contains(var) && arguments.iter().any(|argument| argument.contains(var))
            }
            Ty::List(elements) => elements.iter().any(|element| element.contains(var)),
            Ty::Var(self_var) => match *self_var.borrow() {
                Var::Assigned(ref ty) => ty.contains(var),
                Var::Unassigned(_) => self_var.as_ptr() == var.as_ptr(),
            },
        }
    }

    pub fn extract_function_ty(self: &Rc<Ty>) -> (Option<Rc<RefCell<Var>>>, i32) {
        match self.as_ref() {
            Ty::Application {
                constructor,
                arguments,
            } => match constructor.as_ref() {
                Ty::Constructor(ir::TyConstructor::Function) => {
                    let (ty, depth) = arguments[0].extract_function_ty();
                    (ty, depth + 1)
                }
                _ => (None, 0),
            },
            Ty::Parameter(_) => (None, 0),
            Ty::Constructor(_) => todo!("Runtime error"),
            Ty::List(_) => todo!("Runtime error"),
            Ty::Var(var) => match *var.borrow() {
                Var::Assigned(ref ty) => ty.extract_function_ty(),
                Var::Unassigned(_) => (Some(var.clone()), 0),
            },
        }
    }
}
