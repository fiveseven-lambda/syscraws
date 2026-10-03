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

use std::{ptr, rc::Rc};

use super::ty;

struct Var {
    ty: Rc<ty::Ty>,
    old_rank: u32,
}

pub struct Unifier(Vec<Var>);

impl Unifier {
    pub fn new() -> Unifier {
        Unifier(Vec::new())
    }

    pub fn unify(&mut self, left: &Rc<ty::Ty>, right: &Rc<ty::Ty>) -> bool {
        match (left.as_ref(), right.as_ref()) {
            (ty::Ty::Constructor(left_constructor), ty::Ty::Constructor(right_constructor)) => {
                left_constructor == right_constructor
            }
            (ty::Ty::Parameter(left_index), ty::Ty::Parameter(right_index)) => {
                left_index == right_index
            }
            (
                ty::Ty::Application {
                    constructor: left_constructor,
                    arguments: left_arguments,
                },
                ty::Ty::Application {
                    constructor: right_constructor,
                    arguments: right_arguments,
                },
            ) => {
                self.unify(left_constructor, right_constructor)
                    && self.unify(left_arguments, right_arguments)
            }
            (ty::Ty::Nil, ty::Ty::Nil) => true,
            (
                ty::Ty::Cons {
                    head: left_head,
                    tail: left_tail,
                },
                ty::Ty::Cons {
                    head: right_head,
                    tail: right_tail,
                },
            ) => self.unify(left_head, right_head) && self.unify(left_tail, right_tail),
            (ty::Ty::Var(left_var), ty::Ty::Var(right_var)) => {
                let left_rank = match *left_var.borrow() {
                    ty::Var::Assigned(ref left) => return self.unify(left, right),
                    ty::Var::Unassigned(left_rank) => left_rank,
                };
                let right_rank = match *right_var.borrow() {
                    ty::Var::Assigned(ref right) => return self.unify(left, right),
                    ty::Var::Unassigned(right_rank) => right_rank,
                };
                if ptr::eq(left_var, right_var) {
                    return true;
                }
                match left_rank.cmp(&right_rank) {
                    std::cmp::Ordering::Greater => {
                        self.0.push(Var {
                            ty: right.clone(),
                            old_rank: right_rank,
                        });
                        *right_var.borrow_mut() = ty::Var::Assigned(left.clone());
                    }
                    std::cmp::Ordering::Less => {
                        self.0.push(Var {
                            ty: left.clone(),
                            old_rank: left_rank,
                        });
                        *left_var.borrow_mut() = ty::Var::Assigned(right.clone());
                    }
                    std::cmp::Ordering::Equal => {
                        self.0.push(Var {
                            ty: left.clone(),
                            old_rank: left_rank,
                        });
                        *left_var.borrow_mut() = ty::Var::Unassigned(left_rank + 1);
                        self.0.push(Var {
                            ty: right.clone(),
                            old_rank: right_rank,
                        });
                        *right_var.borrow_mut() = ty::Var::Assigned(left.clone());
                    }
                }
                true
            }
            (ty::Ty::Var(left_var), _) => {
                let left_rank = match *left_var.borrow() {
                    ty::Var::Assigned(ref left) => return self.unify(left, right),
                    ty::Var::Unassigned(left_rank) => left_rank,
                };
                if right.contains(left_var) {
                    return false;
                }
                self.0.push(Var {
                    ty: left.clone(),
                    old_rank: left_rank,
                });
                *left_var.borrow_mut() = ty::Var::Assigned(right.clone());
                true
            }
            (_, ty::Ty::Var(right_var)) => {
                let right_rank = match *right_var.borrow() {
                    ty::Var::Assigned(ref right) => return self.unify(left, right),
                    ty::Var::Unassigned(right_rank) => right_rank,
                };
                if left.contains(right_var) {
                    return false;
                }
                self.0.push(Var {
                    ty: right.clone(),
                    old_rank: right_rank,
                });
                *right_var.borrow_mut() = ty::Var::Assigned(left.clone());
                true
            }
            _ => false,
        }
    }

    pub fn rollback(self) -> impl Iterator<Item = Substitution> {
        self.0.into_iter().rev().map(|Var { ty, old_rank }| {
            if let ty::Ty::Var(ref var) = *ty {
                let var = var.replace(ty::Var::Unassigned(old_rank));
                Substitution(ty, var)
            } else {
                unreachable!();
            }
        })
    }
}

pub struct Substitution(Rc<ty::Ty>, ty::Var);

impl Substitution {
    pub fn commit(self) {
        if let ty::Ty::Var(ref var) = *self.0 {
            *var.borrow_mut() = self.1;
        } else {
            unreachable!();
        }
    }
}
