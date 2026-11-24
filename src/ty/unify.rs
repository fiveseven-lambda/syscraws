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

use super::{Ty, Var};

struct Record {
    ty: Rc<Ty>,
    old_rank: u32,
}

pub struct Unifier(Vec<Record>);

impl Unifier {
    pub fn new() -> Unifier {
        Unifier(Vec::new())
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
                if ptr::eq(left_var, right_var) {
                    return true;
                }
                match left_rank.cmp(&right_rank) {
                    std::cmp::Ordering::Greater => {
                        self.0.push(Record {
                            ty: right.clone(),
                            old_rank: right_rank,
                        });
                        *right_var.borrow_mut() = Var::Assigned(left.clone());
                    }
                    std::cmp::Ordering::Less => {
                        self.0.push(Record {
                            ty: left.clone(),
                            old_rank: left_rank,
                        });
                        *left_var.borrow_mut() = Var::Assigned(right.clone());
                    }
                    std::cmp::Ordering::Equal => {
                        self.0.push(Record {
                            ty: left.clone(),
                            old_rank: left_rank,
                        });
                        *left_var.borrow_mut() = Var::Unassigned(left_rank + 1);
                        self.0.push(Record {
                            ty: right.clone(),
                            old_rank: right_rank,
                        });
                        *right_var.borrow_mut() = Var::Assigned(left.clone());
                    }
                }
                true
            }
            (Ty::Var(left_var), _) => {
                let left_rank = match *left_var.borrow() {
                    Var::Assigned(ref left) => return self.unify(left, right),
                    Var::Unassigned(left_rank) => left_rank,
                };
                if right.contains(left_var) {
                    return false;
                }
                self.0.push(Record {
                    ty: left.clone(),
                    old_rank: left_rank,
                });
                *left_var.borrow_mut() = Var::Assigned(right.clone());
                true
            }
            (_, Ty::Var(right_var)) => {
                let right_rank = match *right_var.borrow() {
                    Var::Assigned(ref right) => return self.unify(left, right),
                    Var::Unassigned(right_rank) => right_rank,
                };
                if left.contains(right_var) {
                    return false;
                }
                self.0.push(Record {
                    ty: right.clone(),
                    old_rank: right_rank,
                });
                *right_var.borrow_mut() = Var::Assigned(left.clone());
                true
            }
            _ => false,
        }
    }

    pub fn rollback(self) -> impl Iterator<Item = Substitution> {
        self.0.into_iter().rev().map(|Record { ty, old_rank }| {
            if let Ty::Var(ref var) = *ty {
                let var = var.replace(Var::Unassigned(old_rank));
                Substitution(ty, var)
            } else {
                unreachable!();
            }
        })
    }
}

pub struct Substitution(Rc<Ty>, Var);

impl Substitution {
    pub fn commit(self) {
        if let Ty::Var(ref var) = *self.0 {
            *var.borrow_mut() = self.1;
        } else {
            unreachable!();
        }
    }
}
