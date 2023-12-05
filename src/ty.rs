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

use std::{
    cell::{Cell, RefCell},
    rc::Rc,
};

mod debug_print;
pub mod log;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Kind {
    Integer,
    Float,
    Boolean,
    String,
    Reference,
    Tuple,
    Function,
}

pub struct Ty {
    pub kind: Kind,
    pub args: Vec<Var>,
}

#[derive(Clone)]
pub struct Var {
    node: Rc<RefCell<Node>>,
}

enum Node {
    Determined(Ty),
    Undetermined { size: Cell<u32> },
    SameAs(Var),
}

impl Ty {
    fn contains(&self, target: &Var) -> bool {
        self.args.iter().any(|arg| arg.contains(target))
    }
}

impl Var {
    pub fn new() -> Var {
        Var {
            node: Rc::new(RefCell::new(Node::Undetermined { size: Cell::new(1) })),
        }
    }
    pub fn ty(kind: Kind, args: Vec<Var>) -> Var {
        Var {
            node: Rc::new(RefCell::new(Node::Determined(Ty { kind, args }))),
        }
    }
    fn contains(&self, target: &Var) -> bool {
        match *self.node.borrow() {
            Node::Determined(ref ty) => ty.contains(target),
            Node::Undetermined { .. } => Rc::ptr_eq(&self.node, &target.node),
            Node::SameAs(ref parent) => parent.contains(target),
        }
    }
    pub fn unify(&self, other: &Var, log: &mut impl log::Log) -> bool {
        let self_binding = self.node.borrow();
        let other_binding = other.node.borrow();
        match (&*self_binding, &*other_binding) {
            (Node::SameAs(parent), _) => {
                drop(other_binding);
                parent.unify(other, log)
            }
            (Node::Determined(self_ty), Node::Determined(other_ty)) => {
                self_ty.kind == other_ty.kind
                    && self_ty.args.len() == other_ty.args.len()
                    && self_ty
                        .args
                        .iter()
                        .zip(&other_ty.args)
                        .all(|(self_arg, other_arg)| self_arg.unify(other_arg, log))
            }
            (Node::Undetermined { size }, Node::Determined(ty)) => {
                if ty.contains(self) {
                    return false;
                }
                log.append(log::Operation {
                    child: self.clone(),
                    old_child_size: size.get(),
                    old_parent_size: None,
                });
                drop(self_binding);
                *self.node.borrow_mut() = Node::SameAs(other.clone());
                true
            }
            (Node::Undetermined { size: self_size }, Node::Undetermined { size: other_size })
                if self_size <= other_size =>
            {
                if Rc::ptr_eq(&self.node, &other.node) {
                    return true;
                }
                let new_size = self_size.get() + other_size.get();
                log.append(log::Operation {
                    child: self.clone(),
                    old_child_size: other_size.get(),
                    old_parent_size: Some(self_size.get()),
                });
                other_size.set(new_size);
                drop(self_binding);
                *self.node.borrow_mut() = Node::SameAs(other.clone());
                true
            }
            _ => {
                drop(self_binding);
                drop(other_binding);
                other.unify(self, log)
            }
        }
    }
}
