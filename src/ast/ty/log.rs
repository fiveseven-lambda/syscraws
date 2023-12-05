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

use super::{Node, Var};
use std::cell::Cell;

pub struct Operation {
    pub child: Var,
    pub old_child_size: u32,
    pub old_parent_size: Option<u32>,
}

pub trait Log {
    fn append(&mut self, _: Operation);
}

impl Log for () {
    fn append(&mut self, _: Operation) {}
}
pub struct History(Vec<Operation>);
impl Log for History {
    fn append(&mut self, operation: Operation) {
        self.0.push(operation)
    }
}
impl History {
    pub fn new() -> History {
        History(Vec::new())
    }
    pub fn rollback(self) {
        for Operation {
            child: Var { node },
            old_child_size,
            old_parent_size,
        } in self.0.into_iter().rev()
        {
            match *node.borrow() {
                Node::SameAs(Var { node: ref parent }) => match old_parent_size {
                    Some(old_parent_size) => match *parent.borrow() {
                        Node::Undetermined {
                            size: ref parent_size,
                        } => parent_size.set(old_parent_size),
                        _ => panic!("rollback error"),
                    },
                    None => assert!(
                        matches!(*parent.borrow(), Node::Determined(_)),
                        "rollback error"
                    ),
                },
                _ => panic!("rollback error"),
            }
            *node.borrow_mut() = Node::Undetermined {
                size: Cell::new(old_child_size),
            };
        }
    }
}
