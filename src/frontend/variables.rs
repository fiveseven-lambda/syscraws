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

use crate::backend;

use super::{context::Context, Item};

pub struct Variables {
    is_local: bool,
    num: usize,
    name_and_indices: Vec<(String, usize)>,
}

impl Variables {
    pub fn new(is_local: bool) -> Variables {
        Variables {
            is_local,
            num: 0,
            name_and_indices: Vec::new(),
        }
    }
    pub fn num(&self) -> usize {
        self.num
    }
    pub fn len(&self) -> usize {
        self.name_and_indices.len()
    }
    pub fn add(&mut self, name: String) -> Item {
        self.name_and_indices.push((name, self.num));
        let ret = if self.is_local {
            Item::LocalVariable(self.num)
        } else {
            Item::GlobalVariable(self.num)
        };
        self.num += 1;
        ret
    }
    pub fn truncate(
        &mut self,
        len: usize,
        builder: &mut backend::BlockBuilder,
        context: &mut Context,
    ) {
        self.free(len, builder);
        for (name, index) in self.name_and_indices.split_off(len) {
            match context.items.remove(&name).unwrap() {
                (_, Item::GlobalVariable(i)) => {
                    assert!(!self.is_local);
                    assert_eq!(index, i);
                }
                (_, Item::LocalVariable(i)) => {
                    assert!(self.is_local);
                    assert_eq!(index, i);
                }
                _ => unreachable!(),
            }
        }
    }
    pub fn free(&mut self, len: usize, builder: &mut backend::BlockBuilder) {
        for &(_, index) in self.name_and_indices[len..].iter().rev() {
            let expr = if self.is_local {
                backend::Expression::LocalVariable(index)
            } else {
                backend::Expression::GlobalVariable(index)
            };
            builder.add_expression(backend::Expression::Function {
                candidates: vec![backend::Function::DeleteInteger],
                calls: vec![backend::Call {
                    arguments: vec![expr],
                }],
            });
        }
    }
}
