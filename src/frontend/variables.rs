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

use super::{BlockBuilder, Context, Item};
use crate::ir;

pub struct Variables {
    storage: ir::Storage,
    num: usize,
    name_and_indices: Vec<(String, usize)>,
}

impl Variables {
    pub fn new(storage: ir::Storage) -> Variables {
        Variables {
            storage,
            num: 0,
            name_and_indices: Vec::new(),
        }
    }
    pub fn num_total(self) -> usize {
        self.num
    }
    pub fn num_alive(&self) -> usize {
        self.name_and_indices.len()
    }
    pub fn add(&mut self, name: String) -> Item {
        self.name_and_indices.push((name, self.num));
        let ret = Item::Variable(self.storage, self.num);
        self.num += 1;
        ret
    }
    pub fn free_and_remove(
        &mut self,
        len: usize,
        builder: &mut BlockBuilder,
        context: &mut Context,
    ) {
        self.free(len, builder);
        for (name, index) in self.name_and_indices.split_off(len) {
            match context.items.remove(&name).unwrap() {
                (_, Item::Variable(s, i)) => {
                    assert_eq!(s, self.storage);
                    assert_eq!(i, index);
                }
                _ => unreachable!(),
            }
        }
    }
    pub fn free(&mut self, len: usize, builder: &mut BlockBuilder) {
        for &(_, index) in self.name_and_indices[len..].iter().rev() {
            let expr = ir::Expression::Variable(self.storage, index);
            builder.add_expression(ir::Expression::Function {
                candidates: vec![ir::Function::DeleteInteger],
                calls: vec![ir::Call {
                    arguments: vec![expr],
                }],
            });
        }
    }
}
