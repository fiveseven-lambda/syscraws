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

/*!
 * Defines [`Variables`] to manage variables currently alive.
 */

use super::{BlockBuilder, Context, Item};
use crate::ir;

/**
 * A list of variables currently alive in a given storage class (global or local).
 * There is one instance for global variables and one per function for local variables.
 *
 * - The [`add`](Variables::add) method registers a new variable name,
 *   assigning it a unique index by incrementing `num`.
 *   This index is used for identification and is never reused,
 *   even if earlier variables go out of scope.
 * - The [`free_and_remove`](Variables::free_and_remove) method is called when a block ends.
 *   It removes variable names from the [`Context`]
 *   and calls the associated delete function in reverse order.
 * - Variable lookup by name is not performed via this structure;
 *   use [`Context`] for name resolution.
 */
pub struct Variables {
    /**
     * The storage class of the variables (global or local).
     */
    storage: ir::Storage,
    /**
     * The next available index for uniquely identifying a new variable.
     */
    num: usize,
    /**
     * A list of variable names and their assigned unique indices,
     * representing variables that are currently alive.
     */
    name_and_indices: Vec<(String, usize)>,
}

impl Variables {
    /**
     * Creates a new `Variables` instance with the given storage class.
     */
    pub fn new(storage: ir::Storage) -> Variables {
        Variables {
            storage,
            num: 0,
            name_and_indices: Vec::new(),
        }
    }
    /**
     * Returns the total number of variables ever added to this instance.
     *
     * This count increases monotonically and is never decremented,
     * ensuring that each variable receives a unique index.
     */
    pub fn num_total(self) -> usize {
        self.num
    }
    /**
     * Returns the number of currently alive variables.
     *
     * Call this at the beginning of a block,
     * and pass the result to [`free_and_remove`](Variables::free_and_remove)
     * when the block ends to clean up variables declared inside the block.
     */
    pub fn num_alive(&self) -> usize {
        self.name_and_indices.len()
    }
    /**
     * Adds a new variable with the given name.
     */
    pub fn add(&mut self, name: String) -> Item {
        self.name_and_indices.push((name, self.num));
        let ret = Item::Variable(self.storage, self.num);
        self.num += 1;
        ret
    }
    /**
     * Frees and removes all variables declared after a given base index.
     *
     * Call this at the end of a block. `len` should be the number
     * returned by [`num_alive`](Variables::num_alive) at the start of the block.
     */
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
    /**
     * Frees all variables declared after a given base index by calling delete functions.
     *
     * This does not modify the context or variable list.
     * It is used when control flow exits a block early,
     * such as in a `break` or `continue` statement.
     */
    pub fn free(&mut self, len: usize, builder: &mut BlockBuilder) {
        for &(_, index) in self.name_and_indices[len..].iter().rev() {
            let expr = ir::Expression::Variable(self.storage, index);
            builder.add_expression(ir::Expression::Function {
                candidates: vec![ir::Function::Delete],
                calls: vec![ir::Call {
                    arguments: vec![expr],
                }],
            });
        }
    }
}
