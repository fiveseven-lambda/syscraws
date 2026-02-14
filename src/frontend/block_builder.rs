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

use crate::ir::{Block, Expression, Statement};

pub struct BlockBuilder {
    block: Block,
    expressions: Vec<Expression>,
}

impl BlockBuilder {
    pub fn new() -> BlockBuilder {
        BlockBuilder {
            block: Block {
                statements: Vec::new(),
                size: 0,
            },
            expressions: Vec::new(),
        }
    }
    pub fn add_expression(&mut self, expression: Expression) {
        self.expressions.push(expression);
    }
    pub fn add_if_statement(
        &mut self,
        condition: Expression,
        then_block: Block,
        else_block: Block,
    ) {
        let antecedents = std::mem::take(&mut self.expressions);
        self.block.size += then_block.size + else_block.size + 1;
        self.block.statements.push(Statement::If {
            antecedents,
            condition,
            then_block,
            else_block,
        });
    }
    pub fn add_while_statement(&mut self, condition: Expression, do_block: Block) {
        if !self.expressions.is_empty() {
            let expressions = std::mem::take(&mut self.expressions);
            self.block
                .statements
                .push(Statement::Expressions(expressions));
            self.block.size += 1;
        }
        self.block.size += do_block.size + 1;
        self.block.statements.push(Statement::While {
            condition,
            do_block,
        });
    }
    pub fn add_break(&mut self) {
        let antecedents = std::mem::take(&mut self.expressions);
        self.block.size += 1;
        self.block.statements.push(Statement::Break(antecedents));
    }
    pub fn add_continue(&mut self) {
        let antecedents = std::mem::take(&mut self.expressions);
        self.block.size += 1;
        self.block.statements.push(Statement::Continue(antecedents));
    }
    pub fn add_return(&mut self, value: Expression) {
        let antecedents = std::mem::take(&mut self.expressions);
        self.block.size += 1;
        self.block
            .statements
            .push(Statement::Return { antecedents, value });
    }
    pub fn finish(mut self) -> Block {
        if !self.expressions.is_empty() {
            self.block
                .statements
                .push(Statement::Expressions(self.expressions));
            self.block.size += 1;
        }
        self.block
    }
}
