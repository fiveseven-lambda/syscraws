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

//! pre AST から AST への変換を行う．

mod error;
use error::Error;
mod context;
use crate::{ast, pre_ast};
use context::Context;
pub use error::eprint_errors;

pub fn into_ast(stmts: Vec<pre_ast::PStmt>) -> Result<ast::Program, Vec<Error>> {
    let mut ctx = Context::new();
    let mut main = ast::Block::new();
    let mut scope = Vec::new();
    for stmt in stmts {
        ctx.add_toplevel_stmt(&mut main, stmt, &mut scope);
    }
    ctx.drop_scope(scope, &mut main);
    ctx.program.defs.push(main);
    if ctx.errors.is_empty() {
        Ok(ctx.program)
    } else {
        Err(ctx.errors)
    }
}
