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

mod ast;
mod lines;
mod parser;
mod pre_ast;
mod range;

use std::io::Read;

fn main() {
    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input).unwrap();
    let stmts = match parser::parse(&input) {
        Ok(stmts) => stmts,
        Err(error) => return error.eprint(&input),
    };
    let (functions_def, variables_ty) = match pre_ast::translate::translate(stmts) {
        Ok(res) => res,
        Err(errors) => return pre_ast::translate::eprint_errors(&errors, &input),
    };
    for (i, ty) in variables_ty.iter().enumerate() {
        eprintln!("v{i}: {ty:?}")
    }
    for (i, defs) in functions_def.iter().enumerate() {
        for (j, def) in defs.iter().enumerate() {
            eprintln!("[f{i}:{j}]");
            def._debug_print();
        }
    }
}
