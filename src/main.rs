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
mod ir;
mod lines;
mod parser;
mod pre_ast;
mod range;
mod ty;

use std::io::Read;

fn main() {
    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input).unwrap();
    let pre_ast = match parser::parse(&input) {
        Ok(pre_ast) => pre_ast,
        Err(error) => return error.eprint(&input),
    };
    pre_ast::_debug_print(&pre_ast);
    let (overloads, defs) = match pre_ast::into_ast(pre_ast) {
        Ok(ast) => ast,
        Err(errors) => return pre_ast::eprint_errors(&errors, &input),
    };
    for (i, funcs) in overloads.iter().enumerate() {
        println!("{i}: {funcs:?}");
    }
    for (i, def) in defs.iter().enumerate() {
        println!("#[{i}]");
        def._debug_print();
    }
    let funcs_ty: Vec<_> = defs.iter().map(ast::FuncDef::get_ty).collect();
    let funcs: Vec<_> = defs
        .into_iter()
        .map(|def| def.translate(&overloads, &funcs_ty))
        .collect();
    let mut memory = ir::Memory::new();
    unsafe {
        funcs.last().unwrap().run(vec![], &mut memory, &funcs);
    }
}
