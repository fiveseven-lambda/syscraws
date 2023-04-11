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

#![feature(char_indices_offset)]
#![feature(is_some_and)]

mod ast;
mod ir;
mod lexer;
mod parser;
mod pre_ast;
mod range;
mod token;
mod ty;

use std::io::Read;

fn main() {
    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input).unwrap();
    let tokens = lexer::tokenize(&input).unwrap();
    token::debug_print(&input, &tokens);
    let pre_ast = parser::parse(&input, &tokens).unwrap();
    pre_ast::debug_print(&pre_ast);
    let (overloads, defs) = match pre_ast::into_ast(pre_ast) {
        Ok(ast) => ast,
        Err(errors) => {
            for error in errors {
                eprintln!("{:?}", error);
            }
            return;
        }
    };
    for (i, funcs) in overloads.iter().enumerate() {
        println!("{i}: {funcs:?}");
    }
    for (i, def) in defs.iter().enumerate() {
        println!("#[{i}]");
        def.debug_print();
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
