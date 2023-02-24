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

use std::io::Read;

fn main() {
    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input).unwrap();
    let tokens = lexer::tokenize(&input).unwrap();
    let pre_ast = parser::parse(&input, &tokens).unwrap();
    let mut errors = Vec::new();
    let Ok(ast) = pre_ast.into_ast(&mut errors) else {
        for error in errors {
            println!("{error:?}");
        }
        return;
    };
    ast.debug_print(0);
}
