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

/*!
 * Receives intermediate representation ([`ir`]) and executes it.
 */

use crate::{ir, ty};
use std::ffi::CString;
use std::ffi::{c_char, c_int};

pub fn translate(
    ir_program: ir::Program,
    ty_info: Vec<ty::TyInfo>,
) -> Result<unsafe extern "C" fn() -> u8, ()> {
    unsafe { initialize_jit() };
    let num_definitions = ir_program.function_definitions.len();
    for (function_index, (ir_function_definition, ty_info)) in ir_program
        .function_definitions
        .into_iter()
        .zip(ty_info)
        .enumerate()
    {
        let function_name = CString::new(format!("{}", function_index)).unwrap();
        unsafe {
            let function_type = get_function_type(false, get_integer_type(), 0);
            add_function(
                function_name.as_ptr(),
                function_type,
                ir_function_definition.blocks.len(),
            );
        }
        for (block_index, block) in ir_function_definition.blocks.into_iter().enumerate() {
            unsafe {
                set_insert_point(block_index);
            }
            match block.next {
                ir::Next::Return(expression) => {
                    let value = match expression {
                        ir::Expression::Integer(value) => unsafe { create_integer(value) },
                        _ => todo!(),
                    };
                    unsafe {
                        create_return(value);
                    }
                }
                _ => todo!(),
            }
        }
        if function_index == num_definitions - 1 {
            let pointer = unsafe { compile_function(function_name.as_ptr()) };
            return Ok(pointer);
        }
    }
    Err(())
}

#[repr(C)]
struct Value {
    _private: [u8; 0],
}

#[repr(C)]
struct Type {
    _private: [u8; 0],
}

unsafe extern "C" {
    fn initialize_jit();
    fn add_function(function_name: *const c_char, function_type: *const Type, num_blocks: usize);
    fn set_insert_point(block_index: usize);
    fn create_integer(value: c_int) -> *mut Value;
    fn create_return(value: *mut Value);
    fn compile_function(function_name: *const c_char) -> unsafe extern "C" fn() -> u8;
    fn get_boolean_type() -> *const Type;
    fn get_integer_type() -> *const Type;
    fn get_size_type() -> *const Type;
    fn get_string_type() -> *const Type;
    fn get_function_type(
        is_variadic: bool,
        return_type: *const Type,
        num_parameters: usize,
        ...
    ) -> *const Type;
}
