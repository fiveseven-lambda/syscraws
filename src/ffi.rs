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

use std::ffi::{c_char, c_int};

#[repr(C)]
pub struct Context {
    _private: [u8; 0],
}

#[repr(C)]
pub struct Type {
    _private: [u8; 0],
}

#[repr(C)]
pub struct Expression {
    _private: [u8; 0],
}

unsafe extern "C" {
    pub fn initialize_jit();
    pub fn create_context() -> *mut Context;
    pub fn add_function(
        context: *mut Context,
        function_name: *const c_char,
        function_type: *const Type,
        num_blocks: usize,
    );
    pub fn set_insert_point(context: *mut Context, block_index: usize);
    pub fn add_expression(context: *mut Context, expression: *const Expression);
    pub fn add_return(context: *mut Context, expression: *const Expression);
    pub fn compile_function(
        context: *mut Context,
        function_name: *const c_char,
    ) -> unsafe extern "C" fn() -> u8;
    pub fn delete_context(context: *mut Context);
    pub fn get_boolean_type() -> *const Type;
    pub fn get_integer_type() -> *const Type;
    pub fn get_size_type() -> *const Type;
    pub fn get_string_type() -> *const Type;
    pub fn get_function_type(
        is_variadic: bool,
        return_type: *const Type,
        num_parameters: usize,
        ...
    ) -> *const Type;
    pub fn create_parameter(index: usize) -> *const Expression;
    pub fn create_integer(value: c_int) -> *const Expression;
    pub fn create_size(value: usize) -> *const Expression;
    pub fn create_app(value: usize) -> *const Expression;
}
