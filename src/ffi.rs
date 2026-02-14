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

use std::ffi::{c_char, c_int};

#[repr(C)]
pub struct Value {
    _private: [u8; 0],
}

#[repr(C)]
pub struct Type {
    _private: [u8; 0],
}

unsafe extern "C" {
    pub fn initialize_jit();
    pub fn add_function(
        function_name: *const c_char,
        function_type: *const Type,
        num_blocks: usize,
    );
    pub fn set_insert_point(block_index: usize);
    pub fn create_integer(value: c_int) -> *mut Value;
    pub fn create_return(value: *mut Value);
    pub fn compile_function(function_name: *const c_char) -> unsafe extern "C" fn() -> u8;
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
}
