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

mod backend;
mod frontend;
mod ir;
mod log;

use std::process::ExitCode;

use clap::Parser;

#[derive(Parser)]
struct CommandLineArguments {
    filename: String,
}

fn main() -> ExitCode {
    let command_line_arguments = CommandLineArguments::parse();
    let mut logger = log::Logger::new(Box::new(std::io::stderr()));
    let Ok(ir_program) =
        frontend::read_input(command_line_arguments.filename.as_ref(), &mut logger)
    else {
        return ExitCode::FAILURE;
    };
    let Ok(entry) = backend::translate(ir_program) else {
        return ExitCode::FAILURE;
    };
    ExitCode::from(unsafe { entry() })
}
