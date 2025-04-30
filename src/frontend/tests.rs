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

#![cfg(test)]

use super::*;

#[test]
fn test() {
    let mut logger = log::Logger::new(Box::new(std::io::stderr()));

    for &dir_name in &[
        "struct",
        "if_else",
        "variables",
        "operator_overload",
        "break",
        "print",
    ] {
        let dir = Path::new("tests/frontend").join(dir_name);
        eprintln!("{}", dir.display());
        let definitions = read_input(&dir.join("input"), &mut logger).unwrap();
        let output = format!("{definitions:?}");
        let expected = std::fs::read_to_string(dir.join("expected.txt")).unwrap();
        if output != expected {
            panic!(
                "Failed: {}.\n\nOutput:\n{output}\nExpected:\n{expected}",
                dir.display()
            )
        }
    }
}
