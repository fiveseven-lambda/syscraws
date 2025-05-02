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

fn test(dir: impl AsRef<Path>) {
    let dir = dir.as_ref();
    let mut logger = log::Logger::new(Box::new(std::io::stderr()));
    let definitions = read_input(&dir.join("input"), &mut logger).unwrap();
    let definitions = serde_json::to_value(&definitions).unwrap();
    let expected = std::fs::read_to_string(dir.join("expected.json")).unwrap();
    let expected: serde_json::Value = serde_json::from_str(&expected).unwrap();
    assert_json_diff::assert_json_eq!(definitions, expected);
}

#[test]
fn test_struct() {
    test("tests/frontend/struct");
}

#[test]
fn test_if_else() {
    test("tests/frontend/if_else");
}

#[test]
fn test_variables() {
    test("tests/frontend/variables");
}

#[test]
fn test_operator_overload() {
    test("tests/frontend/operator_overload");
}

#[test]
fn test_break() {
    test("tests/frontend/break");
}

#[test]
fn test_print() {
    test("tests/frontend/print");
}
