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

use std::path::Path;

use crate::{frontend, log};

use super::*;

#[test]
fn unify() {
    let x = Rc::new(ty::Ty::Var(Rc::new(RefCell::new(ty::Var::Unassigned(0)))));
    let mut u1 = ty::Unifications::new();
    assert!(u1.unify(
        &x,
        &Rc::new(ty::Ty::Constructor(ir::TyConstructor::Integer))
    ));
    assert!(!u1.unify(&x, &Rc::new(ty::Ty::Constructor(ir::TyConstructor::Float))));
    let u1 = u1.undo();
    let mut u2 = ty::Unifications::new();
    assert!(u2.unify(&x, &Rc::new(ty::Ty::Constructor(ir::TyConstructor::Float))));
    assert!(!u2.unify(
        &x,
        &Rc::new(ty::Ty::Constructor(ir::TyConstructor::Integer))
    ));
    u2.undo();
    eprintln!("{}", serde_json::to_string(&x).unwrap());
    u1.undo();
    eprintln!("{}", serde_json::to_string(&x).unwrap());
    let mut u3 = ty::Unifications::new();
    assert!(!u3.unify(&x, &Rc::new(ty::Ty::Constructor(ir::TyConstructor::Float))));
    assert!(u3.unify(
        &x,
        &Rc::new(ty::Ty::Constructor(ir::TyConstructor::Integer))
    ));
}

fn test(dir: impl AsRef<Path>) {
    let dir = dir.as_ref();
    let mut logger = log::Logger::new(Box::new(std::io::stderr()));
    let ir_program = frontend::read_input(&dir.join("input"), &mut logger).unwrap();
    let mut program = Program {
        function_definitions: Vec::new(),
    };
    let global_variables_ty: Vec<_> = (0..ir_program.num_global_variables)
        .map(|_| Rc::new(ty::Ty::Var(Rc::new(RefCell::new(ty::Var::Unassigned(0))))))
        .collect();
    for definition in ir_program.function_definitions {
        let mut body_rev = Vec::new();

        let local_variables_ty: Vec<_> = (0..definition.num_local_variables)
            .map(|_| Rc::new(ty::Ty::Var(Rc::new(RefCell::new(ty::Var::Unassigned(0))))))
            .collect();

        translate_block(
            &definition.body,
            &mut body_rev,
            None,
            &local_variables_ty,
            &global_variables_ty,
            &ir_program.functions_ty,
        );

        program
            .function_definitions
            .push(FunctionDefinition { body_rev });
    }
    let program = serde_json::to_value(&program).unwrap();
    let expected = std::fs::read_to_string(dir.join("expected.json")).unwrap();
    let expected: serde_json::Value = serde_json::from_str(&expected).unwrap();
    assert_json_diff::assert_json_eq!(program, expected);
}

#[test]
fn control_flow() {
    test("tests/backend/control-flow")
}

#[test]
fn add_integer() {
    test("tests/backend/assign-integer")
}
