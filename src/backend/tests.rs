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
fn unify() {
    let x = Rc::new(ty::Ty::Var(RefCell::new(ty::Var::Unassigned(0))));
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
