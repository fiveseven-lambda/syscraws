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

#[test]
fn orders() {
    let inequalities = vec![(0, 0, 1), (0, 0, 2)];
    assert!(get_orders(&inequalities, 1, 0).is_none());
    assert!(get_orders(&inequalities, 1, 1).is_none());
    assert_eq!(get_orders(&inequalities, 1, 2).unwrap(), vec![1, 2]);

    let inequalities = vec![(0, 1, 2)];
    assert_eq!(get_orders(&inequalities, 2, 0).unwrap(), vec![0]);
    assert_eq!(get_orders(&inequalities, 2, 1).unwrap(), vec![0]);
    assert_eq!(get_orders(&inequalities, 2, 2).unwrap(), vec![0]);

    let inequalities = vec![(1, 0, -2)];
    assert_eq!(get_orders(&inequalities, 2, 0).unwrap(), vec![0]);
    assert_eq!(get_orders(&inequalities, 2, 1).unwrap(), vec![1]);
    assert_eq!(get_orders(&inequalities, 2, 2).unwrap(), vec![2]);

    /*
     * Input:
     * |                    arg | param |
     * | ---------------------- | ----- |
     * |             (..)(..)A1 |    T1 |
     * | (..)(..)(..)(..)(..)T1 |    T2 |
     * |                     T2 |    A2 |
     *
     * Output:
     * |                         arg |           param |
     * | --------------------------- | --------------- |
     * |                 (..)(..)A1  |         (..)T1' |
     * | (..)(..)(..)(..)(..)(..)T1' | (..)(..)(..)T2' |
     * |             (..)(..)(..)T2' |             A2  |
     */
    let inequalities = vec![(0, 1, 2), (1, 2, 5), (2, 0, 0)];
    assert!(get_orders(&inequalities, 3, 0).is_none());
    assert!(get_orders(&inequalities, 3, 1).is_none());
    assert!(get_orders(&inequalities, 3, 2).is_none());
    assert_eq!(get_orders(&inequalities, 3, 3).unwrap(), vec![1, 3, 3]);

    let inequalities = vec![(1, 1, 0)];
    assert!(get_orders(&inequalities, 2, 0).is_none());
}
