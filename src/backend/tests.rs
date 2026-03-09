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
fn unify_tuples() {
    let mut unifications = ty::Unifications::new();
    let t1 = Rc::new(ty::Ty::Var(RefCell::new(ty::Var::Unassigned(0))));
    let u1 = Rc::new(ty::Ty::Var(RefCell::new(ty::Var::Unassigned(0))));
    {
        assert!(unifications.unify(
            &t1,
            &Rc::new(ty::Ty::Application {
                constructor: Rc::new(ty::Ty::Constructor(ir::TyConstructor::Tuple)),
                arguments: Rc::new(ty::Ty::Cons {
                    head: Rc::new(ty::Ty::Constructor(ir::TyConstructor::Integer)),
                    tail: Rc::new(ty::Ty::Cons {
                        head: u1.clone(),
                        tail: Rc::new(ty::Ty::Nil),
                    }),
                }),
            }),
        ));
        assert!(!unifications.unify(&t1, &u1));
    };
    let t2 = Rc::new(ty::Ty::Var(RefCell::new(ty::Var::Unassigned(0))));
    let u2 = Rc::new(ty::Ty::Var(RefCell::new(ty::Var::Unassigned(0))));
    {
        assert!(unifications.unify(
            &t2,
            &Rc::new(ty::Ty::Application {
                constructor: Rc::new(ty::Ty::Constructor(ir::TyConstructor::Tuple)),
                arguments: Rc::new(ty::Ty::Cons {
                    head: u2.clone(),
                    tail: Rc::new(ty::Ty::Cons {
                        head: Rc::new(ty::Ty::Constructor(ir::TyConstructor::Float)),
                        tail: Rc::new(ty::Ty::Nil),
                    }),
                }),
            }),
        ));
        assert!(!unifications.unify(&u2, &t2));
    };
    assert!(unifications.unify(&t1, &t2));
    assert!(!unifications.unify(
        &u1,
        &Rc::new(ty::Ty::Constructor(ir::TyConstructor::Integer))
    ));
    assert!(unifications.unify(&u1, &Rc::new(ty::Ty::Constructor(ir::TyConstructor::Float))));
    assert!(!unifications.unify(&Rc::new(ty::Ty::Constructor(ir::TyConstructor::Float)), &u2));
    assert!(unifications.unify(
        &Rc::new(ty::Ty::Constructor(ir::TyConstructor::Integer)),
        &u2,
    ));
}

#[test]
fn unify_vars() {
    let mut unifications = ty::Unifications::new();

    let x1 = Rc::new(ty::Ty::Var(RefCell::new(ty::Var::Unassigned(0))));
    assert!(unifications.unify(&x1, &x1));
    let x2 = Rc::new(ty::Ty::Var(RefCell::new(ty::Var::Unassigned(0))));
    assert!(unifications.unify(&x1, &x2));
    let x3 = Rc::new(ty::Ty::Var(RefCell::new(ty::Var::Unassigned(0))));
    assert!(unifications.unify(&x3, &x1));
    let x4 = Rc::new(ty::Ty::Var(RefCell::new(ty::Var::Unassigned(0))));
    assert!(unifications.unify(&x2, &x4));

    let y = Rc::new(ty::Ty::Var(RefCell::new(ty::Var::Unassigned(0))));
    assert!(unifications.unify(
        &y,
        &Rc::new(ty::Ty::Constructor(ir::TyConstructor::Integer))
    ));
    assert!(unifications.unify(&x3, &y));
    assert!(!unifications.unify(&x4, &Rc::new(ty::Ty::Constructor(ir::TyConstructor::Float))));
    assert!(unifications.unify(
        &x4,
        &Rc::new(ty::Ty::Constructor(ir::TyConstructor::Integer))
    ));
}

#[test]
fn unify_undo() {
    let mut unifications = ty::Unifications::new();
    let x = Rc::new(ty::Ty::Var(RefCell::new(ty::Var::Unassigned(0))));
    let assert_x_rank = |expected: u32| {
        if let ty::Ty::Var(ref var) = *x {
            if let ty::Var::Unassigned(rank) = *var.borrow() {
                assert_eq!(rank, expected);
                return;
            }
        }
        panic!();
    };
    assert_x_rank(0);
    assert!(unifications.unify(
        &x,
        &Rc::new(ty::Ty::Var(RefCell::new(ty::Var::Unassigned(0)))),
    ));
    let y = Rc::new(ty::Ty::Var(RefCell::new(ty::Var::Unassigned(0))));
    assert!(unifications.unify(
        &y,
        &Rc::new(ty::Ty::Var(RefCell::new(ty::Var::Unassigned(0)))),
    ));
    assert!(unifications.unify(&x, &y));
    assert_x_rank(2);
    unifications.undo();
    assert_x_rank(0);
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
