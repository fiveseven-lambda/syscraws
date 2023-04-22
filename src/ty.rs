/*
 * Copyright (c) 2023 Atsushi Komaba
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

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum Kind {
    Integer,
    Float,
    Boolean,
    String,
    Reference,
    Tuple,
    Function,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Ty {
    pub kind: Kind,
    pub args: Vec<Ty>,
}

#[macro_export]
macro_rules! ty {
    ($kind:ident) => { ty!($kind,) };
    ($kind:ident, $($args:expr),*) => {
        ty::Ty {
            kind: ty::Kind::$kind,
            args: vec![$($args),*]
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Var(usize),
    App { kind: Kind, args: Vec<Expr> },
}

#[macro_export]
macro_rules! expr {
    ($id:literal) => { ty::Expr::Var($id) };
    ($kind:ident) => { expr!($kind,) };
    ($kind:ident, $($args:expr),*) => {
        ty::Expr::App {
            kind: ty::Kind::$kind,
            args: vec![$($args),*]
        }
    }
}

impl Expr {
    pub fn subst(&self, vars: &[Option<Ty>]) -> Ty {
        match *self {
            Expr::Var(id) => vars[id].clone().unwrap(),
            Expr::App { kind, ref args } => Ty {
                kind,
                args: args.iter().map(|expr| expr.subst(vars)).collect(),
            },
        }
    }
    pub fn identify(&self, dest: &Ty, vars: &mut Vec<Option<Ty>>) -> bool {
        match *self {
            Expr::Var(id) => match vars[id] {
                Some(ref ty) => ty == dest,
                None => {
                    vars[id] = Some(dest.clone());
                    true
                }
            },
            Expr::App { kind, ref args } => {
                kind == dest.kind
                    && args.len() == dest.args.len()
                    && args
                        .iter()
                        .zip(&dest.args)
                        .all(|(expr, ty)| expr.identify(ty, vars))
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Func {
    pub num_vars: usize,
    pub args: Vec<Expr>,
    pub ret: Expr,
}
