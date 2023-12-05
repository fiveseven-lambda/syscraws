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

use super::{Block, Expr, FuncTy, Program, Stmt, Ty};
use crate::ty;
use std::cell::OnceCell;

pub struct TypeChecker<'pr> {
    program: &'pr Program,
    overloads: Vec<(&'pr OnceCell<usize>, Vec<ty::Var>, ty::Var)>,
    calls: Vec<Vec<(ty::Var, ty::Var)>>,
    vars: Vec<ty::Var>,
}

impl FuncTy {
    fn get_ty(&self) -> ty::Var {
        let vars: Vec<_> = (0..self.num_vars).map(|_| ty::Var::new()).collect();
        let mut args: Vec<_> = self.args.iter().map(|ty| ty.get_ty(&vars)).collect();
        args.push(self.ret.get_ty(&vars));
        ty::Var::ty(ty::Kind::Function, args)
    }
}
impl Ty {
    fn get_ty(&self, vars: &[ty::Var]) -> ty::Var {
        match *self {
            Ty::Const { ref kind, ref args } => ty::Var::ty(
                kind.clone(),
                args.iter().map(|arg| arg.get_ty(vars)).collect(),
            ),
            Ty::Var(id) => vars[id].clone(),
        }
    }
}

impl<'pr> TypeChecker<'pr> {
    pub fn new(program: &Program) -> TypeChecker {
        TypeChecker {
            program,
            overloads: Vec::new(),
            calls: Vec::new(),
            vars: program
                .vars
                .iter()
                .map(|ty| match ty {
                    Some(ty) => ty.get_ty(&[]),
                    None => ty::Var::new(),
                })
                .collect(),
        }
    }
    pub fn run(mut self) {
        for def in &self.program.defs {
            self.collect_info(def);
        }
        for args in &self.calls {
            for (left, right) in args {
                left.unify(right, &mut ());
            }
        }
        loop {
            let mut resolved = Vec::new();
            for (eq_idx, &(ans, ref candidates, ref var)) in self.overloads.iter().enumerate() {
                if ans.get().is_some() {
                    continue;
                }
                let mut ok = Vec::new();
                for (candidate_idx, candidate) in candidates.iter().enumerate() {
                    let mut history = ty::log::History::new();
                    if var.unify(candidate, &mut history) {
                        ok.push(candidate_idx);
                    }
                    history.rollback();
                }
                if let [i] = ok[..] {
                    ans.set(i).unwrap();
                    var.unify(&candidates[i], &mut ());
                    resolved.push(eq_idx);
                }
            }
            if resolved.is_empty() {
                break;
            }
        }
        self._debug_print();
    }
    fn collect_info(&mut self, block: &'pr Block) {
        for stmt in &block.stmts {
            match stmt {
                Stmt::Expr(expr) => {
                    self.get_ty(expr);
                }
                _ => todo!(),
            }
        }
    }
    fn get_ty(&mut self, expr: &'pr Expr) -> ty::Var {
        match *expr {
            Expr::Variable(id) => ty::Var::ty(ty::Kind::Reference, vec![self.vars[id].clone()]),
            Expr::Integer(_) => ty::Var::ty(ty::Kind::Integer, vec![]),
            Expr::Float(_) => ty::Var::ty(ty::Kind::Float, vec![]),
            Expr::String(_) => ty::Var::ty(ty::Kind::String, vec![]),
            Expr::Func(i, ref j) => {
                let ret = ty::Var::new();
                let candidates = self.program.funcs[i]
                    .iter()
                    .map(|(ty, _)| ty.get_ty())
                    .collect();
                self.overloads.push((j, candidates, ret.clone()));
                ret
            }
            Expr::Call(ref func, ref args) => {
                let ret = ty::Var::new();
                let mut args_ty: Vec<_> = args.iter().map(|arg| self.get_ty(arg)).collect();
                args_ty.push(ret.clone());
                let mut func_args_ty: Vec<_> = (0..args.len()).map(|_| ty::Var::new()).collect();
                func_args_ty.push(ty::Var::new());
                self.get_ty(func).unify(
                    &ty::Var::ty(ty::Kind::Function, func_args_ty.clone()),
                    &mut (),
                );
                self.calls
                    .push(args_ty.into_iter().zip(func_args_ty).collect());
                ret
            }
            _ => todo!(),
        }
    }
    fn _debug_print(&self) {
        for (i, ty) in self.vars.iter().enumerate() {
            eprintln!("v{i}: {ty:?}");
        }
        for (selected, candidates, ty) in &self.overloads {
            eprintln!(
                "{ty:?} = {}",
                selected
                    .get()
                    .map(ToString::to_string)
                    .unwrap_or("?".into())
            );
            for (i, candidate) in candidates.iter().enumerate() {
                eprintln!("  {i}: {candidate:?}")
            }
        }
        for args in &self.calls {
            for (left, right) in args {
                eprintln!("{left:?} = [(...)->]? {right:?}");
            }
        }
    }
}
