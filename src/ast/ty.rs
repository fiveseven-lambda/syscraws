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

mod debug_print;
mod log;
use super::{Block, FuncTy, Program, Stmt};
use crate::ast;
use std::{
    cell::{Cell, OnceCell, RefCell},
    rc::Rc,
};

pub struct Checker<'pr> {
    program: &'pr Program,
    overloads: Vec<(&'pr OnceCell<usize>, Vec<Var>, Var)>,
    calls: Vec<Vec<(Var, Var)>>,
    pub vars: Vec<Var>,
}

impl FuncTy {
    fn get_ty(&self) -> Var {
        let vars: Vec<_> = (0..self.num_vars).map(|_| Var::new()).collect();
        let mut args: Vec<_> = self.args.iter().map(|ty| ty.get_ty(&vars)).collect();
        args.push(self.ret.get_ty(&vars));
        Var::ty(Kind::Function, args)
    }
}
impl ast::Ty {
    fn get_ty(&self, vars: &[Var]) -> Var {
        match *self {
            ast::Ty::Const { ref kind, ref args } => Var::ty(
                kind.clone(),
                args.iter().map(|arg| arg.get_ty(vars)).collect(),
            ),
            ast::Ty::Var(id) => vars[id].clone(),
        }
    }
}

impl<'pr> Checker<'pr> {
    pub fn new(program: &Program) -> Checker {
        Checker {
            program,
            overloads: Vec::new(),
            calls: Vec::new(),
            vars: program
                .vars
                .iter()
                .map(|ty| match ty {
                    Some(ty) => ty.get_ty(&[]),
                    None => Var::new(),
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
                    let mut history = log::History::new();
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
                Stmt::While(cond, block) => {
                    self.get_ty(cond)
                        .unify(&Var::ty(Kind::Boolean, vec![]), &mut ());
                    self.collect_info(block);
                }
                _ => todo!(),
            }
        }
    }
    fn get_ty(&mut self, expr: &'pr ast::Expr) -> Var {
        match *expr {
            ast::Expr::Variable(id) => Var::ty(Kind::Reference, vec![self.vars[id].clone()]),
            ast::Expr::Integer(_) => Var::ty(Kind::Integer, vec![]),
            ast::Expr::Float(_) => Var::ty(Kind::Float, vec![]),
            ast::Expr::String(_) => Var::ty(Kind::String, vec![]),
            ast::Expr::Func(i, ref j) => {
                let ret = Var::new();
                let candidates = self.program.funcs[i]
                    .iter()
                    .map(|(ty, _)| ty.get_ty())
                    .collect();
                self.overloads.push((j, candidates, ret.clone()));
                ret
            }
            ast::Expr::Call(ref func, ref args) => {
                let ret = Var::new();
                let mut args_ty: Vec<_> = args.iter().map(|arg| self.get_ty(arg)).collect();
                args_ty.push(ret.clone());
                let mut func_args_ty: Vec<_> = (0..args.len()).map(|_| Var::new()).collect();
                func_args_ty.push(Var::new());
                self.get_ty(func)
                    .unify(&Var::ty(Kind::Function, func_args_ty.clone()), &mut ());
                self.calls
                    .push(args_ty.into_iter().zip(func_args_ty).collect());
                ret
            }
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Kind {
    Integer,
    Float,
    Boolean,
    String,
    Reference,
    Tuple,
    Function,
}

pub struct Expr {
    pub kind: Kind,
    pub args: Vec<Var>,
}

#[derive(Clone)]
pub struct Var {
    node: Rc<RefCell<Node>>,
}

enum Node {
    Determined(Expr),
    Undetermined { size: Cell<u32> },
    SameAs(Var),
}

impl Expr {
    fn contains(&self, target: &Var) -> bool {
        self.args.iter().any(|arg| arg.contains(target))
    }
}

impl Var {
    pub fn new() -> Var {
        Var {
            node: Rc::new(RefCell::new(Node::Undetermined { size: Cell::new(1) })),
        }
    }
    pub fn ty(kind: Kind, args: Vec<Var>) -> Var {
        Var {
            node: Rc::new(RefCell::new(Node::Determined(Expr { kind, args }))),
        }
    }
    fn contains(&self, target: &Var) -> bool {
        match *self.node.borrow() {
            Node::Determined(ref ty) => ty.contains(target),
            Node::Undetermined { .. } => Rc::ptr_eq(&self.node, &target.node),
            Node::SameAs(ref parent) => parent.contains(target),
        }
    }
    pub fn unify(&self, other: &Var, log: &mut impl log::Log) -> bool {
        let self_binding = self.node.borrow();
        let other_binding = other.node.borrow();
        match (&*self_binding, &*other_binding) {
            (Node::SameAs(parent), _) => {
                drop(other_binding);
                parent.unify(other, log)
            }
            (Node::Determined(self_ty), Node::Determined(other_ty)) => {
                self_ty.kind == other_ty.kind
                    && self_ty.args.len() == other_ty.args.len()
                    && self_ty
                        .args
                        .iter()
                        .zip(&other_ty.args)
                        .all(|(self_arg, other_arg)| self_arg.unify(other_arg, log))
            }
            (Node::Undetermined { size }, Node::Determined(ty)) => {
                if ty.contains(self) {
                    return false;
                }
                log.append(log::Operation {
                    child: self.clone(),
                    old_child_size: size.get(),
                    old_parent_size: None,
                });
                drop(self_binding);
                *self.node.borrow_mut() = Node::SameAs(other.clone());
                true
            }
            (Node::Undetermined { size: self_size }, Node::Undetermined { size: other_size })
                if self_size <= other_size =>
            {
                if Rc::ptr_eq(&self.node, &other.node) {
                    return true;
                }
                let new_size = self_size.get() + other_size.get();
                log.append(log::Operation {
                    child: self.clone(),
                    old_child_size: other_size.get(),
                    old_parent_size: Some(self_size.get()),
                });
                other_size.set(new_size);
                drop(self_binding);
                *self.node.borrow_mut() = Node::SameAs(other.clone());
                true
            }
            _ => {
                drop(self_binding);
                drop(other_binding);
                other.unify(self, log)
            }
        }
    }
}
