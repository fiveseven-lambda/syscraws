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
use crate::ast;
use std::{
    cell::{OnceCell, RefCell},
    rc::Rc,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Kind {
    Integer,
    Float,
    Boolean,
    String,
    Reference,
    Tuple,
    Function,
}

#[derive(Clone)]
struct Ty(Rc<RefCell<TyNode>>);
impl Ty {
    fn new(node: TyNode) -> Ty {
        Ty(Rc::new(RefCell::new(node)))
    }
}

#[derive(Clone)]
enum TyNode {
    Var,
    Const { kind: Kind, args: Vec<Ty> },
}

macro_rules! ty {
    ($kind:ident) => {
        ty!($kind,)
    };
    ($kind:ident, $($args:expr),*) => {
        Ty::new(TyNode::Const {
            kind: Kind::$kind,
            args: vec![ $($args),* ],
        })
    };
    ($id:literal) => {
        Rc::new(Ty::Var($id))
    };
}
macro_rules! cand {
    ($kind:ident) => {
        cand!($kind,)
    };
    ($kind:ident, $($args:expr),*) => {
        Candidate::new(CandidateNode::Const {
            kind: Kind::$kind,
            args: vec![ $($args),* ],
        })
    };
    ($id:literal) => {
        Rc::new(Candidate::Var($id))
    };
}

pub struct Checker<'program> {
    vars: Vec<Ty>,
    sigs: Vec<(Ty, Vec<Candidate>)>,
    overloads: Vec<(Ty, Vec<Candidate>, &'program OnceCell<usize>)>,
    calls: Vec<(Ty, Ty, Vec<Ty>)>,
}

#[derive(Clone)]
struct Candidate(Rc<RefCell<CandidateNode>>);
impl Candidate {
    fn new(node: CandidateNode) -> Candidate {
        Candidate(Rc::new(RefCell::new(node)))
    }
}
enum CandidateNode {
    Var,
    Const { kind: Kind, args: Vec<Candidate> },
}

impl Ty {
    fn generate_candidate(&self) -> CandidateNode {
        match *self.0.borrow() {
            TyNode::Var => CandidateNode::Var,
            TyNode::Const { kind, ref args } => CandidateNode::Const {
                kind,
                args: args
                    .iter()
                    .map(|arg| Candidate::new(arg.generate_candidate()))
                    .collect(),
            },
        }
    }
}
impl Candidate {
    fn generate_ty(&self) -> TyNode {
        match *self.0.borrow() {
            CandidateNode::Var => TyNode::Var,
            CandidateNode::Const { kind, ref args } => TyNode::Const {
                kind,
                args: args.iter().map(|arg| Ty::new(arg.generate_ty())).collect(),
            },
        }
    }
}

impl<'program> Checker<'program> {
    pub fn new(program: &super::Program) -> Checker {
        let vars: Vec<_> = program
            .vars
            .iter()
            .map(|opt_ty| match opt_ty {
                Some(ty) => translate_ty(ty),
                None => Ty::new(TyNode::Var),
            })
            .collect();
        let sigs = program
            .defs
            .iter()
            .map(|def| {
                let mut args = vec![vars[def.ret].clone()];
                args.extend(def.args.iter().map(|&arg| vars[arg].clone()));
                (
                    Ty::new(TyNode::Const {
                        kind: Kind::Function,
                        args,
                    }),
                    Vec::new(),
                )
            })
            .collect();
        let mut checker = Checker {
            vars,
            sigs,
            overloads: Vec::new(),
            calls: Vec::new(),
        };
        for def in &program.defs {
            for stmt in &def.body.stmts {
                match stmt {
                    super::Stmt::Expr(expr) => {
                        checker.get_ty_of_expr(expr, &program.funcs);
                    }
                    _ => {}
                }
            }
        }
        checker
    }
    fn get_ty_of_expr(&mut self, expr: &'program super::Expr, funcs: &[Vec<super::Func>]) -> Ty {
        match *expr {
            super::Expr::Variable(id) => ty!(Reference, self.vars[id].clone()),
            super::Expr::Func(id, ref cell) => {
                let ret = Ty::new(TyNode::Var);
                let candidates = funcs[id]
                    .iter()
                    .map(|func| match *func {
                        super::Func::Builtin(ref func) => get_type_of_builtin_func(func),
                        super::Func::Defined(id) => {
                            let (sig, used) = &mut self.sigs[id];
                            let ret = Candidate::new(sig.generate_candidate());
                            used.push(ret.clone());
                            ret
                        }
                    })
                    .collect();
                self.overloads.push((ret.clone(), candidates, cell));
                ret
            }
            super::Expr::Integer(_) => ty!(Integer),
            super::Expr::Float(_) => ty!(Float),
            super::Expr::String(_) => ty!(String),
            super::Expr::Call(ref func, ref args) => {
                let func_ty = self.get_ty_of_expr(func, funcs);
                let args_ty: Vec<_> = args
                    .iter()
                    .map(|arg| self.get_ty_of_expr(arg, funcs))
                    .collect();
                let ret = Ty::new(TyNode::Var);
                self.calls.push((func_ty, ret.clone(), args_ty));
                ret
            }
        }
    }

    pub fn run(&mut self) {
        // 応急処置
        for (func, ret, args) in &self.calls {
            let mut tmp = vec![ret.clone()];
            tmp.extend(args.iter().map(Ty::clone));
            *func.0.borrow_mut() = TyNode::Const {
                kind: Kind::Function,
                args: tmp,
            };
        }
        while !self.overloads.is_empty() {
            for (sig, used) in &self.sigs {
                for candidate in used {
                    propagate(sig, candidate);
                }
            }
            let mut resolved = false;
            self.overloads = std::mem::take(&mut self.overloads)
                .into_iter()
                .filter(|(func, candidates, ans)| {
                    let mut acceptable_candidates_idx = Vec::new();
                    for (candidate_idx, candidate) in candidates.iter().enumerate() {
                        if propagate(func, candidate) {
                            acceptable_candidates_idx.push(candidate_idx);
                        }
                    }
                    match acceptable_candidates_idx[..] {
                        [idx] => {
                            ans.set(idx).unwrap();
                            reverse_propagate(&candidates[idx], func);
                            resolved = true;
                            false
                        }
                        _ => true,
                    }
                })
                .collect();
            if !resolved {
                break;
            }
        }

        for (i, ty) in self.vars.iter().enumerate() {
            println!("v{i}: {ty:?}");
        }
        for (i, (sig, used)) in self.sigs.iter().enumerate() {
            println!(
                "def {i}: {sig:?} (candidate of {})",
                used.iter()
                    .map(|cand| format!("{:p}", Rc::as_ptr(&cand.0)))
                    .collect::<Vec<_>>()
                    .join(", ")
            );
        }
        for (func, candidates, _) in &self.overloads {
            println!(
                "Candidates of {func:?} ... {}",
                candidates
                    .iter()
                    .map(|candidate| format!("{candidate:?}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            );
        }
    }
}

fn propagate(source: &Ty, target: &Candidate) -> bool {
    let source_binding = source.0.borrow();
    let target_binding = target.0.borrow();
    match (&*source_binding, &*target_binding) {
        (
            TyNode::Const {
                kind: source_kind,
                args: source_args,
            },
            CandidateNode::Const {
                kind: target_kind,
                args: target_args,
            },
        ) => {
            source_kind == target_kind
                && source_args.len() == target_args.len()
                && source_args
                    .iter()
                    .zip(target_args)
                    .all(|(source_arg, target_arg)| propagate(source_arg, target_arg))
        }
        (TyNode::Const { .. }, CandidateNode::Var) => {
            drop(source_binding);
            drop(target_binding);
            *target.0.borrow_mut() = source.generate_candidate();
            true
        }
        (TyNode::Var, _) => true,
    }
}
fn reverse_propagate(source: &Candidate, target: &Ty) {
    let source_binding = source.0.borrow();
    let target_binding = target.0.borrow();
    match (&*source_binding, &*target_binding) {
        (
            CandidateNode::Const {
                kind: _,
                args: source_args,
            },
            TyNode::Const {
                kind: _,
                args: target_args,
            },
        ) => {
            for (source_arg, target_arg) in source_args.iter().zip(target_args) {
                reverse_propagate(source_arg, target_arg);
            }
        }
        (CandidateNode::Const { .. }, TyNode::Var) => {
            drop(source_binding);
            drop(target_binding);
            *target.0.borrow_mut() = source.generate_ty();
        }
        (CandidateNode::Var, _) => {}
    }
}

fn translate_ty(ty: &super::Ty) -> Ty {
    Ty::new(TyNode::Const {
        kind: ty.kind,
        args: ty.args.iter().map(|arg| translate_ty(arg)).collect(),
    })
}

fn get_type_of_builtin_func(func: &super::BuiltinFunc) -> Candidate {
    match func {
        super::BuiltinFunc::New => {
            let t = Candidate::new(CandidateNode::Var);
            cand!(Function, cand!(Reference, t.clone()), cand!(Reference, t))
        }
        super::BuiltinFunc::Delete => {
            let t = Candidate::new(CandidateNode::Var);
            cand!(Function, cand!(Tuple), t)
        }
        super::BuiltinFunc::Assign => {
            let t = Candidate::new(CandidateNode::Var);
            cand!(
                Function,
                cand!(Reference, t.clone()),
                cand!(Reference, t.clone()),
                t
            )
        }
        super::BuiltinFunc::Deref => {
            let t = Candidate::new(CandidateNode::Var);
            cand!(Function, t.clone(), cand!(Reference, t))
        }
        super::BuiltinFunc::AddInt => {
            cand!(Function, cand!(Integer), cand!(Integer), cand!(Integer))
        }
        super::BuiltinFunc::AddFloat => {
            cand!(Function, cand!(Float), cand!(Float), cand!(Float))
        }
        _ => todo!(),
    }
}
