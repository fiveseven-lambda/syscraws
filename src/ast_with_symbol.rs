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

use crate::ast;
use crate::range::Range;
use crate::ty;
use num::BigInt;

pub enum Expr<'id> {
    Identifier(&'id str),
    Decl(&'id str, Option<PTy<'id>>),
    Integer(BigInt),
    Float(f64),
    String(String),
    Operator(ast::Operator),
    Call(Box<PExpr<'id>>, Vec<PExpr<'id>>),
}
pub struct PExpr<'id> {
    pos: Range,
    expr: Expr<'id>,
}
impl<'id> PExpr<'id> {
    pub fn new(pos: Range, expr: Expr) -> PExpr {
        PExpr { pos, expr }
    }
}

pub enum Stmt<'id> {
    Expr(Option<PExpr<'id>>),
    Return(Option<PExpr<'id>>),
    If(PExpr<'id>, Box<PStmt<'id>>, Option<Box<PStmt<'id>>>),
    While(PExpr<'id>, Box<PStmt<'id>>),
    Block(Vec<PStmt<'id>>),
    Def {
        name: &'id str,
        args: Vec<&'id str>,
        body: Vec<PStmt<'id>>,
    },
}

pub struct PStmt<'id> {
    pos: Range,
    stmt: Stmt<'id>,
}

impl<'id> PStmt<'id> {
    pub fn new(pos: Range, stmt: Stmt) -> PStmt {
        PStmt { pos, stmt }
    }
}

pub enum Ty<'id> {
    Name(&'id str, Vec<PTy<'id>>),
    TypeOf(Box<PExpr<'id>>),
}

pub struct PTy<'id> {
    pos: Range,
    ty: Ty<'id>,
}
impl<'id> PTy<'id> {
    pub fn new(pos: Range, ty: Ty) -> PTy {
        PTy { pos, ty }
    }
}

use std::collections::HashMap;
struct Variables<'id> {
    names: HashMap<&'id str, Vec<usize>>,
    num: usize,
}
impl<'id> Variables<'id> {
    fn new() -> Variables<'id> {
        Variables {
            names: HashMap::new(),
            num: 0,
        }
    }
    fn insert(&mut self, name: &'id str) -> usize {
        let id = self.num;
        self.num += 1;
        self.names.entry(name).or_insert_with(Vec::new).push(id);
        id
    }
    fn remove(&mut self, name: &str) -> usize {
        self.names.get_mut(name).unwrap().pop().unwrap()
    }
    fn get(&self, name: &str) -> Option<usize> {
        self.names.get(name).and_then(|v| v.last()).copied()
    }
}
struct Funcs<'id> {
    names: HashMap<&'id str, usize>,
    defs: Vec<Vec<(Option<ty::Func>, ast::Func)>>,
}
impl<'id> Funcs<'id> {
    fn new() -> Funcs<'id> {
        Funcs {
            names: HashMap::new(),
            defs: ast::operators(),
        }
    }
    fn get_or_insert(&mut self, name: &'id str) -> usize {
        *self.names.entry(name).or_insert_with(|| {
            let id = self.defs.len();
            self.defs.push(Vec::new());
            id
        })
    }
}
impl<'id> PExpr<'id> {
    fn resolve_symbol(
        self,
        variables: &mut Variables<'id>,
        funcs: &mut Funcs<'id>,
        globals: Option<&Variables<'id>>,
        variables_in_current_scope: &mut Vec<&'id str>,
        tys: &mut Vec<Option<ty::Ty>>,
    ) -> ast::Expr {
        match self.expr {
            Expr::Decl(name, ty) => {
                let id = variables.insert(name);
                assert_eq!(tys.len(), id);
                tys.push(ty.map(|ty| ty.resolve_symbol()));
                variables_in_current_scope.push(name);
                ast::Expr::Variable(id)
            }
            Expr::Identifier(name) => {
                if let Some(id) = variables.get(name) {
                    ast::Expr::Variable(id)
                } else if let Some(id) = globals.and_then(|variables| variables.get(name)) {
                    ast::Expr::Global(id)
                } else {
                    let id = funcs.get_or_insert(name);
                    ast::Expr::Func(id)
                }
            }
            Expr::Integer(value) => ast::Expr::Integer(value),
            Expr::Float(value) => ast::Expr::Float(value),
            Expr::String(value) => ast::Expr::String(value),
            Expr::Operator(operator) => ast::Expr::Func(operator as usize),
            Expr::Call(func, args) => {
                let func =
                    func.resolve_symbol(variables, funcs, globals, variables_in_current_scope, tys);
                let args = args
                    .into_iter()
                    .map(|arg| {
                        arg.resolve_symbol(
                            variables,
                            funcs,
                            globals,
                            variables_in_current_scope,
                            tys,
                        )
                    })
                    .collect();
                ast::Expr::Call(func.into(), args)
            }
        }
    }
}
impl<'id> PStmt<'id> {
    fn resolve_symbol(
        self,
        variables: &mut Variables<'id>,
        funcs: &mut Funcs<'id>,
        globals: Option<&Variables<'id>>,
        variables_in_current_scope: &mut Vec<&'id str>,
        tys: &mut Vec<Option<ty::Ty>>,
        is_toplevel: bool,
    ) -> Vec<ast::Stmt> {
        match self.stmt {
            Stmt::Expr(Some(expr)) => vec![ast::Stmt::Expr(expr.resolve_symbol(
                variables,
                funcs,
                globals,
                variables_in_current_scope,
                tys,
            ))],
            Stmt::Expr(None) => vec![],
            Stmt::Def { name, args, body } => {
                if !is_toplevel {
                    // TODO: push error
                    return vec![];
                }
                let id = funcs.get_or_insert(name);
                let mut variables_in_current_scope = Vec::new();
                let mut locals = Variables::new();
                let mut tys = Vec::new();
                for arg in &args {
                    locals.insert(arg);
                }
                let body = body
                    .into_iter()
                    .flat_map(|stmt| {
                        stmt.resolve_symbol(
                            &mut locals,
                            funcs,
                            Some(variables),
                            &mut variables_in_current_scope,
                            &mut tys,
                            false,
                        )
                    })
                    .collect();
                assert_eq!(locals.num, tys.len());
                funcs.defs[id].push((
                    None,
                    ast::Func::UserDefined(ast::FuncDef::new(args.len(), tys, body)),
                ));
                vec![]
            }
            Stmt::Block(stmts) => {
                let mut variables_in_block = Vec::new();
                let stmts = stmts
                    .into_iter()
                    .flat_map(|stmt| {
                        stmt.resolve_symbol(
                            variables,
                            funcs,
                            globals,
                            &mut variables_in_block,
                            tys,
                            false,
                        )
                    })
                    .collect();
                for variable in variables_in_block {
                    variables.remove(variable);
                }
                stmts
            }
            Stmt::If(cond, stmt_then, stmt_else) => {
                let mut variables_in_cond = Vec::new();
                let cond =
                    cond.resolve_symbol(variables, funcs, globals, &mut variables_in_cond, tys);
                let mut variables_in_stmt_then = Vec::new();
                let stmt_then = stmt_then.resolve_symbol(
                    variables,
                    funcs,
                    globals,
                    &mut variables_in_stmt_then,
                    tys,
                    false,
                );
                for variable in variables_in_stmt_then {
                    variables.remove(variable);
                }
                let stmt_else = match stmt_else {
                    Some(stmt_else) => {
                        let mut variables_in_stmt_else = Vec::new();
                        let stmt_else = stmt_else.resolve_symbol(
                            variables,
                            funcs,
                            globals,
                            &mut variables_in_stmt_else,
                            tys,
                            false,
                        );
                        for variable in variables_in_stmt_else {
                            variables.remove(variable);
                        }
                        stmt_else
                    }
                    None => {
                        vec![]
                    }
                };
                for variable in variables_in_cond {
                    variables.remove(variable);
                }
                vec![ast::Stmt::If(cond, stmt_then, stmt_else)]
            }
            Stmt::While(cond, stmt) => {
                let mut variables_in_cond = Vec::new();
                let cond =
                    cond.resolve_symbol(variables, funcs, globals, &mut variables_in_cond, tys);
                let mut variables_in_stmt = Vec::new();
                let stmt = stmt.resolve_symbol(
                    variables,
                    funcs,
                    globals,
                    &mut variables_in_stmt,
                    tys,
                    false,
                );
                for variable in variables_in_stmt {
                    variables.remove(variable);
                }
                for variable in variables_in_cond {
                    variables.remove(variable);
                }
                vec![ast::Stmt::While(cond, stmt)]
            }
            Stmt::Return(expr) => {
                let expr = expr.map(|expr| {
                    expr.resolve_symbol(variables, funcs, globals, variables_in_current_scope, tys)
                });
                vec![ast::Stmt::Return(expr)]
            }
        }
    }
}
impl<'id> PTy<'id> {
    fn resolve_symbol(self) -> ty::Ty {
        match self.ty {
            Ty::Name("int", _) => ty::Ty::integer(),
            Ty::Name("float", _) => ty::Ty::float(),
            Ty::Name("bool", _) => ty::Ty::boolean(),
            Ty::Name("string", _) => ty::Ty::string(),
            _ => panic!(),
        }
    }
}
pub fn resolve_symbol(
    stmts: Vec<PStmt>,
) -> (
    Vec<ast::Stmt>,
    Vec<Vec<(Option<ty::Func>, ast::Func)>>,
    Vec<Option<ty::Ty>>,
) {
    let mut globals = Variables::new();
    let mut funcs = Funcs::new();
    let mut variables = Vec::new();
    let mut tys = Vec::new();
    let stmts = stmts
        .into_iter()
        .flat_map(|stmt| {
            stmt.resolve_symbol(
                &mut globals,
                &mut funcs,
                None,
                &mut variables,
                &mut tys,
                true,
            )
        })
        .collect();
    for (&name, &index) in &funcs.names {
        if funcs.defs[index].is_empty() {
            eprintln!("`{name}` is not defined");
        }
    }
    assert_eq!(tys.len(), globals.num);
    (stmts, funcs.defs, tys)
}
