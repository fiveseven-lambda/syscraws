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

pub mod error;
use crate::{
    ast,
    pre_ast::{self, Arg},
    ty,
};
use enum_iterator::Sequence;
pub use error::eprint_errors;
use error::Error;
use std::{cell::OnceCell, collections::HashMap};

pub fn into_ast(stmts: Vec<pre_ast::PStmt>) -> Result<ast::Program, Vec<Error>> {
    let mut env = Environment::new();
    let mut main = ast::Block::new();
    let mut scope = Vec::new();
    for stmt in stmts {
        env.add_toplevel_stmt(&mut main, stmt, &mut scope);
    }
    env.program.defs.push(main);
    if env.errors.is_empty() {
        Ok(env.program)
    } else {
        Err(env.errors)
    }
}

struct Environment<'id> {
    program: ast::Program,
    variables_name: HashMap<&'id str, usize>,
    functions_name: HashMap<&'id str, usize>,
    errors: Vec<Error>,
}
#[derive(Debug)]
struct VariableInScope<'id> {
    name: &'id str,
    old_id: Option<usize>,
}
type Scope<'id> = Vec<VariableInScope<'id>>;
impl<'id> Environment<'id> {
    fn new() -> Self {
        let mut funcs = vec![Vec::new(); pre_ast::Operator::CARDINALITY];
        funcs[pre_ast::Operator::Add as usize] = vec![
            (
                ast::FuncTy {
                    num_vars: 0,
                    args: vec![
                        ast::Ty::Const {
                            kind: ty::Kind::Integer,
                            args: vec![],
                        },
                        ast::Ty::Const {
                            kind: ty::Kind::Integer,
                            args: vec![],
                        },
                    ],
                    ret: ast::Ty::Const {
                        kind: ty::Kind::Integer,
                        args: vec![],
                    },
                },
                ast::Func::Builtin(ast::BuiltinFunc::AddInt),
            ),
            (
                ast::FuncTy {
                    num_vars: 0,
                    args: vec![
                        ast::Ty::Const {
                            kind: ty::Kind::Float,
                            args: vec![],
                        },
                        ast::Ty::Const {
                            kind: ty::Kind::Float,
                            args: vec![],
                        },
                    ],
                    ret: ast::Ty::Const {
                        kind: ty::Kind::Float,
                        args: vec![],
                    },
                },
                ast::Func::Builtin(ast::BuiltinFunc::AddFloat),
            ),
        ];
        Environment {
            program: ast::Program {
                funcs,
                defs: Vec::new(),
                vars: Vec::new(),
            },
            variables_name: HashMap::new(),
            functions_name: HashMap::new(),
            errors: Vec::new(),
        }
    }
    fn get_func_id(&mut self, name: &'id str) -> usize {
        *self.functions_name.entry(name).or_insert_with(|| {
            let new_id = self.program.funcs.len();
            self.program.funcs.push(Vec::new());
            new_id
        })
    }
    fn declare_variable(
        &mut self,
        name: &'id str,
        ty: Option<ast::Ty>,
        scope: &mut Scope<'id>,
    ) -> usize {
        let new_id = self.program.vars.len();
        self.program.vars.push(ty);
        let old_id = self.variables_name.insert(name, new_id);
        scope.push(VariableInScope { name, old_id });
        new_id
    }
    fn drop_scope(&mut self, scope: Scope<'id>) {
        for VariableInScope { name, old_id } in scope.into_iter().rev() {
            match old_id {
                Some(old_id) => self.variables_name.insert(name, old_id),
                None => self.variables_name.remove(name),
            };
        }
    }
    fn add_toplevel_stmt(
        &mut self,
        target: &mut ast::Block,
        stmt: pre_ast::PStmt<'id>,
        scope: &mut Scope<'id>,
    ) {
        match stmt.stmt {
            pre_ast::Stmt::Block {
                antecedent: Some(func),
                stmts,
            } => {
                // 関数定義
                let mut func_name_and_args = None;
                let mut ret_ty = None;
                match func.term {
                    pre_ast::Term::ReturnType {
                        arrow_pos,
                        opt_term,
                        opt_ty,
                    } => {
                        match opt_ty {
                            Some(term) => ret_ty = self.term_into_ty(*term),
                            None => self.errors.push(Error::EmptyReturnType {
                                arrow_pos: arrow_pos.clone(),
                            }),
                        }
                        match opt_term {
                            Some(term) => func_name_and_args = Some(*term),
                            None => self
                                .errors
                                .push(Error::EmptyFunctionNameAndArgs { arrow_pos }),
                        }
                    }
                    _ => func_name_and_args = Some(func),
                };
                let mut func_name_term = None;
                let mut args_term = None;
                if self.errors.is_empty() {
                    let func_name_and_args = func_name_and_args.unwrap();
                    match func_name_and_args.term {
                        pre_ast::Term::Parenthesized {
                            opt_antecedent: Some(antecedent),
                            elements,
                            has_trailing_comma: _,
                        } => {
                            func_name_term = Some(antecedent);
                            args_term = Some(elements);
                        }
                        _ => self.errors.push(Error::UnexpectedExpressionBeforeBlock(
                            func_name_and_args.pos,
                        )),
                    }
                }
                let mut func_name = None;
                let mut func_args = Vec::new();
                if self.errors.is_empty() {
                    let func_name_term = func_name_term.unwrap();
                    match func_name_term.term {
                        pre_ast::Term::Identifier(s) => func_name = Some(s),
                        _ => self
                            .errors
                            .push(Error::InvalidFunctionName(func_name_term.pos)),
                    };
                    let args_term = args_term.unwrap();
                    for opt_arg_term in args_term {
                        match opt_arg_term {
                            Arg::Term(arg_term) => {
                                let mut arg_name_term = None;
                                let mut opt_arg_ty = None;
                                match arg_term.term {
                                    pre_ast::Term::TypeAnnotation {
                                        colon_pos,
                                        opt_term,
                                        opt_ty,
                                    } => {
                                        match opt_term {
                                            Some(term) => arg_name_term = Some(*term),
                                            _ => self.errors.push(Error::EmptyArgumentWithType {
                                                colon_pos: colon_pos.clone(),
                                            }),
                                        };
                                        opt_arg_ty =
                                            opt_ty.and_then(|term| self.term_into_ty(*term));
                                    }
                                    _ => arg_name_term = Some(arg_term),
                                }
                                if self.errors.is_empty() {
                                    let arg_name_term = arg_name_term.unwrap();
                                    match arg_name_term.term {
                                        pre_ast::Term::Identifier(arg_name) => {
                                            func_args.push((arg_name, opt_arg_ty));
                                        }
                                        _ => self
                                            .errors
                                            .push(Error::InvalidArgumentInDef(arg_name_term.pos)),
                                    }
                                }
                            }
                            Arg::Empty { comma_pos } => {
                                self.errors.push(Error::EmptyArgument { comma_pos })
                            }
                        }
                    }
                }
                let mut scope = Vec::new();
                let args_ty: Vec<_> = func_args
                    .into_iter()
                    .map(|(arg_name, arg_ty)| {
                        self.declare_variable(arg_name, arg_ty.clone(), &mut scope);
                        arg_ty.expect("Type inference of args is not implemented yet")
                    })
                    .collect();
                let mut body = ast::Block::new();
                for stmt in stmts {
                    self.add_stmt(&mut body, stmt, &mut scope);
                }
                self.drop_scope(scope);
                if self.errors.is_empty() {
                    let func_id = self.get_func_id(func_name.unwrap());
                    let def_id = self.program.defs.len();
                    self.program.defs.push(body);
                    self.program.funcs[func_id].push((
                        ast::FuncTy {
                            num_vars: 0,
                            args: args_ty,
                            ret: ret_ty
                                .expect("Type inference of return type is not implemented yet"),
                        },
                        ast::Func::Defined(def_id),
                    ));
                }
            }
            _ => self.add_stmt(target, stmt, scope),
        }
    }
    fn add_stmt(
        &mut self,
        target: &mut ast::Block,
        stmt: pre_ast::PStmt<'id>,
        scope: &mut Scope<'id>,
    ) {
        match stmt.stmt {
            pre_ast::Stmt::Term(Some(term)) => {
                let expr = self.term_into_expr(term, scope);
                if self.errors.is_empty() {
                    target.add_stmt(ast::Stmt::Expr(expr.unwrap()));
                }
            }
            pre_ast::Stmt::Term(None) => {}
            pre_ast::Stmt::Block {
                antecedent: None,
                stmts,
            } => {
                let mut scope = Scope::new();
                for stmt in stmts {
                    self.add_stmt(target, stmt, &mut scope);
                }
                self.drop_scope(scope);
            }
            pre_ast::Stmt::While {
                cond,
                while_pos,
                stmt,
            } => {
                let mut cond_expr = None;
                match cond {
                    Some(term) => cond_expr = self.term_into_expr(term, scope),
                    None => self
                        .errors
                        .push(Error::EmptyConditionWhile(while_pos.clone())),
                }
                let mut block = ast::Block::new();
                let mut scope = Scope::new();
                match stmt {
                    Some(stmt) => self.add_stmt(&mut block, *stmt, &mut scope),
                    None => self.errors.push(Error::EmptyConditionWhile(while_pos)),
                }
                if self.errors.is_empty() {
                    target.add_stmt(ast::Stmt::While(cond_expr.unwrap(), block))
                }
            }
            _ => todo!(),
        }
    }
    fn term_into_expr(
        &mut self,
        term: pre_ast::PTerm<'id>,
        scope: &mut Scope<'id>,
    ) -> Option<ast::Expr> {
        match term.term {
            pre_ast::Term::Integer(value) => Some(ast::Expr::Integer(value)),
            pre_ast::Term::Float(value) => Some(ast::Expr::Float(value)),
            pre_ast::Term::Parenthesized {
                opt_antecedent: Some(func),
                elements: args_term,
                has_trailing_comma: _,
            } => {
                let func = self.term_into_expr(*func, scope);
                let mut args_expr = Vec::new();
                for arg in args_term {
                    match arg {
                        Arg::Term(term) => args_expr.extend(self.term_into_expr(term, scope)),
                        Arg::Empty { comma_pos } => {
                            self.errors.push(Error::EmptyArgument { comma_pos })
                        }
                    }
                }
                self.errors
                    .is_empty()
                    .then(|| ast::Expr::Call(Box::new(func.unwrap()), args_expr))
            }
            pre_ast::Term::Parenthesized {
                opt_antecedent: None,
                elements: opt_elements,
                has_trailing_comma,
            } => {
                if opt_elements.len() == 1 && !has_trailing_comma {
                    let Arg::Term(term) = opt_elements.into_iter().next().unwrap() else {
                        unreachable!()
                    };
                    self.term_into_expr(term, scope)
                } else {
                    todo!();
                }
            }
            pre_ast::Term::TypeAnnotation {
                colon_pos,
                opt_term,
                opt_ty,
            } => {
                let mut name = None;
                match opt_term {
                    Some(term) => match term.term {
                        pre_ast::Term::Identifier(s) => name = Some(s),
                        _ => self.errors.push(Error::InvalidLeftHandSideDecl {
                            error_pos: term.pos,
                            colon_pos,
                        }),
                    },
                    None => self.errors.push(Error::EmptyLeftHandSideDecl { colon_pos }),
                };
                let ty = opt_ty.and_then(|ty| self.term_into_ty(*ty));
                self.errors.is_empty().then(|| {
                    let id = self.declare_variable(name.unwrap(), ty, scope);
                    ast::Expr::Variable(id)
                })
            }
            pre_ast::Term::Identifier(name) => {
                if let Some(&id) = self.variables_name.get(name) {
                    Some(ast::Expr::Variable(id))
                } else {
                    let func_id = self.get_func_id(name);
                    Some(ast::Expr::Func(func_id, OnceCell::new()))
                }
            }
            pre_ast::Term::UnaryOperation {
                operator,
                operator_pos,
                opt_operand,
            } => {
                let mut operand = None;
                match opt_operand {
                    Some(term) => operand = self.term_into_expr(*term, scope),
                    None => self.errors.push(Error::EmptyUnaryOperand(operator_pos)),
                }
                self.errors.is_empty().then(|| {
                    ast::Expr::Call(
                        Box::new(ast::Expr::Func(operator as usize, OnceCell::new())),
                        vec![operand.unwrap()],
                    )
                })
            }
            pre_ast::Term::BinaryOperation {
                operator,
                operator_pos,
                opt_left_operand,
                opt_right_operand,
            } => {
                let mut left_operand = None;
                match opt_left_operand {
                    Some(term) => left_operand = self.term_into_expr(*term, scope),
                    None => self
                        .errors
                        .push(Error::EmptyLeftOperand(operator_pos.clone())),
                }
                let mut right_operand = None;
                match opt_right_operand {
                    Some(term) => right_operand = self.term_into_expr(*term, scope),
                    None => self
                        .errors
                        .push(Error::EmptyRightOperand(operator_pos.clone())),
                }
                self.errors.is_empty().then(|| {
                    ast::Expr::Call(
                        Box::new(ast::Expr::Func(operator as usize, OnceCell::new())),
                        vec![left_operand.unwrap(), right_operand.unwrap()],
                    )
                })
            }
            pre_ast::Term::Assignment {
                operator,
                operator_pos,
                opt_left_hand_side,
                opt_right_hand_side,
            } => {
                let mut right_hand_side = None;
                match opt_right_hand_side {
                    Some(term) => right_hand_side = self.term_into_expr(*term, scope),
                    None => self
                        .errors
                        .push(Error::EmptyRightHandSide(operator_pos.clone())),
                }
                let mut left_hand_side = None;
                match opt_left_hand_side {
                    Some(term) => left_hand_side = self.term_into_expr(*term, scope),
                    None => self
                        .errors
                        .push(Error::EmptyLeftHandSide(operator_pos.clone())),
                }
                self.errors.is_empty().then(|| {
                    ast::Expr::Call(
                        Box::new(ast::Expr::Func(operator as usize, OnceCell::new())),
                        vec![left_hand_side.unwrap(), right_hand_side.unwrap()],
                    )
                })
            }
            _ => todo!(),
        }
    }
    fn term_into_ty(&mut self, term: pre_ast::PTerm<'id>) -> Option<ast::Ty> {
        match term.term {
            pre_ast::Term::Identifier("int") => Some(ast::Ty::Const {
                kind: ty::Kind::Integer,
                args: vec![],
            }),
            _ => todo!(),
        }
    }
}
