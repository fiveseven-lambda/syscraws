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

//! [`Context`] を定義する．

use super::Error;
use crate::{ast, pre_ast};
use either::Either;
use enum_iterator::Sequence;
use std::{cell::OnceCell, collections::HashMap};

macro_rules! ty {
    ($kind:ident) => {
        ty!($kind,)
    };
    ($kind:ident, $($args:expr),*) => {
        ast::Ty::Const {
            kind: ast::ty::Kind::$kind,
            args: vec![ $($args),* ],
        }
    };
    ($id:literal) => {
        ast::Ty::Var($id)
    };
}

/**
 * 変換の中心となる，文脈を格納する構造体．
 *
 * ちょいとインターフェースがよろしくない．要修正．
 */
pub struct Context<'id> {
    /// 生成した AST の格納先．
    pub program: ast::Program,
    /// 変数名と ID の対応を記録する．
    variables_name: HashMap<&'id str, usize>,
    /// 関数名と ID の対応を記録する．
    functions_name: HashMap<&'id str, usize>,
    /// 変換の過程で起こったエラーを格納する．
    pub errors: Vec<Error>,
}

/// スコープ中の変数．スコープが終わると [`Context::drop_scope()`] で消える．
pub struct VariableInScope<'id> {
    name: &'id str,
    old_id: Option<usize>,
}
type Scope<'id> = Vec<VariableInScope<'id>>;

impl<'id> Context<'id> {
    pub fn new() -> Self {
        let mut funcs = vec![Vec::new(); pre_ast::Operator::CARDINALITY];
        funcs[pre_ast::Operator::Add as usize] = vec![
            (
                ast::FuncTy {
                    num_vars: 0,
                    args: vec![ty!(Integer), ty!(Integer)],
                    ret: ty!(Integer),
                },
                ast::Func::Builtin(ast::BuiltinFunc::AddInt),
            ),
            (
                ast::FuncTy {
                    num_vars: 0,
                    args: vec![ty!(Float), ty!(Float)],
                    ret: ty!(Float),
                },
                ast::Func::Builtin(ast::BuiltinFunc::AddFloat),
            ),
        ];
        funcs[pre_ast::Operator::Less as usize] = vec![(
            ast::FuncTy {
                num_vars: 0,
                args: vec![ty!(Integer), ty!(Integer)],
                ret: ty!(Boolean),
            },
            ast::Func::Builtin(ast::BuiltinFunc::LessInt),
        )];
        funcs[pre_ast::Operator::New as usize] = vec![(
            ast::FuncTy {
                num_vars: 1,
                args: vec![ty!(Reference, ty!(0))],
                ret: ty!(Reference, ty!(0)),
            },
            ast::Func::Builtin(ast::BuiltinFunc::New),
        )];
        funcs[pre_ast::Operator::Assign as usize] = vec![(
            ast::FuncTy {
                num_vars: 1,
                args: vec![ty!(Reference, ty!(0)), ty!(0)],
                ret: ty!(Reference, ty!(0)),
            },
            ast::Func::Builtin(ast::BuiltinFunc::Assign),
        )];
        funcs[pre_ast::Operator::Delete as usize] = vec![(
            ast::FuncTy {
                num_vars: 1,
                args: vec![ty!(0)],
                ret: ty!(Tuple),
            },
            ast::Func::Builtin(ast::BuiltinFunc::Delete),
        )];
        funcs[pre_ast::Operator::Deref as usize] = vec![(
            ast::FuncTy {
                num_vars: 1,
                args: vec![ty!(Reference, ty!(0))],
                ret: ty!(0),
            },
            ast::Func::Builtin(ast::BuiltinFunc::Deref),
        )];
        funcs[pre_ast::Operator::ToString as usize] = vec![(
            ast::FuncTy {
                num_vars: 1,
                args: vec![ty!(0)],
                ret: ty!(String),
            },
            ast::Func::Builtin(ast::BuiltinFunc::ToString),
        )];
        funcs[pre_ast::Operator::Concat as usize] = vec![(
            ast::FuncTy {
                num_vars: 1,
                args: vec![ty!(String), ty!(String)],
                ret: ty!(String),
            },
            ast::Func::Builtin(ast::BuiltinFunc::Concat),
        )];
        Context {
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
    /// スコープ中の変数に対し逆順に delete を呼び出し，[`Context`] を元通りにする．
    pub fn drop_scope(&mut self, scope: Scope<'id>, block: &mut ast::Block) {
        for VariableInScope { name, old_id } in scope.into_iter().rev() {
            let id = match old_id {
                Some(old_id) => self.variables_name.insert(name, old_id),
                None => self.variables_name.remove(name),
            }
            .unwrap();
            block.add_stmt(ast::Stmt::Expr(ast::Expr::Call(
                Box::new(ast::Expr::Func(
                    pre_ast::Operator::Delete as usize,
                    OnceCell::new(),
                )),
                vec![ast::Expr::Variable(id)],
            )))
        }
    }
    pub fn add_toplevel_stmt(
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
                            pre_ast::Arg::Term(arg_term) => {
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
                            pre_ast::Arg::Empty { comma_pos } => {
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
                self.drop_scope(scope, target);
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
                let expr = self.term_into_right_expr(term, scope);
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
                self.drop_scope(scope, target);
            }
            pre_ast::Stmt::While {
                cond,
                while_pos,
                stmt,
            } => {
                let mut cond_expr = None;
                match cond {
                    Some(term) => cond_expr = self.term_into_right_expr(term, scope),
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
    fn term_into_right_expr(
        &mut self,
        term: pre_ast::PTerm<'id>,
        scope: &mut Scope<'id>,
    ) -> Option<ast::Expr> {
        match term.term {
            pre_ast::Term::Integer(value) => Some(ast::Expr::Integer(value)),
            pre_ast::Term::Float(value) => Some(ast::Expr::Float(value)),
            pre_ast::Term::String(components) => {
                let ret = components
                    .into_iter()
                    .map(|component| match component {
                        Either::Left(string) => Some(ast::Expr::String(string)),
                        Either::Right(t) => {
                            let mut expr = None;
                            match t {
                                Some(t) => expr = self.term_into_right_expr(t, scope),
                                None => self.errors.push(Error::EmptyBraceInStringLiteral {
                                    literal_pos: term.pos.clone(),
                                }),
                            }
                            self.errors.is_empty().then(|| {
                                ast::Expr::Call(
                                    Box::new(ast::Expr::Func(
                                        pre_ast::Operator::ToString as usize,
                                        OnceCell::new(),
                                    )),
                                    vec![expr.unwrap()],
                                )
                            })
                        }
                    })
                    .reduce(|s, t| {
                        s.zip(t).map(|(s, t)| {
                            ast::Expr::Call(
                                Box::new(ast::Expr::Func(
                                    pre_ast::Operator::Concat as usize,
                                    OnceCell::new(),
                                )),
                                vec![s, t],
                            )
                        })
                    });
                self.errors.is_empty().then(|| match ret {
                    Some(expr) => expr.unwrap(),
                    None => ast::Expr::String(String::from("")),
                })
            }
            pre_ast::Term::ReturnType { .. } => todo!(),
            pre_ast::Term::Parenthesized {
                opt_antecedent: Some(func),
                elements: args_term,
                has_trailing_comma: _,
            } => {
                let func = self.term_into_right_expr(*func, scope);
                let mut args_expr = Vec::new();
                for arg in args_term {
                    match arg {
                        pre_ast::Arg::Term(term) => {
                            args_expr.extend(self.term_into_right_expr(term, scope))
                        }
                        pre_ast::Arg::Empty { comma_pos } => {
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
                    let pre_ast::Arg::Term(term) = opt_elements.into_iter().next().unwrap() else {
                        unreachable!()
                    };
                    self.term_into_right_expr(term, scope)
                } else {
                    todo!();
                }
            }
            pre_ast::Term::Ref {
                operator_pos,
                opt_operand,
            } => {
                let mut operand = None;
                match opt_operand {
                    Some(term) => operand = self.term_into_left_expr(*term, scope),
                    None => self.errors.push(Error::EmptyUnaryOperand(operator_pos)),
                }
                self.errors.is_empty().then(|| operand.unwrap())
            }
            pre_ast::Term::UnaryOperation {
                operator,
                operator_pos,
                opt_operand,
            } => {
                let mut operand = None;
                match opt_operand {
                    Some(term) => operand = self.term_into_right_expr(*term, scope),
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
                    Some(term) => left_operand = self.term_into_right_expr(*term, scope),
                    None => self
                        .errors
                        .push(Error::EmptyLeftOperand(operator_pos.clone())),
                }
                let mut right_operand = None;
                match opt_right_operand {
                    Some(term) => right_operand = self.term_into_right_expr(*term, scope),
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
                    Some(term) => right_hand_side = self.term_into_right_expr(*term, scope),
                    None => self
                        .errors
                        .push(Error::EmptyRightHandSide(operator_pos.clone())),
                }
                let mut left_hand_side = None;
                match opt_left_hand_side {
                    Some(term) => left_hand_side = self.term_into_left_expr(*term, scope),
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
            pre_ast::Term::Identifier(name) => {
                if let Some(&id) = self.variables_name.get(name) {
                    Some(ast::Expr::Call(
                        Box::new(ast::Expr::Func(
                            pre_ast::Operator::Deref as usize,
                            OnceCell::new(),
                        )),
                        vec![ast::Expr::Variable(id)],
                    ))
                } else {
                    let func_id = self.get_func_id(name);
                    Some(ast::Expr::Func(func_id, OnceCell::new()))
                }
            }
            _ => {
                let term = self.term_into_left_expr(term, scope);
                self.errors.is_empty().then(|| {
                    ast::Expr::Call(
                        Box::new(ast::Expr::Func(
                            pre_ast::Operator::Deref as usize,
                            OnceCell::new(),
                        )),
                        vec![term.unwrap()],
                    )
                })
            }
        }
    }
    fn term_into_left_expr(
        &mut self,
        term: pre_ast::PTerm<'id>,
        scope: &mut Scope<'id>,
    ) -> Option<ast::Expr> {
        match term.term {
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
                    ast::Expr::Call(
                        Box::new(ast::Expr::Func(
                            pre_ast::Operator::New as usize,
                            OnceCell::new(),
                        )),
                        vec![ast::Expr::Variable(id)],
                    )
                })
            }
            pre_ast::Term::Identifier(name) => {
                if let Some(&id) = self.variables_name.get(name) {
                    Some(ast::Expr::Variable(id))
                } else {
                    panic!();
                }
            }
            pre_ast::Term::Deref {
                operator_pos,
                opt_operand,
            } => {
                let mut operand = None;
                match opt_operand {
                    Some(term) => operand = self.term_into_right_expr(*term, scope),
                    None => self.errors.push(Error::EmptyUnaryOperand(operator_pos)),
                }
                self.errors.is_empty().then(|| operand.unwrap())
            }
            pre_ast::Term::Parenthesized {
                opt_antecedent: None,
                elements: opt_elements,
                has_trailing_comma,
            } => {
                if opt_elements.len() == 1 && !has_trailing_comma {
                    let pre_ast::Arg::Term(term) = opt_elements.into_iter().next().unwrap() else {
                        unreachable!()
                    };
                    self.term_into_left_expr(term, scope)
                } else {
                    self.errors.push(Error::ReferenceOfRvalue {
                        error_pos: term.pos,
                    });
                    None
                }
            }
            _ => {
                self.errors.push(Error::ReferenceOfRvalue {
                    error_pos: term.pos,
                });
                None
            }
        }
    }
    fn term_into_ty(&mut self, term: pre_ast::PTerm<'id>) -> Option<ast::Ty> {
        match term.term {
            pre_ast::Term::Identifier("int") => Some(ty!(Integer)),
            _ => todo!(),
        }
    }
}
