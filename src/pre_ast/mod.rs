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

use super::{ast, ty};
use crate::range::Range;
use enum_iterator::Sequence;
use num::BigInt;

mod debug_print;
pub use debug_print::_debug_print;
mod map;

#[derive(Debug, Sequence)]
pub enum Operator {
    Plus,
    Minus,
    LogicalNot,
    BitNot,
    PreInc,
    PreDec,
    PostInc,
    PostDec,
    ForwardShift,
    BackwardShift,
    Mul,
    Div,
    Rem,
    Add,
    Sub,
    RightShift,
    LeftShift,
    BitAnd,
    BitXor,
    BitOr,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Equal,
    NotEqual,
    LogicalAnd,
    LogicalOr,
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    RemAssign,
    RightShiftAssign,
    LeftShiftAssign,
    ForwardShiftAssign,
    BackwardShiftAssign,
    BitAndAssign,
    BitXorAssign,
    BitOrAssign,
}

pub enum Term<'id> {
    Identifier(&'id str),
    Integer(BigInt),
    Float(f64),
    String(String),
    UnaryOperation {
        operator: Operator,
        pos_operator: Range,
        operand: Option<Box<PTerm<'id>>>,
    },
    BinaryOperation {
        operator: Operator,
        pos_operator: Range,
        left_operand: Option<Box<PTerm<'id>>>,
        right_operand: Option<Box<PTerm<'id>>>,
    },
    TypeAnnotation {
        pos_colon: Range,
        term: Option<Box<PTerm<'id>>>,
        ty: Option<Box<PTerm<'id>>>,
    },
    Parenthesized {
        antecedent: Option<Box<PTerm<'id>>>,
        elements: Vec<Result<PTerm<'id>, Range>>,
        has_trailing_comma: bool,
    },
    Assignment {
        operator: Operator,
        pos_operator: Range,
        left_hand_side: Option<Box<PTerm<'id>>>,
        right_hand_side: Option<Box<PTerm<'id>>>,
    },
}
pub struct PTerm<'id> {
    pos: Range,
    term: Term<'id>,
}
impl<'id> PTerm<'id> {
    pub fn new(pos: Range, term: Term) -> PTerm {
        PTerm { pos, term }
    }
    pub fn pos(self) -> Range {
        self.pos
    }
}

pub enum Stmt<'id> {
    Term(Option<PTerm<'id>>),
    Return(Option<PTerm<'id>>),
    If {
        cond: Option<PTerm<'id>>,
        pos_if: Range,
        stmt_then: Option<Box<PStmt<'id>>>,
        pos_else: Option<Range>,
        stmt_else: Option<Box<PStmt<'id>>>,
    },
    While {
        cond: Option<PTerm<'id>>,
        pos_while: Range,
        stmt: Option<Box<PStmt<'id>>>,
    },
    Block {
        antecedent: Option<PTerm<'id>>,
        stmts: Vec<PStmt<'id>>,
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

impl<'id> PTerm<'id> {
    fn into_expr(
        self,
        variables: &mut map::Variables<'id>,
        globals: Option<&map::Variables<'id>>,
        funcs: &mut map::Funcs<'id>,
        scope: &mut Vec<&'id str>,
        errors: &mut Vec<Error>,
    ) -> Result<ast::Expr, ()> {
        match self.term {
            Term::Integer(value) => Ok(ast::Expr::Integer(value)),
            Term::Float(value) => Ok(ast::Expr::Float(value)),
            Term::String(value) => Ok(ast::Expr::String(value)),
            Term::TypeAnnotation {
                pos_colon,
                term,
                ty,
            } => {
                let name = match term {
                    Some(term) => match term.term {
                        Term::Identifier(name) => Ok(name),
                        _ => {
                            errors.push(Error::InvalidLeftHandSideDecl {
                                term: term.pos,
                                colon: pos_colon,
                            });
                            Err(())
                        }
                    },
                    _ => {
                        errors.push(Error::EmptyLeftHandSideDecl { colon: pos_colon });
                        Err(())
                    }
                };
                let ty = ty.map(|term| term.into_ty()).transpose();
                let name = name?;
                let id = variables.insert(name, ty?);
                scope.push(name);
                Ok(ast::Expr::Variable(id))
            }
            Term::Identifier(name) => {
                if let Some(id) = variables.get(name) {
                    Ok(ast::Expr::Variable(id))
                } else if let Some(id) = globals.and_then(|variables| variables.get(name)) {
                    Ok(ast::Expr::Global(id))
                } else {
                    Ok(ast::Expr::Func(funcs.get_or_insert(name)))
                }
            }
            Term::Parenthesized {
                antecedent: Some(func),
                elements: args,
                has_trailing_comma: _,
            } => {
                let func = func.into_expr(variables, globals, funcs, scope, errors);
                let mut iter = args.into_iter().map(|arg| match arg {
                    Ok(expr) => expr.into_expr(variables, globals, funcs, scope, errors),
                    Err(pos_comma) => {
                        errors.push(Error::EmptyArgument { pos_comma });
                        Err(())
                    }
                });
                let args: Result<_, _> = iter.by_ref().collect();
                for _ in iter {}
                Ok(ast::Expr::Call(func?.into(), args?))
            }
            Term::Parenthesized {
                antecedent: None,
                elements,
                has_trailing_comma,
            } => {
                if elements.len() == 1 && !has_trailing_comma {
                    unsafe {
                        elements
                            .into_iter()
                            .next()
                            .unwrap_unchecked()
                            .unwrap_unchecked()
                    }
                    .into_expr(variables, globals, funcs, scope, errors)
                } else {
                    todo!()
                }
            }
            Term::UnaryOperation {
                operator,
                pos_operator,
                operand,
            } => {
                let operand = operand
                    .ok_or_else(|| errors.push(Error::EmptyUnaryOperand(pos_operator.clone())))
                    .and_then(|term| term.into_expr(variables, globals, funcs, scope, errors));
                Ok(ast::Expr::Call(
                    ast::Expr::Func(operator as usize).into(),
                    vec![operand?],
                ))
            }
            Term::BinaryOperation {
                operator,
                pos_operator,
                left_operand,
                right_operand,
            } => {
                let left_operand = left_operand
                    .ok_or_else(|| errors.push(Error::EmptyLeftOperand(pos_operator.clone())))
                    .and_then(|term| term.into_expr(variables, globals, funcs, scope, errors));
                let right_operand = right_operand
                    .ok_or_else(|| errors.push(Error::EmptyRightOperand(pos_operator.clone())))
                    .and_then(|term| term.into_expr(variables, globals, funcs, scope, errors));
                Ok(ast::Expr::Call(
                    ast::Expr::Func(operator as usize).into(),
                    vec![left_operand?, right_operand?],
                ))
            }
            Term::Assignment {
                operator,
                pos_operator,
                left_hand_side,
                right_hand_side,
            } => {
                let right_hand_side = right_hand_side
                    .ok_or_else(|| errors.push(Error::EmptyRightHandSide(pos_operator.clone())))
                    .and_then(|term| term.into_expr(variables, globals, funcs, scope, errors));
                let left_hand_side = left_hand_side
                    .ok_or_else(|| errors.push(Error::EmptyLeftHandSide(pos_operator.clone())))
                    .and_then(|term| term.into_expr(variables, globals, funcs, scope, errors));
                Ok(ast::Expr::Call(
                    ast::Expr::Func(operator as usize).into(),
                    vec![right_hand_side?, left_hand_side?],
                ))
            }
        }
    }
    fn into_ty(self) -> Result<ty::Ty, ()> {
        match self.term {
            Term::Identifier("int") => Ok(ty::Ty::integer()),
            Term::Identifier("float") => Ok(ty::Ty::float()),
            _ => Err(()),
        }
    }
}
impl<'id> PStmt<'id> {
    fn into_ast(
        self,
        variables: &mut map::Variables<'id>,
        globals: Option<&map::Variables<'id>>,
        funcs: &mut map::Funcs<'id>,
        scope: &mut Vec<&'id str>,
        errors: &mut Vec<Error>,
    ) -> Result<ast::StmtWithSize, ()> {
        match self.stmt {
            Stmt::Term(Some(term)) => Ok(ast::StmtWithSize::new_expr(
                term.into_expr(variables, globals, funcs, scope, errors)?,
            )),
            _ => todo!(),
        }
    }
}

pub fn into_ast(stmts: Vec<PStmt>) -> Result<(Vec<Vec<ast::Func>>, Vec<ast::FuncDef>), Vec<Error>> {
    let mut global_variables = map::Variables::new();
    let mut funcs = map::Funcs::new();
    let mut global_scope = Vec::new();
    let mut errors = Vec::new();
    let mut has_error = false;

    let mut toplevel_stmts = Vec::new();

    fn func_arg<'id>(
        arg: Result<PTerm<'id>, Range>,
        errors: &mut Vec<Error>,
    ) -> Result<(&'id str, Option<ty::Ty>), ()> {
        match arg {
            Ok(PTerm { term, pos }) => match term {
                Term::TypeAnnotation {
                    pos_colon: _,
                    term,
                    ty,
                } => {
                    let name = term.ok_or(()).and_then(|term| match term.term {
                        Term::Identifier(name) => Ok(name),
                        _ => {
                            errors.push(Error::InvalidArgumentInDef(term.pos));
                            Err(())
                        }
                    });
                    let ty = ty.ok_or(()).and_then(|ty| ty.into_ty());
                    Ok((name?, Some(ty?)))
                }
                Term::Identifier(name) => Ok((name, None)),
                _ => {
                    errors.push(Error::InvalidArgumentInDef(pos));
                    Err(())
                }
            },
            Err(pos_comma) => {
                errors.push(Error::EmptyArgument { pos_comma });
                Err(())
            }
        }
    }
    fn func_def<'id>(
        sig: PTerm<'id>,
        body: Vec<PStmt<'id>>,
        globals: &map::Variables<'id>,
        funcs: &mut map::Funcs<'id>,
        errors: &mut Vec<Error>,
    ) -> Result<(), ()> {
        let (name, args, ret_ty) = match sig.term {
            Term::TypeAnnotation {
                pos_colon: _,
                term: Some(name_and_args),
                ty: Some(ret_ty),
            } => match name_and_args.term {
                Term::Parenthesized {
                    antecedent: Some(name),
                    elements: args,
                    has_trailing_comma: _,
                } => (name, args, Some(ret_ty)),
                _ => {
                    errors.push(Error::UnexpectedExpressionBeforeBlock(sig.pos));
                    return Err(());
                }
            },
            Term::Parenthesized {
                antecedent: Some(name),
                elements: args,
                has_trailing_comma: _,
            } => (name, args, None),
            _ => {
                errors.push(Error::UnexpectedExpressionBeforeBlock(sig.pos));
                return Err(());
            }
        };
        let name = match name.term {
            Term::Identifier(name) => Ok(name),
            _ => {
                errors.push(Error::InvalidFunctionName(name.pos));
                Err(())
            }
        };
        let mut args_iter = args.into_iter().map(|arg| func_arg(arg, errors));
        let args: Result<Vec<_>, _> = args_iter.by_ref().collect();
        for _ in args_iter {}

        let mut local_variables = map::Variables::new();

        let name = name?;
        let args = args?;
        let num_args = args.len();
        for (name, ty) in args {
            local_variables.insert(name, ty);
        }

        let mut scope = Vec::new();
        let mut body_iter = body.into_iter().map(|stmt| {
            stmt.into_ast(
                &mut local_variables,
                Some(globals),
                funcs,
                &mut scope,
                errors,
            )
        });
        let body: Result<Vec<_>, _> = body_iter.by_ref().collect();
        for _ in body_iter {}

        let ret_ty = ret_ty.map(|ty| ty.into_ty()).transpose();

        let id = funcs.get_or_insert(name);
        funcs.add_def(
            id,
            ast::FuncDef::new(num_args, local_variables.tys(), ret_ty?, body?),
        );
        Ok(())
    }

    for stmt in stmts {
        match stmt.stmt {
            Stmt::Block {
                antecedent: Some(func),
                stmts: body,
            } => {
                has_error |=
                    func_def(func, body, &global_variables, &mut funcs, &mut errors).is_err();
            }
            _ => match stmt.into_ast(
                &mut global_variables,
                None,
                &mut funcs,
                &mut global_scope,
                &mut errors,
            ) {
                Ok(stmt) => toplevel_stmts.push(stmt),
                Err(()) => has_error = true,
            },
        }
    }
    assert_eq!(has_error, !errors.is_empty());
    if has_error {
        return Err(errors);
    }
    let (_, overloads, mut defs) = funcs.into_inner();
    defs.push(ast::FuncDef::new(
        0,
        global_variables.tys(),
        Some(ty::Ty::tuple(vec![])),
        toplevel_stmts,
    ));
    Ok((overloads, defs))
}

#[derive(Debug)]
pub enum Error {
    EmptyUnaryOperand(Range),
    EmptyLeftOperand(Range),
    EmptyRightOperand(Range),
    EmptyLeftHandSide(Range),
    EmptyRightHandSide(Range),
    EmptyLeftHandSideDecl { colon: Range },
    InvalidLeftHandSideDecl { term: Range, colon: Range },
    EmptyArgument { pos_comma: Range },
    EmptyConditionIf(Range),
    EmptyStatementIf(Range),
    EmptyStatementElse(Range),
    EmptyConditionWhile(Range),
    EmptyStatementWhile(Range),
    UnexpectedExpressionBeforeBlock(Range),
    InvalidFunctionName(Range),
    InvalidArgumentInDef(Range),
}