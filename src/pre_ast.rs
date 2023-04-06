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

use crate::ast_with_symbol;
use crate::range::Range;
use num::BigInt;

#[derive(Debug)]
pub enum UnaryOperator {
    Plus,
    Minus,
    LogicalNot,
    BitNot,
    PreInc,
    PreDec,
    PostInc,
    PostDec,
}
#[derive(Debug)]
pub enum BinaryOperator {
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
    Type,
}
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BracketKind {
    Round,
    Square,
}

pub enum Term<'id> {
    Identifier(&'id str),
    Integer(BigInt),
    Float(f64),
    String(String),
    UnaryOperation {
        operator: UnaryOperator,
        pos_operator: Range,
        operand: Option<Box<PTerm<'id>>>,
    },
    BinaryOperation {
        operator: BinaryOperator,
        pos_operator: Range,
        left_operand: Option<Box<PTerm<'id>>>,
        right_operand: Option<Box<PTerm<'id>>>,
    },
    Bracket {
        antecedent: Option<Box<PTerm<'id>>>,
        bracket_kind: BracketKind,
        elements: Vec<Result<PTerm<'id>, Range>>,
        has_trailing_comma: bool,
    },
    Assign {
        pos_equal: Range,
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
    fn into_expr(self, errors: &mut Vec<Error>) -> Result<ast_with_symbol::PExpr<'id>, ()> {
        let expr = 'expr: {
            match self.term {
                Term::Identifier(name) => ast_with_symbol::Expr::Identifier(name),
                Term::Integer(value) => ast_with_symbol::Expr::Integer(value),
                Term::Float(value) => ast_with_symbol::Expr::Float(value),
                Term::String(value) => ast_with_symbol::Expr::String(value),
                Term::Assign {
                    pos_equal,
                    left_hand_side,
                    right_hand_side,
                } => {
                    let left_hand_side = left_hand_side
                        .ok_or_else(|| errors.push(Error::EmptyLeftHandSide(pos_equal.clone())))
                        .and_then(|term| term.into_expr(errors));
                    let right_hand_side = right_hand_side
                        .ok_or_else(|| errors.push(Error::EmptyRightHandSide(pos_equal.clone())))
                        .and_then(|term| term.into_expr(errors));
                    ast_with_symbol::Expr::Call(
                        ast_with_symbol::PExpr::new(
                            pos_equal,
                            ast_with_symbol::Expr::Operator(ast_with_symbol::Operator::Assign),
                        )
                        .into(),
                        vec![right_hand_side?, left_hand_side?],
                    )
                }
                Term::UnaryOperation {
                    operator,
                    pos_operator,
                    operand,
                } => {
                    let operator = match operator {
                        UnaryOperator::Plus => ast_with_symbol::Operator::Plus,
                        UnaryOperator::Minus => ast_with_symbol::Operator::Minus,
                        UnaryOperator::LogicalNot => ast_with_symbol::Operator::LogicalNot,
                        UnaryOperator::BitNot => ast_with_symbol::Operator::BitNot,
                        UnaryOperator::PreInc => ast_with_symbol::Operator::PreInc,
                        UnaryOperator::PreDec => ast_with_symbol::Operator::PreDec,
                        UnaryOperator::PostInc => ast_with_symbol::Operator::PostInc,
                        UnaryOperator::PostDec => ast_with_symbol::Operator::PostDec,
                    };
                    let operand = operand
                        .ok_or_else(|| errors.push(Error::EmptyUnaryOperand(pos_operator.clone())))
                        .and_then(|term| term.into_expr(errors));
                    ast_with_symbol::Expr::Call(
                        ast_with_symbol::PExpr::new(
                            pos_operator,
                            ast_with_symbol::Expr::Operator(operator),
                        )
                        .into(),
                        vec![operand?],
                    )
                }
                Term::BinaryOperation {
                    operator,
                    pos_operator,
                    left_operand,
                    right_operand,
                } => {
                    let operator = match operator {
                        BinaryOperator::ForwardShift => ast_with_symbol::Operator::ForwardShift,
                        BinaryOperator::BackwardShift => ast_with_symbol::Operator::BackwardShift,
                        BinaryOperator::Mul => ast_with_symbol::Operator::Mul,
                        BinaryOperator::Div => ast_with_symbol::Operator::Div,
                        BinaryOperator::Rem => ast_with_symbol::Operator::Rem,
                        BinaryOperator::Add => ast_with_symbol::Operator::Add,
                        BinaryOperator::Sub => ast_with_symbol::Operator::Sub,
                        BinaryOperator::RightShift => ast_with_symbol::Operator::RightShift,
                        BinaryOperator::LeftShift => ast_with_symbol::Operator::LeftShift,
                        BinaryOperator::BitAnd => ast_with_symbol::Operator::BitAnd,
                        BinaryOperator::BitXor => ast_with_symbol::Operator::BitXor,
                        BinaryOperator::BitOr => ast_with_symbol::Operator::BitOr,
                        BinaryOperator::Greater => ast_with_symbol::Operator::Greater,
                        BinaryOperator::GreaterEqual => ast_with_symbol::Operator::GreaterEqual,
                        BinaryOperator::Less => ast_with_symbol::Operator::Less,
                        BinaryOperator::LessEqual => ast_with_symbol::Operator::LessEqual,
                        BinaryOperator::Equal => ast_with_symbol::Operator::Equal,
                        BinaryOperator::NotEqual => ast_with_symbol::Operator::NotEqual,
                        BinaryOperator::LogicalAnd => ast_with_symbol::Operator::LogicalAnd,
                        BinaryOperator::LogicalOr => ast_with_symbol::Operator::LogicalOr,
                        BinaryOperator::Type => {
                            let name = left_operand
                                .ok_or_else(|| {
                                    errors.push(Error::EmptyLeftHandSideDecl(pos_operator.clone()))
                                })
                                .and_then(|lhs| match lhs.term {
                                    Term::Identifier(name) => Ok(name),
                                    _ => {
                                        errors.push(Error::InvalidLeftHandSideDecl(lhs.pos));
                                        Err(())
                                    }
                                });
                            let ty = right_operand.map(|term| term.into_ty(errors)).transpose();
                            break 'expr ast_with_symbol::Expr::Decl(name?, ty?);
                        }
                    };
                    let left_operand = left_operand
                        .ok_or_else(|| errors.push(Error::EmptyLeftOperand(pos_operator.clone())))
                        .and_then(|term| term.into_expr(errors));
                    let right_operand = right_operand
                        .ok_or_else(|| errors.push(Error::EmptyRightOperand(pos_operator.clone())))
                        .and_then(|term| term.into_expr(errors));
                    ast_with_symbol::Expr::Call(
                        ast_with_symbol::PExpr::new(
                            pos_operator,
                            ast_with_symbol::Expr::Operator(operator),
                        )
                        .into(),
                        vec![left_operand?, right_operand?],
                    )
                }
                Term::Bracket {
                    antecedent: Some(func),
                    bracket_kind: BracketKind::Round,
                    elements: args,
                    has_trailing_comma: _,
                } => {
                    let func = func.into_expr(errors);
                    let mut iter = args.into_iter().map(|arg| match arg {
                        Ok(expr) => expr.into_expr(errors),
                        Err(pos_comma) => {
                            errors.push(Error::EmptyArgument(pos_comma));
                            Err(())
                        }
                    });
                    let args: Result<_, _> = iter.by_ref().collect();
                    for _ in iter {}
                    ast_with_symbol::Expr::Call(func?.into(), args?)
                }
                Term::Bracket {
                    antecedent: None,
                    bracket_kind: BracketKind::Round,
                    mut elements,
                    has_trailing_comma: false,
                } if elements.len() == 1 => {
                    return unsafe { elements.pop().unwrap_unchecked().unwrap_unchecked() }
                        .into_expr(errors)
                }
                Term::Bracket { .. } => todo!(),
            }
        };
        Ok(ast_with_symbol::PExpr::new(self.pos, expr))
    }
    fn into_ty(self, errors: &mut Vec<Error>) -> Result<ast_with_symbol::PTy<'id>, ()> {
        let ty = match self.term {
            Term::Identifier(name) => ast_with_symbol::Ty::Name(name, vec![]),
            Term::Bracket {
                antecedent,
                bracket_kind: BracketKind::Square,
                elements: args,
                has_trailing_comma: _,
            } => {
                let name = match antecedent.unwrap().term {
                    Term::Identifier(name) => name,
                    _ => panic!(),
                };
                let args: Result<_, _> = args
                    .into_iter()
                    .map(|arg| arg.unwrap().into_ty(errors))
                    .collect();
                ast_with_symbol::Ty::Name(name, args?)
            }
            _ => panic!(),
        };
        Ok(ast_with_symbol::PTy::new(self.pos, ty))
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
    fn into_ast(self, errors: &mut Vec<Error>) -> Result<ast_with_symbol::PStmt<'id>, ()> {
        let stmt = match self.stmt {
            Stmt::Term(Some(term)) => ast_with_symbol::Stmt::Expr(Some(term.into_expr(errors)?)),
            Stmt::Term(None) => ast_with_symbol::Stmt::Expr(None),
            Stmt::Return(Some(term)) => {
                ast_with_symbol::Stmt::Return(Some(term.into_expr(errors)?))
            }
            Stmt::Return(None) => ast_with_symbol::Stmt::Return(None),
            Stmt::If {
                cond,
                pos_if,
                stmt_then,
                pos_else,
                stmt_else,
            } => {
                let cond = cond
                    .ok_or_else(|| errors.push(Error::EmptyConditionIf(pos_if.clone())))
                    .and_then(|term| term.into_expr(errors));
                let stmt_then = stmt_then
                    .ok_or_else(|| errors.push(Error::EmptyStatementIf(pos_if.clone())))
                    .and_then(|stmt| stmt.into_ast(errors));
                let stmt_else = pos_else
                    .map(|pos_else| {
                        stmt_else
                            .ok_or_else(|| errors.push(Error::EmptyStatementElse(pos_else)))
                            .and_then(|stmt| stmt.into_ast(errors))
                    })
                    .transpose();
                ast_with_symbol::Stmt::If(cond?, stmt_then?.into(), stmt_else?.map(Box::new))
            }
            Stmt::While {
                cond,
                pos_while,
                stmt,
            } => {
                let cond = cond
                    .ok_or_else(|| errors.push(Error::EmptyConditionWhile(pos_while.clone())))
                    .and_then(|term| term.into_expr(errors));
                let stmt = stmt
                    .ok_or_else(|| errors.push(Error::EmptyStatementWhile(pos_while.clone())))
                    .and_then(|stmt| stmt.into_ast(errors));
                ast_with_symbol::Stmt::While(cond?, stmt?.into())
            }
            Stmt::Block {
                antecedent: None,
                stmts,
            } => {
                let mut iter = stmts.into_iter().map(|stmt| stmt.into_ast(errors));
                let stmts: Result<_, _> = iter.by_ref().collect();
                for _ in iter {}
                ast_with_symbol::Stmt::Block(stmts?)
            }
            Stmt::Block {
                antecedent: Some(func),
                stmts,
            } => match func.term {
                Term::BinaryOperation {
                    operator: BinaryOperator::Type,
                    pos_operator,
                    left_operand: Some(name_and_args),
                    right_operand: Some(ret_ty),
                } => match name_and_args.term {
                    Term::Bracket {
                        antecedent: Some(name),
                        bracket_kind: BracketKind::Round,
                        elements: args,
                        has_trailing_comma: _,
                    } => {
                        let name = match name.term {
                            Term::Identifier(name) => Ok(name),
                            _ => {
                                errors.push(Error::InvalidFunctionName(name.pos));
                                Err(())
                            }
                        };
                        let mut args_iter = args.into_iter().map(|arg| match arg {
                            Ok(PTerm { term, pos }) => match term {
                                Term::BinaryOperation {
                                    operator: BinaryOperator::Type,
                                    pos_operator,
                                    left_operand: Some(arg),
                                    right_operand: Some(arg_ty),
                                } => {
                                    let arg_name = match arg.term {
                                        Term::Identifier(arg_name) => Ok(arg_name),
                                        _ => Err(()),
                                    };
                                    let arg_ty = arg_ty.into_ty(errors);
                                    match (arg_name, arg_ty) {
                                        (Ok(arg_name), Ok(arg_ty)) => Ok((arg_name, Some(arg_ty))),
                                        _ => Err(()),
                                    }
                                }
                                _ => Err(()),
                            },
                            Err(pos_comma) => {
                                errors.push(Error::EmptyArgumentInDef(pos_comma));
                                Err(())
                            }
                        });
                        let args: Result<_, _> = args_iter.by_ref().collect();
                        for _ in args_iter {}
                        let mut stmts_iter = stmts.into_iter().map(|stmt| stmt.into_ast(errors));
                        let stmts: Result<_, _> = stmts_iter.by_ref().collect();
                        for _ in stmts_iter {}
                        let ret_ty = ret_ty.into_ty(errors);
                        ast_with_symbol::Stmt::Def {
                            name: name?,
                            args: args?,
                            ret_ty: Some(ret_ty?),
                            body: stmts?,
                        }
                    }
                    _ => todo!(),
                },
                _ => {
                    errors.push(Error::UnexpectedExpressionBeforeBlock(func.pos));
                    return Err(());
                }
            },
        };
        Ok(ast_with_symbol::PStmt::new(self.pos, stmt))
    }
}

pub fn into_ast(pre_ast: Vec<PStmt>) -> Result<Vec<ast_with_symbol::PStmt>, Vec<Error>> {
    let mut errors = Vec::new();
    let mut iter = pre_ast.into_iter().map(|stmt| stmt.into_ast(&mut errors));
    let ast: Result<_, _> = iter.by_ref().collect();
    for _ in iter {}
    ast.map_err(|()| errors)
}

#[derive(Debug)]
pub enum Error {
    EmptyUnaryOperand(Range),
    EmptyLeftOperand(Range),
    EmptyRightOperand(Range),
    EmptyLeftHandSide(Range),
    EmptyRightHandSide(Range),
    EmptyLeftHandSideDecl(Range),
    InvalidLeftHandSideDecl(Range),
    InvalidCompoundOperator(Range),
    EmptyArgument(Range),
    EmptyConditionIf(Range),
    EmptyStatementIf(Range),
    EmptyStatementElse(Range),
    EmptyConditionWhile(Range),
    EmptyStatementWhile(Range),
    UnexpectedExpressionBeforeBlock(Range),
    InvalidFunctionName(Range),
    InvalidArgumentInDef(Range),
    EmptyArgumentInDef(Range),
}
