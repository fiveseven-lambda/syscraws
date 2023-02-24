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

pub enum Term {
    Identifier(String),
    Integer(BigInt),
    Float(f64),
    String(String),
    UnaryOperation {
        operator: UnaryOperator,
        pos_operator: Range,
        operand: Option<Box<PTerm>>,
    },
    BinaryOperation {
        operator: BinaryOperator,
        pos_operator: Range,
        left_operand: Option<Box<PTerm>>,
        right_operand: Option<Box<PTerm>>,
    },
    Bracket {
        antecedent: Option<Box<PTerm>>,
        bracket_kind: BracketKind,
        elements: Vec<Result<PTerm, Range>>,
        has_trailing_comma: bool,
    },
    Assign {
        pos_equal: Range,
        left_hand_side: Option<Box<PTerm>>,
        right_hand_side: Option<Box<PTerm>>,
    },
}
pub struct PTerm {
    pos: Range,
    term: Term,
}
impl PTerm {
    pub fn new(pos: Range, term: Term) -> PTerm {
        PTerm { pos, term }
    }
    pub fn pos(self) -> Range {
        self.pos
    }
    pub fn into_expr(self, errors: &mut Vec<Error>) -> Result<ast::PExpr, ()> {
        let expr = match self.term {
            Term::Identifier(name) => ast::Expr::Identifier(name),
            Term::Integer(value) => ast::Expr::Integer(value),
            Term::Float(value) => ast::Expr::Float(value),
            Term::String(value) => ast::Expr::String(value),
            Term::Assign {
                pos_equal,
                left_hand_side,
                right_hand_side,
            } => {
                let left_hand_side = left_hand_side
                    .ok_or_else(|| errors.push(Error::EmptyLeftHandSide(pos_equal.clone())));
                let right_hand_side = right_hand_side
                    .ok_or_else(|| errors.push(Error::EmptyRightHandSide(pos_equal.clone())))
                    .and_then(|term| term.into_expr(errors));
                let left_hand_side = left_hand_side?;
                match left_hand_side.term {
                    Term::BinaryOperation {
                        operator: BinaryOperator::Type,
                        left_operand,
                        ..
                    } => {
                        let name = left_operand
                            .ok_or_else(|| errors.push(Error::EmptyLeftHandSideDecl(pos_equal)))
                            .and_then(|operand| {
                                let PTerm { pos, term } = *operand;
                                match term {
                                    Term::Identifier(name) => Ok(name),
                                    _ => {
                                        errors.push(Error::InvalidLeftHandSideDecl(pos));
                                        Err(())
                                    }
                                }
                            })?;
                        ast::Expr::Decl(name, Some(right_hand_side?.into()))
                    }
                    Term::BinaryOperation {
                        right_operand: None,
                        operator,
                        pos_operator,
                        left_operand,
                    } => {
                        let pos_operator = pos_operator + pos_equal;
                        let operator = match operator {
                            BinaryOperator::ForwardShift => Ok(ast::Operator::ForwardShiftAssign),
                            BinaryOperator::BackwardShift => Ok(ast::Operator::BackwardShiftAssign),
                            BinaryOperator::Mul => Ok(ast::Operator::MulAssign),
                            BinaryOperator::Div => Ok(ast::Operator::DivAssign),
                            BinaryOperator::Rem => Ok(ast::Operator::RemAssign),
                            BinaryOperator::Add => Ok(ast::Operator::AddAssign),
                            BinaryOperator::Sub => Ok(ast::Operator::SubAssign),
                            BinaryOperator::RightShift => Ok(ast::Operator::RightShiftAssign),
                            BinaryOperator::LeftShift => Ok(ast::Operator::LeftShiftAssign),
                            BinaryOperator::BitAnd => Ok(ast::Operator::BitAndAssign),
                            BinaryOperator::BitXor => Ok(ast::Operator::BitXorAssign),
                            BinaryOperator::BitOr => Ok(ast::Operator::BitOrAssign),
                            _ => {
                                errors.push(Error::InvalidCompoundOperator(pos_operator.clone()));
                                Err(())
                            }
                        };
                        let left_operand = left_operand
                            .ok_or_else(|| {
                                errors.push(Error::EmptyLeftOperand(pos_operator.clone()))
                            })
                            .and_then(|term| term.into_expr(errors));
                        ast::Expr::Call(
                            ast::PExpr::new(pos_operator, ast::Expr::Operator(operator?)).into(),
                            vec![left_operand?, right_hand_side?],
                        )
                    }
                    _ => {
                        let left_hand_side = left_hand_side.into_expr(errors);
                        ast::Expr::Call(
                            ast::PExpr::new(pos_equal, ast::Expr::Operator(ast::Operator::Assign))
                                .into(),
                            vec![left_hand_side?, right_hand_side?],
                        )
                    }
                }
            }
            Term::UnaryOperation {
                operator,
                pos_operator,
                operand,
            } => {
                let operator = match operator {
                    UnaryOperator::Plus => ast::Operator::Plus,
                    UnaryOperator::Minus => ast::Operator::Minus,
                    UnaryOperator::LogicalNot => ast::Operator::LogicalNot,
                    UnaryOperator::BitNot => ast::Operator::BitNot,
                    UnaryOperator::PreInc => ast::Operator::PreInc,
                    UnaryOperator::PreDec => ast::Operator::PreDec,
                    UnaryOperator::PostInc => ast::Operator::PostInc,
                    UnaryOperator::PostDec => ast::Operator::PostDec,
                };
                let operand = operand
                    .ok_or_else(|| errors.push(Error::EmptyUnaryOperand(pos_operator.clone())))
                    .and_then(|term| term.into_expr(errors));
                ast::Expr::Call(
                    ast::PExpr::new(pos_operator, ast::Expr::Operator(operator)).into(),
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
                    BinaryOperator::ForwardShift => ast::Operator::ForwardShift,
                    BinaryOperator::BackwardShift => ast::Operator::BackwardShift,
                    BinaryOperator::Mul => ast::Operator::Mul,
                    BinaryOperator::Div => ast::Operator::Div,
                    BinaryOperator::Rem => ast::Operator::Rem,
                    BinaryOperator::Add => ast::Operator::Add,
                    BinaryOperator::Sub => ast::Operator::Sub,
                    BinaryOperator::RightShift => ast::Operator::RightShift,
                    BinaryOperator::LeftShift => ast::Operator::LeftShift,
                    BinaryOperator::BitAnd => ast::Operator::BitAnd,
                    BinaryOperator::BitXor => ast::Operator::BitXor,
                    BinaryOperator::BitOr => ast::Operator::BitOr,
                    BinaryOperator::Greater => ast::Operator::Greater,
                    BinaryOperator::GreaterEqual => ast::Operator::GreaterEqual,
                    BinaryOperator::Less => ast::Operator::Less,
                    BinaryOperator::LessEqual => ast::Operator::LessEqual,
                    BinaryOperator::Equal => ast::Operator::Equal,
                    BinaryOperator::NotEqual => ast::Operator::NotEqual,
                    BinaryOperator::LogicalAnd => ast::Operator::LogicalAnd,
                    BinaryOperator::LogicalOr => ast::Operator::LogicalOr,
                    BinaryOperator::Type => ast::Operator::Cast,
                };
                let left_operand = left_operand
                    .ok_or_else(|| errors.push(Error::EmptyLeftOperand(pos_operator.clone())))
                    .and_then(|term| term.into_expr(errors));
                let right_operand = right_operand
                    .ok_or_else(|| errors.push(Error::EmptyRightOperand(pos_operator.clone())))
                    .and_then(|term| term.into_expr(errors));
                ast::Expr::Call(
                    ast::PExpr::new(pos_operator, ast::Expr::Operator(operator)).into(),
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
                ast::Expr::Call(func?.into(), args?)
            }
            Term::Bracket { .. } => todo!(),
        };
        Ok(ast::PExpr::new(self.pos, expr))
    }
}

pub enum Stmt {
    Term(Option<PTerm>),
    Return(Option<PTerm>),
    If {
        cond: Option<PTerm>,
        pos_if: Range,
        stmt_then: Option<Box<PStmt>>,
        pos_else: Option<Range>,
        stmt_else: Option<Box<PStmt>>,
    },
    While {
        cond: Option<PTerm>,
        pos_while: Range,
        stmt: Option<Box<PStmt>>,
    },
    Block {
        antecedent: Option<PTerm>,
        stmts: Vec<PStmt>,
    },
}

pub struct PStmt {
    pos: Range,
    stmt: Stmt,
}
impl PStmt {
    pub fn new(pos: Range, stmt: Stmt) -> PStmt {
        PStmt { pos, stmt }
    }
    pub fn into_ast(self, errors: &mut Vec<Error>) -> Result<ast::PStmt, ()> {
        let stmt = match self.stmt {
            Stmt::Term(Some(term)) => ast::Stmt::Expr(Some(term.into_expr(errors)?)),
            Stmt::Term(None) => ast::Stmt::Expr(None),
            Stmt::Return(Some(term)) => ast::Stmt::Return(Some(term.into_expr(errors)?)),
            Stmt::Return(None) => ast::Stmt::Return(None),
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
                ast::Stmt::If(cond?, stmt_then?.into(), stmt_else?.map(Box::new))
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
                ast::Stmt::While(cond?, stmt?.into())
            }
            Stmt::Block {
                antecedent: None,
                stmts,
            } => {
                let mut iter = stmts.into_iter().map(|stmt| stmt.into_ast(errors));
                let stmts: Result<_, _> = iter.by_ref().collect();
                for _ in iter {}
                ast::Stmt::Block(stmts?)
            }
            Stmt::Block {
                antecedent: Some(func),
                stmts,
            } => match func.term {
                Term::Bracket {
                    antecedent: name,
                    bracket_kind: BracketKind::Round,
                    elements: args,
                    has_trailing_comma: _,
                } => {
                    let name = name
                        .ok_or_else(|| errors.push(Error::EmptyFunctionName(func.pos)))
                        .and_then(|name| match name.term {
                            Term::Identifier(name) => Ok(name),
                            _ => {
                                errors.push(Error::InvalidFunctionName(name.pos));
                                Err(())
                            }
                        });
                    let mut args_iter = args.into_iter().map(|arg| match arg {
                        Ok(PTerm {
                            term: Term::Identifier(arg),
                            ..
                        }) => Ok(arg),
                        Ok(PTerm { pos, .. }) => {
                            errors.push(Error::InvalidArgumentInDef(pos));
                            Err(())
                        }
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
                    ast::Stmt::Def {
                        name: name?,
                        args: args?,
                        body: stmts?,
                    }
                }
                _ => {
                    errors.push(Error::UnexpectedExpressionBeforeBlock(func.pos));
                    return Err(());
                }
            },
        };
        Ok(ast::PStmt::new(self.pos, stmt))
    }
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
    EmptyFunctionName(Range),
    InvalidFunctionName(Range),
    InvalidArgumentInDef(Range),
    EmptyArgumentInDef(Range),
}
