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

use crate::pre_ast::{Operator, PStmt, PTerm, Stmt, Term};
use std::iter;
mod lexer;
use lexer::Lexer;
mod token;
use token::Token;
mod chars_peekable;
use chars_peekable::CharsPeekable;
mod error;
pub use error::Error;

pub fn parse(input: &str) -> Result<Vec<PStmt>, Error> {
    let mut chars = CharsPeekable::new(input);
    let mut lexer = Lexer::new(&mut chars)?;
    iter::from_fn(|| parse_stmt(&mut lexer).transpose()).collect()
}

fn parse_stmt<'id>(lexer: &mut Lexer<'id, '_>) -> Result<Option<PStmt<'id>>, Error> {
    let first_token_start = lexer.next_start();
    let Some(ref mut first_token) = lexer.peeked else {
        return Ok(None);
    };
    match first_token {
        Token::KeywordIf => {
            lexer.consume()?;
            let if_pos = lexer.range_from(first_token_start);
            let open_start = lexer.next_start();
            lexer.consume_if_or_err(
                |token| matches!(token, Token::OpeningParenthesis),
                if_pos.clone(),
            )?;
            let open_pos = lexer.range_from(open_start);
            let cond = parse_assign(lexer)?;
            lexer
                .consume_if_or_err(|token| matches!(token, Token::ClosingParenthesis), open_pos)?;
            let stmt_then = parse_stmt(lexer)?;
            let (else_pos, stmt_else) = if let Some(Token::KeywordElse) = lexer.peeked {
                let else_start = lexer.next_start();
                lexer.consume()?;
                let else_pos = lexer.range_from(else_start);
                (Some(else_pos), parse_stmt(lexer)?)
            } else {
                (None, None)
            };
            let pos = lexer.range_from(first_token_start);
            Ok(Some(PStmt::new(
                pos,
                Stmt::If {
                    cond,
                    if_pos,
                    stmt_then: stmt_then.map(Box::new),
                    else_pos,
                    stmt_else: stmt_else.map(Box::new),
                },
            )))
        }
        Token::KeywordWhile => {
            lexer.consume()?;
            let while_pos = lexer.range_from(first_token_start);
            let open_start = lexer.next_start();
            lexer.consume_if_or_err(
                |token| matches!(token, Token::OpeningParenthesis),
                while_pos.clone(),
            )?;
            let open_pos = lexer.range_from(open_start);
            let cond = parse_assign(lexer)?;
            lexer
                .consume_if_or_err(|token| matches!(token, Token::ClosingParenthesis), open_pos)?;
            let stmt = parse_stmt(lexer)?;
            let pos = lexer.range_from(first_token_start);
            Ok(Some(PStmt::new(
                pos,
                Stmt::While {
                    cond,
                    while_pos,
                    stmt: stmt.map(Box::new),
                },
            )))
        }
        Token::KeywordReturn => {
            lexer.consume()?;
            let term = parse_assign(lexer)?;
            let pos_excluding_semicolon = lexer.range_from(first_token_start);
            lexer.consume_if_or_err(
                |token| matches!(token, Token::Semicolon),
                pos_excluding_semicolon,
            )?;
            let pos = lexer.range_from(first_token_start);
            Ok(Some(PStmt::new(pos, Stmt::Return(term))))
        }
        _ => {
            let term = parse_assign(lexer)?;
            match lexer.peeked {
                Some(Token::Semicolon) => {
                    lexer.consume()?;
                    let pos = lexer.range_from(first_token_start);
                    Ok(Some(PStmt::new(pos, Stmt::Term(term))))
                }
                Some(Token::OpeningBrace) => {
                    let open_start = lexer.next_start();
                    lexer.consume()?;
                    let open_pos = lexer.range_from(open_start);
                    let mut stmts = Vec::new();
                    loop {
                        if let Some(Token::ClosingBrace) = lexer.peeked {
                            lexer.consume()?;
                            break;
                        } else if let Some(stmt) = parse_stmt(lexer)? {
                            stmts.push(stmt);
                        } else {
                            return Err(Error::EOFAfter {
                                reason_pos: open_pos,
                            });
                        }
                    }
                    let pos = lexer.range_from(first_token_start);
                    Ok(Some(PStmt::new(
                        pos,
                        Stmt::Block {
                            antecedent: term,
                            stmts,
                        },
                    )))
                }
                Some(_) => {
                    let error_start = lexer.next_start();
                    lexer.consume()?;
                    let error_pos = lexer.range_from(error_start);
                    match term {
                        Some(term) => Err(Error::UnexpectedTokenAfter {
                            error_pos,
                            reason_pos: term.pos(),
                        }),
                        None => Err(Error::UnexpectedToken { error_pos }),
                    }
                }
                None => Err(Error::EOFAfter {
                    reason_pos: term.unwrap().pos(),
                }),
            }
        }
    }
}

pub fn parse_assign<'id>(lexer: &mut Lexer<'id, '_>) -> Result<Option<PTerm<'id>>, Error> {
    let start = lexer.next_start();
    let left_hand_side = parse_binary_operator(lexer)?;
    if let Some(operator) = lexer.peeked.as_ref().and_then(assignment_operator) {
        let operator_start = lexer.next_start();
        lexer.consume()?;
        let operator_pos = lexer.range_from(operator_start);
        let right_hand_side = parse_assign(lexer)?;
        let pos = lexer.range_from(start);
        Ok(Some(PTerm::new(
            pos,
            Term::Assignment {
                operator,
                operator_pos,
                left_hand_side: left_hand_side.map(Box::new),
                right_hand_side: right_hand_side.map(Box::new),
            },
        )))
    } else {
        Ok(left_hand_side)
    }
}

fn parse_binary_operator<'id>(lexer: &mut Lexer<'id, '_>) -> Result<Option<PTerm<'id>>, Error> {
    parse_binary_operator_rec(lexer, Precedence::first())
}

fn parse_binary_operator_rec<'id>(
    lexer: &mut Lexer<'id, '_>,
    precedence: Option<Precedence>,
) -> Result<Option<PTerm<'id>>, Error> {
    let Some(precedence) = precedence else {
        return parse_factor(lexer);
    };
    let start = lexer.next_start();
    let mut left_operand = parse_binary_operator_rec(lexer, precedence.next())?;
    while let Some(operator) = lexer
        .peeked
        .as_ref()
        .and_then(|token| infix_operator(token, precedence))
    {
        let operator_start = lexer.next_start();
        lexer.consume()?;
        let operator_pos = lexer.range_from(operator_start);
        let right_operand = parse_binary_operator_rec(lexer, precedence.next())?;
        let pos = lexer.range_from(start);
        left_operand = Some(PTerm::new(
            pos,
            Term::BinaryOperation {
                operator_pos,
                left_operand: left_operand.map(Box::new),
                operator,
                right_operand: right_operand.map(Box::new),
            },
        ));
    }
    Ok(left_operand)
}

fn parse_factor<'id>(lexer: &mut Lexer<'id, '_>) -> Result<Option<PTerm<'id>>, Error> {
    let first_token_start = lexer.next_start();
    let Some(ref mut first_token) = lexer.peeked else {
        return Ok(None);
    };
    let mut antecedent = 'ant: {
        let term = if let Token::Identifier(name) = *first_token {
            lexer.consume()?;
            Term::Identifier(name)
        } else if let Token::Number(str) = *first_token {
            lexer.consume()?;
            let value: String = str.chars().filter(|&ch| ch != '_').collect();
            if value.chars().all(|ch| ch.is_ascii_digit()) {
                Term::Integer(value.parse().unwrap())
            } else {
                Term::Float(value.parse().unwrap())
            }
        } else if let Token::String(components) = first_token {
            let components = std::mem::take(components);
            lexer.consume()?;
            Term::String(components)
        } else if let Some(operator) = prefix_operator(first_token) {
            lexer.consume()?;
            let operator_pos = lexer.range_from(first_token_start);
            let operand = parse_factor(lexer)?;
            return Ok(Some(PTerm::new(
                lexer.range_from(first_token_start),
                Term::UnaryOperation {
                    operator,
                    operator_pos,
                    operand: operand.map(Box::new),
                },
            )));
        } else {
            break 'ant None;
        };
        Some(PTerm::new(lexer.range_from(first_token_start), term))
    };
    loop {
        let term = {
            let Some(ref token) = lexer.peeked else {
                return Ok(antecedent);
            };
            let token_start = lexer.next_start();
            if let Token::OpeningParenthesis = token {
                lexer.consume()?;
                let open_pos = lexer.range_from(token_start);
                let mut elements = Vec::new();
                let has_trailing_comma;
                loop {
                    let element = parse_assign(lexer)?;
                    if let Some(Token::Comma) = lexer.peeked {
                        let comma_start = lexer.next_start();
                        lexer.consume()?;
                        let comma_pos = lexer.range_from(comma_start);
                        elements.push(element.ok_or(comma_pos));
                    } else {
                        if let Some(element) = element {
                            has_trailing_comma = false;
                            elements.push(Ok(element));
                        } else {
                            has_trailing_comma = true;
                        }
                        break;
                    }
                }
                lexer.consume_if_or_err(
                    |token| matches!(token, Token::ClosingParenthesis),
                    open_pos,
                )?;
                Term::Parenthesized {
                    antecedent: antecedent.map(Box::new),
                    elements,
                    has_trailing_comma,
                }
            } else if let Token::Colon = token {
                lexer.consume()?;
                let colon_pos = lexer.range_from(token_start);
                let ty = parse_factor(lexer)?;
                Term::TypeAnnotation {
                    colon_pos,
                    term: antecedent.map(Box::new),
                    ty: ty.map(Box::new),
                }
            } else if let Token::HyphenGreater = token {
                lexer.consume()?;
                let arrow_pos = lexer.range_from(token_start);
                let ty = parse_factor(lexer)?;
                Term::ReturnType {
                    arrow_pos,
                    term: antecedent.map(Box::new),
                    ty: ty.map(Box::new),
                }
            } else if let Some(operator) = postfix_operator(token) {
                lexer.consume()?;
                let operator_pos = lexer.range_from(token_start);
                Term::UnaryOperation {
                    operator,
                    operator_pos,
                    operand: antecedent.map(Box::new),
                }
            } else {
                return Ok(antecedent);
            }
        };
        let pos = lexer.range_from(first_token_start);
        antecedent = Some(PTerm::new(pos, term));
    }
}

fn prefix_operator(token: &Token) -> Option<Operator> {
    match token {
        Token::Plus => Some(Operator::Plus),
        Token::Hyphen => Some(Operator::Minus),
        Token::Slash => Some(Operator::Recip),
        Token::Exclamation => Some(Operator::LogicalNot),
        Token::Tilde => Some(Operator::BitNot),
        Token::DoublePlus => Some(Operator::PreInc),
        Token::DoubleHyphen => Some(Operator::PreDec),
        _ => None,
    }
}
fn postfix_operator(token: &Token) -> Option<Operator> {
    match token {
        Token::DoublePlus => Some(Operator::PostInc),
        Token::DoubleHyphen => Some(Operator::PostDec),
        _ => None,
    }
}

use enum_iterator::Sequence;
#[derive(Clone, Copy, Sequence)]
enum Precedence {
    LogicalOr,
    LogicalAnd,
    Equality,
    Inequality,
    BitOr,
    BitXor,
    BitAnd,
    BitShift,
    AddSub,
    MulDivRem,
    TimeShift,
}

fn infix_operator(token: &Token, precedence: Precedence) -> Option<Operator> {
    match (token, precedence) {
        (Token::TripleGreater, Precedence::TimeShift) => Some(Operator::ForwardShift),
        (Token::TripleLess, Precedence::TimeShift) => Some(Operator::BackwardShift),
        (Token::Asterisk, Precedence::MulDivRem) => Some(Operator::Mul),
        (Token::Slash, Precedence::MulDivRem) => Some(Operator::Div),
        (Token::Percent, Precedence::MulDivRem) => Some(Operator::Rem),
        (Token::Plus, Precedence::AddSub) => Some(Operator::Add),
        (Token::Hyphen, Precedence::AddSub) => Some(Operator::Sub),
        (Token::DoubleGreater, Precedence::BitShift) => Some(Operator::RightShift),
        (Token::DoubleLess, Precedence::BitShift) => Some(Operator::LeftShift),
        (Token::Ampersand, Precedence::BitAnd) => Some(Operator::BitAnd),
        (Token::Circumflex, Precedence::BitXor) => Some(Operator::BitXor),
        (Token::Bar, Precedence::BitOr) => Some(Operator::BitOr),
        (Token::Greater, Precedence::Inequality) => Some(Operator::Greater),
        (Token::GreaterEqual, Precedence::Inequality) => Some(Operator::GreaterEqual),
        (Token::Less, Precedence::Inequality) => Some(Operator::Less),
        (Token::LessEqual, Precedence::Inequality) => Some(Operator::LessEqual),
        (Token::DoubleEqual, Precedence::Equality) => Some(Operator::Equal),
        (Token::ExclamationEqual, Precedence::Equality) => Some(Operator::NotEqual),
        (Token::DoubleAmpersand, Precedence::LogicalAnd) => Some(Operator::LogicalAnd),
        (Token::DoubleBar, Precedence::LogicalOr) => Some(Operator::LogicalOr),
        _ => None,
    }
}

fn assignment_operator(token: &Token) -> Option<Operator> {
    match token {
        Token::Equal => Some(Operator::Assign),
        Token::PlusEqual => Some(Operator::AddAssign),
        Token::HyphenEqual => Some(Operator::SubAssign),
        Token::AsteriskEqual => Some(Operator::MulAssign),
        Token::SlashEqual => Some(Operator::DivAssign),
        Token::PercentEqual => Some(Operator::RemAssign),
        Token::DoubleGreaterEqual => Some(Operator::RightShiftAssign),
        Token::TripleGreaterEqual => Some(Operator::ForwardShiftAssign),
        Token::DoubleLessEqual => Some(Operator::LeftShiftAssign),
        Token::TripleLessEqual => Some(Operator::BackwardShiftAssign),
        Token::AmpersandEqual => Some(Operator::BitAndAssign),
        Token::CircumflexEqual => Some(Operator::BitXorAssign),
        Token::BarEqual => Some(Operator::BitOrAssign),
        _ => None,
    }
}
