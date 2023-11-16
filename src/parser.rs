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
use crate::range::Range;
use std::iter;

mod token;
use token::Token;
mod chars_peekable;
use chars_peekable::CharsPeekable;
mod error;
pub use error::Error;

pub fn parse(input: &str) -> Result<Vec<PStmt>, Error> {
    let mut chars = CharsPeekable::new(input);
    let mut peeked = token::next(&mut chars)?;
    let ret = iter::from_fn(|| parse_stmt(&mut chars, &mut peeked).transpose()).collect();
    assert!(peeked.is_none());
    ret
}

fn parse_stmt<'id>(
    chars: &mut CharsPeekable<'id>,
    peeked: &mut Option<Token<'id>>,
) -> Result<Option<PStmt<'id>>, Error> {
    let Some(first_token) = peeked else {
        return Ok(None);
    };
    match first_token {
        Token::KeywordIf => {
            *peeked = token::next(chars)?;
            match peeked {
                Some(Token::OpeningParenthesis) => *peeked = token::next(chars)?,
                _ => panic!(),
            }
            let cond = parse_assign(chars, peeked)?;
            match peeked {
                Some(Token::ClosingParenthesis) => *peeked = token::next(chars)?,
                _ => panic!(),
            }
            let stmt_then = parse_stmt(chars, peeked)?;
            let (pos_else, stmt_else) = if let Some(Token::KeywordElse) = peeked {
                *peeked = token::next(chars)?;
                (Some(Range::new(0, 0)), parse_stmt(chars, peeked)?)
            } else {
                (None, None)
            };
            Ok(Some(PStmt::new(
                Range::new(0, 0),
                Stmt::If {
                    cond,
                    pos_if: Range::new(0, 0),
                    stmt_then: stmt_then.map(Box::new),
                    pos_else,
                    stmt_else: stmt_else.map(Box::new),
                },
            )))
        }
        Token::KeywordWhile => {
            *peeked = token::next(chars)?;
            match peeked {
                Some(Token::OpeningParenthesis) => *peeked = token::next(chars)?,
                _ => panic!(),
            }
            let cond = parse_assign(chars, peeked)?;
            match peeked {
                Some(Token::ClosingParenthesis) => *peeked = token::next(chars)?,
                _ => panic!(),
            }
            let stmt = parse_stmt(chars, peeked)?;
            Ok(Some(PStmt::new(
                Range::new(0, 0),
                Stmt::While {
                    cond,
                    pos_while: Range::new(0, 0),
                    stmt: stmt.map(Box::new),
                },
            )))
        }
        Token::KeywordReturn => {
            *peeked = token::next(chars)?;
            let term = parse_assign(chars, peeked)?;
            match peeked {
                Some(Token::OpeningParenthesis) => *peeked = token::next(chars)?,
                _ => panic!(),
            }
            Ok(Some(PStmt::new(Range::new(0, 0), Stmt::Return(term))))
        }
        _ => {
            let term = parse_assign(chars, peeked)?;
            match peeked {
                Some(Token::Semicolon) => {
                    *peeked = token::next(chars)?;
                    Ok(Some(PStmt::new(Range::new(0, 0), Stmt::Term(term))))
                }
                Some(Token::OpeningBrace) => {
                    *peeked = token::next(chars)?;
                    let mut stmts = Vec::new();
                    loop {
                        if let Some(Token::ClosingBrace) = peeked {
                            *peeked = token::next(chars)?;
                            break;
                        } else if let Some(stmt) = parse_stmt(chars, peeked)? {
                            stmts.push(stmt);
                        } else {
                            return Err(Error::EOFAfter {
                                reason: Range::new(0, 0),
                            });
                        }
                    }
                    Ok(Some(PStmt::new(
                        Range::new(0, 0),
                        Stmt::Block {
                            antecedent: term,
                            stmts,
                        },
                    )))
                }
                Some(_) => match term {
                    Some(term) => Err(Error::UnexpectedTokenAfter {
                        token: Range::new(0, 0),
                        reason: term.pos(),
                    }),
                    None => Err(Error::UnexpectedToken {
                        token: Range::new(0, 0),
                    }),
                },
                None => Err(Error::EOFAfter {
                    reason: term.unwrap().pos(),
                }),
            }
        }
    }
}

pub fn parse_assign<'id>(
    chars: &mut CharsPeekable<'id>,
    peeked: &mut Option<Token<'id>>,
) -> Result<Option<PTerm<'id>>, Error> {
    let left_hand_side = parse_binary_operator(chars, peeked)?;
    if let Some(operator) = peeked.as_ref().and_then(assignment_operator) {
        *peeked = token::next(chars)?;
        let right_hand_side = parse_assign(chars, peeked)?;
        Ok(Some(PTerm::new(
            Range::new(0, 0),
            Term::Assignment {
                operator,
                pos_operator: Range::new(0, 0),
                left_hand_side: left_hand_side.map(Box::new),
                right_hand_side: right_hand_side.map(Box::new),
            },
        )))
    } else {
        Ok(left_hand_side)
    }
}

fn parse_binary_operator<'id>(
    chars: &mut CharsPeekable<'id>,
    peeked: &mut Option<Token<'id>>,
) -> Result<Option<PTerm<'id>>, Error> {
    parse_binary_operator_rec(chars, peeked, Precedence::first())
}

fn parse_binary_operator_rec<'id>(
    chars: &mut CharsPeekable<'id>,
    peeked: &mut Option<Token<'id>>,
    precedence: Option<Precedence>,
) -> Result<Option<PTerm<'id>>, Error> {
    let Some(precedence) = precedence else {
        return parse_factor(chars, peeked);
    };
    let mut left_operand = parse_binary_operator_rec(chars, peeked, precedence.next())?;
    while let Some(operator) = peeked
        .as_ref()
        .and_then(|token| infix_operator(token, precedence))
    {
        *peeked = token::next(chars)?;
        let right_operand = parse_binary_operator_rec(chars, peeked, precedence.next())?;
        left_operand = Some(PTerm::new(
            Range::new(0, 0),
            Term::BinaryOperation {
                pos_operator: Range::new(0, 0),
                left_operand: left_operand.map(Box::new),
                operator,
                right_operand: right_operand.map(Box::new),
            },
        ));
    }
    Ok(left_operand)
}

fn parse_factor<'id>(
    chars: &mut CharsPeekable<'id>,
    peeked: &mut Option<Token<'id>>,
) -> Result<Option<PTerm<'id>>, Error> {
    let Some(first_token) = peeked else {
        return Ok(None);
    };
    let mut antecedent = 'ant: {
        let term = if let Token::Identifier(name) = *first_token {
            *peeked = token::next(chars)?;
            Term::Identifier(name)
        } else if let Token::Number(str) = *first_token {
            *peeked = token::next(chars)?;
            let value: String = str.chars().filter(|&ch| ch != '_').collect();
            if value.chars().all(|ch| ch.is_ascii_digit()) {
                Term::Integer(value.parse().unwrap())
            } else {
                Term::Float(value.parse().unwrap())
            }
        } else if let Token::String(components) = first_token {
            let components = std::mem::take(components);
            *peeked = token::next(chars)?;
            Term::String(components)
        } else if let Some(operator) = prefix_operator(&first_token) {
            *peeked = token::next(chars)?;
            let operand = parse_factor(chars, peeked)?;
            return Ok(Some(PTerm::new(
                Range::new(0, 0),
                Term::UnaryOperation {
                    operator,
                    pos_operator: Range::new(0, 0),
                    operand: operand.map(Box::new),
                },
            )));
        } else {
            break 'ant None;
        };
        Some(PTerm::new(Range::new(0, 0), term))
    };
    loop {
        let term = {
            let Some(token) = peeked else {
                return Ok(antecedent);
            };
            if let Token::OpeningParenthesis = token {
                *peeked = token::next(chars)?;
                let mut elements = Vec::new();
                let has_trailing_comma;
                loop {
                    let element = parse_assign(chars, peeked)?;
                    if let Some(Token::Comma) = peeked {
                        *peeked = token::next(chars)?;
                        elements.push(element.ok_or(Range::new(0, 0)));
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
                match peeked {
                    Some(Token::ClosingParenthesis) => {
                        *peeked = token::next(chars)?;
                    }
                    Some(other) => {
                        return Err(Error::UnexpectedTokenAfter {
                            token: Range::new(0, 0),
                            reason: Range::new(0, 0),
                        })
                    }
                    None => {
                        return Err(Error::EOFAfter {
                            reason: Range::new(0, 0),
                        })
                    }
                }
                Term::Parenthesized {
                    antecedent: antecedent.map(Box::new),
                    elements,
                    has_trailing_comma,
                }
            } else if let Token::Colon = token {
                *peeked = token::next(chars)?;
                let ty = parse_factor(chars, peeked)?;
                Term::TypeAnnotation {
                    pos_colon: Range::new(0, 0),
                    term: antecedent.map(Box::new),
                    ty: ty.map(Box::new),
                }
            } else if let Token::HyphenGreater = token {
                *peeked = token::next(chars)?;
                let ty = parse_factor(chars, peeked)?;
                Term::ReturnType {
                    pos_arrow: Range::new(0, 0),
                    term: antecedent.map(Box::new),
                    ty: ty.map(Box::new),
                }
            } else if let Some(operator) = postfix_operator(token) {
                *peeked = token::next(chars)?;
                Term::UnaryOperation {
                    operator,
                    pos_operator: Range::new(0, 0),
                    operand: antecedent.map(Box::new),
                }
            } else {
                return Ok(antecedent);
            }
        };
        antecedent = Some(PTerm::new(Range::new(0, 0), term));
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
