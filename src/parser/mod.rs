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
use crate::token::{Token, TokenKind};
use std::iter;

mod token_seq;
use token_seq::TokenSeq;
mod error;
use error::Error;

pub fn parse<'id>(input: &'id str, tokens: &[Token]) -> Result<Vec<PStmt<'id>>, Error> {
    let mut tokens = TokenSeq::new(tokens);
    iter::from_fn(|| parse_stmt(input, &mut tokens).transpose()).collect()
}

fn parse_stmt<'id>(input: &'id str, tokens: &mut TokenSeq) -> Result<Option<PStmt<'id>>, Error> {
    let Some(Token {token_kind: first_token_kind, start: first_token_start, end: first_token_end}) = tokens.peek() else {
        return Ok(None);
    };
    match first_token_kind {
        TokenKind::KeywordIf => {
            // SAFETY: consume "if"
            unsafe { tokens.consume() }
            consume_if_eq_or_err(
                tokens,
                TokenKind::OpeningParenthesis,
                Range::new(first_token_start, first_token_end),
            )?;
            let cond = parse_assign(input, tokens)?;
            consume_if_eq_or_err(
                tokens,
                TokenKind::ClosingParenthesis,
                Range::new(first_token_start, first_token_end),
            )?;
            let stmt_then = parse_stmt(input, tokens)?;
            let (pos_else, stmt_else) = if let Some(Token {
                token_kind: TokenKind::KeywordElse,
                start,
                end,
            }) = tokens.peek()
            {
                // SAFETY: consume "else"
                unsafe { tokens.consume() }
                (Some(Range::new(start, end)), parse_stmt(input, tokens)?)
            } else {
                (None, None)
            };
            Ok(Some(PStmt::new(
                Range::new(first_token_start, tokens.prev_end()),
                Stmt::If {
                    cond,
                    pos_if: Range::new(first_token_start, first_token_end),
                    stmt_then: stmt_then.map(Box::new),
                    pos_else,
                    stmt_else: stmt_else.map(Box::new),
                },
            )))
        }
        TokenKind::KeywordWhile => {
            // SAFETY: consume "while"
            unsafe { tokens.consume() }
            consume_if_eq_or_err(
                tokens,
                TokenKind::OpeningParenthesis,
                Range::new(first_token_start, first_token_end),
            )?;
            let cond = parse_assign(input, tokens)?;
            consume_if_eq_or_err(
                tokens,
                TokenKind::ClosingParenthesis,
                Range::new(first_token_start, first_token_end),
            )?;
            let stmt = parse_stmt(input, tokens)?;
            Ok(Some(PStmt::new(
                Range::new(first_token_start, tokens.prev_end()),
                Stmt::While {
                    cond,
                    pos_while: Range::new(first_token_start, first_token_end),
                    stmt: stmt.map(Box::new),
                },
            )))
        }
        TokenKind::KeywordReturn => {
            // SAFETY: consume "return"
            unsafe { tokens.consume() }
            let term = parse_assign(input, tokens)?;
            consume_if_eq_or_err(
                tokens,
                TokenKind::Semicolon,
                Range::new(first_token_start, first_token_end),
            )?;
            Ok(Some(PStmt::new(
                Range::new(first_token_start, tokens.prev_end()),
                Stmt::Return(term),
            )))
        }
        _ => {
            let term = parse_assign(input, tokens)?;
            match tokens.peek() {
                Some(Token {
                    token_kind: TokenKind::Semicolon,
                    start: _,
                    end: semicolon_end,
                }) => {
                    // SAFETY: consume semicolon
                    unsafe { tokens.consume() }
                    Ok(Some(PStmt::new(
                        Range::new(first_token_start, semicolon_end),
                        Stmt::Term(term),
                    )))
                }
                Some(Token {
                    token_kind: TokenKind::OpeningBrace,
                    start,
                    end,
                }) => {
                    // SAFETY: consume opening brace
                    unsafe { tokens.consume() }
                    let mut stmts = Vec::new();
                    loop {
                        if let Some(Token {
                            token_kind: TokenKind::ClosingBrace,
                            ..
                        }) = tokens.peek()
                        {
                            // SAFETY: consume closing brace
                            unsafe { tokens.consume() }
                            break;
                        } else if let Some(stmt) = parse_stmt(input, tokens)? {
                            stmts.push(stmt);
                        } else {
                            return Err(Error::EOFAfter {
                                reason: Range::new(start, end),
                            });
                        }
                    }
                    Ok(Some(PStmt::new(
                        Range::new(first_token_start, tokens.prev_end()),
                        Stmt::Block {
                            antecedent: term,
                            stmts,
                        },
                    )))
                }
                Some(Token { start, end, .. }) => match term {
                    Some(term) => Err(Error::UnexpectedTokenAfter {
                        token: Range::new(start, end),
                        reason: term.pos(),
                    }),
                    None => Err(Error::UnexpectedToken {
                        token: Range::new(start, end),
                    }),
                },
                None => Err(Error::EOFAfter {
                    reason: term.unwrap().pos(),
                }),
            }
        }
    }
}

fn parse_assign<'id>(input: &'id str, tokens: &mut TokenSeq) -> Result<Option<PTerm<'id>>, Error> {
    let Some(start) = tokens.next_start() else {
        return Ok(None);
    };
    let left_hand_side = parse_type_annotation(input, tokens)?;
    if let Some(Token {
        token_kind,
        start: op_start,
        end: op_end,
    }) = tokens.peek()
    {
        if let Some(operator) = assignment_operator(token_kind) {
            // SAFETY: consume assignment operator
            unsafe { tokens.consume() }
            let right_hand_side = parse_assign(input, tokens)?;
            return Ok(Some(PTerm::new(
                Range::new(start, tokens.prev_end()),
                Term::Assignment {
                    operator,
                    pos_operator: Range::new(op_start, op_end),
                    left_hand_side: left_hand_side.map(Box::new),
                    right_hand_side: right_hand_side.map(Box::new),
                },
            )));
        }
    }
    Ok(left_hand_side)
}

fn parse_type_annotation<'id>(
    input: &'id str,
    tokens: &mut TokenSeq,
) -> Result<Option<PTerm<'id>>, Error> {
    let Some(start) = tokens.next_start() else {
        return Ok(None);
    };
    let term = parse_binary_operator(input, tokens)?;
    if let Some(Token {
        token_kind: TokenKind::Colon,
        start: op_start,
        end: op_end,
    }) = tokens.peek()
    {
        unsafe { tokens.consume() }
        let ty = parse_factor(input, tokens)?;
        Ok(Some(PTerm::new(
            Range::new(start, tokens.prev_end()),
            Term::TypeAnnotation {
                pos_colon: Range::new(op_start, op_end),
                term: term.map(Box::new),
                ty: ty.map(Box::new),
            },
        )))
    } else {
        Ok(term)
    }
}

fn parse_binary_operator<'id>(
    input: &'id str,
    tokens: &mut TokenSeq,
) -> Result<Option<PTerm<'id>>, Error> {
    parse_binary_operator_rec(input, tokens, Precedence::first().unwrap())
}

fn parse_binary_operator_rec<'id>(
    input: &'id str,
    tokens: &mut TokenSeq,
    current_precedence: Precedence,
) -> Result<Option<PTerm<'id>>, Error> {
    let Some(next_precedence) = current_precedence.next() else {
        return parse_factor(input, tokens);
    };
    let Some(start) = tokens.next_start() else {
        return Ok(None);
    };
    let mut left_operand = parse_binary_operator_rec(input, tokens, next_precedence)?;
    loop {
        if let Some(Token {
            token_kind,
            start: op_start,
            end: op_end,
        }) = tokens.peek()
        {
            if let Some(operator) = infix_operator(token_kind, current_precedence) {
                // SAFETY: consume infix operator
                unsafe { tokens.consume() }
                let right_operand = parse_binary_operator_rec(input, tokens, next_precedence)?;
                left_operand = Some(PTerm::new(
                    Range::new(start, tokens.prev_end()),
                    Term::BinaryOperation {
                        operator,
                        pos_operator: Range::new(op_start, op_end),
                        left_operand: left_operand.map(Box::new),
                        right_operand: right_operand.map(Box::new),
                    },
                ));
                continue;
            }
        }
        return Ok(left_operand);
    }
}

fn parse_factor<'id>(input: &'id str, tokens: &mut TokenSeq) -> Result<Option<PTerm<'id>>, Error> {
    let Some(Token {token_kind, start, end}) = tokens.peek() else {
        return Ok(None);
    };
    let mut antecedent = 'ant: {
        let term = if token_kind == TokenKind::Identifier {
            Term::Identifier(unsafe { input.get_unchecked(start..end) })
        } else if token_kind == TokenKind::Number {
            let value: String = unsafe { input.get_unchecked(start..end) }
                .chars()
                .filter(|&ch| ch != '_')
                .collect();
            if value.chars().all(|ch| ch.is_ascii_digit()) {
                Term::Integer(value.parse().unwrap())
            } else {
                Term::Float(value.parse().unwrap())
            }
        } else if token_kind == TokenKind::String {
            let mut chars = unsafe { input.get_unchecked(start + 1..end - 1) }.chars();
            let mut value = String::new();
            while let Some(mut ch) = chars.next() {
                if ch == '\\' {
                    ch = match chars.next().unwrap() {
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        '0' => '\0',
                        'x' => {
                            let code: String = chars.by_ref().take(2).collect();
                            let code: u8 = code.parse().unwrap();
                            code as char
                        }
                        ch => ch,
                    }
                };
                value.push(ch);
            }
            Term::String(value)
        } else if let Some(operator) = prefix_operator(token_kind) {
            unsafe { tokens.consume() }
            let operand = parse_factor(input, tokens)?;
            return Ok(Some(PTerm::new(
                Range::new(start, tokens.prev_end()),
                Term::UnaryOperation {
                    operator,
                    pos_operator: Range::new(start, end),
                    operand: operand.map(Box::new),
                },
            )));
        } else {
            break 'ant None;
        };
        unsafe { tokens.consume() }
        Some(PTerm::new(Range::new(start, end), term))
    };
    loop {
        let term = {
            let Some(Token { token_kind, start, end }) = tokens.peek() else {
                return Ok(antecedent);
            };
            if token_kind == TokenKind::OpeningParenthesis {
                unsafe { tokens.consume() }
                let mut elements = Vec::new();
                let has_trailing_comma;
                loop {
                    let element = parse_assign(input, tokens)?;
                    if let Some(Token {
                        token_kind: TokenKind::Comma,
                        start,
                        end,
                    }) = tokens.peek()
                    {
                        unsafe { tokens.consume() }
                        elements.push(element.ok_or(Range::new(start, end)));
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
                consume_if_eq_or_err(
                    tokens,
                    TokenKind::ClosingParenthesis,
                    Range::new(start, end),
                )?;
                Term::Parenthesized {
                    antecedent: antecedent.map(Box::new),
                    elements,
                    has_trailing_comma,
                }
            } else if let Some(operator) = postfix_operator(token_kind) {
                unsafe { tokens.consume() }
                Term::UnaryOperation {
                    operator,
                    pos_operator: Range::new(start, end),
                    operand: antecedent.map(Box::new),
                }
            } else {
                return Ok(antecedent);
            }
        };
        antecedent = Some(PTerm::new(Range::new(start, tokens.prev_end()), term));
    }
}

fn consume_if_eq_or_err(
    tokens: &mut TokenSeq,
    expected: TokenKind,
    reason: Range,
) -> Result<(), Error> {
    if let Some(Token {
        token_kind,
        start,
        end,
    }) = tokens.peek()
    {
        if token_kind == expected {
            unsafe { tokens.consume() }
            Ok(())
        } else {
            Err(Error::UnexpectedTokenAfter {
                token: Range::new(start, end),
                reason,
            })
        }
    } else {
        Err(Error::EOFAfter { reason })
    }
}

fn prefix_operator(token_kind: TokenKind) -> Option<Operator> {
    match token_kind {
        TokenKind::Plus => Some(Operator::Plus),
        TokenKind::Minus => Some(Operator::Minus),
        TokenKind::Exclamation => Some(Operator::LogicalNot),
        TokenKind::Tilde => Some(Operator::BitNot),
        TokenKind::DoublePlus => Some(Operator::PreInc),
        TokenKind::DoubleMinus => Some(Operator::PreDec),
        _ => None,
    }
}
fn postfix_operator(token_kind: TokenKind) -> Option<Operator> {
    match token_kind {
        TokenKind::DoublePlus => Some(Operator::PostInc),
        TokenKind::DoubleMinus => Some(Operator::PostDec),
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

fn infix_operator(token_kind: TokenKind, precedence: Precedence) -> Option<Operator> {
    match (token_kind, precedence) {
        (TokenKind::TripleGreater, Precedence::TimeShift) => Some(Operator::ForwardShift),
        (TokenKind::TripleLess, Precedence::TimeShift) => Some(Operator::BackwardShift),
        (TokenKind::Asterisk, Precedence::MulDivRem) => Some(Operator::Mul),
        (TokenKind::Slash, Precedence::MulDivRem) => Some(Operator::Div),
        (TokenKind::Percent, Precedence::MulDivRem) => Some(Operator::Rem),
        (TokenKind::Plus, Precedence::AddSub) => Some(Operator::Add),
        (TokenKind::Minus, Precedence::AddSub) => Some(Operator::Sub),
        (TokenKind::DoubleGreater, Precedence::BitShift) => Some(Operator::RightShift),
        (TokenKind::DoubleLess, Precedence::BitShift) => Some(Operator::LeftShift),
        (TokenKind::Ampersand, Precedence::BitAnd) => Some(Operator::BitAnd),
        (TokenKind::Circumflex, Precedence::BitXor) => Some(Operator::BitXor),
        (TokenKind::Bar, Precedence::BitOr) => Some(Operator::BitOr),
        (TokenKind::Greater, Precedence::Inequality) => Some(Operator::Greater),
        (TokenKind::GreaterEqual, Precedence::Inequality) => Some(Operator::GreaterEqual),
        (TokenKind::Less, Precedence::Inequality) => Some(Operator::Less),
        (TokenKind::LessEqual, Precedence::Inequality) => Some(Operator::LessEqual),
        (TokenKind::DoubleEqual, Precedence::Equality) => Some(Operator::Equal),
        (TokenKind::ExclamationEqual, Precedence::Equality) => Some(Operator::NotEqual),
        (TokenKind::DoubleAmpersand, Precedence::LogicalAnd) => Some(Operator::LogicalAnd),
        (TokenKind::DoubleBar, Precedence::LogicalOr) => Some(Operator::LogicalOr),
        _ => None,
    }
}

fn assignment_operator(token_kind: TokenKind) -> Option<Operator> {
    match token_kind {
        TokenKind::Equal => Some(Operator::Assign),
        TokenKind::PlusEqual => Some(Operator::AddAssign),
        TokenKind::MinusEqual => Some(Operator::SubAssign),
        TokenKind::AsteriskEqual => Some(Operator::MulAssign),
        TokenKind::SlashEqual => Some(Operator::DivAssign),
        TokenKind::PercentEqual => Some(Operator::RemAssign),
        TokenKind::DoubleGreaterEqual => Some(Operator::RightShiftAssign),
        TokenKind::TripleGreaterEqual => Some(Operator::ForwardShiftAssign),
        TokenKind::DoubleLessEqual => Some(Operator::LeftShiftAssign),
        TokenKind::TripleLessEqual => Some(Operator::BackwardShiftAssign),
        TokenKind::AmpersandEqual => Some(Operator::BitAndAssign),
        TokenKind::CircumflexEqual => Some(Operator::BitXorAssign),
        TokenKind::BarEqual => Some(Operator::BitOrAssign),
        _ => None,
    }
}
