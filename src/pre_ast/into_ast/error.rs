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

//! pre AST から AST へ変換する過程で起こるエラーを定義する．

use crate::lines::Lines;
use std::ops::Range;

#[derive(Debug)]
pub enum Error {
    EmptyUnaryOperand(Range<usize>),
    EmptyLeftOperand(Range<usize>),
    EmptyRightOperand(Range<usize>),
    EmptyLeftHandSide(Range<usize>),
    EmptyRightHandSide(Range<usize>),
    EmptyLeftHandSideDecl {
        colon_pos: Range<usize>,
    },
    InvalidLeftHandSideDecl {
        error_pos: Range<usize>,
        colon_pos: Range<usize>,
    },
    ReferenceOfRvalue {
        error_pos: Range<usize>,
    },
    EmptyArgument {
        comma_pos: Range<usize>,
    },
    EmptyArgumentWithType {
        colon_pos: Range<usize>,
    },
    EmptyBraceInStringLiteral {
        literal_pos: Range<usize>,
    },
    EmptyConditionIf(Range<usize>),
    EmptyStatementIf(Range<usize>),
    EmptyStatementElse(Range<usize>),
    EmptyConditionWhile(Range<usize>),
    EmptyStatementWhile(Range<usize>),
    UnexpectedExpressionBeforeBlock(Range<usize>),
    InvalidFunctionName(Range<usize>),
    EmptyReturnType {
        arrow_pos: Range<usize>,
    },
    EmptyFunctionNameAndArgs {
        arrow_pos: Range<usize>,
    },
    InvalidArgumentInDef(Range<usize>),
}

pub fn eprint_errors(errors: &[Error], input: &str) {
    let lines = Lines::new(input);
    for error in errors {
        match error {
            Error::EmptyUnaryOperand(range) => {
                eprintln!("No operand of unary operator at {range:?}");
                lines.eprint_range(range);
            }
            Error::EmptyLeftOperand(range) => {
                eprintln!("No left operand of binary operator at {range:?}");
                lines.eprint_range(range);
            }
            Error::EmptyRightOperand(range) => {
                eprintln!("No right operand of binary operator at {range:?}");
                lines.eprint_range(range);
            }
            Error::EmptyLeftHandSide(range) => {
                eprintln!("No left hand side of binary operator at {range:?}");
                lines.eprint_range(range);
            }
            Error::EmptyRightHandSide(range) => {
                eprintln!("No right hand side of binary operator at {range:?}");
                lines.eprint_range(range);
            }
            Error::EmptyLeftHandSideDecl { colon_pos } => {
                eprintln!("No name given for declaration (colon at {colon_pos:?})");
                lines.eprint_range(colon_pos);
            }
            Error::InvalidLeftHandSideDecl {
                error_pos,
                colon_pos,
            } => {
                eprintln!("Invalid expression at {error_pos:?} for declaration");
                lines.eprint_range(error_pos);
                eprintln!("Note: colon at {colon_pos:?}");
                lines.eprint_range(colon_pos);
            }
            Error::ReferenceOfRvalue { error_pos } => {
                eprintln!("Cannot take reference of rvalue at {error_pos:?}");
                lines.eprint_range(error_pos);
            }
            Error::EmptyBraceInStringLiteral { literal_pos } => {
                eprintln!("Empty brace in string literal at {literal_pos:?}");
                lines.eprint_range(literal_pos);
            }
            Error::EmptyArgument { comma_pos } => {
                eprintln!("No argument before comma at {comma_pos:?}");
                lines.eprint_range(comma_pos);
            }
            Error::EmptyArgumentWithType { colon_pos } => {
                eprintln!("No argument before colon at {colon_pos:?}");
                lines.eprint_range(colon_pos);
            }
            Error::EmptyConditionIf(range) => {
                eprintln!("No condition after `if` at {range:?}");
                lines.eprint_range(range);
            }
            Error::EmptyStatementIf(range) => {
                eprintln!("No statement after `if` at {range:?}");
                lines.eprint_range(range);
            }
            Error::EmptyStatementElse(range) => {
                eprintln!("No statement after `else` at {range:?}");
                lines.eprint_range(range);
            }
            Error::EmptyConditionWhile(range) => {
                eprintln!("No condition after `while` at {range:?}");
                lines.eprint_range(range);
            }
            Error::EmptyStatementWhile(range) => {
                eprintln!("No statement after `while` at {range:?}");
                lines.eprint_range(range);
            }
            Error::UnexpectedExpressionBeforeBlock(expr) => {
                eprintln!("Unexpected expression at {expr:?} before block statement");
                lines.eprint_range(expr);
            }
            Error::InvalidFunctionName(term) => {
                eprintln!("Invalid function name at {term:?}");
                lines.eprint_range(term);
            }
            Error::InvalidArgumentInDef(term) => {
                eprintln!("Invalid argument {term:?} in function definition");
                lines.eprint_range(term);
            }
            Error::EmptyFunctionNameAndArgs { arrow_pos } => {
                eprintln!("Empty function name and args before `->` at {arrow_pos:?}");
                lines.eprint_range(arrow_pos);
            }
            Error::EmptyReturnType { arrow_pos } => {
                eprintln!("Empty return type after `->` at {arrow_pos:?}");
                lines.eprint_range(arrow_pos);
            }
        }
    }
}
