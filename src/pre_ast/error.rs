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

use crate::lines::Lines;
use crate::range::Range;

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
            Error::EmptyLeftHandSideDecl { colon } => {
                eprintln!("No name given for declaration (colon at {colon:?})");
                lines.eprint_range(colon);
            }
            Error::InvalidLeftHandSideDecl { term, colon } => {
                eprintln!("Invalid expression at {term:?} for declaration");
                lines.eprint_range(term);
                eprintln!("Note: colon at {colon:?}");
                lines.eprint_range(colon);
            }
            Error::EmptyArgument { pos_comma } => {
                eprintln!("No argument before comma at {pos_comma:?}");
                lines.eprint_range(pos_comma);
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
        }
    }
}
