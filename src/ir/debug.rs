/*
 * Copyright (c) 2023-2025 Atsushi Komaba
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

use super::*;
use std::fmt::{self, Debug, Formatter};

impl Debug for Definitions {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for (i, (kind, definition)) in self.structures.iter().enumerate() {
            writeln!(f, "S{i}: {kind:?}")?;
            for (j, field_ty) in definition.fields_ty.iter().enumerate() {
                writeln!(f, "S{i}.{j}: {field_ty:?}")?;
            }
        }
        for (i, (ty, definition)) in self
            .functions_ty
            .iter()
            .zip(&self.function_definitions)
            .enumerate()
        {
            writeln!(f, "F{i}{ty:?}")?;
            writeln!(f, "{definition:?}")?;
        }
        Ok(())
    }
}

impl Debug for TyKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TyKind::Ty => write!(f, "*"),
            TyKind::Abstraction { parameters, ret } => {
                write!(f, "({parameters:?}): {ret:?}")
            }
        }
    }
}

impl Debug for TyListKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TyListKind::Nil => write!(f, ""),
            TyListKind::Cons(head, tail) => match **tail {
                TyListKind::Nil => write!(f, "{head:?}"),
                _ => write!(f, "{head:?}, {tail:?}"),
            },
            TyListKind::Rest => write!(f, ".."),
        }
    }
}

impl Debug for Ty {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Ty::Constructor(constructor) => write!(f, "{constructor:?}"),
            Ty::Parameter(index) => write!(f, "T{index}"),
            Ty::Application {
                constructor,
                arguments,
            } => {
                write!(
                    f,
                    "{constructor:?}[{}]",
                    arguments
                        .iter()
                        .map(|argument| format!("{argument:?}"))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
    }
}

impl Debug for FunctionTy {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "({}): {:?}",
            self.parameters_ty
                .iter()
                .map(|parameter_ty| format!("{parameter_ty:?}"))
                .collect::<Vec<_>>()
                .join(", "),
            self.return_ty
        )
    }
}

impl Debug for FunctionDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:#?}", self.body)
    }
}

impl Debug for Block {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut f = f.debug_struct("Block");
        f.field("size", &self.size);
        for (i, statement) in self.statements.iter().enumerate() {
            f.field(&format!("{i}"), statement);
        }
        f.finish()
    }
}

impl Debug for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Expr(expr) => {
                let mut f = f.debug_struct("Expr");
                for (i, expr) in expr.iter().enumerate() {
                    f.field(&format!("{i}"), expr);
                }
                f.finish()
            }
            Statement::If {
                antecedents,
                condition,
                then_block,
                else_block,
            } => {
                let mut f = f.debug_struct("If");
                for (i, expr) in antecedents.iter().enumerate() {
                    f.field(&format!("{i}"), expr);
                }
                f.field("condition", condition);
                f.field("then", then_block);
                f.field("else", else_block);
                f.finish()
            }
            Statement::While {
                condition,
                do_block,
            } => {
                let mut f = f.debug_struct("While");
                f.field("condition", condition);
                f.field("do", do_block);
                f.finish()
            }
            Statement::Break(expr) => {
                let mut f = f.debug_struct("Break");
                for (i, expr) in expr.iter().enumerate() {
                    f.field(&format!("{i}"), expr);
                }
                f.finish()
            }
            Statement::Continue(expr) => {
                let mut f = f.debug_struct("Continue");
                for (i, expr) in expr.iter().enumerate() {
                    f.field(&format!("{i}"), expr);
                }
                f.finish()
            }
        }
    }
}

impl Debug for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Integer(value) => write!(f, "{value}i"),
            Expression::Float(value) => write!(f, "{value}f"),
            Expression::String(value) => write!(f, "{value:?}"),
            Expression::GlobalVariable(index) => {
                write!(f, "G{index}")
            }
            Expression::LocalVariable(index) => {
                write!(f, "L{index}")
            }
            Expression::Function { candidates, calls } => {
                write!(
                    f,
                    "<{}>",
                    candidates
                        .iter()
                        .map(|candidate| format!("{candidate:?}"))
                        .collect::<Vec<_>>()
                        .join("/"),
                )?;
                for Call { arguments } in calls {
                    write!(
                        f,
                        "({})",
                        arguments
                            .iter()
                            .map(|argument| format!("{argument:?}"))
                            .collect::<Vec<_>>()
                            .join(", ")
                    )?;
                }
                Ok(())
            }
        }
    }
}

impl Debug for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Function::AddInteger => write!(f, "AddInteger"),
            Function::DereferenceInteger => write!(f, "DereferenceInteger"),
            Function::Identity => write!(f, "Identity"),
            Function::AssignInteger => write!(f, "AssignInteger"),
            Function::DeleteInteger => write!(f, "DeleteInteger"),
            Function::IntegerToString => write!(f, "IntegerToString"),
            Function::ConcatenateString => write!(f, "ConcatenateString"),
            Function::Print => write!(f, "Print"),
            Function::UserDefined(index) => write!(f, "F{index}"),
            Function::Field {
                structure_index,
                field_index,
            } => write!(f, "S{structure_index}.{field_index}"),
            Function::FieldRef {
                structure_index,
                field_index,
            } => write!(f, "S{structure_index}.{field_index}"),
        }
    }
}

impl Debug for TyConstructor {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TyConstructor::Integer => write!(f, "Integer"),
            TyConstructor::Float => write!(f, "Float"),
            TyConstructor::Reference => write!(f, "Reference"),
            TyConstructor::Tuple => write!(f, "Tuple"),
            TyConstructor::Function => write!(f, "Function"),
            TyConstructor::Structure(i) => write!(f, "S{i}"),
        }
    }
}
