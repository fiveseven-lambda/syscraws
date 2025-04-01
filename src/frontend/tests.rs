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

#![cfg(test)]

use super::*;
use std::fmt::{self, Debug, Formatter};

impl Debug for backend::Definitions {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for (i, (kind, definition)) in self.structures.iter().enumerate() {
            writeln!(f, "S{i}: {kind:?}")?;
            for (j, field_ty) in definition.fields_ty.iter().enumerate() {
                writeln!(f, "S{i}.{j}: {field_ty:?}")?;
            }
        }
        for (i, (ty, definition)) in self.functions.iter().enumerate() {
            writeln!(f, "F{i}{ty:?}")?;
            writeln!(f, "{definition:?}")?;
        }
        Ok(())
    }
}

impl Debug for backend::TyKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            backend::TyKind::Ty => write!(f, "*"),
            backend::TyKind::Abstraction { parameters, ret } => {
                write!(f, "({parameters:?}): {ret:?}")
            }
        }
    }
}

impl Debug for backend::TyListKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            backend::TyListKind::Nil => write!(f, ""),
            backend::TyListKind::Cons(head, tail) => match **tail {
                backend::TyListKind::Nil => write!(f, "{head:?}"),
                _ => write!(f, "{head:?}, {tail:?}"),
            },
            backend::TyListKind::Rest => write!(f, ".."),
        }
    }
}

impl Debug for backend::TyBuilder {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            backend::TyBuilder::Constructor(constructor) => write!(f, "{constructor:?}"),
            backend::TyBuilder::Parameter(index) => write!(f, "T{index}"),
            backend::TyBuilder::Application {
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

impl Debug for backend::TyConstructor {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            backend::TyConstructor::Integer => write!(f, "Integer"),
            backend::TyConstructor::Float => write!(f, "Float"),
            backend::TyConstructor::Reference => write!(f, "Reference"),
            backend::TyConstructor::Tuple => write!(f, "Tuple"),
            backend::TyConstructor::Function => write!(f, "Function"),
            backend::TyConstructor::Structure(i) => write!(f, "S{i}"),
        }
    }
}

impl Debug for backend::FunctionTy {
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

impl Debug for backend::FunctionDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:#?}", self.body)
    }
}

impl Debug for backend::Block {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut f = f.debug_struct("Block");
        f.field("size", &self.size);
        for (i, statement) in self.statements.iter().enumerate() {
            f.field(&format!("{i}"), statement);
        }
        f.finish()
    }
}

impl Debug for backend::Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            backend::Statement::Expr(expr) => {
                let mut f = f.debug_struct("Expr");
                for (i, expr) in expr.iter().enumerate() {
                    f.field(&format!("{i}"), expr);
                }
                f.finish()
            }
            backend::Statement::If {
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
            backend::Statement::While {
                condition,
                do_block,
            } => {
                let mut f = f.debug_struct("While");
                f.field("condition", condition);
                f.field("do", do_block);
                f.finish()
            }
            backend::Statement::Break(expr) => {
                let mut f = f.debug_struct("Break");
                for (i, expr) in expr.iter().enumerate() {
                    f.field(&format!("{i}"), expr);
                }
                f.finish()
            }
            backend::Statement::Continue(expr) => {
                let mut f = f.debug_struct("Continue");
                for (i, expr) in expr.iter().enumerate() {
                    f.field(&format!("{i}"), expr);
                }
                f.finish()
            }
        }
    }
}

impl Debug for backend::Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            backend::Expression::Integer(value) => write!(f, "{value}i"),
            backend::Expression::Float(value) => write!(f, "{value}f"),
            backend::Expression::Variable(backend::LocalOrGlobal::Global, index) => {
                write!(f, "G{index}")
            }
            backend::Expression::Variable(backend::LocalOrGlobal::Local, index) => {
                write!(f, "L{index}")
            }
            backend::Expression::Function { candidates, calls } => {
                write!(
                    f,
                    "<{}>",
                    candidates
                        .iter()
                        .map(|candidate| format!("{candidate:?}"))
                        .collect::<Vec<_>>()
                        .join("/"),
                )?;
                for backend::Call { arguments } in calls {
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

impl Debug for backend::Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            backend::Function::IAdd => write!(f, "IAdd"),
            backend::Function::Deref => write!(f, "Deref"),
            backend::Function::Identity => write!(f, "Identity"),
            backend::Function::IAssign => write!(f, "IAssign"),
            backend::Function::Delete => write!(f, "Delete"),
            backend::Function::UserDefined(index) => write!(f, "F{index}"),
            backend::Function::Field {
                structure_index,
                field_index,
            } => write!(f, "S{structure_index}.{field_index}"),
            backend::Function::FieldRef {
                structure_index,
                field_index,
            } => write!(f, "S{structure_index}.{field_index}"),
        }
    }
}

#[test]
fn test() {
    let mut logger = log::Logger::new(Box::new(std::io::sink()));
    for &dir_name in &[
        "struct",
        "if_else",
        "variables",
        "operator_overload",
        "break",
    ] {
        let dir = Path::new("tests/frontend").join(dir_name);
        eprintln!("{}", dir.display());
        let definitions = read_input(&dir.join("input"), &mut logger).unwrap();
        let output = format!("{definitions:?}");
        let expected = std::fs::read_to_string(dir.join("expected.txt")).unwrap();
        if output != expected {
            panic!(
                "Failed: {}.\n\nOutput:\n{output}\nExpected:\n{expected}",
                dir.display()
            )
        }
    }
}
