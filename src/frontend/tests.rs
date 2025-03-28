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
        let mut f = f.debug_struct("Definitions");
        for (i, (kind, definition)) in self.structures.iter().enumerate() {
            f.field(&format!("S{i}Kind"), kind);
            f.field(&format!("S{i}Definition"), definition);
        }
        f.finish()
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

impl Debug for backend::Structure {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut f = f.debug_struct("Structure");
        for (i, field_ty) in self.fields_ty.iter().enumerate() {
            f.field(&format!("Field{i}"), field_ty);
        }
        f.finish()
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

#[test]
fn test() {
    for &dir_name in &["struct"] {
        let dir = Path::new("tests/frontend").join(dir_name);
        let definitions = read_input(&dir.join("input")).unwrap();
        let output = format!("{definitions:#?}");
        let expected = std::fs::read_to_string(dir.join("expected.txt")).unwrap();
        if output != expected {
            panic!("{output}\n{expected}")
        }
    }
}
