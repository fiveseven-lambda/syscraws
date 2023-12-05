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

use super::{Expr, Kind, Node, Var};
use std::fmt::{self, Debug, Formatter};

impl Debug for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.kind == Kind::Function {
            let mut iter = self.args.iter().rev();
            write!(
                f,
                "func({args}) -> {ret:?}",
                ret = iter.next().unwrap(),
                args = iter
                    .rev()
                    .map(|arg| format!("{arg:?}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        } else {
            write!(f, "{:?}", self.kind)?;
            if !self.args.is_empty() {
                write!(
                    f,
                    "[{}]",
                    self.args
                        .iter()
                        .map(|arg| format!("{arg:?}"))
                        .collect::<Vec<_>>()
                        .join(", ")
                )?;
            }
            Ok(())
        }
    }
}

impl Debug for Var {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.node.borrow())
    }
}

impl Debug for Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Node::Determined(ty) => write!(f, "{ty:?}"),
            Node::SameAs(var) => write!(f, "{var:?}"),
            Node::Undetermined { .. } => write!(f, "{self:p}"),
        }
    }
}
