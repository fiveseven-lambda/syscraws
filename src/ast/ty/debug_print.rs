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

use super::{Candidate, CandidateNode, Kind, Ty, TyNode};
use std::fmt::{self, Debug, Formatter};

impl Debug for CandidateNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match *self {
            CandidateNode::Const { kind, ref args } => {
                if kind == Kind::Function {
                    let mut iter = args.iter().rev();
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
                    write!(f, "{:?}", kind)?;
                    if !args.is_empty() {
                        write!(
                            f,
                            "[{}]",
                            args.iter()
                                .map(|arg| format!("{arg:?}"))
                                .collect::<Vec<_>>()
                                .join(", ")
                        )?;
                    }
                    Ok(())
                }
            }
            CandidateNode::Var => {
                write!(f, "?")
            }
        }
    }
}
impl Debug for Candidate {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.0.borrow())
    }
}
impl Debug for TyNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match *self {
            TyNode::Const { kind, ref args } => {
                if kind == Kind::Function {
                    let mut iter = args.iter().rev();
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
                    write!(f, "{:?}", kind)?;
                    if !args.is_empty() {
                        write!(
                            f,
                            "[{}]",
                            args.iter()
                                .map(|arg| format!("{arg:?}"))
                                .collect::<Vec<_>>()
                                .join(", ")
                        )?;
                    }
                    Ok(())
                }
            }
            TyNode::Var => {
                write!(f, "?")
            }
        }
    }
}

impl Debug for Ty {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.0.borrow())
    }
}
