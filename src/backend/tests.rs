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
use std::collections::HashMap;
use std::fmt::{self, Debug, Formatter};
use std::sync::{LazyLock, Mutex};

impl Debug for Ty {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        static UNIQUE_ID_MAP: LazyLock<Mutex<HashMap<usize, usize>>> =
            LazyLock::new(|| Mutex::new(HashMap::new()));
        let unique_id = {
            let mut unique_id_map = UNIQUE_ID_MAP.lock().unwrap();
            let next_id = unique_id_map.len();
            *unique_id_map
                .entry(Rc::as_ptr(&self.inner) as usize)
                .or_insert(next_id)
        };
        write!(f, "{:x}:{:?}", unique_id, *self.inner.borrow())
    }
}

impl Debug for TyInner {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TyInner::Constructor(constructor) => write!(f, "{constructor:?}"),
            TyInner::Parameter(index) => write!(f, "T{index}"),
            TyInner::Application {
                constructor,
                arguments,
            } => write!(f, "{constructor:?}{arguments:?}"),
            TyInner::List(elements) => write!(f, "{elements:?}"),
            TyInner::Undetermined => write!(f, "?"),
            TyInner::SameAs(ty) => write!(f, "{ty:?}"),
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
