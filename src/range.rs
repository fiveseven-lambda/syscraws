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

#[derive(Clone)]
pub struct Range {
    start: usize,
    end: usize,
}

impl Range {
    pub fn new(start: usize, end: usize) -> Range {
        Range { start, end }
    }
}

impl std::ops::Add for Range {
    type Output = Range;
    fn add(self, other: Self) -> Self::Output {
        Range {
            start: self.start,
            end: other.end,
        }
    }
}

use std::fmt::{Debug, Formatter, Result as FmtResult};
impl Debug for Range {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{:03}-{:03}:", self.start, self.end)
    }
}
