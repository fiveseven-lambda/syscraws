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

use crate::range::Range;

#[derive(Debug)]
pub struct Lines<'s> {
    lines: Vec<&'s str>,
    fronts: Vec<usize>,
}

impl<'s> Lines<'s> {
    pub fn new(input: &'s str) -> Lines<'s> {
        let mut fronts = Vec::new();
        let mut iter = input.char_indices();
        while let Some((i, _)) = iter.next() {
            fronts.push(i);
            iter.find(|&(_, ch)| ch == '\n');
        }
        fn split_lines<'s>(input: &'s str, line_fronts: &[usize], lines: &mut Vec<&'s str>) {
            if let Some((&last_front, rem_fronts)) = line_fronts.split_last() {
                let (rem, last_line) = input.split_at(last_front);
                split_lines(rem, rem_fronts, lines);
                lines.push(last_line);
            }
        }
        let mut lines = Vec::new();
        split_lines(input, &fronts, &mut lines);
        Lines { lines, fronts }
    }
    pub fn line_column(&self, pos: usize) -> (usize, usize) {
        let line = self.fronts.partition_point(|&front| front <= pos) - 1;
        let column = pos - self.fronts[line];
        (line, column)
    }
    pub fn eprint_pos(&self, pos: usize) {
        let (line, column) = self.line_column(pos);
        eprintln!(
            "L{}: {} !-> {}",
            line + 1,
            &self.lines[line][..column].escape_default(),
            &self.lines[line][column..].escape_default()
        );
    }
    pub fn eprint_range(&self, &Range { start, end }: &Range) {
        let (start_line, start_column) = self.line_column(start);
        let (end_line, end_column) = self.line_column(end);
        if start_line == end_line {
            eprintln!(
                "L{}: {} !-> {} <-! {}",
                start_line + 1,
                &self.lines[start_line][..start_column].escape_default(),
                &self.lines[start_line][start_column..end_column].escape_default(),
                &self.lines[start_line][end_column..].escape_default(),
            );
        } else {
            eprintln!(
                "L{}: {} !-> {}",
                start_line + 1,
                &self.lines[start_line][..start_column].escape_default(),
                &self.lines[start_line][start_column..].escape_default(),
            );
            eprintln!(
                "L{}: {} <-! {}",
                end_line + 1,
                &self.lines[end_line][..end_column].escape_default(),
                &self.lines[end_line][end_column..].escape_default(),
            );
        }
    }
}
