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

//! [`Error`] を定義する．[`parse()`](super::parse) が吐く．

use crate::lines::Lines;
use std::ops::Range;

/**
 * ソースコードの文字列から pre AST を得る過程（字句解析，構文解析）で起こるエラーを定義する．
 */
#[derive(Debug)]
pub enum Error {
    /**
     * 字句解析の段階で，予期せぬ文字が見つかった．
     */
    UnexpectedCharacter(usize),
    /**
     * コメントが終了しなかった．
     *
     * コメントはネストされている可能性がある．終了しなかったコメントの開始位置を `Vec` で全て持っている．
     */
    UnterminatedComment(Vec<usize>),
    /**
     * 文字列リテラルが終了しなかった．
     *
     * 文字列リテラルもネストの可能性があるが，今のところ最も内側しか報告できない．
     */
    UnterminatedStringLiteral(usize),
    /**
     * 文字列リテラル中に無効なエスケープシーケンスがあった．
     */
    InvalidEscapeSequence(usize),
    /**
     * 予期せぬトークンがあった．
     *
     * 文の先頭で起こる．
     */
    UnexpectedToken { error_pos: Range<usize> },
    /**
     * 予期せぬトークンがあった．
     *
     * 文の最後のセミコロンを忘れた可能性が高い．
     */
    UnexpectedTokenAfter {
        /// 予期せぬトークン．
        error_pos: Range<usize>,
        /// 直前のトークン．
        reason_pos: Range<usize>,
    },
    /**
     * トークンが期待されたが，EOF があった．
     *
     * 文の最後のセミコロンを忘れたか，`{` を閉じ忘れたか．
     */
    EOFAfter {
        /// 直前のトークン．
        reason_pos: Range<usize>,
    },
}

impl Error {
    pub fn eprint(&self, input: &str) {
        let lines = Lines::new(input);
        match *self {
            Error::UnexpectedCharacter(pos) => {
                eprintln!("Unexpected character at {:?}", lines.line_column(pos));
                lines.eprint_pos(pos);
            }
            Error::UnterminatedComment(ref pos_comments) => {
                eprintln!("Unterminated comment");
                for &pos in pos_comments {
                    lines.eprint_pos(pos);
                }
            }
            Error::UnterminatedStringLiteral(pos) => {
                eprintln!(
                    "Unterminated string literal started at {:?}",
                    lines.line_column(pos)
                );
                lines.eprint_pos(pos);
            }
            Error::InvalidEscapeSequence(pos) => {
                eprintln!("Invalid escape sequence at {:?}", lines.line_column(pos));
                lines.eprint_pos(pos);
            }
            Error::UnexpectedToken {
                error_pos: ref token,
            } => {
                eprintln!("Unexpected token at {:?}", token);
                lines.eprint_range(token);
            }
            Error::UnexpectedTokenAfter {
                error_pos: ref token,
                reason_pos: ref reason,
            } => {
                eprintln!("Unexpected token at {:?}", token);
                lines.eprint_range(token);
                eprintln!("Note:");
                lines.eprint_range(reason);
            }
            Error::EOFAfter {
                reason_pos: ref reason,
            } => {
                eprintln!("Unexpected EOF");
                lines.eprint_range(reason);
            }
        }
    }
}
