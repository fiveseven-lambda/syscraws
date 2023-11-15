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

mod debug_print;
use crate::pre_ast::PTerm;
use either::Either;

pub enum Token<'id> {
    Identifier(&'id str),
    KeywordIf,
    KeywordElse,
    KeywordWhile,
    KeywordReturn,
    Number(&'id str),
    String(Vec<Either<String, Option<PTerm<'id>>>>),
    Plus,
    PlusEqual,
    DoublePlus,
    Hyphen,
    HyphenEqual,
    DoubleHyphen,
    HyphenGreater,
    Asterisk,
    AsteriskEqual,
    Slash,
    SlashEqual,
    Percent,
    PercentEqual,
    Equal,
    DoubleEqual,
    EqualGreater,
    Exclamation,
    ExclamationEqual,
    Greater,
    GreaterEqual,
    DoubleGreater,
    DoubleGreaterEqual,
    TripleGreater,
    TripleGreaterEqual,
    Less,
    LessEqual,
    DoubleLess,
    DoubleLessEqual,
    TripleLess,
    TripleLessEqual,
    Ampersand,
    AmpersandEqual,
    DoubleAmpersand,
    Bar,
    BarEqual,
    DoubleBar,
    Circumflex,
    CircumflexEqual,
    Dot,
    Colon,
    Semicolon,
    Comma,
    Question,
    Hash,
    Tilde,
    OpeningParenthesis,
    ClosingParenthesis,
    OpeningBracket,
    ClosingBracket,
    OpeningBrace,
    ClosingBrace,
}
