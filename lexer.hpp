/**
 * Copyright (c) 2022 Atsushi Komaba
 *
 * This file is part of Syscraws.
 * Syscraws is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or any later version.
 * Syscraws is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with Syscraws. If not, see <https://www.gnu.org/licenses/>. 
 *
 * @file lexer.hpp
 * @brief 字句解析を行う
 */
#ifndef LEXER_HPP
#define LEXER_HPP

#include <queue>
#include <istream>
#include <unicode/unistr.h>

#include "token.hpp"

/**
 * @brief 字句解析を行う
 */
namespace lexer {
    /**
     * @brief Lexer が内部で用いる．
     *
     * 入力を読まない．
     */
    class LineLexer {
        std::vector<pos::Pos> comments;
        std::optional<std::pair<pos::Pos, icu::UnicodeString>> string;
    public:
        /**
         * @brief 何かトークンを読むと，false を代入されるまでずっと true になる．
         */
        bool in_stmt;
        bool in_stmt_or_comment() const;
        LineLexer();
        void run(
            std::size_t,
            const std::string_view &,
            std::queue<std::unique_ptr<token::Token>> &
        );
        void deal_with_eof();
    };

    /**
     * @brief 入力を読みながら，トークンに分解する．
     */
    class Lexer {
        std::istream &source;
        bool prompt;
        std::deque<std::string> log;
        std::queue<std::unique_ptr<token::Token>> tokens;
        LineLexer line_lexer;
    public:
        Lexer(std::istream &, bool);
        void end_stmt();
        const std::deque<std::string> &get_log() const;
        std::unique_ptr<token::Token> next(), &peek();
    };
}

#endif
