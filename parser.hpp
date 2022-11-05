/**
 * Copyright (c) 2022 Atsushi Komaba
 *
 * This file is part of Syscraws.
 * Syscraws is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or any later version.
 * Syscraws is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with Syscraws. If not, see <https://www.gnu.org/licenses/>. 
 *
 * @file parser.hpp
 * @brief 構文解析を行い， token::Token の列を pre_ast::Stmt に変換する．
 */

#ifndef PARSER_HPP
#define PARSER_HPP

#include "lexer.hpp"

/**
 * @brief token::Token の列を pre_ast::Stmt に変換する．
 *
 * @retval nullptr EOF に達した．
 */
std::unique_ptr<pre_ast::Stmt> parse(lexer::Lexer &);

#endif
