/**
 * Copyright (c) 2022 Atsushi Komaba
 *
 * This file is part of Syscraws.
 * Syscraws is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or any later version.
 * Syscraws is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with Syscraws. If not, see <https://www.gnu.org/licenses/>. 
 *
 * @file pos.hpp
 * @brief エラー出力のための位置情報をもつクラスを定義する．
 */
#ifndef POS_HPP
#define POS_HPP

#include <cstddef>
#include <iostream>
#include <utility>
#include <vector>
#include <deque>
#include <string>

/**
 * @brief エラー出力のための位置情報をもつクラスを定義する．
 */
namespace pos {
    /**
     * @brief ソースコード上の文字の位置
     */
    class Pos {
        std::size_t line;
        std::size_t byte;
    public:
        Pos();
        Pos(std::size_t, std::size_t);
        std::pair<std::size_t, std::size_t> into_pair() const;
        friend std::ostream &operator<<(std::ostream &, const Pos &);
        void eprint(const std::deque<std::string> &) const;
    };

    /**
     * @brief ソースコード上の式や文の範囲
     */
    class Range {
        Pos start;
        Pos end;
    public:
        Range();
        Range(std::size_t, std::size_t, std::size_t);
        Range(Pos, Pos);
        Range(const Range &) = delete;
        Range &operator=(const Range &) = delete;
        Range(Range &&);
        Range &operator=(Range &&);
        Range &operator+=(const Range &);
        friend Range operator+(const Range &, const Range &);
        Range clone();
        friend std::ostream &operator<<(std::ostream &, const Range &);
        void eprint(const std::deque<std::string> &) const;
    };
}

#endif
