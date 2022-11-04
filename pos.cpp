/**
 * Copyright (c) 2022 Atsushi Komaba
 *
 * This file is part of Syscraws.
 * Syscraws is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or any later version.
 * Syscraws is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with Syscraws. If not, see <https://www.gnu.org/licenses/>. 
 *
 * @file pos.cpp
 */
#include "pos.hpp"

namespace pos {
    /**
     * @brief デフォルトコンストラクタ
     *
     * `line`，`byte` をともに 0 で初期化する．
     */
    Pos::Pos(): line(0), byte(0) {}

    /**
     * @brief コンストラクタ
     * @param line 何行目か（0-indexed で）
     * @param byte 何バイト目か（0-indexed で）
     */
    Pos::Pos(std::size_t line, std::size_t byte): line(line), byte(byte) {}

    /**
     * @brief デフォルトコンストラクタ
     *
     * 各値を 0 で初期化する．
     */
    Range::Range() = default;

    /**
     * @brief コンストラクタ
     *
     * @param start 開始（自身含む）
     * @param end 終了（自身含まない）
     */
    Range::Range(Pos start, Pos end): start(start), end(end) {}

    /**
     * @brief コンストラクタ
     *
     * 複数行にまたがらない場合（開始と終了が同じ行にある場合）に使う．
     * @param line 何行目か（0-indexed で）
     * @param start_byte 何バイト目からか（0-indexed で，`start_byte` 自身も含む）
     * @param end_byte 何バイト目の手前までか（0-indexed で，`end_byte` 自身は含まない）
     */
    Range::Range(std::size_t line, std::size_t start_byte, std::size_t end_byte):
        start(line, start_byte),
        end(line, end_byte) {}

    /**
     * @brief ムーブコンストラクタ
     */
    Range::Range(Range &&) = default;

    /**
     * @brief ムーブ代入演算子
     */
    Range &Range::operator=(Range &&) = default;

    /**
     * @brief クローン
     */
    Range Range::clone(){
        return Range(start, end);
    }

    /**
     * @brief Pos から `line`，`byte` の値を取り出す．
     * @return `first` が `line`，`second` が `byte`．
     */
    std::pair<std::size_t, std::size_t> Pos::into_pair() const {
        return {line, byte};
    }

    /**
     * @brief 範囲を結合する．
     */
    Range &Range::operator+=(const Range &other){
        end = other.end;
        return *this;
    }

    /**
     * @brief 範囲を結合する．
     */
    Range operator+(const Range &left, const Range &right){
        return Range(left.start, right.end);
    }

    /**
     * @brief `line`，`byte` の値を 1-indexed に直して出力する．
     */
    std::ostream &operator<<(std::ostream &os, const Pos &pos){
        return os << pos.line + 1 << ":" << pos.byte + 1;
    }
    /**
     * @brief 開始と終了の `line`，`byte` を 1-indexed，閉区間に直して出力する．
     */
    std::ostream &operator<<(std::ostream &os, const Range &range){
        auto [sline, sbyte] = range.start.into_pair();
        auto [eline, ebyte] = range.end.into_pair();
        return os
            << sline + 1 << ":" << sbyte + 1
            << "-" << eline + 1 << ":" << ebyte;
    }

    /**
     * @brief ソースコードから当該の行を切り出して出力する．
     * @param source ソースコード（文字列）
     */
    void Pos::eprint(const std::deque<std::string> &source) const {
        std::cerr
            << source[line].substr(0, byte)
            << " !-> "
            << source[line].substr(byte)
            << std::endl;
    }
    /**
     * @brief ソースコードから当該の範囲の前後を切り出して出力する．
     * @param source ソースコード（文字列）
     */
    void Range::eprint(const std::deque<std::string> &source) const {
        auto [sline, sbyte] = start.into_pair();
        auto [eline, ebyte] = end.into_pair();
        if(sline == eline){
            std::cerr
                << source[sline].substr(0, sbyte)
                << " !-> "
                << source[sline].substr(sbyte, ebyte - sbyte)
                << " <-! "
                << source[sline].substr(ebyte)
                << std::endl;
        } else {
            std::cerr
                << source[sline].substr(0, sbyte)
                << " !-> "
                << source[sline].substr(sbyte)
                << std::endl;
            if(eline - sline == 1){
            }else if(eline - sline == 2){
                std::cerr
                << source[sline + 1]
                << std::endl;
            }else{
                std::cerr
                << " ("
                << eline - sline - 1
                << " lines)"
                << std::endl;
            }
            std::cerr
                << source[eline].substr(0, ebyte)
                << " <-! "
                << source[eline].substr(ebyte)
                << std::endl;
        }
    }
}
