/**
 * Copyright (c) 2022 Atsushi Komaba
 *
 * This file is part of Syscraws.
 * Syscraws is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or any later version.
 * Syscraws is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with Syscraws. If not, see <https://www.gnu.org/licenses/>. 
 *
 * @file type.hpp
 * @brief 型を定義する．
 */
#ifndef TYPE_HPP
#define TYPE_HPP

#include <vector>
#include <memory>
#include <functional>
#include <unordered_set>

/**
 * @brief 型を定義する．
 */
namespace type {
    /**
     * @brief 全ての型の基底クラス
     */
    class Type {
    public:
        virtual ~Type();
#ifdef DEBUG
        virtual void debug_print(int) const = 0;
#endif
    };

    /**
     * @brief 論理型
     */
    class Bool : public Type {
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };

    /**
     * @brief 整数型
     */
    class Int : public Type {
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };

    /**
     * @brief 浮動小数点数型
     */
    class Float : public Type {
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };

    /**
     * @brief 文字列型
     */
    class Str : public Type {
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };

    /**
     * @brief 関数型
     */
    class Func : public Type {
        std::vector<std::reference_wrapper<const Type>> args;
        const Type &ret;
    public:
        Func(const std::vector<std::reference_wrapper<const Type>> &, const Type &);
        const std::vector<std::reference_wrapper<const Type>> &get_args() const;
        const Type &get_ret() const;
        std::pair<const std::vector<std::reference_wrapper<const Type>> &, const Type &> get_pair() const;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };

    /**
     * @brief `std::unique_ptr<Func>` をもつ `std::unordered_set`に用いるハッシュ関数オブジェクト
     */
    struct FuncHash {
        using is_transparent = void;
        std::size_t operator()(const std::pair<const std::vector<std::reference_wrapper<const Type>> &, const Type &> &) const noexcept;
        std::size_t operator()(const std::unique_ptr<Func> &) const noexcept;
    };
    /**
     * @brief `std::unique_ptr<Func>` をもつ `std::unordered_set`に用いる比較関数オブジェクト
     */
    struct FuncEq {
        using is_transparent = void;
        bool operator()(const std::pair<const std::vector<std::reference_wrapper<const Type>> &, const Type &> &, const std::pair<const std::vector<std::reference_wrapper<const Type>> &, const Type &> &) const noexcept;
        bool operator()(const std::pair<const std::vector<std::reference_wrapper<const Type>> &, const Type &> &, const std::unique_ptr<Func> &) const noexcept;
        bool operator()(const std::unique_ptr<Func> &, const std::unique_ptr<Func> &) const noexcept;
    };

    /**
     * @brief 型を管理する．
     *
     * 同じ型を表すオブジェクトを複数作ることはないため，アドレスで比較できる．
     */
    class Context {
        Bool bool_ty;
        Int int_ty;
        Float float_ty;
        Str str_ty;
        std::unordered_set<std::unique_ptr<Func>, FuncHash, FuncEq> funcs;
    public:
        /**
         * @brief 論理型を得る．
         */
        const Bool &get_bool() &;
        /**
         * @brief 整数型を得る．
         */
        const Int &get_int() &;
        /**
         * @brief 浮動小数点数型を得る．
         */
        const Float &get_float() &;
        /**
         * @brief 文字列型を得る．
         */
        const Str &get_str() &;
        /**
         * @brief 関数型を得る．
         */
        const Func &get_func(const std::vector<std::reference_wrapper<const Type>> &, const Type &) &;
#ifdef DEBUG
        void debug_print(int) const;
#endif
    };
}

#endif
