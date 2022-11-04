/**
 * Copyright (c) 2022 Atsushi Komaba
 *
 * This file is part of Syscraws.
 * Syscraws is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or any later version.
 * Syscraws is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with Syscraws. If not, see <https://www.gnu.org/licenses/>. 
 *
 * @file error.hpp
 * @brief エラーを定義する
 */
#ifndef ERROR_HPP
#define ERROR_HPP

#include <memory>
#include "pos.hpp"

/**
 * @brief 各種エラーを定義する
 */
namespace error {
    /**
     * @brief 全てのエラーの基底クラス．
     */
    class Error {
    public:
        virtual ~Error();
        /**
         * @brief 標準エラー出力でエラーの内容を説明する．
         * @param source ソースコードの文字列
         */
        void virtual eprint(const std::deque<std::string> &source) const = 0;
    };

    /**
     * @brief エラーを生成するときのヘルパ関数．
     *
     * `T` が `Error` の派生クラスのとき，`make<T>(...)` は `T` のコンストラクタを呼び出し `std::unique_ptr<Error>` にして返す．
     */
    template<class Err, class... Args>
    std::unique_ptr<Error> make(Args&&... args){
        return std::make_unique<Err>(std::forward<Args>(args)...);
    }

    /**
     * @brief 字句解析：予期せぬ文字が現れた．
     * 空白とコメント以外に，トークンの始まりとして適さない文字があった．
     */
    class UnexpectedCharacter : public Error {
        pos::Pos pos;
    public:
        UnexpectedCharacter(pos::Pos);
        void eprint(const std::deque<std::string> &) const override;
    };

    /**
     * @brief 字句解析：コメントが終了しないまま EOF に達した．
     */
    class UnterminatedComment : public Error {
        std::vector<pos::Pos> poss;
    public:
        UnterminatedComment(std::vector<pos::Pos>);
        void eprint(const std::deque<std::string> &) const override;
    };

    /**
     * @brief 字句解析：文字列リテラル中に無効なエスケープシーケンスがあった．
     */
    class InvalidEscapeSequence : public Error {
        pos::Pos pos;
    public:
        InvalidEscapeSequence(pos::Pos);
        void eprint(const std::deque<std::string> &) const override;
    };

    /**
     * @brief 字句解析：文字列リテラルが終了しないまま EOF に達した．
     */
    class UnterminatedStringLiteral : public Error {
        pos::Pos pos;
    public:
        UnterminatedStringLiteral(pos::Pos);
        void eprint(const std::deque<std::string> &) const override;
    };

    /**
     * @brief 字句解析：無効な数値リテラルがあった．
     */
    class InvalidNumericLiteral : public Error {
        pos::Range pos;
    public:
        InvalidNumericLiteral(pos::Range);
        void eprint(const std::deque<std::string> &) const override;
    };

    /**
     * @brief 構文解析（pre-AST）：演算子の後に予期せぬトークンがあった．
     */
    class UnexpectedTokenAfterOperator : public Error {
        pos::Range op, token;
    public:
        UnexpectedTokenAfterOperator(pos::Range, pos::Range);
        void eprint(const std::deque<std::string> &) const override;
    };

    /**
     * @brief 構文解析（pre-AST）：演算子の後に EOF があった．
     */
    class EOFAfterOperator : public Error {
        pos::Range op;
    public:
        EOFAfterOperator(pos::Range);
        void eprint(const std::deque<std::string> &) const override;
    };

    /**
     * @brief 構文解析（pre-AST）：開き括弧に対応する閉じ括弧が来ないまま EOF に達した．
     */
    class NoClosingBracket: public Error {
        pos::Range open;
    public:
        NoClosingBracket(pos::Range);
        void eprint(const std::deque<std::string> &) const override;
    };

    /**
     * @brief 構文解析（pre-AST）：括弧の中に予期せぬトークンがあった．
     */
    class UnexpectedTokenInBracket : public Error {
        pos::Range open, token;
    public:
        UnexpectedTokenInBracket(pos::Range, pos::Range);
        void eprint(const std::deque<std::string> &) const override;
    };

    /**
     * @brief 構文解析（pre-AST）：対応する箇所にある開き括弧と閉じ括弧が，別の種類だった．
     */
    class DifferentClosingBracket: public Error {
        pos::Range open, close;
    public:
        DifferentClosingBracket(pos::Range, pos::Range);
        void eprint(const std::deque<std::string> &) const override;
    };

    /**
     * @brief 構文解析（pre-AST）：リストの中に空の要素があった．
     */
    class EmptyItemInList : public Error {
        pos::Range comma;
    public:
        EmptyItemInList(pos::Range);
        void eprint(const std::deque<std::string> &) const override;
    };

    /**
     * @brief 構文解析（pre-AST）：キーワードの後に予期せぬ EOF があった．
     */
    class UnexpectedEOFAfterKeyword : public Error {
        pos::Range keyword;
    public:
        UnexpectedEOFAfterKeyword(pos::Range);
        void eprint(const std::deque<std::string> &) const override;
    };
    /**
     * @brief 構文解析（pre-AST）：キーワードの後に予期せぬ EOF があった．
     */
    class UnexpectedTokenAfterKeyword : public Error {
        pos::Range keyword, token;
    public:
        UnexpectedTokenAfterKeyword(pos::Range, pos::Range);
        void eprint(const std::deque<std::string> &) const override;
    };

    /**
     * @brief 構文解析（pre-AST）：文の最後にセミコロンがなく EOF に達した．
     */
    class EOFAtEndOfStmt : public Error {
        pos::Range stmt;
    public:
        EOFAtEndOfStmt(pos::Range);
        void eprint(const std::deque<std::string> &) const override;
    };

    /**
     * @brief 構文解析（pre-AST）：文の後にセミコロンがなく予期せぬトークンがあった．
     */
    class UnexpectedTokenAtEndOfStmt : public Error {
        pos::Range stmt, token;
    public:
        UnexpectedTokenAtEndOfStmt(pos::Range, pos::Range);
        void eprint(const std::deque<std::string> &) const override;
    };

    /**
     * @brief 構文解析（pre-AST）：文の先頭としてありえないトークンがあった．
     */
    class UnexpectedTokenAtBeginningOfStmt : public Error {
        pos::Range token;
    public:
        UnexpectedTokenAtBeginningOfStmt(pos::Range);
        void eprint(const std::deque<std::string> &) const override;
    };

    /**
     * @brief 構文解析（pre-AST）：制御構文の中に予期せぬトークンがあった．
     */
    class UnexpectedTokenInControl : public Error {
        pos::Range control, token;
    public:
        UnexpectedTokenInControl(pos::Range, pos::Range);
        void eprint(const std::deque<std::string> &) const override;
    };
    /**
     * @brief 構文解析（pre-AST）：制御構文の中に予期せぬ EOF があった．
     */
    class UnexpectedEOFInControl : public Error {
        pos::Range control;
    public:
        UnexpectedEOFInControl(pos::Range);
        void eprint(const std::deque<std::string> &) const override;
    };

    /**
     * @brief 構文解析（AST）：
     */
    class OverflowInIntegerLiteral : public Error {
        std::exception &error;
        pos::Range pos;
    public:
        OverflowInIntegerLiteral(std::exception &, pos::Range);
        void eprint(const std::deque<std::string> &) const override;
    };

    /**
     * @brief エラーメッセージが未実装
     */
    class Unimplemented: public Error {
        const char *file;
        unsigned line;
    public:
        Unimplemented(const char *, unsigned);
        void eprint(const std::deque<std::string> &) const override;
    };

}

#define TODO throw error::make<error::Unimplemented>(__FILE__, __LINE__)

#endif
