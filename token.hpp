/**
 * Copyright (c) 2022 Atsushi Komaba
 *
 * This file is part of Syscraws.
 * Syscraws is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or any later version.
 * Syscraws is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with Syscraws. If not, see <https://www.gnu.org/licenses/>. 
 *
 * @file token.hpp
 * @brief トークンを定義する
 */
#ifndef TOKEN_HPP
#define TOKEN_HPP

#include "pre_ast.hpp"

/**
 * @brief トークンを定義する．
 */
namespace token {
    enum class Keyword {
        If,
        Else,
        While,
        Break,
        Continue,
        Return,
    };
    /**
     * @brief 全てのトークンの基底クラス
     */
    class Token {
    public:
        //! ソースコード中の位置
        pos::Range pos;
        virtual ~Token();
        virtual std::optional<Keyword> keyword();
        virtual std::unique_ptr<pre_ast::Term> factor();
        virtual std::optional<pre_ast::UnaryOperator> prefix(), suffix();
        virtual std::optional<pre_ast::BinaryOperator> infix();
        virtual std::optional<pre_ast::BracketType> opening_bracket_type() const, closing_bracket_type() const;
        virtual bool
            is_comma() const,
            is_semicolon() const,
            is_opening_parenthesis() const,
            is_closing_parenthesis() const,
            is_opening_bracket() const,
            is_closing_bracket() const,
            is_opening_brace() const,
            is_closing_brace() const;
#ifdef DEBUG
        virtual void debug_print(int) const = 0;
#endif
    };
    /**
     * @brief 識別子 `[a-zA-Z_$][a-zA-Z0-9_$]*`
     */
    class Identifier : public Token {
        std::string_view name;
    public:
        Identifier(std::string_view);
        std::unique_ptr<pre_ast::Term> factor() override;
        std::optional<Keyword> keyword() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    /**
     * @brief 数値リテラル
     */
    class Number : public Token {
        std::string_view value;
    public:
        Number(std::string_view);
        std::unique_ptr<pre_ast::Term> factor() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    /**
     * @brief 文字列リテラル
     */
    class String : public Token {
        std::string value;
    public:
        String(std::string value);
        std::unique_ptr<pre_ast::Term> factor() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `+`
    class Plus : public Token {
        std::optional<pre_ast::UnaryOperator> prefix() override;
        std::optional<pre_ast::BinaryOperator> infix() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `++`
    class DoublePlus : public Token {
        std::optional<pre_ast::UnaryOperator> prefix() override;
        std::optional<pre_ast::UnaryOperator> suffix() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `+=`
    class PlusEqual : public Token {
        std::optional<pre_ast::BinaryOperator> infix() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `-`
    class Hyphen : public Token {
        std::optional<pre_ast::UnaryOperator> prefix() override;
        std::optional<pre_ast::BinaryOperator> infix() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `--`
    class DoubleHyphen : public Token {
        std::optional<pre_ast::UnaryOperator> prefix() override;
        std::optional<pre_ast::UnaryOperator> suffix() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `-=`
    class HyphenEqual : public Token {
        std::optional<pre_ast::BinaryOperator> infix() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `*`
    class Asterisk : public Token {
        std::optional<pre_ast::BinaryOperator> infix() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `*=`
    class AsteriskEqual : public Token {
        std::optional<pre_ast::BinaryOperator> infix() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `/`
    class Slash : public Token {
        std::optional<pre_ast::UnaryOperator> prefix() override;
        std::optional<pre_ast::BinaryOperator> infix() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `/=`
    class SlashEqual : public Token {
        std::optional<pre_ast::BinaryOperator> infix() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `%`
    class Percent : public Token {
        std::optional<pre_ast::BinaryOperator> infix() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `%=`
    class PercentEqual : public Token {
        std::optional<pre_ast::BinaryOperator> infix() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `=`
    class Equal : public Token {
        std::optional<pre_ast::BinaryOperator> infix() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `==`
    class DoubleEqual : public Token {
        std::optional<pre_ast::BinaryOperator> infix() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `!`
    class Exclamation : public Token {
        std::optional<pre_ast::UnaryOperator> prefix() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `!=`
    class ExclamationEqual : public Token {
        std::optional<pre_ast::BinaryOperator> infix() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `<`
    class Less : public Token {
        std::optional<pre_ast::BinaryOperator> infix() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `<=`
    class LessEqual : public Token {
        std::optional<pre_ast::BinaryOperator> infix() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `<<`
    class DoubleLess : public Token {
        std::optional<pre_ast::BinaryOperator> infix() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `<<=`
    class DoubleLessEqual : public Token {
        std::optional<pre_ast::BinaryOperator> infix() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `<<<`
    class TripleLess : public Token {
        std::optional<pre_ast::BinaryOperator> infix() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `<<<=`
    class TripleLessEqual : public Token {
        std::optional<pre_ast::BinaryOperator> infix() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `>`
    class Greater : public Token {
        std::optional<pre_ast::BinaryOperator> infix() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `>=`
    class GreaterEqual : public Token {
        std::optional<pre_ast::BinaryOperator> infix() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `>>`
    class DoubleGreater : public Token {
        std::optional<pre_ast::BinaryOperator> infix() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `>>=`
    class DoubleGreaterEqual : public Token {
        std::optional<pre_ast::BinaryOperator> infix() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `>>>`
    class TripleGreater : public Token {
        std::optional<pre_ast::BinaryOperator> infix() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `>>>=`
    class TripleGreaterEqual : public Token {
        std::optional<pre_ast::BinaryOperator> infix() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `&`
    class Ampersand : public Token {
        std::optional<pre_ast::BinaryOperator> infix() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `&=`
    class AmpersandEqual : public Token {
        std::optional<pre_ast::BinaryOperator> infix() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `&&`
    class DoubleAmpersand : public Token {
        std::optional<pre_ast::BinaryOperator> infix() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `|`
    class Bar : public Token {
        std::optional<pre_ast::BinaryOperator> infix() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `|=`
    class BarEqual : public Token {
        std::optional<pre_ast::BinaryOperator> infix() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `||`
    class DoubleBar : public Token {
        std::optional<pre_ast::BinaryOperator> infix() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `^`
    class Circumflex : public Token {
        std::optional<pre_ast::BinaryOperator> infix() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `^=`
    class CircumflexEqual : public Token {
        std::optional<pre_ast::BinaryOperator> infix() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `.`
    class Dot : public Token {
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `:`
    class Colon : public Token {
        std::optional<pre_ast::BinaryOperator> infix() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `;`
    class Semicolon : public Token {
        bool is_semicolon() const override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `,`
    class Comma : public Token {
        bool is_comma() const override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `?`
    class Question : public Token {
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `#`
    class Hash : public Token {
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `~`
    class Tilde : public Token {
        std::optional<pre_ast::UnaryOperator> prefix() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `(`
    class OpeningParenthesis : public Token {
        bool is_opening_parenthesis() const override;
        std::optional<pre_ast::BracketType> opening_bracket_type() const override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `)`
    class ClosingParenthesis : public Token {
        bool is_closing_parenthesis() const override;
        std::optional<pre_ast::BracketType> closing_bracket_type() const override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `[`
    class OpeningBracket : public Token {
        bool is_opening_bracket() const override;
        std::optional<pre_ast::BracketType> opening_bracket_type() const override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `]`
    class ClosingBracket : public Token {
        bool is_closing_bracket() const override;
        std::optional<pre_ast::BracketType> closing_bracket_type() const override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `{`
    class OpeningBrace : public Token {
        bool is_opening_brace() const override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    //! `}`
    class ClosingBrace : public Token {
        bool is_closing_brace() const override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
}

#endif
