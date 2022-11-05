/**
 * Copyright (c) 2022 Atsushi Komaba
 *
 * This file is part of Syscraws.
 * Syscraws is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or any later version.
 * Syscraws is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with Syscraws. If not, see <https://www.gnu.org/licenses/>. 
 *
 * @file pre_ast.hpp
 * @brief pre-AST を定義する．
 *
 * トークン列は，pre-AST を経て AST へと変換される．
 */
#ifndef PRE_AST_HPP
#define PRE_AST_HPP

#include "ast.hpp"

/**
 * @brief pre-AST を定義する．
 *
 * トークン列は，pre-AST を経て AST へと変換される．
 */
namespace pre_ast {
    enum class BracketType {
        Round,
        Square,
    };

    struct State {
        State();
    };

    /**
     * @brief 全ての式の基底クラス．
     */
    class Term {
    public:
        pos::Range pos;
        virtual ~Term();
        virtual std::unique_ptr<ast::Expr> to_expr() = 0;
        virtual std::unique_ptr<ast::Type> to_type();
        virtual std::unique_ptr<ast::Pat> to_pat();
        virtual std::unique_ptr<ast::Stmt> to_stmt(pos::Range);
        virtual std::unique_ptr<ast::Item> to_item(pos::Range);
#ifdef DEBUG
        virtual void debug_print(int) const = 0;
#endif
    };

    /**
     * @brief 単一の識別子からなる式．
     */
    class Identifier : public Term {
        std::string_view name;
    public:
        Identifier(std::string_view);
        std::unique_ptr<ast::Expr> to_expr() override;
        std::unique_ptr<ast::Type> to_type() override;
        std::unique_ptr<ast::Pat> to_pat() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    /**
     * @brief 単一の10進整数リテラルからなる式．
     */
    class DecInt : public Term {
        std::string_view value;
    public:
        DecInt(std::string_view);
        std::unique_ptr<ast::Expr> to_expr() override;
        std::string_view get_value();
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    /**
     * @brief 単一の2進整数リテラルからなる式．
     */
    class BinInt : public Term {
        std::string_view value;
    public:
        BinInt(std::string_view);
        std::unique_ptr<ast::Expr> to_expr() override;
        std::string_view get_value();
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    /**
     * @brief 単一の8進整数リテラルからなる式．
     */
    class OctInt : public Term {
        std::string_view value;
    public:
        OctInt(std::string_view);
        std::unique_ptr<ast::Expr> to_expr() override;
        std::string_view get_value();
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    /**
     * @brief 単一の16進整数リテラルからなる式．
     */
    class HexInt : public Term {
        std::string_view value;
    public:
        HexInt(std::string_view);
        std::unique_ptr<ast::Expr> to_expr() override;
        std::string_view get_value();
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    /**
     * @brief 単一の浮動小数点数リテラルからなる式．
     */
    class Float : public Term {
        std::string_view value;
    public:
        Float(std::string_view);
        std::unique_ptr<ast::Expr> to_expr() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    /**
     * @brief 単一の文字列リテラルからなる式．
     */
    class String : public Term {
        std::string value;
    public:
        String(std::string);
        std::unique_ptr<ast::Expr> to_expr() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };

    /**
     * @brief 単項演算子．
     */
    enum class UnaryOperator {
        Plus, ///< 正号
        Minus, ///< 負号
        Recip, ///< 逆数
        LogicalNot, ///< 論理否定
        BitNot, ///< ビット毎否定
        PreInc, ///< 前置インクリメント
        PreDec, ///< 前置デクリメント
        PostInc, ///< 後置インクリメント
        PostDec, ///< 後置デクリメント
    };

    /**
     * @brief 単項演算．
     */
    class UnaryOperation : public Term {
        pos::Range pos_op;
        UnaryOperator op;
        std::unique_ptr<Term> operand;
    public:
        UnaryOperation(pos::Range, UnaryOperator, std::unique_ptr<Term>);
        std::unique_ptr<ast::Expr> to_expr() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };

    /**
     * @brief 二項演算子．
     */
    enum class BinaryOperator {
        Add, ///< 加算
        Sub, ///< 減算
        Mul, ///< 乗算
        Div, ///< 除算（商）
        Rem, ///< 除算（余り）
        RightShift, ///< 右シフト
        LeftShift, ///< 左シフト
        ForwardShift, ///< 先送り
        BackwardShift, ///< 巻戻し
        Equal, ///< 等しい
        NotEqual, ///< 等しくない
        Greater, ///< 大なり
        GreaterEqual, ///< 大なりイコール
        Less, ///< 小なり
        LessEqual, ///< 小なりイコール
        LogicalAnd, ///< 論理積
        LogicalOr, ///< 論理和
        BitAnd, ///< ビット毎の論理積
        BitOr, ///< ビット毎の論理和
        BitXor, ///< ビット毎の排他的論理和
        Type, ///< 型の指定
        Assign, ///< 代入
        AddAssign, ///< 加算代入
        SubAssign, ///< 減算代入
        MulAssign, ///< 乗算代入
        DivAssign, ///< 除算（商）代入
        RemAssign, ///< 除算（余り）代入
        BitAndAssign, ///< ビット毎の論理積の代入
        BitOrAssign, ///< ビット毎の論理和の代入
        BitXorAssign, ///< ビット毎の排他的論理和の代入
        RightShiftAssign, ///< 右シフト代入
        LeftShiftAssign, ///< 左シフト代入
        ForwardShiftAssign, ///< 先送り代入
        BackwardShiftAssign, ///< 巻戻し代入
    };

    using DeclTriple = std::tuple<std::unique_ptr<Term>, std::unique_ptr<ast::Type>, std::unique_ptr<ast::Expr>>;

    /**
     * @brief 二項演算．
     */
    class BinaryOperation : public Term {
        pos::Range pos_op;
        BinaryOperator op;
        std::unique_ptr<Term> left, right;
    public:
        BinaryOperation(pos::Range, BinaryOperator, std::unique_ptr<Term>, std::unique_ptr<Term>);
        std::unique_ptr<ast::Expr> to_expr() override;
        std::optional<DeclTriple> to_decl_or_def();
        std::unique_ptr<ast::Stmt> to_stmt(pos::Range) override;
        std::unique_ptr<ast::Item> to_item(pos::Range) override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };

    /**
     * @brief 括弧でくくられた式．関数呼び出し．配列の要素へのアクセス．型引数．
     */
    class Bracket : public Term {
        BracketType bracket_type;
        std::unique_ptr<Term> left;
        std::vector<std::unique_ptr<Term>> right;
        bool trailing_comma;
    public:
        Bracket(BracketType, std::unique_ptr<Term>, std::vector<std::unique_ptr<Term>>, bool);
        std::unique_ptr<ast::Expr> to_expr() override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };

    /**
     * @brief 全ての文の基底クラス．
     */
    class Stmt {
    public:
        pos::Range pos;
        virtual ~Stmt();
        virtual std::unique_ptr<ast::Item> to_item();
        virtual std::unique_ptr<ast::Stmt> to_stmt(State) = 0;
#ifdef DEBUG
        virtual void debug_print(int) const = 0;
#endif
    };

    /**
     * @brief 単一の項にセミコロンの付いた文か，空文
     */
    class TermStmt : public Stmt {
        std::unique_ptr<Term> term;
    public:
        TermStmt(std::unique_ptr<Term>);
        std::unique_ptr<ast::Item> to_item() override;
        std::unique_ptr<ast::Stmt> to_stmt(State) override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };

    /**
     * @brief 文のブロック．
     */
    class Block : public Stmt {
        std::unique_ptr<Term> term;
        std::vector<std::unique_ptr<Stmt>> stmts;
    public:
        Block(std::unique_ptr<Term>, std::vector<std::unique_ptr<Stmt>>);
        std::unique_ptr<ast::Stmt> to_stmt(State) override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };

    /**
     * @brief if 文．
     */
    class If : public Stmt {
        std::unique_ptr<Term> cond;
        std::unique_ptr<Stmt> stmt_true, stmt_false;
    public:
        If(std::unique_ptr<Term>, std::unique_ptr<Stmt>, std::unique_ptr<Stmt>);
        std::unique_ptr<ast::Stmt> to_stmt(State) override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };

    /**
     * @brief while 文
     */
    class While : public Stmt {
        std::unique_ptr<Term> cond;
        std::unique_ptr<Stmt> stmt;
    public:
        While(std::unique_ptr<Term>, std::unique_ptr<Stmt>);
        std::unique_ptr<ast::Stmt> to_stmt(State) override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };

    /**
     * @brief break 文．
     */
    class Break : public Stmt {
    public:
        std::unique_ptr<ast::Stmt> to_stmt(State) override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };

    /**
     * @brief continue 文．
     */
    class Continue : public Stmt {
    public:
        std::unique_ptr<ast::Stmt> to_stmt(State) override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };

    /**
     * @brief return 文．
     */
    class Return : public Stmt {
        std::unique_ptr<Term> term;
    public:
        Return(std::unique_ptr<Term>);
        std::unique_ptr<ast::Stmt> to_stmt(State) override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
}

#endif
