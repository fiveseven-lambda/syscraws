/**
 * Copyright (c) 2022 Atsushi Komaba
 *
 * This file is part of Syscraws.
 * Syscraws is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or any later version.
 * Syscraws is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with Syscraws. If not, see <https://www.gnu.org/licenses/>. 
 *
 * @file ast.hpp
 * @brief 抽象構文木 AST を定義する．
 */
#ifndef AST_HPP
#define AST_HPP

#include <string_view>

#include "pos.hpp"
#include "ir.hpp"
#include "type.hpp"

/**
 * @brief 抽象構文木 AST を定義する．
 */
namespace ast {
    /**
     * @brief 演算子
     */
    enum Operator {
        Plus,
        Minus,
        Recip,
        LogicalNot,
        BitNot,
        PreInc,
        PreDec,
        PostInc,
        PostDec,
        Add,
        Sub,
        Mul,
        Div,
        Rem,
        LeftShift,
        RightShift,
        ForwardShift,
        BackwardShift,
        Equal,
        NotEqual,
        Less,
        LessEqual,
        Greater,
        GreaterEqual,
        LogicalAnd,
        LogicalOr,
        BitAnd,
        BitOr,
        BitXor,
        Assign,
        AddAssign,
        SubAssign,
        MulAssign,
        DivAssign,
        RemAssign,
        BitAndAssign,
        BitOrAssign,
        BitXorAssign,
        LeftShiftAssign,
        RightShiftAssign,
        ForwardShiftAssign,
        BackwardShiftAssign,
        NumOps,
    };

    /**
     * @brief 文脈
     */
    struct Context {
        type::Context ty;
        using Global = std::pair<std::reference_wrapper<const type::Type>, std::size_t>;
        std::unordered_map<std::string_view, Global> globals;
        using Local = std::pair<std::reference_wrapper<const type::Type>, std::size_t>;
        std::unordered_map<std::string_view, Local> locals;
        using Func = std::pair<std::reference_wrapper<const type::Func>, std::shared_ptr<ir::Func>>;
        std::unordered_map<std::string_view, std::vector<Func>> funcs;
        std::vector<std::vector<Func>> ops;
        std::shared_ptr<ir::Deref> deref;
        std::shared_ptr<ir::Assign> assign;
        Context();
    };

    /**
     * @brief 全ての式の基底クラス．
     */
    class Expr {
        pos::Range pos;
    public:
        Expr(pos::Range);
        virtual ~Expr();
        const pos::Range &get_pos() const;
        virtual std::pair<const type::Type &, std::unique_ptr<ir::Expr>> translate(Context &, bool = false) = 0;
        virtual std::pair<const type::Func &, std::unique_ptr<ir::Expr>> translate_func(Context &, const std::vector<std::reference_wrapper<const type::Type>> &);
#ifdef DEBUG
        virtual void debug_print(int) const = 0;
#endif
    };

    /**
     * @brief 単一の識別子からなる式．
     */
    class Identifier : public Expr {
        std::string_view name;
    public:
        Identifier(pos::Range, std::string_view);
        std::pair<const type::Type &, std::unique_ptr<ir::Expr>> translate(Context &, bool) override;
        std::pair<const type::Func &, std::unique_ptr<ir::Expr>> translate_func(Context &, const std::vector<std::reference_wrapper<const type::Type>> &) override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };

    /**
     * @brief 単一の整数リテラルからなる式．
     */
    class Int : public Expr {
        std::int32_t value;
    public:
        Int(pos::Range, std::int32_t);
        std::pair<const type::Type &, std::unique_ptr<ir::Expr>> translate(Context &, bool) override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };

    /**
     * @brief 単一の浮動小数点数リテラルからなる式．
     */
    class Float : public Expr {
        double value;
    public:
        Float(pos::Range, double);
        std::pair<const type::Type &, std::unique_ptr<ir::Expr>> translate(Context &, bool) override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };

    /**
     * @brief 単一の文字列リテラルからなる式．
     */
    class String : public Expr {
        std::string value;
    public:
        String(pos::Range, std::string);
        std::pair<const type::Type &, std::unique_ptr<ir::Expr>> translate(Context &, bool) override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };

    /**
     * @brief 関数呼び出し，二項演算，単項演算
     */
    class Call : public Expr {
        std::unique_ptr<Expr> func;
        std::vector<std::unique_ptr<Expr>> args;
    public:
        Call(pos::Range, std::unique_ptr<Expr>, std::vector<std::unique_ptr<Expr>>);
        std::pair<const type::Type &, std::unique_ptr<ir::Expr>> translate(Context &, bool) override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };

    /**
     * @brief 演算子の式
     */
    class OperatorExpr : public Expr {
        Operator op;
    public:
        OperatorExpr(pos::Range, Operator);
        std::pair<const type::Type &, std::unique_ptr<ir::Expr>> translate(Context &, bool) override;
        std::pair<const type::Func &, std::unique_ptr<ir::Expr>> translate_func(Context &, const std::vector<std::reference_wrapper<const type::Type>> &) override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };

    /**
     * @brief 全ての型の基底クラス．
     */
    class Type {
        pos::Range pos;
    public:
        Type(pos::Range);
        virtual ~Type();
        const pos::Range &get_pos() const;
        virtual const type::Type &get(Context &) = 0;
#ifdef DEBUG
        virtual void debug_print(int) const = 0;
#endif
    };

    /**
     * @brief 型の名前．
     */
    class TypeName : public Type {
        std::string_view name;
    public:
        TypeName(pos::Range, std::string_view);
        const type::Type &get(Context &) override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };

    /**
     * @brief 全てのパターンの基底クラス．
     */
    class Pat {
        pos::Range pos;
    public:
        Pat(pos::Range);
        virtual ~Pat();
        const pos::Range &get_pos() const;
#ifdef DEBUG
        virtual void debug_print(int) const = 0;
#endif
    };

    /**
     * @brief 単一の変数名からなるパターン．
     */
    class IdPat : public Pat {
    public:
        std::string_view name;
        IdPat(pos::Range, std::string_view);
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };

    /** 
     * @brief 全てのトップレベルアイテムの基底クラス
     */
    class Item {
        pos::Range pos;
    public:
        Item(pos::Range);
        const pos::Range &get_pos() const;
        virtual ~Item();
        virtual void run(Context &, ir::Env &) = 0;
#ifdef DEBUG
        virtual void debug_print(int) const = 0;
#endif
    };

    /**
     * @brief 全ての文の基底クラス
     */
    class Stmt : public Item {
    public:
        Stmt(pos::Range);
        virtual ~Stmt() override;
        void run(Context &, ir::Env &) override;
        virtual std::shared_ptr<ir::Stmt> translate(Context &, std::shared_ptr<ir::Stmt>, std::size_t &) = 0;
#ifdef DEBUG
        virtual void debug_print(int) const override = 0;
#endif
    };

    /**
     * @brief 単一の式からなる文
     */
    class ExprStmt : public Stmt {
        std::unique_ptr<Expr> expr;
    public:
        ExprStmt(pos::Range, std::unique_ptr<Expr>);
        std::shared_ptr<ir::Stmt> translate(Context &, std::shared_ptr<ir::Stmt>, std::size_t &) override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };

    /**
     * @brief ブロック
     */
    class Block : public Stmt {
        std::vector<std::unique_ptr<Stmt>> stmts;
    public:
        Block(pos::Range, std::vector<std::unique_ptr<Stmt>>);
        std::shared_ptr<ir::Stmt> translate(Context &, std::shared_ptr<ir::Stmt>, std::size_t &) override;
        std::shared_ptr<ir::Stmt> translate_rec(Context &, std::shared_ptr<ir::Stmt>, std::size_t &, std::size_t);
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };

    /**
     * @brief If 文
     */
    class If : public Stmt {
        std::unique_ptr<Expr> cond;
        std::unique_ptr<Stmt> stmt_true;
        std::unique_ptr<Stmt> stmt_false;
    public:
        If(pos::Range, std::unique_ptr<Expr>, std::unique_ptr<Stmt>, std::unique_ptr<Stmt>);
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };

    /**
     * @brief While 文
     */
    class While : public Stmt {
        std::unique_ptr<Expr> cond;
        std::unique_ptr<Stmt> stmt;
    public:
        While(pos::Range, std::unique_ptr<Expr>, std::unique_ptr<Stmt>);
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };

    /**
     * @brief break 文
     */
    class Break : public Stmt {
    public:
        Break(pos::Range);
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };

    /**
     * @brief continue 文
     */
    class Continue : public Stmt {
        Continue(pos::Range);
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };

    /**
     * @brief return 文
     */
    class Return : public Stmt {
        std::unique_ptr<Expr> expr;
    public:
        Return(pos::Range, std::unique_ptr<Expr>);
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };

    /**
     * @brief ローカル変数の定義
     */
    class DeclLocal : public Stmt {
        std::unique_ptr<Pat> lhs;
        std::unique_ptr<Type> type;
        std::unique_ptr<Expr> rhs;
        std::unique_ptr<ir::Expr> translated_rhs;
        std::size_t index;
    public:
        DeclLocal(pos::Range, std::unique_ptr<Pat>, std::unique_ptr<Type>, std::unique_ptr<Expr>);
        std::pair<Pat &, const type::Type &> get_prototype(Context &, std::size_t);
        std::shared_ptr<ir::Stmt> translate(Context &, std::shared_ptr<ir::Stmt>, std::size_t &) override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    /**
     * @brief グローバル変数の定義
     *
     * 今のところ right は std::unique_ptr<Expr> だけど，
     * `a := b := c;` みたいなのも可能にしたい
     * （その場合 `b := c` のもつ位置情報にセミコロンを含めないことに注意）．
     */
    class DeclGlobal : public Item {
        std::unique_ptr<Pat> left;
        std::unique_ptr<Type> type;
        std::unique_ptr<Expr> right;
    public:
        DeclGlobal(pos::Range, std::unique_ptr<Pat>, std::unique_ptr<Type>, std::unique_ptr<Expr>);
        void run(Context &, ir::Env &) override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };

    /**
     * @brief 式による関数定義
     */
    class DefExpr : public Item {
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };

    /**
     * @brief 文による関数定義
     */
    class DefStmt : public Item {
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
}

#endif
