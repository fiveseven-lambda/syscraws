/**
 * Copyright (c) 2022 Atsushi Komaba
 *
 * This file is part of Syscraws.
 * Syscraws is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or any later version.
 * Syscraws is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with Syscraws. If not, see <https://www.gnu.org/licenses/>. 
 *
 * @file ir.hpp
 * @brief 実行可能な内部表現を定義する．
 */
#ifndef IR_HPP
#define IR_HPP

#include <cstdint>
#include <cstddef>
#include <variant>
#include <memory>
#include <vector>
#include <string>
#include <optional>
#include <unordered_map>

/**
 * @brief 実行可能な内部表現を定義する．
 */
namespace ir {
    using Int = std::int32_t;
    using Bool = bool;
    using Float = double;
    using Str = std::string;

    using StackId = std::size_t;

    struct GlobalAddr {
        std::size_t pos;
        GlobalAddr(std::size_t);
    };
    struct StackAddr {
        StackId stack_id;
        std::size_t pos;
        StackAddr(std::size_t, std::size_t);
    };

    using Addr = std::variant<GlobalAddr, StackAddr>;

    struct Func;

    using Value = std::variant<Int, Bool, Float, Str, Addr, std::shared_ptr<Func>>;
    void print(const Value &);
#ifdef DEBUG
    void debug_print(const Value &, int);
#endif

    struct Env {
        std::vector<std::optional<Value>> global;
        std::unordered_map<StackId, std::vector<std::optional<Value>>> stack;
        std::size_t num_stack;
        Env();
        std::optional<Value> &operator()(const GlobalAddr &);
        std::optional<Value> &operator()(const StackAddr &);
    };

    struct Func {
        virtual ~Func();
        virtual Value invoke(Env &, const std::vector<Value> &) const = 0;
    };

    struct Assign : public Func { Value invoke(Env &, const std::vector<Value> &) const override; };
    struct Deref : public Func { Value invoke(Env &, const std::vector<Value> &) const override; };
    struct IAdd : public Func { Value invoke(Env &, const std::vector<Value> &) const override; };
    struct IMul : public Func { Value invoke(Env &, const std::vector<Value> &) const override; };
    struct IEq : public Func { Value invoke(Env &, const std::vector<Value> &) const override; };
    struct IPrint : public Func { Value invoke(Env &, const std::vector<Value> &) const override; };
    struct FAdd : public Func { Value invoke(Env &, const std::vector<Value> &) const override; };

    struct Expr {
        virtual ~Expr();
        virtual Value eval(Env &, StackId) const = 0;
#ifdef DEBUG
        virtual void debug_print(int) const = 0;
#endif
    };
    struct Imm : public Expr {
        Value value;
        Imm(Value);
        Value eval(Env &, StackId) const override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    struct Local : public Expr {
        std::size_t index;
        Local(std::size_t);
        Value eval(Env &, StackId) const override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };
    struct Call : public Expr {
        std::unique_ptr<Expr> func_expr;
        std::vector<std::unique_ptr<Expr>> args_expr;
        Call(std::unique_ptr<Expr>, std::vector<std::unique_ptr<Expr>>);
        Value eval(Env &, StackId) const override;
#ifdef DEBUG
        void debug_print(int) const override;
#endif
    };

    struct ExprStmt;
    struct BrStmt;

    struct Stmt {
        virtual ~Stmt();
        virtual const std::shared_ptr<Stmt> run(Env &, Value &, StackId) const = 0;
#ifdef DEBUG
        virtual void debug_map(std::unordered_map<Stmt *, std::size_t> &) = 0;
#endif
    };
    struct ExprStmt : public Stmt {
        std::unique_ptr<Expr> expr;
        std::shared_ptr<Stmt> next;
        ExprStmt(std::unique_ptr<Expr>, std::shared_ptr<Stmt>);
        const std::shared_ptr<Stmt> run(Env &, Value &, StackId) const override;
#ifdef DEBUG
        void debug_map(std::unordered_map<Stmt *, std::size_t> &) override;
#endif
    };
    struct BrStmt : public Stmt {
        std::unique_ptr<Expr> cond;
        std::shared_ptr<Stmt> next_true, next_false;
        BrStmt(std::unique_ptr<Expr>, std::shared_ptr<Stmt>, std::shared_ptr<Stmt>);
        const std::shared_ptr<Stmt> run(Env &, Value &, StackId) const override;
#ifdef DEBUG
        void debug_map(std::unordered_map<Stmt *, std::size_t> &) override;
#endif
    };

#ifdef DEBUG
    void debug_print(Stmt &, int);
#endif

    struct FuncDef : public Func {
        std::size_t num_locals;
        std::shared_ptr<Stmt> entry;
        Value invoke(Env &, const std::vector<Value> &) const override;
    };

}


#endif
