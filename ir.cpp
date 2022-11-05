/**
 * Copyright (c) 2022 Atsushi Komaba
 *
 * This file is part of Syscraws.
 * Syscraws is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or any later version.
 * Syscraws is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with Syscraws. If not, see <https://www.gnu.org/licenses/>. 
 *
 * @file ir.cpp
 */
#include "ir.hpp"
#include <iostream>

namespace ir {
    GlobalAddr::GlobalAddr(std::size_t pos): pos(pos) {}
    StackAddr::StackAddr(StackId stack_id, std::size_t pos): stack_id(stack_id), pos(pos) {}
    Env::Env(): num_stack(0) {}
    std::optional<Value> &Env::operator()(const GlobalAddr &addr){
        return global.at(addr.pos);
    }
    std::optional<Value> &Env::operator()(const StackAddr &addr){
        auto it = stack.find(addr.stack_id);
        if(it == stack.end()) throw;
        return it->second.at(addr.pos);
    }
    void print(const Value &value){
        if(const Int *v_int = std::get_if<Int>(&value)){
            std::cout << *v_int << std::endl;
        }else if(const Bool *v_bool = std::get_if<Bool>(&value)){
            std::cout << *v_bool << std::endl;
        }else if(const Float *v_float = std::get_if<Float>(&value)){
            std::cout << *v_float << std::endl;
        }else if(const Str *v_str = std::get_if<Str>(&value)){
            std::cout << *v_str << std::endl;
        }else if(const Addr *v_addr = std::get_if<Addr>(&value)){
            if(const GlobalAddr *v_global = std::get_if<GlobalAddr>(v_addr)){
                std::cout << "global address (" << v_global->pos << ")" << std::endl;
            }else if(const StackAddr *v_stack = std::get_if<StackAddr>(v_addr)){
                std::cout << "stack address (" << v_stack->stack_id << ", " << v_stack->pos << ")" << std::endl;
            }
        }else{
            std::cout << "function" << std::endl;
        }
    }
    Func::~Func() = default;
    Value Assign::invoke(Env &env, const std::vector<Value> &args) const {
        std::visit(env, std::get<Addr>(args[0])) = args[1];
        return args[1];
    }
    Value Deref::invoke(Env & env, const std::vector<Value> &args) const {
        return std::visit(env, std::get<Addr>(args[0])).value();
    }
    Value IAdd::invoke(Env &, const std::vector<Value> &args) const {
        return std::get<Int>(args[0]) + std::get<Int>(args[1]);
    }
    Value IMul::invoke(Env &, const std::vector<Value> &args) const {
        return std::get<Int>(args[0]) * std::get<Int>(args[1]);
    }
    Value IEq::invoke(Env &, const std::vector<Value> &args) const {
        return std::get<Int>(args[0]) == std::get<Int>(args[1]);
    }
    Value IPrint::invoke(Env &, const std::vector<Value> &args) const {
        std::cout << std::get<Int>(args[0]) << std::endl;
        return std::get<Int>(args[0]);
    }
    Value FAdd::invoke(Env &, const std::vector<Value> &args) const {
        return std::get<Float>(args[0]) + std::get<Float>(args[1]);
    }
    Expr::~Expr() = default;
    Imm::Imm(Value value): value(value) {}
    Local::Local(std::size_t index): index(index) {}
    Value Imm::eval(Env &, StackId) const {
        return value;
    }
    Value Local::eval(Env &, StackId stack_id) const {
        return StackAddr(stack_id, index);
    }
    Call::Call(std::unique_ptr<Expr> func_expr, std::vector<std::unique_ptr<Expr>> args_expr):
        func_expr(std::move(func_expr)),
        args_expr(std::move(args_expr)) {}
    Value Call::eval(Env &env, StackId stack_id) const {
        auto func = std::get<std::shared_ptr<Func>>(func_expr->eval(env, stack_id));
        std::size_t num_args = args_expr.size();
        std::vector<Value> args(num_args);
        for(std::size_t i = 0; i < num_args; i++){
            args[i] = args_expr[i]->eval(env, stack_id);
        }
        return func->invoke(env, args);
    }
    Stmt::~Stmt() = default;
    ExprStmt::ExprStmt(std::unique_ptr<Expr> expr, std::shared_ptr<Stmt> next):
        expr(std::move(expr)),
        next(std::move(next)) {}
    const std::shared_ptr<Stmt> ExprStmt::run(Env &env, Value &dest, StackId stack_id) const {
        dest = expr->eval(env, stack_id);
        return next;
    }
    BrStmt::BrStmt(std::unique_ptr<Expr> cond, std::shared_ptr<Stmt> next_true, std::shared_ptr<Stmt> next_false):
        cond(std::move(cond)),
        next_true(std::move(next_true)),
        next_false(std::move(next_false)) {}
    const std::shared_ptr<Stmt> BrStmt::run(Env &env, Value &, StackId stack_id) const {
        if(std::get<Bool>(cond->eval(env, stack_id))){
            return next_true;
        }else{
            return next_false;
        }
    }
    Value FuncDef::invoke(Env &env, const std::vector<Value> &args) const {
        StackId stack_id = env.num_stack;
        std::vector<std::optional<Value>> locals(num_locals);
        for(std::size_t i = 0; i < args.size(); i++) locals[i] = args[i];
        auto stack_iter = env.stack.emplace(stack_id, std::move(locals)).first;
        env.num_stack++;
        Value ret;
        std::shared_ptr<Stmt> stmt = entry;
        while(stmt) stmt = stmt->run(env, ret, stack_id);
        env.stack.erase(stack_iter);
        return ret;
    }
}

#ifdef DEBUG
#include <iostream>
class indent {
    int depth;
public:
    indent(int depth): depth(depth) {}
    friend std::ostream &operator<<(std::ostream &os, const indent &ind){
        for(int i = 0; i < ind.depth; i++) os << "  ";
        return os;
    }
};

namespace ir {
    void debug_print(const Value &value, int depth){
        if(const Int *v_int = std::get_if<Int>(&value)){
            std::cout << indent(depth) << "int(" << *v_int << ")" << std::endl;
        }else if(const Bool *v_bool = std::get_if<Bool>(&value)){
            std::cout << indent(depth) << "bool(" << *v_bool << ")" << std::endl;
        }else if(const Float *v_float = std::get_if<Float>(&value)){
            std::cout << indent(depth) << "float(" << *v_float << ")" << std::endl;
        }else if(const Str *v_str = std::get_if<Str>(&value)){
            std::cout << indent(depth) << "str(" << *v_str << ")" << std::endl;
        }else if(const Addr *v_addr = std::get_if<Addr>(&value)){
            if(const GlobalAddr *v_global = std::get_if<GlobalAddr>(v_addr)){
                std::cout << indent(depth) << "global address (" << v_global->pos << ")" << std::endl;
            }else if(const StackAddr *v_stack = std::get_if<StackAddr>(v_addr)){
                std::cout << indent(depth) << "stack address (" << v_stack->stack_id << ", " << v_stack->pos << ")" << std::endl;
            }
        }else if(auto v_func = std::get_if<std::shared_ptr<Func>>(&value)){
            if(auto func_def = dynamic_cast<const FuncDef *>(v_func->get())){
                std::cout << indent(depth) << "function" << std::endl;
                debug_print(*func_def->entry, depth + 1);
            }else{
                std::cout << indent(depth) << "function (builtin)" << std::endl;
            }
        }
    }
    void Imm::debug_print(int depth) const {
        std::cout << indent(depth) << "imm" << std::endl;
        ::ir::debug_print(value, depth + 1);
    }
    void Local::debug_print(int depth) const {
        std::cout << indent(depth) << "local(" << index << ")" << std::endl;
    }
    void Call::debug_print(int depth) const {
        std::cout << indent(depth) << "call" << std::endl;
        func_expr->debug_print(depth + 1);
        for(auto &arg : args_expr){
            arg->debug_print(depth + 1);
        }
    }

    void ExprStmt::debug_map(std::unordered_map<Stmt *, std::size_t> &map){
        auto it = map.find(this);
        if(it == map.end()){
            std::size_t n = map.size();
            map.emplace(this, n);
            if(next) next->debug_map(map);
        }
    }
    void BrStmt::debug_map(std::unordered_map<Stmt *, std::size_t> &map){
        auto it = map.find(this);
        if(it == map.end()){
            std::size_t n = map.size();
            map.emplace(this, n);
            if(next_true) next_true->debug_map(map);
            if(next_false) next_false->debug_map(map);
        }
    }
    void debug_print(Stmt &stmt, int depth){
        std::unordered_map<Stmt *, std::size_t> map;
        map.emplace(nullptr, 0);
        stmt.debug_map(map);
        for(auto &[s, i] : map){
            if(!s) continue;
            std::cout << indent(depth) << "#" << i;
            if(auto expr_stmt = dynamic_cast<ExprStmt *>(s)){
                std::cout << " -> #" << map[expr_stmt->next.get()] << std::endl;
                expr_stmt->expr->debug_print(depth + 1);
            }else if(auto br_stmt = dynamic_cast<BrStmt *>(s)){
                std::cout << " -> #" << map[br_stmt->next_true.get()] << ", #" << map[br_stmt->next_false.get()] << std::endl;
                br_stmt->cond->debug_print(depth + 1);
            }
        }
    }
}

#endif
