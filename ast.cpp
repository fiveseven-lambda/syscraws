/**
 * Copyright (c) 2022 Atsushi Komaba
 *
 * This file is part of Syscraws.
 * Syscraws is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or any later version.
 * Syscraws is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with Syscraws. If not, see <https://www.gnu.org/licenses/>. 
 *
 * @file ast.cpp
 */
#include "ast.hpp"
#include "error.hpp"

namespace ast {
    Context::Context():
        ops(NumOps),
        deref(new ir::Deref),
        assign(new ir::Assign)
    {
        auto &int_ty = ty.get_int();
        auto &bool_ty = ty.get_bool();
        auto &float_ty = ty.get_float();
        ops[Add].emplace_back(ty.get_func({int_ty, int_ty}, int_ty), std::make_shared<ir::IAdd>());
        ops[Add].emplace_back(ty.get_func({float_ty, float_ty}, float_ty), std::make_shared<ir::FAdd>());
        ops[Equal].emplace_back(ty.get_func({int_ty, int_ty}, bool_ty), std::make_shared<ir::IEq>());
        ops[Mul].emplace_back(ty.get_func({int_ty, int_ty}, int_ty), std::make_shared<ir::IMul>());
        funcs["print"].emplace_back(ty.get_func({int_ty}, int_ty), std::make_shared<ir::IPrint>());
    }
    Expr::Expr(pos::Range pos):
        pos(std::move(pos)) {}
    Expr::~Expr() = default;
    std::pair<const type::Func &, std::unique_ptr<ir::Expr>> Expr::translate_func(Context &ctx, const std::vector<std::reference_wrapper<const type::Type>> &){
        auto [ty, expr] = translate(ctx, false);
        if(auto func_ty = dynamic_cast<const type::Func *>(&ty)){
            return {*func_ty, std::move(expr)};
        }else{
            TODO;
        }
    }
    const pos::Range &Expr::get_pos() const {
        return pos;
    }
    Identifier::Identifier(pos::Range pos, std::string_view name):
        Expr(std::move(pos)),
        name(name) {}
    std::pair<const type::Type &, std::unique_ptr<ir::Expr>> Identifier::translate(Context &ctx, bool lvalue){
        if(lvalue){
            auto it_local = ctx.locals.find(name);
            if(it_local != ctx.locals.end()){
                return {it_local->second.first.get(), std::make_unique<ir::Local>(it_local->second.second)};
            }
            auto it_global = ctx.globals.find(name);
            if(it_global != ctx.globals.end()){
                return {it_global->second.first.get(), std::make_unique<ir::Imm>(ir::GlobalAddr(it_global->second.second))};
            }
            auto it_func = ctx.funcs.find(name);
            if(it_func != ctx.funcs.end()){
                TODO;
            }
            throw error::make<error::UndefinedVariable>(get_pos().clone());
        }else{
            auto [ty, addr] = translate(ctx, true);
            std::vector<std::unique_ptr<ir::Expr>> args;
            args.push_back(std::move(addr));
            return {ty, std::make_unique<ir::Call>(std::make_unique<ir::Imm>(ctx.deref), std::move(args))};
        }
    }
    static std::pair<const type::Func &, std::unique_ptr<ir::Expr>> resolve_overload(const std::vector<Context::Func> &candidates, const std::vector<std::reference_wrapper<const type::Type>> &expected_args){
        std::size_t expected_num_args = expected_args.size();
        for(auto &[ty, func] : candidates){
            auto &args = ty.get().get_args();
            if(args.size() != expected_num_args) continue;
            bool is_matched = true;
            for(std::size_t i = 0; i < expected_num_args; i++){
                if(&args[i].get() != &expected_args[i].get()){
                    is_matched = false;
                }
            }
            if(is_matched){
                return {ty, std::make_unique<ir::Imm>(func)};
            }
        }
    }
    std::pair<const type::Func &, std::unique_ptr<ir::Expr>> Identifier::translate_func(Context &ctx, const std::vector<std::reference_wrapper<const type::Type>> &expected_args){
        auto it_func = ctx.funcs.find(name);
        if(it_func == ctx.funcs.end()) TODO;
        return resolve_overload(it_func->second, expected_args);
    }
    Int::Int(pos::Range pos, std::int32_t value):
        Expr(std::move(pos)),
        value(value) {}
    std::pair<const type::Type &, std::unique_ptr<ir::Expr>> Int::translate(Context &ctx, bool lvalue){
        if(lvalue) TODO;
        return {ctx.ty.get_int(), std::make_unique<ir::Imm>(value)};
    }
    Float::Float(pos::Range pos, double value):
        Expr(std::move(pos)),
        value(value) {}
    std::pair<const type::Type &, std::unique_ptr<ir::Expr>> Float::translate(Context &ctx, bool lvalue){
        if(lvalue) TODO;
        return {ctx.ty.get_float(), std::make_unique<ir::Imm>(value)};
    }
    String::String(pos::Range pos, std::string value):
        Expr(std::move(pos)),
        value(value) {}
    std::pair<const type::Type &, std::unique_ptr<ir::Expr>> String::translate(Context &ctx, bool lvalue){
        if(lvalue) TODO;
        return {ctx.ty.get_str(), std::make_unique<ir::Imm>(value)};
    }
    Call::Call(pos::Range pos, std::unique_ptr<Expr> func, std::vector<std::unique_ptr<Expr>> args):
        Expr(std::move(pos)),
        func(std::move(func)),
        args(std::move(args)) {}
    std::pair<const type::Type &, std::unique_ptr<ir::Expr>> Call::translate(Context &ctx, bool lvalue){
        std::size_t num_args = args.size();
        std::vector<std::reference_wrapper<const type::Type>> args_type;
        std::vector<std::unique_ptr<ir::Expr>> args_expr;
        args_type.reserve(num_args);
        args_expr.reserve(num_args);
        for(auto &arg : args){
            auto [type, expr] = arg->translate(ctx, false);
            args_type.push_back(type);
            args_expr.push_back(std::move(expr));
        }
        auto [func_type, func_expr] = func->translate_func(ctx, args_type);
        return {func_type.get_ret(), std::make_unique<ir::Call>(std::move(func_expr), std::move(args_expr))};
    }
    OperatorExpr::OperatorExpr(pos::Range pos, Operator op):
        Expr(std::move(pos)),
        op(op) {}
    std::pair<const type::Type &, std::unique_ptr<ir::Expr>> OperatorExpr::translate(Context &ctx, bool){
    }
    std::pair<const type::Func &, std::unique_ptr<ir::Expr>> OperatorExpr::translate_func(Context &ctx, const std::vector<std::reference_wrapper<const type::Type>> &expected_args){
        return resolve_overload(ctx.ops[op], expected_args);
    }

    Type::Type(pos::Range pos):
        pos(std::move(pos)) {}
    Type::~Type() = default;
    const pos::Range &Type::get_pos() const {
        return pos;
    }
    TypeName::TypeName(pos::Range pos, std::string_view name):
        Type(std::move(pos)),
        name(name) {}
    const type::Type &TypeName::get(Context &ctx){
        if(name == "int"){
            return ctx.ty.get_int();
        }else if(name == "bool"){
            return ctx.ty.get_bool();
        }else if(name == "float"){
            return ctx.ty.get_float();
        }else if(name == "str"){
            return ctx.ty.get_str();
        }
    }

    Pat::Pat(pos::Range pos):
        pos(std::move(pos)) {}
    Pat::~Pat() = default;
    const pos::Range &Pat::get_pos() const {
        return pos;
    }
    IdPat::IdPat(pos::Range pos, std::string_view name):
        Pat(std::move(pos)),
        name(name) {}

    Item::Item(pos::Range pos):
        pos(std::move(pos)) {}
    Item::~Item() = default;
    const pos::Range &Item::get_pos() const {
        return pos;
    }

    Stmt::Stmt(pos::Range pos):
        Item(std::move(pos)) {}
    Stmt::~Stmt() = default;
    void Stmt::run(Context &ctx, ir::Env &env){
        // これローカル変数 ir::FuncDef tmp; だと何故かダメっぽい？
        auto tmp = std::make_unique<ir::FuncDef>();
        tmp->entry = translate(ctx, Dests(), tmp->num_locals);
        // ir::debug_print(*tmp->entry, 0);
        tmp->invoke(env, {});
    }

    /**
     * @brief コンストラクタ．
     * @param pos 位置．
     * @param expr 式．nullptr でもよい．
     */
    ExprStmt::ExprStmt(pos::Range pos, std::unique_ptr<Expr> expr):
        Stmt(std::move(pos)),
        expr(std::move(expr)) {}
    std::shared_ptr<ir::Stmt> ExprStmt::translate(Context &ctx, Dests dests, std::size_t &){
        return std::make_shared<ir::ExprStmt>(expr->translate(ctx).second, dests.next);
    }
    Block::Block(pos::Range pos, std::vector<std::unique_ptr<Stmt>> stmts):
        Stmt(std::move(pos)),
        stmts(std::move(stmts)) {}
    std::shared_ptr<ir::Stmt> Block::translate(Context &ctx, Dests dests, std::size_t &num_locals){
        return translate_rec(ctx, dests, num_locals, 0);
    }
    std::shared_ptr<ir::Stmt> Block::translate_rec(Context &ctx, Dests dests, std::size_t &num_locals, std::size_t stmt){
        if(stmt == stmts.size()){
            return dests.next;
        }
        auto decl = dynamic_cast<DeclLocal *>(stmts[stmt].get());
        std::string_view name;
        std::optional<Context::Local> prev;
        if(decl){
            auto [pat, type] = decl->get_prototype(ctx, num_locals);
            name = dynamic_cast<ast::IdPat &>(pat).name;
            auto it = ctx.locals.find(name);
            if(it == ctx.locals.end()){
                ctx.locals.emplace(name, std::make_pair(std::cref(type), num_locals));
            }else{
                prev = it->second;
                it->second = std::make_pair(std::cref(type), num_locals);
            }
            num_locals++;
        }
        auto next = translate_rec(ctx, dests, num_locals, stmt + 1);
        if(decl){
            auto it = ctx.locals.find(name);
            if(prev){
                it->second = prev.value();
            }else{
                ctx.locals.erase(it);
            }
        }
        return stmts[stmt]->translate(ctx, dests, num_locals);
    }
    If::If(pos::Range pos, std::unique_ptr<Expr> cond, std::unique_ptr<Stmt> stmt_true, std::unique_ptr<Stmt> stmt_false):
        Stmt(std::move(pos)),
        cond(std::move(cond)),
        stmt_true(std::move(stmt_true)),
        stmt_false(std::move(stmt_false)) {}
    std::shared_ptr<ir::Stmt> If::translate(Context &ctx, Dests dests, std::size_t &num_locals){
        auto cond_ir = cond->translate(ctx).second;
        auto next_true = stmt_true->translate(ctx, dests, num_locals);
        auto next_false = stmt_false ? stmt_false->translate(ctx, dests, num_locals) : dests.next;
        return std::make_shared<ir::BrStmt>(std::move(cond_ir), next_true, next_false);
    }
    While::While(pos::Range pos, std::unique_ptr<Expr> cond, std::unique_ptr<Stmt> stmt):
        Stmt(std::move(pos)),
        cond(std::move(cond)),
        stmt(std::move(stmt)) {}
    std::shared_ptr<ir::Stmt> While::translate(Context &ctx, Dests dests, std::size_t &num_locals){
        auto cond_ir = cond->translate(ctx).second;
        auto ret = std::make_shared<ir::BrStmt>(std::move(cond_ir), nullptr, dests.next);
        dests.brk = dests.next;
        dests.ctn = ret;
        ret->next_true = stmt->translate(ctx, dests, num_locals);
        return ret;
    }
    Break::Break(pos::Range pos):
        Stmt(std::move(pos)) {}
    std::shared_ptr<ir::Stmt> Break::translate(Context &, Dests dests, std::size_t &){
        return dests.brk;
    }
    Continue::Continue(pos::Range pos):
        Stmt(std::move(pos)) {}
    std::shared_ptr<ir::Stmt> Continue::translate(Context &, Dests dests, std::size_t &){
        return dests.ctn;
    }
    Return::Return(pos::Range pos, std::unique_ptr<Expr> expr):
        Stmt(std::move(pos)),
        expr(std::move(expr)) {}
    /**
     * @brief コンストラクタ．
     * @param pos 位置．
     * @param lhs 左辺．
     * @param type 型．rhs が nullptr でなければ， type は nullptr でもよい．
     * @param rhs 右辺．type が nullptr でなければ， rhs は nullptr でもよい．
     */
    DeclLocal::DeclLocal(pos::Range pos, std::unique_ptr<Pat> lhs, std::unique_ptr<Type> type, std::unique_ptr<Expr> rhs):
        Stmt(std::move(pos)),
        lhs(std::move(lhs)),
        type(std::move(type)),
        rhs(std::move(rhs)) {}
    std::pair<Pat &, const type::Type &> DeclLocal::get_prototype(Context &ctx, std::size_t idx){
        index = idx;
        auto res = rhs->translate(ctx);
        translated_rhs = std::move(res.second);
        return {*lhs, type ? type->get(ctx) : res.first};
    }
    std::shared_ptr<ir::Stmt> DeclLocal::translate(Context &ctx, Dests dests, std::size_t &num_locals){
        // 行うのは代入のみ．
        std::vector<std::unique_ptr<ir::Expr>> args(2);
        args[0] = std::make_unique<ir::Local>(index);
        args[1] = std::move(translated_rhs);
        return std::make_shared<ir::ExprStmt>(
            std::make_unique<ir::Call>(
                std::make_unique<ir::Imm>(ctx.assign),
                std::move(args)
            ),
            dests.next
        );
    }
    /**
     * @brief コンストラクタ．
     * @param pos 位置．
     * @param left 左辺．
     * @param type 型．right が nullptr でなければ， type は nullptr でもよい．
     * @param right 右辺．type が nullptr でなければ， right は nullptr でもよい．
     */
    DeclGlobal::DeclGlobal(pos::Range pos, std::unique_ptr<Pat> left, std::unique_ptr<Type> type, std::unique_ptr<Expr> right):
        Item(std::move(pos)),
        left(std::move(left)),
        type(std::move(type)),
        right(std::move(right)) {}
    void DeclGlobal::run(Context &ctx, ir::Env &env){
        // グローバル変数の宣言
        // シャドーイングを許可する場合，ctx.globals と env.global のサイズは一致しない
        auto name = dynamic_cast<IdPat &>(*left).name;
        auto pos = env.global.size();
        env.global.emplace_back();
        const type::Type *ty = nullptr;
        if(right){
            auto tmp = std::make_unique<ir::FuncDef>();
            std::vector<std::unique_ptr<ir::Expr>> args(2);
            args[0] = std::make_unique<ir::Imm>(ir::GlobalAddr(pos));
            auto right_expr = right->translate(ctx);
            ty = &right_expr.first;
            args[1] = std::move(right_expr.second);
            tmp->entry = std::make_shared<ir::ExprStmt>(
                std::make_unique<ir::Call>(
                    std::make_unique<ir::Imm>(ctx.assign),
                    std::move(args)
                ),
                nullptr
            );
            tmp->invoke(env, {});
        }
        if(!ty) ty = &type->get(ctx);
        ctx.globals.insert_or_assign(name, std::make_pair(std::cref(*ty), pos));
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

namespace ast {
    void Identifier::debug_print(int depth) const {
        std::cout << indent(depth) << get_pos() << " identifier(" << name << ")" << std::endl;
    }
    void Int::debug_print(int depth) const {
        std::cout << indent(depth) << get_pos() << " integer(" << value << ")" << std::endl;
    }
    void Float::debug_print(int depth) const {
        std::cout << indent(depth) << get_pos() << " float(" << value << ")" << std::endl;
    }
    void String::debug_print(int depth) const {
        std::cout << indent(depth) << get_pos() << " string(" << value << ")" << std::endl;
    }
    void Call::debug_print(int depth) const {
        std::cout << indent(depth) << get_pos() << " call" << std::endl;
        func->debug_print(depth + 1);
        std::cout << indent(depth) << "args(" << args.size() << "):" << std::endl;
        for(auto &arg : args) arg->debug_print(depth + 1);
    }
    void OperatorExpr::debug_print(int depth) const {
        std::string_view name;
        switch(op){
            case Operator::Plus: name = "plus"; break;
            case Operator::Minus: name = "minus"; break;
            case Operator::Recip: name = "reciprocal"; break;
            case Operator::LogicalNot: name = "logical not"; break;
            case Operator::BitNot: name = "bitwise not"; break;
            case Operator::PreInc: name = "prefix increment"; break;
            case Operator::PreDec: name = "prefix decrement"; break;
            case Operator::PostInc: name = "postfix increment"; break;
            case Operator::PostDec: name = "postfix decrement"; break;
            case Operator::Add: name = "add"; break;
            case Operator::Sub: name = "sub"; break;
            case Operator::Mul: name = "mul"; break;
            case Operator::Div: name = "div"; break;
            case Operator::Rem: name = "rem"; break;
            case Operator::LeftShift: name = "left shift"; break;
            case Operator::RightShift: name = "right shift"; break;
            case Operator::ForwardShift: name = "forward shift"; break;
            case Operator::BackwardShift: name = "backward shift"; break;
            case Operator::Equal: name = "equal to"; break;
            case Operator::NotEqual: name = "not equal to"; break;
            case Operator::Less: name = "less than"; break;
            case Operator::LessEqual: name = "less than or equal to"; break;
            case Operator::Greater: name = "greater than"; break;
            case Operator::GreaterEqual: name = "greater than or equal to"; break;
            case Operator::LogicalAnd: name = "logical and"; break;
            case Operator::LogicalOr: name = "logical or"; break;
            case Operator::BitAnd: name = "bitwise and"; break;
            case Operator::BitOr: name = "bitwise or"; break;
            case Operator::BitXor: name = "bitwise xor"; break;
            case Operator::Assign: name = "assign"; break;
            case Operator::AddAssign: name = "add assign"; break;
            case Operator::SubAssign: name = "sub assign"; break;
            case Operator::MulAssign: name = "mul assign"; break;
            case Operator::DivAssign: name = "div assign"; break;
            case Operator::RemAssign: name = "rem assign"; break;
            case Operator::BitAndAssign: name = "bitwise and assign"; break;
            case Operator::BitOrAssign: name = "bitwise or assign"; break;
            case Operator::BitXorAssign: name = "bitwise xor assign"; break;
            case Operator::LeftShiftAssign: name = "left shift assign"; break;
            case Operator::RightShiftAssign: name = "right shift assign"; break;
            case Operator::ForwardShiftAssign: name = "forward shift assign"; break;
            case Operator::BackwardShiftAssign: name = "backward shift assign"; break;
        }
        std::cout << indent(depth) << get_pos() << " operator(" << name << ")" << std::endl;
    }
    void TypeName::debug_print(int depth) const {
        std::cout << indent(depth) << get_pos() << " type name(" << name << ")" << std::endl;
    }
    void IdPat::debug_print(int depth) const {
        std::cout << indent(depth) << get_pos() << " identifier pattern(" << name << ")" << std::endl;
    }
    void ExprStmt::debug_print(int depth) const {
        if(expr){
            std::cout << indent(depth) << get_pos() << " expression statement" << std::endl;
            expr->debug_print(depth + 1);
        }else{
            std::cout << indent(depth) << get_pos() << " expression statement (empty)" << std::endl;
        }
    }
    void While::debug_print(int depth) const {
        std::cout << indent(depth) << get_pos() << " while" << std::endl;
        cond->debug_print(depth + 1);
        std::cout << indent(depth) << "do" << std::endl;
        stmt->debug_print(depth + 1);
        std::cout << indent(depth) << "end while" << std::endl;
    }
    void If::debug_print(int depth) const {
        std::cout << indent(depth) << get_pos() << " if" << std::endl;
        cond->debug_print(depth + 1);
        std::cout << indent(depth) << "then" << std::endl;
        stmt_true->debug_print(depth + 1);
        if(stmt_false){
            std::cout << indent(depth) << "else" << std::endl;
            stmt_false->debug_print(depth + 1);
        }
        std::cout << indent(depth) << "end if" << std::endl;
    }
    void Block::debug_print(int depth) const {
        std::cout << indent(depth) << get_pos() << " block" << std::endl;
        for(auto &stmt : stmts){
            stmt->debug_print(depth + 1);
        }
        std::cout << indent(depth) << "end block" << std::endl;
    }
    void Break::debug_print(int depth) const {
        std::cout << indent(depth) << get_pos() << " break" << std::endl;
    }
    void Continue::debug_print(int depth) const {
        std::cout << indent(depth) << get_pos() << " continue" << std::endl;
    }
    void Return::debug_print(int depth) const {
        std::cout << indent(depth) << get_pos() << " return" << std::endl;
        if(expr) expr->debug_print(depth + 1);
    }
    void DeclLocal::debug_print(int depth) const {
        std::cout << indent(depth) << get_pos() << " declaration (local)" << std::endl;
        lhs->debug_print(depth + 1);
        if(type) type->debug_print(depth + 1);
        if(rhs) rhs->debug_print(depth + 1);
    }
    void DeclGlobal::debug_print(int depth) const {
        std::cout << indent(depth) << get_pos() << " declaration (global)" << std::endl;
        left->debug_print(depth + 1);
        if(type) type->debug_print(depth + 1);
        if(right) right->debug_print(depth + 1);
    }
}
#endif
