/**
 * Copyright (c) 2022 Atsushi Komaba
 *
 * This file is part of Syscraws.
 * Syscraws is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or any later version.
 * Syscraws is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with Syscraws. If not, see <https://www.gnu.org/licenses/>. 
 *
 * @file pre_ast.cpp
 */
#include "pre_ast.hpp"
#include "error.hpp"

#include <boost/safe_numerics/safe_integer.hpp>

namespace pre_ast {
    Term::~Term() = default;
    Identifier::Identifier(std::string_view name): name(name) {}
    DecInt::DecInt(std::string_view value): value(value) {}
    BinInt::BinInt(std::string_view value): value(value) {}
    OctInt::OctInt(std::string_view value): value(value) {}
    HexInt::HexInt(std::string_view value): value(value) {}
    Float::Float(std::string_view value): value(value) {}
    String::String(std::string value): value(std::move(value)) {}
    /**
     * @brief コンストラクタ
     * @param pos_op 演算子の位置．
     * @param op 演算子．
     * @param operand オペランド．nullptr でないこと．
     */
    UnaryOperation::UnaryOperation(pos::Range pos_op, UnaryOperator op, std::unique_ptr<Term> operand):
        pos_op(std::move(pos_op)),
        op(op),
        operand(std::move(operand)) {}
    /**
     * @brief コンストラクタ
     * @param pos_op 演算子の位置．
     * @param op 演算子．
     * @param left 左オペランド．nullptr でないこと．
     * @param right 右オペランド．op が BinaryOperator::Type でない限り，nullptr でないこと．
     */
    BinaryOperation::BinaryOperation(pos::Range pos_op, BinaryOperator op, std::unique_ptr<Term> left, std::unique_ptr<Term> right):
        pos_op(std::move(pos_op)),
        op(op),
        left(std::move(left)),
        right(std::move(right)) {}
    /**
     * @brief コンストラクタ
     * @param bracket_type 括弧の種類．
     * @param left 括弧の前の式．nullptr であってもよい．
     * @param right 括弧の中身．どの要素も nullptr でないこと．
     * @param trailing_comma 括弧中の最後の要素の後に trailing_comma が付いていたか．
     */
    Bracket::Bracket(BracketType bracket_type, std::unique_ptr<Term> left, std::vector<std::unique_ptr<Term>> right, bool trailing_comma):
        bracket_type(bracket_type),
        left(std::move(left)),
        right(std::move(right)),
        trailing_comma(trailing_comma) {}

    Stmt::~Stmt() = default;
    /**
     * @brief コンストラクタ
     * @param term 項．nullptr であってもよい．
     */
    TermStmt::TermStmt(std::unique_ptr<Term> term):
        term(std::move(term)) {}
    /**
     * @brief コンストラクタ
     * @param term 項．nullptr であってもよい．
     * @param stmts 中身．どの要素も nullptr でないこと．
     */
    Block::Block(std::unique_ptr<Term> term, std::vector<std::unique_ptr<Stmt>> stmts):
        term(std::move(term)),
        stmts(std::move(stmts)) {}
    If::If(std::unique_ptr<Term> cond, std::unique_ptr<Stmt> stmt_true, std::unique_ptr<Stmt> stmt_false):
        cond(std::move(cond)),
        stmt_true(std::move(stmt_true)),
        stmt_false(std::move(stmt_false)) {}
    While::While(std::unique_ptr<Term> cond, std::unique_ptr<Stmt> stmt):
        cond(std::move(cond)),
        stmt(std::move(stmt)) {}
    Return::Return(std::unique_ptr<Term> term):
        term(std::move(term)) {}

    std::unique_ptr<ast::Type> Term::to_type(){
        throw error::make<error::TermNotType>(std::move(pos));
    }
    std::unique_ptr<ast::Pat> Term::to_pat(){
        return nullptr;
    }
    std::unique_ptr<ast::Item> Term::to_item(pos::Range pos_stmt){
        return to_stmt(std::move(pos_stmt));
    }
    std::unique_ptr<ast::Stmt> Term::to_stmt(pos::Range pos_stmt){
        return std::make_unique<ast::ExprStmt>(std::move(pos_stmt), to_expr());
    }
    std::unique_ptr<ast::Expr> Identifier::to_expr(){
        return std::make_unique<ast::Identifier>(std::move(pos), name);
    }
    std::unique_ptr<ast::Type> Identifier::to_type(){
        return std::make_unique<ast::TypeName>(std::move(pos), name);
    }
    std::unique_ptr<ast::Pat> Identifier::to_pat(){
        return std::make_unique<ast::IdPat>(std::move(pos), name);
    }
    template <std::int32_t Base, bool Sign>
    std::int32_t read_integer(std::string_view str, pos::Range &pos){
        using safe_i32 = boost::safe_numerics::safe<std::int32_t>;
        safe_i32 ret(0);
        try{
            for(char ch : str){
                std::int32_t digit;
                if('0' <= ch && ch <= '9') digit = ch - '0';
                else if('a' <= ch && ch <= 'f') digit = ch - 'a' + 10;
                else if('A' <= ch && ch <= 'F') digit = ch - 'A' + 10;
                else continue;
                if(Sign) ret = ret * safe_i32(Base) + safe_i32(digit);
                else ret = ret * safe_i32(Base) - safe_i32(digit);
            }
        }catch(std::exception &error){
            throw error::make<error::OverflowInIntegerLiteral>(error, std::move(pos));
        }
        return ret;
    }
    std::unique_ptr<ast::Expr> DecInt::to_expr(){
        std::int32_t n = read_integer<10, true>(value, pos);
        return std::make_unique<ast::Int>(std::move(pos), n);
    }
    std::string_view DecInt::get_value(){
        return value;
    }
    std::unique_ptr<ast::Expr> BinInt::to_expr(){
        std::int32_t n = read_integer<2, true>(value, pos);
        return std::make_unique<ast::Int>(std::move(pos), n);
    }
    std::string_view BinInt::get_value(){
        return value;
    }
    std::unique_ptr<ast::Expr> OctInt::to_expr(){
        std::int32_t n = read_integer<8, true>(value, pos);
        return std::make_unique<ast::Int>(std::move(pos), n);
    }
    std::string_view OctInt::get_value(){
        return value;
    }
    std::unique_ptr<ast::Expr> HexInt::to_expr(){
        std::int32_t n = read_integer<16, true>(value, pos);
        return std::make_unique<ast::Int>(std::move(pos), n);
    }
    std::string_view HexInt::get_value(){
        return value;
    }
    std::unique_ptr<ast::Expr> Float::to_expr(){
        std::string tmp(value.begin(), value.end());
        return std::make_unique<ast::Float>(std::move(pos), std::stod(tmp));
    }
    std::unique_ptr<ast::Expr> String::to_expr(){
        return std::make_unique<ast::String>(std::move(pos), std::move(value));
    }
    std::unique_ptr<ast::Expr> UnaryOperation::to_expr(){
        if(op == UnaryOperator::Minus){
            if(auto dec = dynamic_cast<DecInt *>(operand.get())){
                std::int32_t n = read_integer<10, false>(dec->get_value(), pos);
                return std::make_unique<ast::Int>(std::move(pos), n);
            }else if(auto bin = dynamic_cast<BinInt *>(operand.get())){
                std::int32_t n = read_integer<2, false>(bin->get_value(), pos);
                return std::make_unique<ast::Int>(std::move(pos), n);
            }else if(auto oct = dynamic_cast<OctInt *>(operand.get())){
                std::int32_t n = read_integer<8, false>(oct->get_value(), pos);
                return std::make_unique<ast::Int>(std::move(pos), n);
            }else if(auto hex = dynamic_cast<HexInt *>(operand.get())){
                std::int32_t n = read_integer<16, false>(hex->get_value(), pos);
                return std::make_unique<ast::Int>(std::move(pos), n);
            }
        }
        ast::Operator ast_op;
        switch(op){
            case UnaryOperator::Plus: ast_op = ast::Operator::Plus; break;
            case UnaryOperator::Minus: ast_op = ast::Operator::Minus; break;
            case UnaryOperator::Recip: ast_op = ast::Operator::Recip; break;
            case UnaryOperator::LogicalNot: ast_op = ast::Operator::LogicalNot; break;
            case UnaryOperator::BitNot: ast_op = ast::Operator::BitNot; break;
            case UnaryOperator::PreInc: ast_op = ast::Operator::PreInc; break;
            case UnaryOperator::PreDec: ast_op = ast::Operator::PreDec; break;
            case UnaryOperator::PostInc: ast_op = ast::Operator::PostInc; break;
            case UnaryOperator::PostDec: ast_op = ast::Operator::PostDec; break;
        }
        auto op_expr = std::make_unique<ast::OperatorExpr>(std::move(pos_op), ast_op);
        std::vector<std::unique_ptr<ast::Expr>> args;
        args.push_back(operand->to_expr());
        return std::make_unique<ast::Call>(std::move(pos), std::move(op_expr), std::move(args));
    }
    std::unique_ptr<ast::Expr> BinaryOperation::to_expr(){
        ast::Operator ast_op;
        switch(op){
            case BinaryOperator::Add: ast_op = ast::Operator::Add; break;
            case BinaryOperator::Sub: ast_op = ast::Operator::Sub; break;
            case BinaryOperator::Mul: ast_op = ast::Operator::Mul; break;
            case BinaryOperator::Div: ast_op = ast::Operator::Div; break;
            case BinaryOperator::Rem: ast_op = ast::Operator::Rem; break;
            case BinaryOperator::RightShift: ast_op = ast::Operator::RightShift; break;
            case BinaryOperator::LeftShift: ast_op = ast::Operator::LeftShift; break;
            case BinaryOperator::ForwardShift: ast_op = ast::Operator::ForwardShift; break;
            case BinaryOperator::BackwardShift: ast_op = ast::Operator::BackwardShift; break;
            case BinaryOperator::Equal: ast_op = ast::Operator::Equal; break;
            case BinaryOperator::NotEqual: ast_op = ast::Operator::NotEqual; break;
            case BinaryOperator::Greater: ast_op = ast::Operator::Greater; break;
            case BinaryOperator::GreaterEqual: ast_op = ast::Operator::GreaterEqual; break;
            case BinaryOperator::Less: ast_op = ast::Operator::Less; break;
            case BinaryOperator::LessEqual: ast_op = ast::Operator::LessEqual; break;
            case BinaryOperator::LogicalAnd: ast_op = ast::Operator::LogicalAnd; break;
            case BinaryOperator::LogicalOr: ast_op = ast::Operator::LogicalOr; break;
            case BinaryOperator::BitAnd: ast_op = ast::Operator::BitAnd; break;
            case BinaryOperator::BitOr: ast_op = ast::Operator::BitOr; break;
            case BinaryOperator::BitXor: ast_op = ast::Operator::BitXor; break;
            case BinaryOperator::Assign: ast_op = ast::Operator::Assign; break;
            case BinaryOperator::AddAssign: ast_op = ast::Operator::AddAssign; break;
            case BinaryOperator::SubAssign: ast_op = ast::Operator::SubAssign; break;
            case BinaryOperator::MulAssign: ast_op = ast::Operator::MulAssign; break;
            case BinaryOperator::DivAssign: ast_op = ast::Operator::DivAssign; break;
            case BinaryOperator::RemAssign: ast_op = ast::Operator::RemAssign; break;
            case BinaryOperator::BitAndAssign: ast_op = ast::Operator::BitAndAssign; break;
            case BinaryOperator::BitOrAssign: ast_op = ast::Operator::BitOrAssign; break;
            case BinaryOperator::BitXorAssign: ast_op = ast::Operator::BitXorAssign; break;
            case BinaryOperator::RightShiftAssign: ast_op = ast::Operator::RightShiftAssign; break;
            case BinaryOperator::LeftShiftAssign: ast_op = ast::Operator::LeftShiftAssign; break;
            case BinaryOperator::ForwardShiftAssign: ast_op = ast::Operator::ForwardShiftAssign; break;
            case BinaryOperator::BackwardShiftAssign: ast_op = ast::Operator::BackwardShiftAssign; break;
            case BinaryOperator::Type: {
                TODO;
            }
        }
        auto op_expr = std::make_unique<ast::OperatorExpr>(std::move(pos_op), ast_op);
        std::vector<std::unique_ptr<ast::Expr>> args;
        args.push_back(left->to_expr());
        args.push_back(right->to_expr());
        return std::make_unique<ast::Call>(std::move(pos), std::move(op_expr), std::move(args));
    }
    std::optional<DeclTriple> BinaryOperation::to_decl_or_def(){
        if(op == BinaryOperator::Type){
            if(!right) throw error::make<error::DeclWithoutTypeOrRHS>(std::move(pos));
            return std::make_optional<DeclTriple>(std::move(left), right->to_type(), nullptr);
        }else if(op == BinaryOperator::Assign){
            auto lhs = dynamic_cast<BinaryOperation *>(left.get());
            if(lhs && lhs->op == BinaryOperator::Type){
                auto type = lhs->right ? lhs->right->to_type() : nullptr;
                return std::make_optional<DeclTriple>(std::move(lhs->left), std::move(type), right->to_expr());
            }
        }
        return std::nullopt;
    }
    std::unique_ptr<ast::Item> BinaryOperation::to_item(pos::Range pos_stmt){
        if(auto decl_triple = to_decl_or_def()){
            auto [lhs, type, rhs] = std::move(decl_triple.value());
            auto pat = lhs->to_pat();
            if(!pat) throw error::make<error::InvalidLHSOfDecl>(std::move(lhs->pos));
            return std::make_unique<ast::DeclGlobal>(std::move(pos_stmt), std::move(pat), std::move(type), std::move(rhs));
        }
        return std::make_unique<ast::ExprStmt>(std::move(pos_stmt), to_expr());
    }
    std::unique_ptr<ast::Stmt> BinaryOperation::to_stmt(pos::Range pos_stmt){
        if(auto decl_triple = to_decl_or_def()){
            auto [lhs, type, rhs] = std::move(decl_triple.value());
            auto pat = lhs->to_pat();
            if(!pat) throw error::make<error::InvalidLHSOfDecl>(std::move(lhs->pos));
            return std::make_unique<ast::DeclLocal>(std::move(pos_stmt), std::move(pat), std::move(type), std::move(rhs));
        }
        return std::make_unique<ast::ExprStmt>(std::move(pos_stmt), to_expr());
    }
    std::unique_ptr<ast::Expr> Bracket::to_expr(){
        if(bracket_type == BracketType::Round){
            if(left){
                // 関数呼び出し．
                auto func = left->to_expr();
                std::vector<std::unique_ptr<ast::Expr>> args;
                for(auto &arg : right){
                    args.push_back(arg->to_expr());
                }
                return std::make_unique<ast::Call>(std::move(pos), std::move(func), std::move(args));
            }else{
                if(right.size() == 1 && !trailing_comma){
                    // 括弧．
                    return right.front()->to_expr();
                }else{
                    // タプルの生成．
                    TODO;
                }
            }
        }else{
            if(left){
                // 配列へのインデックスアクセス．
                TODO;
            }else{
                // 配列の生成
                TODO;
            }
        }
    }

    std::unique_ptr<ast::Item> Stmt::to_item(){
        return to_stmt(false);
    }
    std::unique_ptr<ast::Item> TermStmt::to_item(){
        return term->to_item(std::move(pos));
    }
    std::unique_ptr<ast::Stmt> TermStmt::to_stmt(bool){
        return term->to_stmt(std::move(pos));
    }
    std::unique_ptr<ast::Stmt> Block::to_stmt(bool loop){
        if(term){
            // 関数定義
        }else{
            // ブロック
            std::vector<std::unique_ptr<ast::Stmt>> stmts_ast;
            stmts_ast.reserve(stmts.size());
            for(auto &stmt : stmts){
                stmts_ast.push_back(stmt->to_stmt(loop));
            }
            return std::make_unique<ast::Block>(std::move(pos), std::move(stmts_ast));
        }
    }
    std::unique_ptr<ast::Stmt> If::to_stmt(bool loop){
        auto ast_cond = cond->to_expr();
        auto ast_stmt_true = stmt_true->to_stmt(loop);
        auto ast_stmt_false = stmt_true ? stmt_true->to_stmt(loop) : nullptr;
        return std::make_unique<ast::If>(std::move(pos), std::move(ast_cond), std::move(ast_stmt_true), std::move(ast_stmt_false));
    }
    std::unique_ptr<ast::Stmt> While::to_stmt(bool){
        auto ast_cond = cond->to_expr();
        auto ast_stmt = stmt->to_stmt(true);
        return std::make_unique<ast::While>(std::move(pos), std::move(ast_cond), std::move(ast_stmt));
    }
    std::unique_ptr<ast::Stmt> Break::to_stmt(bool){
        return std::make_unique<ast::Break>(std::move(pos));
    }
    std::unique_ptr<ast::Stmt> Continue::to_stmt(bool){
        return std::make_unique<ast::Continue>(std::move(pos));
    }
    std::unique_ptr<ast::Stmt> Return::to_stmt(bool){}
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

namespace pre_ast {
    void Identifier::debug_print(int depth) const {
        std::cout << indent(depth) << pos << " identifier(" << name << ")" << std::endl;
    }
    void DecInt::debug_print(int depth) const {
        std::cout << indent(depth) << pos << " decimal integer(" << value << ")" << std::endl;
    }
    void BinInt::debug_print(int depth) const {
        std::cout << indent(depth) << pos << " binary integer(" << value << ")" << std::endl;
    }
    void OctInt::debug_print(int depth) const {
        std::cout << indent(depth) << pos << " octal integer(" << value << ")" << std::endl;
    }
    void HexInt::debug_print(int depth) const {
        std::cout << indent(depth) << pos << " hexadecimal integer(" << value << ")" << std::endl;
    }
    void Float::debug_print(int depth) const {
        std::cout << indent(depth) << pos << " float(" << value << ")" << std::endl;
    }
    void String::debug_print(int depth) const {
        std::cout << indent(depth) << pos << " string(" << value << ")" << std::endl;
    }
    void UnaryOperation::debug_print(int depth) const {
        std::string_view name;
        switch(op){
            case UnaryOperator::Plus: name = "plus"; break;
            case UnaryOperator::Minus: name = "minus"; break;
            case UnaryOperator::Recip: name = "reciprocal"; break;
            case UnaryOperator::LogicalNot: name = "logical not"; break;
            case UnaryOperator::BitNot: name = "bitwise not"; break;
            case UnaryOperator::PreInc: name = "prefix increment"; break;
            case UnaryOperator::PreDec: name = "prefix decrement"; break;
            case UnaryOperator::PostInc: name = "postfix increment"; break;
            case UnaryOperator::PostDec: name = "postfix decrement";
        }
        std::cout << indent(depth) << pos << " unary operation (" << pos_op << ": " << name << ")" << std::endl;
        operand->debug_print(depth + 1);
    }
    void BinaryOperation::debug_print(int depth) const {
        std::string_view name;
        switch(op){
            case BinaryOperator::Add: name = "add"; break;
            case BinaryOperator::Sub: name = "sub"; break;
            case BinaryOperator::Mul: name = "mul"; break;
            case BinaryOperator::Div: name = "div"; break;
            case BinaryOperator::Rem: name = "rem"; break;
            case BinaryOperator::LeftShift: name = "left shift"; break;
            case BinaryOperator::RightShift: name = "right shift"; break;
            case BinaryOperator::ForwardShift: name = "forward shift"; break;
            case BinaryOperator::BackwardShift: name = "backward shift"; break;
            case BinaryOperator::Equal: name = "equal to"; break;
            case BinaryOperator::NotEqual: name = "not equal to"; break;
            case BinaryOperator::Less: name = "less than"; break;
            case BinaryOperator::LessEqual: name = "less than or equal to"; break;
            case BinaryOperator::Greater: name = "greater than"; break;
            case BinaryOperator::GreaterEqual: name = "greater than or equal to"; break;
            case BinaryOperator::LogicalAnd: name = "logical and"; break;
            case BinaryOperator::LogicalOr: name = "logical or"; break;
            case BinaryOperator::BitAnd: name = "bitwise and"; break;
            case BinaryOperator::BitOr: name = "bitwise or"; break;
            case BinaryOperator::BitXor: name = "bitwise xor"; break;
            case BinaryOperator::Type: name = "type"; break;
            case BinaryOperator::Assign: name = "assign"; break;
            case BinaryOperator::AddAssign: name = "add assign"; break;
            case BinaryOperator::SubAssign: name = "sub assign"; break;
            case BinaryOperator::MulAssign: name = "mul assign"; break;
            case BinaryOperator::DivAssign: name = "div assign"; break;
            case BinaryOperator::RemAssign: name = "rem assign"; break;
            case BinaryOperator::BitAndAssign: name = "bitwise and assign"; break;
            case BinaryOperator::BitOrAssign: name = "bitwise or assign"; break;
            case BinaryOperator::BitXorAssign: name = "bitwise xor assign"; break;
            case BinaryOperator::LeftShiftAssign: name = "left shift assign"; break;
            case BinaryOperator::RightShiftAssign: name = "right shift assign"; break;
            case BinaryOperator::ForwardShiftAssign: name = "forward shift assign"; break;
            case BinaryOperator::BackwardShiftAssign: name = "backward shift assign"; break;
        }
        std::cout << indent(depth) << pos << " binary operation (" << pos_op << ": " << name << ")" << std::endl;
        left->debug_print(depth + 1);
        if(right) right->debug_print(depth + 1);
    }
    void Bracket::debug_print(int depth) const {
        std::string_view name;
        switch(bracket_type){
            case BracketType::Round: name = "round"; break;
            case BracketType::Square: name = "square"; break;
        }
        std::cout << indent(depth) << pos << " bracket (" << name << ")" << std::endl;
        if(left) left->debug_print(depth + 1);
        std::cout << indent(depth) << "right(" << right.size() << "), trailing_comma: " << std::boolalpha << trailing_comma << std::endl;
        for(auto &elem : right) elem->debug_print(depth + 1);
    }
    void TermStmt::debug_print(int depth) const {
        std::cout << indent(depth) << pos << " term statement" << std::endl;
        if(term) term->debug_print(depth + 1);
    }
    void While::debug_print(int depth) const {
        std::cout << indent(depth) << pos << " while" << std::endl;
        cond->debug_print(depth + 1);
        std::cout << indent(depth) << "do" << std::endl;
        stmt->debug_print(depth + 1);
        std::cout << indent(depth) << "end while" << std::endl;
    }
    void If::debug_print(int depth) const {
        std::cout << indent(depth) << pos << " if" << std::endl;
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
        std::cout << indent(depth) << pos << " block" << std::endl;
        if(term) term->debug_print(depth + 1);
        std::cout << indent(depth) << "stmts(" << stmts.size() << ")" << std::endl;
        for(auto &stmt : stmts){
            stmt->debug_print(depth + 1);
        }
        std::cout << indent(depth) << "end block" << std::endl;
    }
    void Break::debug_print(int depth) const {
        std::cout << indent(depth) << pos << " break" << std::endl;
    }
    void Continue::debug_print(int depth) const {
        std::cout << indent(depth) << pos << " continue" << std::endl;
    }
    void Return::debug_print(int depth) const {
        std::cout << indent(depth) << pos << " return" << std::endl;
        if(term) term->debug_print(depth + 1);
    }
}
#endif
