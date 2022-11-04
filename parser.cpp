/**
 * Copyright (c) 2022 Atsushi Komaba
 *
 * This file is part of Syscraws.
 * Syscraws is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or any later version.
 * Syscraws is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with Syscraws. If not, see <https://www.gnu.org/licenses/>. 
 *
 * @file parser.cpp
 */
#include "parser.hpp"
#include "error.hpp"

static std::unique_ptr<pre_ast::Term> parse_term(lexer::Lexer &);

static std::unique_ptr<pre_ast::Term> parse_factor(lexer::Lexer &lexer){
    std::unique_ptr<pre_ast::Term> ret;
    {
        auto &token_ref = lexer.peek();
        pos::Range pos;
        if(!token_ref){
            ret = nullptr;
        }else if(auto factor = token_ref->factor()){
            ret = std::move(factor);
            pos = std::move(lexer.next()->pos);
        }else if(auto prefix = token_ref->prefix()){
            pos::Range pos_op = std::move(lexer.next()->pos);
            auto operand = parse_factor(lexer);
            if(!operand){
                if(auto token = lexer.next()) throw error::make<error::UnexpectedTokenAfterOperator>(std::move(pos), std::move(token->pos));
                else throw error::make<error::EOFAfterOperator>(std::move(pos));
            }
            pos = pos_op + operand->pos;
            ret = std::make_unique<pre_ast::UnaryOperation>(std::move(pos_op), prefix.value(), std::move(operand));
        }else{
            ret = nullptr;
        }
        if(ret) ret->pos = std::move(pos);
    }
    while(true){
        auto &token_ref = lexer.peek();
        pos::Range pos;
        if(!token_ref) return ret;
        if(auto suffix = token_ref->suffix()){
            pos::Range pos_op = std::move(lexer.next()->pos);
            pos = ret->pos + pos_op;
            ret = std::make_unique<pre_ast::UnaryOperation>(std::move(pos_op), suffix.value(), std::move(ret));
        }else if(auto bracket_type = token_ref->opening_bracket_type()){
            auto pos_open = std::move(lexer.next()->pos);
            bool trailing_comma = true;
            std::vector<std::unique_ptr<pre_ast::Term>> right;
            while(true){
                auto term = parse_term(lexer);
                // ここで term は nullptr の可能性がある
                auto &token = lexer.peek();
                if(token && token->is_comma()){
                    auto pos_comma = std::move(lexer.next()->pos);
                    if(!term) throw error::make<error::EmptyItemInList>(std::move(pos_comma));
                    right.push_back(std::move(term));
                }else{
                    if(term){
                        right.push_back(std::move(term));
                        trailing_comma = false;
                    }
                    break;
                }
            }
            auto close = lexer.next();
            if(!close) throw error::make<error::NoClosingBracket>(std::move(pos_open));
            auto closing_bracket_type = close->closing_bracket_type();
            if(!closing_bracket_type) throw error::make<error::UnexpectedTokenInBracket>(std::move(pos_open), std::move(close->pos));
            if(closing_bracket_type != bracket_type.value()) throw error::make<error::DifferentClosingBracket>(std::move(pos_open), std::move(close->pos));
            pos = (ret ? ret->pos : pos_open) + close->pos;
            ret = std::make_unique<pre_ast::Bracket>(bracket_type.value(), std::move(ret), std::move(right), trailing_comma);
        }else{
            return ret;
        }
        ret->pos = std::move(pos);
    }
}

enum Precedence {
    AssignPrecedence,
    TypePrecedence,
    LogicalOrPrecedence,
    LogicalAndPrecedence,
    ComparisonPrecedence,
    BitOrPrecedence,
    BitXorPrecedence,
    BitAndPrecedence,
    BitShiftPrecedence,
    AddSubPrecedence,
    MulDivRemPrecedence,
    TimeShiftPrecedence,
    MaxPrecedence,
};
static Precedence precedence(pre_ast::BinaryOperator op){
    switch(op){
        case pre_ast::BinaryOperator::ForwardShift:
        case pre_ast::BinaryOperator::BackwardShift:
            return TimeShiftPrecedence;
        case pre_ast::BinaryOperator::Mul:
        case pre_ast::BinaryOperator::Div:
        case pre_ast::BinaryOperator::Rem:
            return MulDivRemPrecedence;
        case pre_ast::BinaryOperator::Add:
        case pre_ast::BinaryOperator::Sub:
            return AddSubPrecedence;
        case pre_ast::BinaryOperator::RightShift:
        case pre_ast::BinaryOperator::LeftShift:
            return BitShiftPrecedence;
        case pre_ast::BinaryOperator::BitAnd:
            return BitAndPrecedence;
        case pre_ast::BinaryOperator::BitXor:
            return BitXorPrecedence;
        case pre_ast::BinaryOperator::BitOr:
            return BitOrPrecedence;
        case pre_ast::BinaryOperator::Equal:
        case pre_ast::BinaryOperator::NotEqual:
        case pre_ast::BinaryOperator::Less:
        case pre_ast::BinaryOperator::LessEqual:
        case pre_ast::BinaryOperator::Greater:
        case pre_ast::BinaryOperator::GreaterEqual:
            return ComparisonPrecedence;
        case pre_ast::BinaryOperator::LogicalAnd:
            return LogicalAndPrecedence;
        case pre_ast::BinaryOperator::LogicalOr:
            return LogicalOrPrecedence;
        case pre_ast::BinaryOperator::Type:
            return TypePrecedence;
        case pre_ast::BinaryOperator::Assign:
        case pre_ast::BinaryOperator::AddAssign:
        case pre_ast::BinaryOperator::SubAssign:
        case pre_ast::BinaryOperator::MulAssign:
        case pre_ast::BinaryOperator::DivAssign:
        case pre_ast::BinaryOperator::RemAssign:
        case pre_ast::BinaryOperator::BitAndAssign:
        case pre_ast::BinaryOperator::BitOrAssign:
        case pre_ast::BinaryOperator::BitXorAssign:
        case pre_ast::BinaryOperator::LeftShiftAssign:
        case pre_ast::BinaryOperator::RightShiftAssign:
        case pre_ast::BinaryOperator::ForwardShiftAssign:
        case pre_ast::BinaryOperator::BackwardShiftAssign:
            return AssignPrecedence;
    }
}
enum class Associativity{
    LeftToRight,
    RightToLeft,
};
static Associativity associativity(int precedence){
    if(precedence == AssignPrecedence) return Associativity::RightToLeft;
    else return Associativity::LeftToRight;
}
static std::unique_ptr<pre_ast::Term> parse_binary_operator(lexer::Lexer &lexer, int current_precedence){
    if(current_precedence == MaxPrecedence){
        return parse_factor(lexer);
    }
    auto left = parse_binary_operator(lexer, current_precedence + 1);
    if(!left) return nullptr;
    bool left_to_right = associativity(current_precedence) == Associativity::LeftToRight;
    while(true){
        auto &op_token = lexer.peek();
        if(!op_token) return left;
        auto op = op_token->infix();
        if(op && precedence(op.value()) == current_precedence){
            auto op_pos = std::move(lexer.next()->pos);
            auto right = parse_binary_operator(lexer, current_precedence + left_to_right);
            pos::Range pos;
            if(right) pos = left->pos + right->pos;
            else if(current_precedence == TypePrecedence) pos = left->pos + op_pos;
            else if(auto token = lexer.next()) throw error::make<error::UnexpectedTokenAfterOperator>(std::move(op_pos), std::move(token->pos));
            else throw error::make<error::EOFAfterOperator>(std::move(op_pos));
            left = std::make_unique<pre_ast::BinaryOperation>(std::move(op_pos), op.value(), std::move(left), std::move(right));
            left->pos = std::move(pos);
            if(left_to_right) continue;
        }
        return left;
    }
}

std::unique_ptr<pre_ast::Term> parse_term(lexer::Lexer &lexer){
    return parse_binary_operator(lexer, 0);
}

template <class EOFError, class UnexpectedTokenError>
std::unique_ptr<token::Token> consume(lexer::Lexer &lexer, bool (token::Token::*cond)() const, pos::Range &arg){
    auto token = lexer.next();
    if(!token) throw error::make<EOFError>(std::move(arg));
    if(!((*token).*cond)()) throw error::make<UnexpectedTokenError>(std::move(arg), std::move(token->pos));
    return token;
}

static std::unique_ptr<pre_ast::Stmt> parse_stmt(lexer::Lexer &lexer){
    auto &token_ref = lexer.peek();
    if(!token_ref){
        // 入力の終端に達した．
        return nullptr;
    }
    if(auto keyword = token_ref->keyword()){
        if(keyword.value() == token::Keyword::If){
            auto pos_if = std::move(lexer.next()->pos);
            auto cond_open = consume<error::UnexpectedEOFAfterKeyword, error::UnexpectedTokenAfterKeyword>(lexer, &token::Token::is_opening_parenthesis, pos_if);
            auto cond = parse_term(lexer);
            if(!cond){
                if(auto token = lexer.next()) throw error::make<error::UnexpectedTokenInBracket>(std::move(cond_open->pos), std::move(token->pos));
                else throw error::make<error::NoClosingBracket>(std::move(cond_open->pos));
            }
            auto cond_close = consume<error::NoClosingBracket, error::UnexpectedTokenInBracket>(lexer, &token::Token::is_closing_parenthesis, cond_open->pos);
            auto stmt_true = parse_stmt(lexer);
            if(!stmt_true){
                auto pos = pos_if + cond_close->pos;
                if(auto token = lexer.next()) throw error::make<error::UnexpectedTokenInControl>(std::move(pos), std::move(token->pos));
                else throw error::make<error::UnexpectedEOFInControl>(std::move(pos));
            }
            auto pos = pos_if + stmt_true->pos;
            std::unique_ptr<pre_ast::Stmt> stmt_false;
            auto &maybe_else = lexer.peek();
            if(maybe_else && maybe_else->keyword() == token::Keyword::Else){
                auto pos_else = std::move(lexer.next()->pos);
                stmt_false = parse_stmt(lexer);
                if(!stmt_false){
                    auto pos_control = pos_if + pos_else;
                    if(auto token = lexer.next()) throw error::make<error::UnexpectedTokenInControl>(std::move(pos_control), std::move(token->pos));
                    else throw error::make<error::UnexpectedEOFInControl>(std::move(pos_control));
                }
                pos += stmt_false->pos;
            }
            auto ret = std::make_unique<pre_ast::If>(std::move(cond), std::move(stmt_true), std::move(stmt_false));
            ret->pos = std::move(pos);
            return ret;
        }else if(keyword.value() == token::Keyword::While){
            auto pos_while = std::move(lexer.next()->pos);
            auto cond_open = consume<error::UnexpectedEOFAfterKeyword, error::UnexpectedTokenAfterKeyword>(lexer, &token::Token::is_opening_parenthesis, pos_while);
            auto cond = parse_term(lexer);
            if(!cond){
                if(auto token = lexer.next()) throw error::make<error::UnexpectedTokenInBracket>(std::move(cond_open->pos), std::move(token->pos));
                else throw error::make<error::NoClosingBracket>(std::move(cond_open->pos));
            }
            auto cond_close = consume<error::NoClosingBracket, error::UnexpectedTokenInBracket>(lexer, &token::Token::is_closing_parenthesis, cond_open->pos);
            auto stmt = parse_stmt(lexer);
            if(!stmt){
                auto pos = pos_while + cond_close->pos;
                if(auto token = lexer.next()) throw error::make<error::UnexpectedTokenInControl>(std::move(pos), std::move(token->pos));
                else throw error::make<error::UnexpectedEOFInControl>(std::move(pos));
            }
            auto pos = pos_while + stmt->pos;
            auto ret = std::make_unique<pre_ast::While>(std::move(cond), std::move(stmt));
            ret->pos = std::move(pos);
            return ret;
        }else if(keyword.value() == token::Keyword::Break){
            auto pos_break = std::move(lexer.next()->pos);
            auto semicolon = consume<error::UnexpectedEOFAfterKeyword, error::UnexpectedTokenAfterKeyword>(lexer, &token::Token::is_semicolon, pos_break);
            auto ret = std::make_unique<pre_ast::Break>();
            ret->pos = pos_break + semicolon->pos;
            return ret;
        }else if(keyword.value() == token::Keyword::Continue){
            auto pos_continue = std::move(lexer.next()->pos);
            auto semicolon = consume<error::UnexpectedEOFAfterKeyword, error::UnexpectedTokenAfterKeyword>(lexer, &token::Token::is_semicolon, pos_continue);
            auto ret = std::make_unique<pre_ast::Continue>();
            ret->pos = pos_continue + semicolon->pos;
            return ret;
        }else if(keyword.value() == token::Keyword::Return){
            auto pos = std::move(lexer.next()->pos);
            auto term = parse_term(lexer);
            if(term) pos += term->pos;
            auto semicolon = consume<error::EOFAtEndOfStmt, error::UnexpectedTokenAtEndOfStmt>(lexer, &token::Token::is_semicolon, pos);
            auto ret = std::make_unique<pre_ast::Return>(std::move(term));
            ret->pos = pos + semicolon->pos;
            return ret;
        }else{
            throw error::make<error::UnexpectedTokenAtBeginningOfStmt>(std::move(lexer.next()->pos));
        }
    }
    auto term = parse_term(lexer);
    auto &token = lexer.peek();
    if(!token){
        // token_ref は nullptr でないが，token は nullptr．
        // よってここで term は空でない
        throw error::make<error::EOFAtEndOfStmt>(std::move(term->pos));
    }else if(token->is_semicolon()){
        auto pos_semicolon = std::move(lexer.next()->pos);
        auto pos = term ? term->pos + pos_semicolon : std::move(pos_semicolon);
        auto ret = std::make_unique<pre_ast::TermStmt>(std::move(term));
        ret->pos = std::move(pos);
        return ret;
    }else if(token->is_opening_brace()){
        auto pos_open = std::move(lexer.next()->pos);
        auto pos = term ? std::move(term->pos) : pos_open.clone();
        std::vector<std::unique_ptr<pre_ast::Stmt>> stmts;
        while(auto stmt = parse_stmt(lexer)) stmts.push_back(std::move(stmt));
        // ここで stmts のどの要素も nullptr でない．
        pos += std::move(consume<error::NoClosingBracket, error::UnexpectedTokenInBracket>(lexer, &token::Token::is_closing_brace, pos_open)->pos);
        auto ret = std::make_unique<pre_ast::Block>(std::move(term), std::move(stmts));
        ret->pos = std::move(pos);
        return ret;
    }else if(!term){
        // parse_stmt() は stmt ::= term? { stmt* } をパースするときに parse_stmt() 自身を呼び出す．
        // このとき，} に達したら nullptr を返す必要がある．
        return nullptr;
    }else{
        throw error::make<error::UnexpectedTokenAtEndOfStmt>(std::move(term->pos), std::move(token->pos));
    }
}

std::unique_ptr<pre_ast::Stmt> parse(lexer::Lexer &lexer){
    if(auto ret = parse_stmt(lexer)){
        lexer.end_stmt();
        return ret;
    }else if(auto token = lexer.next()){
        throw error::make<error::UnexpectedTokenAtBeginningOfStmt>(std::move(token->pos));
    }else{
        return nullptr;
    }
}
