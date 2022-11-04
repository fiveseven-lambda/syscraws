/**
 * Copyright (c) 2022 Atsushi Komaba
 *
 * This file is part of Syscraws.
 * Syscraws is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or any later version.
 * Syscraws is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with Syscraws. If not, see <https://www.gnu.org/licenses/>. 
 *
 * @file token.cpp
 */
#include "token.hpp"
#include "error.hpp"

namespace token {
    Token::~Token() = default;
    Identifier::Identifier(std::string_view name): name(name) {}
    Number::Number(std::string_view value): value(value) {}
    String::String(std::string value) : value(std::move(value)) {}

    std::unique_ptr<pre_ast::Term> Token::factor(){ return nullptr; }
    std::unique_ptr<pre_ast::Term> Identifier::factor(){ return std::make_unique<pre_ast::Identifier>(name); }
    std::unique_ptr<pre_ast::Term> Number::factor(){
        if(value[0] == '0' && value.length() >= 2){
            if(value[1] == 'b'){
                bool has_digit = false;
                for(std::size_t i = 2; i < value.length(); i++){
                    if(value[i] == '_') continue;
                    else if(value[i] == '0' || value[i] == '1');
                    else throw error::make<error::InvalidNumericLiteral>(std::move(pos));
                    has_digit = true;
                }
                if(has_digit) return std::make_unique<pre_ast::BinInt>(value.substr(2));
                else throw error::make<error::InvalidNumericLiteral>(std::move(pos));
            }else if(value[1] == 'o'){
                bool has_digit = false;
                for(std::size_t i = 2; i < value.length(); i++){
                    if(value[i] == '_') continue;
                    if('0' <= value[i] && value[i] <= '8');
                    else throw error::make<error::InvalidNumericLiteral>(std::move(pos));
                    has_digit = true;
                }
                if(has_digit) return std::make_unique<pre_ast::OctInt>(value.substr(2));
                else throw error::make<error::InvalidNumericLiteral>(std::move(pos));
            }else if(value[1] == 'x'){
                bool has_digit = false;
                for(std::size_t i = 2; i < value.length(); i++){
                    if(value[i] == '_') continue;
                    else if('0' <= value[i] && value[i] <= '9');
                    else if('a' <= value[i] && value[i] <= 'f');
                    else if('A' <= value[i] && value[i] <= 'F');
                    else throw error::make<error::InvalidNumericLiteral>(std::move(pos));
                    has_digit = true;
                }
                if(has_digit) return std::make_unique<pre_ast::HexInt>(value.substr(2));
                else throw error::make<error::InvalidNumericLiteral>(std::move(pos));
            }
        }
        for(std::size_t i = 0; i < value.length(); i++){
            if(('0' <= value[i] && value[i] <= '9') || value[i] == '_'){
            }else{
                return std::make_unique<pre_ast::Float>(value);
            }
        }
        return std::make_unique<pre_ast::DecInt>(value);
    }
    std::unique_ptr<pre_ast::Term> String::factor(){ return std::make_unique<pre_ast::String>(std::move(value)); }

    std::optional<Keyword> Token::keyword(){ return std::nullopt; }
    std::optional<Keyword> Identifier::keyword(){
        if(name == "if") return Keyword::If;
        else if(name == "else") return Keyword::Else;
        else if(name == "while") return Keyword::While;
        else if(name == "break") return Keyword::Break;
        else if(name == "continue") return Keyword::Continue;
        else if(name == "return") return Keyword::Return;
        else return std::nullopt;
    }

    std::optional<pre_ast::UnaryOperator> Token::prefix(){ return std::nullopt; }
    std::optional<pre_ast::UnaryOperator> Plus::prefix(){ return pre_ast::UnaryOperator::Plus; }
    std::optional<pre_ast::UnaryOperator> Hyphen::prefix(){ return pre_ast::UnaryOperator::Minus; }
    std::optional<pre_ast::UnaryOperator> DoublePlus::prefix(){ return pre_ast::UnaryOperator::PreInc; }
    std::optional<pre_ast::UnaryOperator> DoubleHyphen::prefix(){ return pre_ast::UnaryOperator::PreDec; }
    std::optional<pre_ast::UnaryOperator> Slash::prefix(){ return pre_ast::UnaryOperator::Recip; }
    std::optional<pre_ast::UnaryOperator> Exclamation::prefix(){ return pre_ast::UnaryOperator::LogicalNot; }
    std::optional<pre_ast::UnaryOperator> Tilde::prefix(){ return pre_ast::UnaryOperator::BitNot; }

    std::optional<pre_ast::UnaryOperator> Token::suffix(){ return std::nullopt; }
    std::optional<pre_ast::UnaryOperator> DoublePlus::suffix(){ return pre_ast::UnaryOperator::PostInc; }
    std::optional<pre_ast::UnaryOperator> DoubleHyphen::suffix(){ return pre_ast::UnaryOperator::PostDec; }

    std::optional<pre_ast::BinaryOperator> Token::infix(){ return std::nullopt; }
    std::optional<pre_ast::BinaryOperator> Plus::infix(){ return pre_ast::BinaryOperator::Add; }
    std::optional<pre_ast::BinaryOperator> Hyphen::infix(){ return pre_ast::BinaryOperator::Sub; }
    std::optional<pre_ast::BinaryOperator> Asterisk::infix(){ return pre_ast::BinaryOperator::Mul; }
    std::optional<pre_ast::BinaryOperator> Slash::infix(){ return pre_ast::BinaryOperator::Div; }
    std::optional<pre_ast::BinaryOperator> Percent::infix(){ return pre_ast::BinaryOperator::Rem; }
    std::optional<pre_ast::BinaryOperator> DoubleGreater::infix(){ return pre_ast::BinaryOperator::RightShift; }
    std::optional<pre_ast::BinaryOperator> DoubleLess::infix(){ return pre_ast::BinaryOperator::LeftShift; }
    std::optional<pre_ast::BinaryOperator> TripleGreater::infix(){ return pre_ast::BinaryOperator::ForwardShift; }
    std::optional<pre_ast::BinaryOperator> TripleLess::infix(){ return pre_ast::BinaryOperator::BackwardShift; }
    std::optional<pre_ast::BinaryOperator> DoubleEqual::infix(){ return pre_ast::BinaryOperator::Equal; }
    std::optional<pre_ast::BinaryOperator> ExclamationEqual::infix(){ return pre_ast::BinaryOperator::NotEqual; }
    std::optional<pre_ast::BinaryOperator> Less::infix(){ return pre_ast::BinaryOperator::Less; }
    std::optional<pre_ast::BinaryOperator> LessEqual::infix(){ return pre_ast::BinaryOperator::LessEqual; }
    std::optional<pre_ast::BinaryOperator> Greater::infix(){ return pre_ast::BinaryOperator::Greater; }
    std::optional<pre_ast::BinaryOperator> GreaterEqual::infix(){ return pre_ast::BinaryOperator::GreaterEqual; }
    std::optional<pre_ast::BinaryOperator> DoubleAmpersand::infix(){ return pre_ast::BinaryOperator::LogicalAnd; }
    std::optional<pre_ast::BinaryOperator> DoubleBar::infix(){ return pre_ast::BinaryOperator::LogicalOr; }
    std::optional<pre_ast::BinaryOperator> Ampersand::infix(){ return pre_ast::BinaryOperator::BitAnd; }
    std::optional<pre_ast::BinaryOperator> Bar::infix(){ return pre_ast::BinaryOperator::BitOr; }
    std::optional<pre_ast::BinaryOperator> Circumflex::infix(){ return pre_ast::BinaryOperator::BitXor; }
    std::optional<pre_ast::BinaryOperator> Colon::infix(){ return pre_ast::BinaryOperator::Type; }
    std::optional<pre_ast::BinaryOperator> Equal::infix(){ return pre_ast::BinaryOperator::Assign; }
    std::optional<pre_ast::BinaryOperator> PlusEqual::infix(){ return pre_ast::BinaryOperator::AddAssign; }
    std::optional<pre_ast::BinaryOperator> HyphenEqual::infix(){ return pre_ast::BinaryOperator::SubAssign; }
    std::optional<pre_ast::BinaryOperator> AsteriskEqual::infix(){ return pre_ast::BinaryOperator::MulAssign; }
    std::optional<pre_ast::BinaryOperator> SlashEqual::infix(){ return pre_ast::BinaryOperator::DivAssign; }
    std::optional<pre_ast::BinaryOperator> PercentEqual::infix(){ return pre_ast::BinaryOperator::RemAssign; }
    std::optional<pre_ast::BinaryOperator> AmpersandEqual::infix(){ return pre_ast::BinaryOperator::BitAndAssign; }
    std::optional<pre_ast::BinaryOperator> BarEqual::infix(){ return pre_ast::BinaryOperator::BitOrAssign; }
    std::optional<pre_ast::BinaryOperator> CircumflexEqual::infix(){ return pre_ast::BinaryOperator::BitXorAssign; }
    std::optional<pre_ast::BinaryOperator> DoubleGreaterEqual::infix(){ return pre_ast::BinaryOperator::RightShiftAssign; }
    std::optional<pre_ast::BinaryOperator> DoubleLessEqual::infix(){ return pre_ast::BinaryOperator::LeftShiftAssign; }
    std::optional<pre_ast::BinaryOperator> TripleGreaterEqual::infix(){ return pre_ast::BinaryOperator::ForwardShiftAssign; }
    std::optional<pre_ast::BinaryOperator> TripleLessEqual::infix(){ return pre_ast::BinaryOperator::BackwardShiftAssign; }

    bool Token::is_comma() const { return false; }
    bool Comma::is_comma() const { return true; }
    bool Token::is_semicolon() const { return false; }
    bool Semicolon::is_semicolon() const { return true; }
    bool Token::is_opening_parenthesis() const { return false; }
    bool OpeningParenthesis::is_opening_parenthesis() const { return true; }
    bool Token::is_closing_parenthesis() const { return false; }
    bool ClosingParenthesis::is_closing_parenthesis() const { return true; }
    bool Token::is_opening_bracket() const { return false; }
    bool OpeningBracket::is_opening_bracket() const { return true; }
    bool Token::is_closing_bracket() const { return false; }
    bool ClosingBracket::is_closing_bracket() const { return true; }
    bool Token::is_opening_brace() const { return false; }
    bool OpeningBrace::is_opening_brace() const { return true; }
    bool Token::is_closing_brace() const { return false; }
    bool ClosingBrace::is_closing_brace() const { return true; }

    std::optional<pre_ast::BracketType> Token::opening_bracket_type() const { return std::nullopt; }
    std::optional<pre_ast::BracketType> Token::closing_bracket_type() const { return std::nullopt; }
    std::optional<pre_ast::BracketType> OpeningParenthesis::opening_bracket_type() const { return pre_ast::BracketType::Round; }
    std::optional<pre_ast::BracketType> ClosingParenthesis::closing_bracket_type() const { return pre_ast::BracketType::Round; }
    std::optional<pre_ast::BracketType> OpeningBracket::opening_bracket_type() const { return pre_ast::BracketType::Square; }
    std::optional<pre_ast::BracketType> ClosingBracket::closing_bracket_type() const { return pre_ast::BracketType::Square; }
}

#ifdef DEBUG
#include <iostream>
static void indent(int depth){
    for(int i = 0; i < depth; i++) std::cout << "    ";
}
namespace token {
    void Identifier::debug_print(int depth) const {
        indent(depth);
        std::cout << "identifier (" << name << ")" << std::endl;
    }
    void Number::debug_print(int depth) const {
        indent(depth);
        std::cout << "number (" << value << ")" << std::endl;
    }
    void String::debug_print(int depth) const {
        indent(depth);
        std::cout << "string (" << value << ")" << std::endl;
    }

#define define_debug_print(token, text) \
    void token::debug_print(int depth) const { \
        indent(depth); \
        std::cout << text << std::endl; \
    } \

    define_debug_print(Plus, "plus (+)")
    define_debug_print(DoublePlus, "double plus (++)")
    define_debug_print(PlusEqual, "plus equal (+=)")
    define_debug_print(Hyphen, "hyphen (-)")
    define_debug_print(DoubleHyphen, "double hyphen (--)")
    define_debug_print(HyphenEqual, "hyphen equal (-=)")
    define_debug_print(Asterisk, "asterisk (*)")
    define_debug_print(AsteriskEqual, "asterisk equal (*=)")
    define_debug_print(Slash, "slash (/)")
    define_debug_print(SlashEqual, "slash equal (/=)")
    define_debug_print(Percent, "percent (%)")
    define_debug_print(PercentEqual, "percent equal (%=)")
    define_debug_print(Equal, "equal (=)")
    define_debug_print(DoubleEqual, "double equal (==)")
    define_debug_print(Exclamation, "exclamation (!)")
    define_debug_print(ExclamationEqual, "exclamation equal (!=)")
    define_debug_print(Less, "less (<)")
    define_debug_print(LessEqual, "less equal (<=)")
    define_debug_print(DoubleLess, "double less (<<)")
    define_debug_print(DoubleLessEqual, "double less equal (<<=)")
    define_debug_print(TripleLess, "triple less (<<<)")
    define_debug_print(TripleLessEqual, "triple less equal (<<<=)")
    define_debug_print(Greater, "greater (>)")
    define_debug_print(GreaterEqual, "greater equal (>=)")
    define_debug_print(DoubleGreater, "double greater (>>)")
    define_debug_print(DoubleGreaterEqual, "double greater equal (>>=)")
    define_debug_print(TripleGreater, "triple greater (>>>)")
    define_debug_print(TripleGreaterEqual, "triple greater equal (>>>=)")
    define_debug_print(Ampersand, "ampersand (&)")
    define_debug_print(AmpersandEqual, "ampersand equal (&=)")
    define_debug_print(DoubleAmpersand, "double ampersand (&&)")
    define_debug_print(Bar, "bar (|)")
    define_debug_print(BarEqual, "bar equal (|=)")
    define_debug_print(DoubleBar, "double bar (||)")
    define_debug_print(Circumflex, "circumflex (^)")
    define_debug_print(CircumflexEqual, "circumflex equal (^=)")
    define_debug_print(Dot, "dot (.)")
    define_debug_print(Colon, "colon (:)")
    define_debug_print(Semicolon, "semicolon (;)")
    define_debug_print(Comma, "comma (,)")
    define_debug_print(Question, "question (?)")
    define_debug_print(Hash, "hash (#)")
    define_debug_print(Tilde, "tilde (~)")
    define_debug_print(OpeningParenthesis, "opening parenthesis '('")
    define_debug_print(ClosingParenthesis, "closing parenthesis ')'")
    define_debug_print(OpeningBracket, "opening bracket '['")
    define_debug_print(ClosingBracket, "closing bracket ']'")
    define_debug_print(OpeningBrace, "opening brace '{'")
    define_debug_print(ClosingBrace, "closing brace '}'")
}
#endif
