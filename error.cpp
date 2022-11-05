/**
 * Copyright (c) 2022 Atsushi Komaba
 *
 * This file is part of Syscraws.
 * Syscraws is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or any later version.
 * Syscraws is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with Syscraws. If not, see <https://www.gnu.org/licenses/>. 
 *
 * @file error.cpp
 */

#include "error.hpp"

namespace error {
    Error::~Error() = default;
    /**
     * @brief コンストラクタ
     * @param pos 予期せぬ文字のあった場所
     */
    UnexpectedCharacter::UnexpectedCharacter(pos::Pos pos):
        pos(pos) {}
    void UnexpectedCharacter::eprint(const std::deque<std::string> &log) const {
        std::cerr << "unexpected character at " << pos << std::endl;
        pos.eprint(log);
    }

    /**
     * @brief コンストラクタ
     * @param poss コメントの開始位置．ネストしていた場合それら全て
     */
    UnterminatedComment::UnterminatedComment(std::vector<pos::Pos> poss):
        poss(std::move(poss)) {}
    void UnterminatedComment::eprint(const std::deque<std::string> &log) const {
        std::cerr << "unterminated comment" << std::endl;
        for(const pos::Pos &pos : poss){
            std::cerr << "started at " << pos << std::endl;
            pos.eprint(log);
        }
    }

    /**
     * @brief コンストラクタ
     * @param pos 文字列リテラルの開始位置．
     */
    UnterminatedStringLiteral::UnterminatedStringLiteral(pos::Pos pos):
        pos(pos) {}
    void UnterminatedStringLiteral::eprint(const std::deque<std::string> &log) const {
        std::cerr << "unterminated string literal (started at " << pos << ")" << std::endl;
        pos.eprint(log);
    }   

    InvalidEscapeSequence::InvalidEscapeSequence(pos::Pos pos):
        pos(pos) {}
    void InvalidEscapeSequence::eprint(const std::deque<std::string> &log) const {
        std::cerr << "invalid escape sequence at " << pos << std::endl;
        pos.eprint(log);
    }

    InvalidNumericLiteral::InvalidNumericLiteral(pos::Range pos):
        pos(std::move(pos)) {}
    void InvalidNumericLiteral::eprint(const std::deque<std::string> &log) const {
        std::cerr << "invalid numeric literal at " << pos << std::endl;
        pos.eprint(log);
    }

    /**
     * @brief コンストラクタ
     * @param op 演算子の位置
     * @param token 予期せぬトークンの位置
     */
    UnexpectedTokenAfterOperator::UnexpectedTokenAfterOperator(pos::Range op, pos::Range token):
        op(std::move(op)),
        token(std::move(token)) {}
    void UnexpectedTokenAfterOperator::eprint(const std::deque<std::string> &log) const {
        std::cerr << "unexpected token at " << token << std::endl;
        token.eprint(log);
        std::cerr << "after operator at " << op << std::endl;
        op.eprint(log);
    }

    /**
     * @brief コンストラクタ
     * @param op 演算子の位置
     */
    EOFAfterOperator::EOFAfterOperator(pos::Range op):
        op(std::move(op)) {}
    void EOFAfterOperator::eprint(const std::deque<std::string> &log) const {
        std::cerr << "unexpected EOF after operator at " << op << std::endl;
        op.eprint(log);
    }

    /**
     * @brief コンストラクタ
     * @param open 開き括弧の位置
     */
    NoClosingBracket::NoClosingBracket(pos::Range open):
        open(std::move(open)) {}
    void NoClosingBracket::eprint(const std::deque<std::string> &log) const {
        std::cerr << "no closing bracket corresponding to opening bracket at " << open << std::endl;
        open.eprint(log);
    }

    /**
     * @brief コンストラクタ
     * @param open 開き括弧の位置
     * @param token 予期せぬトークンの位置
     */
    UnexpectedTokenInBracket::UnexpectedTokenInBracket(pos::Range open, pos::Range token):
        open(std::move(open)),
        token(std::move(token)) {}
    void UnexpectedTokenInBracket::eprint(const std::deque<std::string> &log) const {
        std::cerr << "unexpected token at " << token << std::endl;
        token.eprint(log);
        std::cerr << "bracket opened at " << open << std::endl;
        open.eprint(log);
    }

    DifferentClosingBracket::DifferentClosingBracket(pos::Range open, pos::Range close):
        open(std::move(open)),
        close(std::move(close)) {}
    void DifferentClosingBracket::eprint(const std::deque<std::string> &log) const {
        std::cerr << "closing bracket at " << close << std::endl;
        close.eprint(log);
        std::cerr << "does not match opening bracket at " << open << std::endl;
        open.eprint(log);
    }

    /**
     * @brief コンストラクタ
     * @param comma コンマの位置
     */
    EmptyItemInList::EmptyItemInList(pos::Range comma):
        comma(std::move(comma)) {}
    void EmptyItemInList::eprint(const std::deque<std::string> &log) const {
        std::cerr << "expected expression before comma at " << comma << std::endl;
        comma.eprint(log);
    }

    UnexpectedEOFAfterKeyword::UnexpectedEOFAfterKeyword(pos::Range keyword):
        keyword(std::move(keyword)) {}
    void UnexpectedEOFAfterKeyword::eprint(const std::deque<std::string> &log) const {
        std::cerr << "unexpected EOF after keyword at " << keyword << std::endl;
        keyword.eprint(log);
    }

    UnexpectedTokenAfterKeyword::UnexpectedTokenAfterKeyword(pos::Range keyword, pos::Range token):
        keyword(std::move(keyword)),
        token(std::move(token)) {}
    void UnexpectedTokenAfterKeyword::eprint(const std::deque<std::string> &log) const {
        std::cerr << "unexpected token at " << token << std::endl;
        token.eprint(log);
        std::cerr << "after keyword at " << keyword << std::endl;
        keyword.eprint(log);
    }

    EOFAtEndOfStmt::EOFAtEndOfStmt(pos::Range stmt):
        stmt(std::move(stmt)) {}
    void EOFAtEndOfStmt::eprint(const std::deque<std::string> &log) const {
        std::cerr << "unexpected EOF, expected semicolon at the end of statement at " << stmt << std::endl;
        stmt.eprint(log);
    }

    UnexpectedTokenAtEndOfStmt::UnexpectedTokenAtEndOfStmt(pos::Range stmt, pos::Range token):
        stmt(std::move(stmt)),
        token(std::move(token)) {}
    void UnexpectedTokenAtEndOfStmt::eprint(const std::deque<std::string> &log) const {
        std::cerr << "unexpected token at " << token << std::endl;
        token.eprint(log);
        std::cerr << "expected semicolon at the end of statement at " << stmt << std::endl;
        stmt.eprint(log);
    }

    UnexpectedTokenAtBeginningOfStmt::UnexpectedTokenAtBeginningOfStmt(pos::Range token):
        token(std::move(token)) {}
    void UnexpectedTokenAtBeginningOfStmt::eprint(const std::deque<std::string> &log) const {
        std::cerr << "expected sentence, unexpected token at " << token << std::endl;
        token.eprint(log);
    }

    UnexpectedTokenInControl::UnexpectedTokenInControl(pos::Range control, pos::Range token):
        control(std::move(control)),
        token(std::move(token)) {}
    void UnexpectedTokenInControl::eprint(const std::deque<std::string> &log) const {
        std::cerr << "unexpected token at " << token << std::endl;
        token.eprint(log);
        std::cerr << "expected statement in control structure " << control << std::endl;
        control.eprint(log);
    }

    UnexpectedEOFInControl::UnexpectedEOFInControl(pos::Range control):
        control(std::move(control)) {}
    void UnexpectedEOFInControl::eprint(const std::deque<std::string> &log) const {
        std::cerr << "expected statement, found EOF in control structure " << control << std::endl;
        control.eprint(log);
    }

    OverflowInIntegerLiteral::OverflowInIntegerLiteral(std::exception &error, pos::Range pos):
        error(error),
        pos(std::move(pos)) {}
    void OverflowInIntegerLiteral::eprint(const std::deque<std::string> &log) const {
        std::cerr << "overflow occurred in integer literal at " << pos << " (" << error.what() << ")" << std::endl;
        pos.eprint(log);
    }

    TermNotType::TermNotType(pos::Range term):
        term(std::move(term)) {}
    void TermNotType::eprint(const std::deque<std::string> &log) const {
        std::cerr << "expression cannot be recognized as a type" << std::endl;
        term.eprint(log);
    }

    InvalidLHSOfDecl::InvalidLHSOfDecl(pos::Range term):
        term(std::move(term)) {}
    void InvalidLHSOfDecl::eprint(const std::deque<std::string> &log) const {
        std::cerr << "invalid left hand side of declaration" << std::endl;
        term.eprint(log);
    }

    DeclWithoutTypeOrRHS::DeclWithoutTypeOrRHS(pos::Range pos):
        pos(std::move(pos)) {}
    void DeclWithoutTypeOrRHS::eprint(const std::deque<std::string> &log) const {
        std::cerr << "declaration requires either type or right hand side" << std::endl;
        pos.eprint(log);
    }

    UndefinedVariable::UndefinedVariable(pos::Range pos):
        pos(std::move(pos)) {}
    void UndefinedVariable::eprint(const std::deque<std::string> &log) const {
        std::cerr << "undefined variable at " << pos << std::endl;
        pos.eprint(log);
    }

    Unimplemented::Unimplemented(const char *file, unsigned line):
        file(file),
        line(line) {}
    void Unimplemented::eprint(const std::deque<std::string> &) const {
        std::cerr << "error message unimplemented. file \"" << file << "\" line " << line << std::endl;
    }
}
