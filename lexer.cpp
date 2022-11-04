/**
 * Copyright (c) 2022 Atsushi Komaba
 *
 * This file is part of Syscraws.
 * Syscraws is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or any later version.
 * Syscraws is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with Syscraws. If not, see <https://www.gnu.org/licenses/>. 
 *
 * @file lexer.cpp
 */
#include "lexer.hpp"
#include "error.hpp"

#include <unicode/uchar.h>
#include <unicode/utf8.h>

namespace lexer {
    /**
     * @brief コンストラクタ．
     */
    LineLexer::LineLexer(): in_stmt(false) {}
    /**
     * @brief コンストラクタ．
     */
    Lexer::Lexer(std::istream &source, bool prompt):
        source(source),
        prompt(prompt) {}

    /**
     * @brief `LineLexer` の `in_stmt` を false にセットする．
     */
    void Lexer::end_stmt(){
        line_lexer.in_stmt = false;
    }
    /**
     * @brief prompt の切り替えに用いる．
     *
     * in_stmt が true なら stmt の途中．
     * comments が空でないならコメントの途中．
     * いずれの場合も，プロンプトに `>` ではなく `+` を用いる．
     */
    bool LineLexer::in_stmt_or_comment() const {
        return in_stmt || !comments.empty();
    }
    /**
     * @brief 今までに読んだ入力の記録を返す．
     */
    const std::deque<std::string> &Lexer::get_log() const {
        return log;
    }

    /**
     * @brief 次のトークンを消費せずに返す．
     */
    std::unique_ptr<token::Token> &Lexer::peek(){
        while(tokens.empty()){
            if(source){
                // まだ EOF に達していない
                // 次の行が何行目か
                auto line_num = log.size();
                // log に空の std::string を追加し，1 行読んで格納
                log.emplace_back();
                if(prompt){
                    std::cout << (line_lexer.in_stmt_or_comment() ? "+ " : "> ");
                }
                std::getline(source, log.back());
                // 字句解析を行う
                line_lexer.run(line_num, log.back(), tokens);
            }else{
                // EOF に達した
                // コメント中なら例外を投げる
                line_lexer.deal_with_eof();
                // トークンの代わりに nullptr を入れる
                tokens.emplace();
            }
        }
        // ここで tokens は空でない
        return tokens.front();
    }

    /**
     * @brief 次のトークンを消費して返す．
     */
    std::unique_ptr<token::Token> Lexer::next(){
        auto ret = std::move(peek());
        tokens.pop();
        return ret;
    }

    /**
     * @brief 1 行分の文字列を受け取って，トークンに分解する．
     * @param line_num 位置情報に用いられる行番号．
     * @param line_view 1 行ぶんの文字列．
     * @param tokens トークンの格納先．
     * @throw error::UnexpectedCharacter トークンの開始として適さない文字列があった．
     */
    void LineLexer::run(
        std::size_t line_num,
        const std::string_view &line_view,
        std::queue<std::unique_ptr<token::Token>> &tokens
    ){
        const char *line = line_view.data();
        const std::size_t len = line_view.length();
        std::size_t cursor = 0;
        while(cursor < len){
            std::size_t start = cursor;
            UChar32 ch;
            U8_NEXT(line, cursor, len, ch);
            std::size_t next = cursor;
            auto next_if = [&](bool cond){
                if(cond) cursor = next;
                return cond;
            };
            if(!comments.empty()){
                if(ch == '/'){
                    U8_NEXT(line, next, len, ch);
                    if(next_if(ch == '*')){
                        comments.emplace_back(line_num, start);
                    }
                }else if(ch == '*'){
                    U8_NEXT(line, next, len, ch);
                    if(next_if(ch == '/')){
                        comments.pop_back();
                    }
                }
            }else if(string){
                if(ch == '"'){
                    std::string tmp;
                    string.value().second.toUTF8String(tmp);
                    std::unique_ptr<token::Token> token = std::make_unique<token::String>(std::move(tmp));
                    token->pos = pos::Range(string.value().first, pos::Pos(line_num, cursor));
                    tokens.push(std::move(token));
                    string.reset();
                }else{
                    if(ch == '\\'){
                        U8_NEXT(line, cursor, len, ch);
                        switch(ch){
                            case 'n': ch = '\n'; break;
                            case 't': ch = '\t'; break;
                            case 'r': ch = '\r'; break;
                            case '0': ch = '\0'; break;
                            case '"':
                            case '\\':
                                break;
                            default: throw error::make<error::InvalidEscapeSequence>(pos::Pos(line_num, start));
                        }
                    }
                    string.value().second.append(ch);
                }
            }else if(ch == '"'){
                // 文字列リテラルの開始
                in_stmt = true;
                string.emplace(pos::Pos(line_num, start), "");
            }else if(u_hasBinaryProperty(ch, UCHAR_WHITE_SPACE)){
                continue;
            }else{
                auto parse_number = [&]{
                    for(;; cursor = next){
                        UChar32 prev = ch;
                        U8_NEXT(line, next, len, ch);
                        if(u_hasBinaryProperty(ch, UCHAR_ID_CONTINUE)) continue;
                        if(ch == '.') continue;
                        if(ch == '_') continue;
                        if((prev == 'e' || prev == 'E') && (ch == '+' || ch == '-')) continue;
                        break;
                    }
                    return std::make_unique<token::Number>(line_view.substr(start, cursor - start));
                };
                std::unique_ptr<token::Token> token;
                if(u_hasBinaryProperty(ch, UCHAR_ID_START) || ch == '_' || ch == '$'){
                    do U8_NEXT(line, next, len, ch);
                    while(next_if(u_hasBinaryProperty(ch, UCHAR_ID_CONTINUE) || ch == '_' || ch == '$'));
                    token = std::make_unique<token::Identifier>(line_view.substr(start, cursor - start));
                }else if(ch == '.'){
                    U8_NEXT(line, next, len, ch);
                    if(next_if('0' <= ch && ch <= '9')) token = parse_number();
                    else token = std::make_unique<token::Dot>();
                }else if('0' <= ch && ch <= '9'){
                    token = parse_number();
                }else if(ch == '+'){
                    U8_NEXT(line, next, len, ch);
                    if(next_if(ch == '+')) token = std::make_unique<token::DoublePlus>();
                    else if(next_if(ch == '=')) token = std::make_unique<token::PlusEqual>();
                    else token = std::make_unique<token::Plus>();
                }else if(ch == '-'){
                    U8_NEXT(line, next, len, ch);
                    if(next_if(ch == '-')) token = std::make_unique<token::DoubleHyphen>();
                    else if(next_if(ch == '=')) token = std::make_unique<token::HyphenEqual>();
                    else token = std::make_unique<token::Hyphen>();
                }else if(ch == '*'){
                    U8_NEXT(line, next, len, ch);
                    if(next_if(ch == '=')) token = std::make_unique<token::AsteriskEqual>();
                    else token = std::make_unique<token::Asterisk>();
                }else if(ch == '/'){
                    U8_NEXT(line, next, len, ch);
                    if(next_if(ch == '=')) token = std::make_unique<token::SlashEqual>();
                    else if(next_if(ch == '/')){
                        return;
                    }else if(next_if(ch == '*')){
                        comments.emplace_back(line_num, start);
                        continue;
                    }else token = std::make_unique<token::Slash>();
                }else if(ch == '%'){
                    U8_NEXT(line, next, len, ch);
                    if(next_if(ch == '=')) token = std::make_unique<token::PercentEqual>();
                    else token = std::make_unique<token::Percent>();
                }else if(ch == '='){
                    U8_NEXT(line, next, len, ch);
                    if(next_if(ch == '=')) token = std::make_unique<token::DoubleEqual>();
                    else token = std::make_unique<token::Equal>();
                }else if(ch == '!'){
                    U8_NEXT(line, next, len, ch);
                    if(next_if(ch == '=')) token = std::make_unique<token::ExclamationEqual>();
                    else token = std::make_unique<token::Exclamation>();
                }else if(ch == '>'){
                    U8_NEXT(line, next, len, ch);
                    if(next_if(ch == '>')){
                        U8_NEXT(line, next, len, ch);
                        if(next_if(ch == '>')){
                            U8_NEXT(line, next, len, ch);
                            if(next_if(ch == '=')) token = std::make_unique<token::TripleGreaterEqual>();
                            else token = std::make_unique<token::TripleGreater>();
                        }else if(next_if(ch == '=')) token = std::make_unique<token::DoubleGreaterEqual>();
                        else token = std::make_unique<token::DoubleGreater>();
                    }else if(next_if(ch == '=')) token = std::make_unique<token::GreaterEqual>();
                    else token = std::make_unique<token::Greater>();
                }else if(ch == '<'){
                    U8_NEXT(line, next, len, ch);
                    if(next_if(ch == '<')){
                        U8_NEXT(line, next, len, ch);
                        if(next_if(ch == '<')){
                            U8_NEXT(line, next, len, ch);
                            if(next_if(ch == '=')) token = std::make_unique<token::TripleLessEqual>();
                            else token = std::make_unique<token::TripleLess>();
                        }else if(next_if(ch == '=')) token = std::make_unique<token::DoubleLessEqual>();
                        else token = std::make_unique<token::DoubleLess>();
                    }else if(next_if(ch == '=')) token = std::make_unique<token::LessEqual>();
                    else token = std::make_unique<token::Less>();
                }else if(ch == '&'){
                    U8_NEXT(line, next, len, ch);
                    if(next_if(ch == '&')) token = std::make_unique<token::DoubleAmpersand>();
                    else if(next_if(ch == '=')) token = std::make_unique<token::AmpersandEqual>();
                    else token = std::make_unique<token::Ampersand>();
                }else if(ch == '|'){
                    U8_NEXT(line, next, len, ch);
                    if(next_if(ch == '|')) token = std::make_unique<token::DoubleBar>();
                    else if(next_if(ch == '=')) token = std::make_unique<token::BarEqual>();
                    else token = std::make_unique<token::Bar>();
                }else if(ch == '^'){
                    U8_NEXT(line, next, len, ch);
                    if(next_if(ch == '=')) token = std::make_unique<token::CircumflexEqual>();
                    else token = std::make_unique<token::Circumflex>();
                }else if(ch == '.') token = std::make_unique<token::Dot>();
                else if(ch == ':') token = std::make_unique<token::Colon>();
                else if(ch == ';') token = std::make_unique<token::Semicolon>();
                else if(ch == ',') token = std::make_unique<token::Comma>();
                else if(ch == '?') token = std::make_unique<token::Question>();
                else if(ch == '#') token = std::make_unique<token::Hash>();
                else if(ch == '~') token = std::make_unique<token::Tilde>();
                else if(ch == '(') token = std::make_unique<token::OpeningParenthesis>();
                else if(ch == ')') token = std::make_unique<token::ClosingParenthesis>();
                else if(ch == '[') token = std::make_unique<token::OpeningBracket>();
                else if(ch == ']') token = std::make_unique<token::ClosingBracket>();
                else if(ch == '{') token = std::make_unique<token::OpeningBrace>();
                else if(ch == '}') token = std::make_unique<token::ClosingBrace>();
                else throw error::make<error::UnexpectedCharacter>(pos::Pos(line_num, start));
                in_stmt = true;
                token->pos = pos::Range(line_num, start, cursor);
                tokens.push(std::move(token));
            }
        }
        if(string){
            string.value().second.append("\n");
        }
    }

    /**
     * @brief EOF に達したときに呼び出す．
     * コメントが終了しているか，文字列リテラルが終了しているか確認する．
     * @throw error::UnterminatedComment コメントが終了していなかった．
     * @throw error::UnterminatedStringLiteral 文字列リテラルが終了していなかった．
     */
    void LineLexer::deal_with_eof(){
        if(!comments.empty()){
            throw error::make<error::UnterminatedComment>(std::move(comments));
        }else if(string){
            throw error::make<error::UnterminatedStringLiteral>(string.value().first);
        }
    }
}
