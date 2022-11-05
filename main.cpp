/**
 * Copyright (c) 2022 Atsushi Komaba
 *
 * This file is part of Syscraws.
 * Syscraws is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or any later version.
 * Syscraws is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with Syscraws. If not, see <https://www.gnu.org/licenses/>. 
 *
 * @file main.cpp
*/
#include <fstream>
#include "error.hpp"
#include "parser.hpp"

struct Config {
    std::istream &source;
    bool prompt;
};

static void run(const Config &config){
    lexer::Lexer lexer(config.source, config.prompt);
    ast::Context ctx;
    ir::Env env;
    std::cout << std::boolalpha;
    try {
        while(true){
            auto stmt = parse(lexer);
            if(!stmt) break;
            // stmt->debug_print(0);
            std::unique_ptr<ast::Item> item = stmt->to_item();
            // item->debug_print(0);
            item->run(ctx, env);
        }
    }catch(std::unique_ptr<error::Error> &error){
        error->eprint(lexer.get_log());
    }
}

int main(int argc, char *argv[]) {
    if(argc == 1){
        run(Config{
            .source = std::cin,
            .prompt = true,
        });
    }else{
        std::ifstream source(argv[1]);
        run(Config{
            .source = source,
            .prompt = false,
        });
    }
    return 0;
}
