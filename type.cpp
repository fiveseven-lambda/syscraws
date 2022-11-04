/**
 * Copyright (c) 2022 Atsushi Komaba
 *
 * This file is part of Syscraws.
 * Syscraws is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or any later version.
 * Syscraws is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with Syscraws. If not, see <https://www.gnu.org/licenses/>. 
 *
 * @file type.cpp
 */
#include "type.hpp"

#include <boost/functional/hash.hpp>

namespace type {
    Type::~Type() = default;

    Func::Func(const std::vector<std::reference_wrapper<const Type>> &args, const Type &ret): args(std::move(args)), ret(ret) {}

    const std::vector<std::reference_wrapper<const Type>> &Func::get_args() const { return args; }
    const Type &Func::get_ret() const { return ret; }
    std::pair<const std::vector<std::reference_wrapper<const Type>> &, const Type &> Func::get_pair() const { return {args, ret}; }

    std::size_t FuncHash::operator()(const std::pair<const std::vector<std::reference_wrapper<const Type>> &, const Type &> & pair) const noexcept {
        std::size_t seed = 0;
        for(const Type &arg : pair.first) boost::hash_combine<const Type *>(seed, &arg);
        boost::hash_combine<const Type *>(seed, &pair.second);
        return seed;
    }
    std::size_t FuncHash::operator()(const std::unique_ptr<Func> &type) const noexcept { return (*this)(type->get_pair()); }
    bool FuncEq::operator()(const std::pair<const std::vector<std::reference_wrapper<const Type>> &, const Type &> &left, const std::pair<const std::vector<std::reference_wrapper<const Type>> &, const Type &> &right) const noexcept {
        if(&left.second != &right.second) return false;
        if(left.first.size() != right.first.size()) return false;
        for(std::size_t i = 0; i < left.first.size(); i++){
            if(&left.first[i].get() != &right.first[i].get()) return false;
        }
        return true;
    }
    bool FuncEq::operator()(const std::pair<const std::vector<std::reference_wrapper<const Type>> &, const Type &> &left, const std::unique_ptr<Func> &right) const noexcept { return (*this)(left, right->get_pair()); }
    bool FuncEq::operator()(const std::unique_ptr<Func> &left, const std::unique_ptr<Func> &right) const noexcept { return (*this)(left->get_pair(), right->get_pair()); }

    const Bool &Context::get_bool() & { return bool_ty; }
    const Int &Context::get_int() & { return int_ty; }
    const Float &Context::get_float() & { return float_ty; }
    const Str &Context::get_str() & { return str_ty; }
    const Func &Context::get_func(const std::vector<std::reference_wrapper<const Type>> &args, const Type &ret) & {
        auto it = funcs.find(std::pair<const std::vector<std::reference_wrapper<const Type>> &, const Type &>(args, ret));
        if(it == funcs.end()) it = funcs.insert(std::make_unique<Func>(args, ret)).first;
        return **it;
    }
}

#ifdef DEBUG
#include <iostream>
static void indent(int depth){
    for(int i = 0; i < depth; i++) std::cout << "    ";
}
namespace type {
    void Bool::debug_print(int depth) const {
        indent(depth);
        std::cout << "bool" << std::endl;
    }
    void Int::debug_print(int depth) const {
        indent(depth);
        std::cout << "int" << std::endl;
    }
    void Float::debug_print(int depth) const {
        indent(depth);
        std::cout << "float" << std::endl;
    }
    void Str::debug_print(int depth) const {
        indent(depth);
        std::cout << "str" << std::endl;
    }
    void Func::debug_print(int depth) const {
        indent(depth);
        std::cout << "func(" << args.size() << ") (" << std::endl;
        for(const Type &arg: args){
            arg.debug_print(depth + 1);
        }
        indent(depth);
        std::cout << ") ->" << std::endl;
        ret.debug_print(depth + 1);
    }
    void Context::debug_print(int depth) const {
        for(auto &func : funcs) func->debug_print(depth);
    }
}
#endif
