/*
 * Copyright (c) 2023-2026 Atsushi Komaba
 *
 * This file is part of Syscraws.
 * Syscraws is free software: you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation, either version 3
 * of the License, or any later version.
 *
 * Syscraws is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Syscraws. If not, see <https://www.gnu.org/licenses/>.
 */

#ifndef FFI_HPP
#define FFI_HPP

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include <vector>

class Type {
public:
  virtual ~Type();
  virtual llvm::Type *into_llvm_type(llvm::LLVMContext &) const = 0;
};

class BooleanType : public Type {
public:
  llvm::Type *into_llvm_type(llvm::LLVMContext &) const override;
};

class IntegerType : public Type {
public:
  llvm::Type *into_llvm_type(llvm::LLVMContext &) const override;
};

class SizeType : public Type {
public:
  llvm::Type *into_llvm_type(llvm::LLVMContext &) const override;
};

class StringType : public Type {
public:
  llvm::Type *into_llvm_type(llvm::LLVMContext &) const override;
};

class FunctionType : public Type {
  bool is_variadic;
  const Type *return_type;
  std::vector<const Type *> parameters_type;

public:
  FunctionType(bool, const Type *, const std::vector<const Type *> &);
  llvm::Type *into_llvm_type(llvm::LLVMContext &) const override;
  bool operator==(const FunctionType &) const;
  struct Hasher {
    std::size_t operator()(const FunctionType &) const;
  };
};

class Expression {
public:
  virtual ~Expression();
  virtual llvm::Value *codegen(llvm::IRBuilderBase &,
                               const std::vector<llvm::Value *> &) const = 0;
};

class Parameter : public Expression {
  std::size_t index;
  llvm::Value *codegen(llvm::IRBuilderBase &,
                       const std::vector<llvm::Value *> &) const override;

public:
  Parameter(std::size_t);
};

class Integer : public Expression {
  int value;
  llvm::Value *codegen(llvm::IRBuilderBase &,
                       const std::vector<llvm::Value *> &) const override;

public:
  Integer(int);
};

class Size : public Expression {
  std::size_t value;
  llvm::Value *codegen(llvm::IRBuilderBase &,
                       const std::vector<llvm::Value *> &) const override;

public:
  Size(std::size_t);
};

class App : public Expression {
  const Expression *function;
  std::vector<const Expression *> arguments;
  llvm::Value *codegen(llvm::IRBuilderBase &,
                       const std::vector<llvm::Value *> &) const override;

public:
  App(const Expression *, std::vector<const Expression *>);
};

class AddInteger : public Expression {
  llvm::Value *codegen(llvm::IRBuilderBase &,
                       const std::vector<llvm::Value *> &) const override;
};

class Call : public Expression {
  const char *function_name;
  const FunctionType *function_type;
  llvm::Value *codegen(llvm::IRBuilderBase &,
                       const std::vector<llvm::Value *> &) const override;

public:
  Call(const char *, const FunctionType *);
};

class CompileAndCall : public Expression {
  Expression *expression;
  const FunctionType *function_type;
  llvm::Value *codegen(llvm::IRBuilderBase &,
                       const std::vector<llvm::Value *> &) const override;

public:
  CompileAndCall(Expression *, const FunctionType *);
};

extern "C" {
void initialize_jit();
void add_function(const char *, const Type *, std::size_t);
void set_insert_point(std::size_t);
llvm::Value *create_integer(int);
void create_return(llvm::Value *);
void *compile_function(const char *);
const Type *get_boolean_type();
const Type *get_integer_type();
const Type *get_size_type();
const Type *get_string_type();
const Type *get_function_type(bool, const Type *, std::size_t, ...);
const Expression *new_parameter(std::size_t);
const Expression *new_integer(int);
const Expression *new_size(std::size_t);
const Expression *new_app(const Expression *, std::size_t, ...);
const Expression *new_add_integer();
void *compile_expression(const Expression *, const Type *);
}

#endif
