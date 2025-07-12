/*
 * Copyright (c) 2023-2025 Atsushi Komaba
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

#include "ffi.hpp"
#include "llvm/ExecutionEngine/Orc/ExecutionUtils.h"
#include "llvm/ExecutionEngine/Orc/LLJIT.h"
#include "llvm/ExecutionEngine/Orc/Shared/ExecutorAddress.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/ConstantFolder.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/TargetSelect.h"
#include <boost/functional/hash.hpp>
#include <cstdarg>
#include <memory>
#include <unordered_set>

static llvm::ExitOnError exit_on_error;

static BooleanType boolean_type;
static IntegerType integer_type;
static SizeType size_type;
static StringType string_type;
static std::unordered_set<FunctionType, FunctionType::Hasher> function_types;

static std::unique_ptr<llvm::LLVMContext> llvm_context =
    std::make_unique<llvm::LLVMContext>();
static llvm::IRBuilder<llvm::ConstantFolder, llvm::IRBuilderDefaultInserter>
    ir_builder(*llvm_context);
static std::unique_ptr<llvm::Module> main_module =
    std::make_unique<llvm::Module>("", *llvm_context);
static std::vector<llvm::BasicBlock *> basic_blocks;
static std::unique_ptr<llvm::orc::LLJIT> jit;

Type::~Type() = default;

llvm::Type *BooleanType::into_llvm_type(llvm::LLVMContext &context) const {
  return llvm::Type::getInt1Ty(context);
}

llvm::Type *IntegerType::into_llvm_type(llvm::LLVMContext &context) const {
  return llvm::IntegerType::get(context, sizeof(int) * CHAR_BIT);
}

llvm::Type *SizeType::into_llvm_type(llvm::LLVMContext &context) const {
  return llvm::IntegerType::get(context, sizeof(std::size_t) * CHAR_BIT);
}

llvm::Type *StringType::into_llvm_type(llvm::LLVMContext &context) const {
  llvm::Type *field_type = get_size_type()->into_llvm_type(context);
  return llvm::StructType::get(field_type, field_type);
}

llvm::Type *FunctionType::into_llvm_type(llvm::LLVMContext &context) const {
  std::vector<llvm::Type *> llvm_parameters_type;
  for (const Type *parameter_type : parameters_type) {
    llvm_parameters_type.push_back(parameter_type->into_llvm_type(context));
  }
  llvm::Type *llvm_return_type = return_type->into_llvm_type(context);
  return llvm::FunctionType::get(llvm_return_type, llvm_parameters_type,
                                 is_variadic);
}

FunctionType::FunctionType(bool is_variadic, const Type *return_type,
                           const std::vector<const Type *> &parameters_type)
    : is_variadic(is_variadic), return_type(return_type),
      parameters_type(std::move(parameters_type)) {}

bool FunctionType::operator==(const FunctionType &other) const {
  return return_type == other.return_type &&
         parameters_type == other.parameters_type;
}

std::size_t
FunctionType::Hasher::operator()(const FunctionType &function_type) const {
  std::size_t seed = 0;
  for (const Type *parameter_type : function_type.parameters_type)
    boost::hash_combine<const Type *>(seed, parameter_type);
  boost::hash_combine<const Type *>(seed, function_type.return_type);
  return seed;
}

Expression::~Expression() = default;

Integer::Integer(int value) : value(value) {}

llvm::Value *
Integer::codegen(llvm::IRBuilderBase &builder,
                 const std::vector<llvm::Value *> &parameters) const {
  llvm::Type *integer_type =
      get_integer_type()->into_llvm_type(builder.getContext());
  return llvm::ConstantInt::get(integer_type, value);
}

App::App(const Expression *function, std::vector<const Expression *> arguments)
    : function(function), arguments(arguments) {}

llvm::Value *App::codegen(llvm::IRBuilderBase &builder,
                          const std::vector<llvm::Value *> &parameters) const {
  std::vector<llvm::Value *> intermediates;
  for (const Expression *argument : arguments) {
    intermediates.push_back(argument->codegen(builder, parameters));
  }
  return function->codegen(builder, intermediates);
}

extern "C" void initialize_jit() {
  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();
  llvm::orc::LLJITBuilder jit_builder;
  jit = exit_on_error(jit_builder.create());
  char global_prefix = jit->getDataLayout().getGlobalPrefix();
  auto generator = exit_on_error(
      llvm::orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(
          global_prefix));
  jit->getMainJITDylib().addGenerator(std::move(generator));
}

extern "C" void add_function(const char *function_name,
                             const Type *function_type,
                             std::size_t num_blocks) {
  llvm::FunctionType *llvm_function_type = llvm::dyn_cast<llvm::FunctionType>(
      function_type->into_llvm_type(*llvm_context));
  llvm::Function *function = llvm::Function::Create(
      llvm_function_type, llvm::Function::ExternalLinkage, function_name,
      *main_module);
  basic_blocks = std::vector<llvm::BasicBlock *>();
  for (std::size_t block_index = 0; block_index < num_blocks; block_index++) {
    basic_blocks.push_back(
        llvm::BasicBlock::Create(*llvm_context, "", function));
  }
}

extern "C" void set_insert_point(std::size_t block_index) {
  ir_builder.SetInsertPoint(basic_blocks[block_index]);
}

extern "C" llvm::Value *create_integer(int value) {
  llvm::Type *integer_type =
      get_integer_type()->into_llvm_type(ir_builder.getContext());
  return llvm::ConstantInt::get(integer_type, value);
}

extern "C" void create_return(llvm::Value *value) {
  ir_builder.CreateRet(value);
}

extern "C" void add_expression(const Expression *expression) {
  expression->codegen(ir_builder, {});
}

extern "C" void add_return(const Expression *expression) {
  llvm::Value *value = expression->codegen(ir_builder, {});
  ir_builder.CreateRet(value);
}

extern "C" void *compile_function(const char *function_name) {
  // main_module->print(llvm::outs(), nullptr);
  exit_on_error(jit->addIRModule(llvm::orc::ThreadSafeModule(
      std::move(main_module), std::move(llvm_context))));
  llvm::orc::ExecutorAddr addr = exit_on_error(jit->lookup(function_name));
  return addr.toPtr<void *>();
}

extern "C" const Type *get_boolean_type() { return &boolean_type; }

extern "C" const Type *get_integer_type() { return &integer_type; }

extern "C" const Type *get_size_type() { return &size_type; }

extern "C" const Type *get_string_type() { return &string_type; }

extern "C" const Type *get_function_type(bool is_variadic,
                                         const Type *return_type,
                                         std::size_t num_parameters, ...) {
  std::va_list va_list;
  va_start(va_list, num_parameters);
  std::vector<const Type *> parameters_type;
  for (std::size_t parameter_index = 0; parameter_index < num_parameters;
       parameter_index++) {
    parameters_type.push_back(va_arg(va_list, const Type *));
  }
  va_end(va_list);

  return &*function_types.emplace(is_variadic, return_type, parameters_type)
               .first;
}

extern "C" const Expression *new_integer(int value) {
  return new Integer(value);
}

extern "C" const Expression *new_app(const Expression *function,
                                     std::size_t num_arguments, ...) {
  std::va_list va_list;
  va_start(va_list, num_arguments);
  std::vector<const Expression *> arguments;
  for (std::size_t argument_index = 0; argument_index < num_arguments;
       argument_index++) {
    arguments.push_back(va_arg(va_list, const Expression *));
  }
  va_end(va_list);

  return new App(function, arguments);
}
