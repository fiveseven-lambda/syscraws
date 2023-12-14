/*
 * Copyright (c) 2023 Atsushi Komaba
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

use super::Stmt;
use crate::ast;
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::Module;

pub fn translate(program: &ast::Program) {
    let module =
        JITModule::new(JITBuilder::new(cranelift_module::default_libcall_names()).unwrap());

    for def in &program.defs {
        let mut ctx = module.make_context();
        let mut fn_builder_ctx = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut fn_builder_ctx);
        let blocks: Vec<_> = (0..def.body.size).map(|_| builder.create_block()).collect();

        let vars: Vec<_> = program
            .vars
            .iter()
            .enumerate()
            .map(|(i, _)| {
                let var = Variable::new(i);
                builder.declare_var(var, types::I32);
                var
            })
            .collect();

        translate_block(&mut builder, program, &def.body, &blocks, &vars, 0, None);
        builder.seal_all_blocks();
        builder.finalize();
        println!("{}", ctx.func.display());
    }
}

pub fn translate_block(
    builder: &mut FunctionBuilder,
    program: &ast::Program,
    block: &ast::Block,
    blocks: &[Block],
    vars: &[Variable],
    current_idx: usize,
    next_idx: Option<usize>,
) {
    translate_block_rec(
        builder,
        program,
        &block.stmts,
        blocks,
        vars,
        current_idx,
        next_idx,
    )
}
pub fn translate_block_rec(
    builder: &mut FunctionBuilder,
    program: &ast::Program,
    stmts: &[ast::Stmt],
    blocks: &[Block],
    vars: &[Variable],
    current_idx: usize,
    next_idx: Option<usize>,
) {
    match stmts {
        [] => {}
        [stmt] => translate_stmt(builder, program, stmt, blocks, vars, current_idx, next_idx),
        [head, tail @ ..] => {
            let idx = match head {
                Stmt::Expr(_) => current_idx + 1,
                Stmt::If(_, x, y) => current_idx + 1 + x.size + y.size,
                Stmt::While(_, x) => current_idx + 1 + x.size,
                Stmt::Return(_) => current_idx + 1,
            };
            translate_stmt(builder, program, head, blocks, vars, current_idx, Some(idx));
            translate_block_rec(builder, program, tail, blocks, vars, idx, next_idx);
        }
    }
}
pub fn translate_stmt(
    builder: &mut FunctionBuilder,
    program: &ast::Program,
    stmt: &ast::Stmt,
    blocks: &[Block],
    vars: &[Variable],
    current_idx: usize,
    next_idx: Option<usize>,
) {
    match stmt {
        ast::Stmt::Expr(expr) => {
            builder.switch_to_block(blocks[current_idx]);
            translate_expr(builder, program, expr, vars);
            match next_idx {
                Some(idx) => builder.ins().jump(blocks[idx], &[]),
                None => builder.ins().return_(&[]),
            };
        }
        ast::Stmt::While(cond, body) => {
            builder.switch_to_block(blocks[current_idx]);
            let cond_value = translate_expr(builder, program, cond, vars);
            builder.ins().brif(
                cond_value,
                blocks[current_idx + 1],
                &[],
                blocks[next_idx.unwrap()],
                &[],
            );
            translate_block(
                builder,
                program,
                body,
                blocks,
                vars,
                current_idx + 1,
                Some(current_idx),
            );
        }
        _ => todo!(),
    }
}

pub fn translate_expr(
    builder: &mut FunctionBuilder,
    program: &ast::Program,
    expr: &ast::Expr,
    vars: &[Variable],
) -> Value {
    match expr {
        ast::Expr::Integer(value) => builder.ins().iconst(types::I32, *value as i64),
        ast::Expr::Call(func, args) => match **func {
            ast::Expr::Func(id, ref selected) => {
                match program.funcs[id][*selected.get().unwrap()] {
                    ast::Func::Defined(_) => todo!(),
                    ast::Func::Builtin(ref func) => {
                        let args_value: Vec<_> = args
                            .iter()
                            .map(|arg| translate_expr(builder, program, arg, vars))
                            .collect();
                        match func {
                            ast::BuiltinFunc::AddInt => {
                                builder.ins().iadd(args_value[0], args_value[1])
                            }
                            ast::BuiltinFunc::LessInt => builder.ins().icmp(
                                IntCC::SignedLessThan,
                                args_value[0],
                                args_value[1],
                            ),
                            ast::BuiltinFunc::Assign => match args[0] {
                                ast::Expr::Variable(id) => {
                                    builder.def_var(vars[id], args_value[1]);
                                    args_value[1]
                                }
                                _ => todo!(),
                            },
                            _ => args_value[0],
                        }
                    }
                }
            }
            _ => {
                let func = translate_expr(builder, program, func, vars);
                // builder.ins().call_indirect(SIG, callee, args);
                todo!();
            }
        },
        ast::Expr::Variable(id) => builder.use_var(vars[*id]),
        _ => todo!(),
    }
}
