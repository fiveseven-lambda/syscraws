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

/*!
 * Defines [`Context`] used to translate AST into IR.
 */

use std::collections::HashMap;

use super::{Class, Item, Variables, ast};
use crate::{ir, log};

/**
 * Represents the name resolution context.
 */
pub struct Context {
    /**
     * A mapping from identifiers to their corresponding items and source positions.
     *
     * Items defined in the same file can be referenced by their names directly.
     * Referencing items defined in another file requires dot notation ([`ast::Term::FieldByName`]).
     */
    pub items: HashMap<String, (Option<log::Pos>, Item)>,
    pub submodules: Vec<usize>,
    pub eq_instances: Vec<ir::Function>,
    pub add_instances: Vec<ir::Function>,
    pub instances: Vec<Vec<ir::Function>>,
}

impl Context {
    pub fn translate_structure_definition(
        &mut self,
        ast::StructureDefinition {
            ty_parameters: ast_ty_parameters,
            fields: ast_fields,
            extra_tokens_pos,
        }: ast::StructureDefinition,
        exports: &[Context],
        logger: &mut log::Logger,
    ) -> Result<(ir::Constant, ir::StructureDefinition), ()> {
        let mut parameter_tys = Vec::new();
        let mut parameter_names = Vec::new();
        let mut depth = 0;
        self.translate_constant_declaration(
            ast_ty_parameters.unwrap(),
            &mut parameter_names,
            &mut parameter_tys,
            &mut depth,
            exports,
            logger,
        );
        let ty = parameter_tys
            .into_iter()
            .rev()
            .fold(ir::Constant::Ty, |ty, tys| {
                ir::Constant::Product(tys, Box::new(ty))
            });
        if let Some(extra_tokens_pos) = extra_tokens_pos {
            logger.extra_tokens(extra_tokens_pos);
        }
        let mut field_tys = Vec::new();
        for ast::WithExtraTokens {
            content: ast_field,
            extra_tokens_pos,
        } in ast_fields
        {
            match ast_field.term {
                ast::Term::TypeAnnotation {
                    term_left: _,
                    colon_pos: _,
                    term_right: Some(ast_field_ty),
                } => {
                    let field_ty_pos = ast_field_ty.pos.clone();
                    match self.translate_constant(*ast_field_ty, depth, &exports, logger) {
                        Ok(field_ty) => field_tys.push(field_ty),
                        Err(()) => {}
                    }
                }
                _ => {
                    logger.invalid_structure_field(ast_field.pos);
                }
            }
            if let Some(extra_tokens_pos) = extra_tokens_pos {
                logger.extra_tokens(extra_tokens_pos);
            }
        }
        for parameter_name in parameter_names {
            self.items.remove(&parameter_name);
        }
        Ok((ty, ir::StructureDefinition { field_tys }))
    }

    pub fn translate_constant_declaration(
        &mut self,
        ast::TermWithPos {
            term: ast_term,
            pos,
        }: ast::TermWithPos,
        constant_names: &mut Vec<String>,
        constant_tys: &mut Vec<Vec<ir::Constant>>,
        depth: &mut usize,
        exports: &[Context],
        logger: &mut log::Logger,
    ) -> Result<(log::Pos, String), ()> {
        match ast_term {
            ast::Term::Identifier(name) => Ok((pos, name)),
            ast::Term::TypeParameters {
                term_left: ast_function,
                parameters: ast_arguments,
            } => {
                let name_and_pos = self.translate_constant_declaration(
                    *ast_function,
                    constant_names,
                    constant_tys,
                    depth,
                    exports,
                    logger,
                );
                let mut parameter_tys = Vec::new();
                for (argument_index, ast_argument) in ast_arguments.into_iter().enumerate() {
                    let ast_argument = match ast_argument {
                        ast::ListElement::Empty { comma_pos } => {
                            logger.empty_ty_parameter(comma_pos);
                            continue;
                        }
                        ast::ListElement::NonEmpty(ast_argument) => ast_argument,
                    };
                    let ast::Term::TypeAnnotation {
                        term_left: ast_name_and_parameters,
                        colon_pos,
                        term_right: Some(ast_ret),
                    } = ast_argument.term
                    else {
                        logger.invalid_ty_parameter(ast_argument.pos);
                        continue;
                    };
                    let mut parameter_depth = *depth;
                    let mut names = Vec::new();
                    let mut tys = Vec::new();
                    let parameter = self.translate_constant_declaration(
                        *ast_name_and_parameters,
                        &mut names,
                        &mut tys,
                        &mut parameter_depth,
                        exports,
                        logger,
                    );
                    for name in names {
                        self.items.remove(&name);
                    }
                    let ret_ty = self
                        .translate_constant(*ast_ret, parameter_depth, exports, logger)
                        .unwrap();
                    let ty = tys.into_iter().rev().fold(ret_ty, |ret_ty, parameter_tys| {
                        ir::Constant::Product(parameter_tys, Box::new(ret_ty))
                    });
                    if let Ok((pos, name)) = parameter {
                        self.items.insert(
                            name.clone(),
                            (Some(pos), Item::Parameter(parameter_depth, argument_index)),
                        );
                        constant_names.push(name);
                    }
                    parameter_tys.push(ty);
                }
                constant_tys.push(parameter_tys);
                name_and_pos
            }
            _ => {
                logger.invalid_ty_parameter(pos);
                Err(())
            }
        }
    }

    pub fn translate_function_definition(
        &mut self,
        ast::FunctionDefinition {
            ty_parameters: ast_ty_parameters,
            parameters: ast_parameters,
            return_ty: ast_return_ty,
            body: ast_body,
            extra_tokens_pos,
        }: ast::FunctionDefinition,
        exports: &[Context],
        logger: &mut log::Logger,
    ) -> Option<(ir::FunctionTy, ir::FunctionDefinition)> {
        todo!();
    }

    pub fn translate_statement(
        &mut self,
        statement: ast::Statement,
        function_uses: &mut Vec<ir::FunctionUse>,
        calls: &mut Vec<ir::Call>,
        blocks: &mut Vec<ir::Block>,
        variables: &mut Variables,
        num_outer_variables: usize,
        break_index: Option<usize>,
        continue_index: Option<usize>,
        exports: &[Context],
        logger: &mut log::Logger,
    ) {
        match statement {
            ast::Statement::Term(term) => {
                self.translate_expression_or_import(
                    term,
                    false,
                    function_uses,
                    calls,
                    exports,
                    logger,
                );
            }
            ast::Statement::VariableDeclaration {
                keyword_var_pos,
                term,
            } => {
                let Some(ast_name) = term else {
                    logger.missing_variable_name(keyword_var_pos);
                    return;
                };
                match ast_name.term {
                    ast::Term::Identifier(name) => match self.items.entry(name.clone()) {
                        std::collections::hash_map::Entry::Occupied(mut entry) => {
                            if let (Some(pos), _) = entry.get() {
                                logger.duplicate_definition(ast_name.pos, pos.clone());
                            } else {
                                let item = variables.add(name);
                                entry.insert((Some(ast_name.pos), item));
                            }
                        }
                        std::collections::hash_map::Entry::Vacant(entry) => {
                            let item = variables.add(name);
                            entry.insert((Some(ast_name.pos), item));
                        }
                    },
                    _ => {
                        logger.invalid_variable_name(ast_name.pos);
                        return;
                    }
                }
            }
            ast::Statement::If {
                keyword_if_pos,
                extra_tokens_pos,
                condition: ast_condition,
                then_index,
                then_block: ast_then_block,
                else_index,
                else_block: ast_else_block,
                end_index,
            } => {
                let condition = if let Some(ast_condition) = ast_condition {
                    let condition_pos = ast_condition.pos.clone();
                    self.translate_expression_or_import(
                        ast_condition,
                        false,
                        function_uses,
                        calls,
                        exports,
                        logger,
                    )
                    .and_then(
                        |expression_or_import| match expression_or_import {
                            ExpressionOrImport::Expression(condition) => Ok(condition),
                            _ => {
                                logger.expected_expression(condition_pos);
                                Err(())
                            }
                        },
                    )
                } else {
                    logger.missing_if_condition(keyword_if_pos);
                    Err(())
                };
                assert_eq!(blocks.len(), then_index.get());
                blocks.push(ir::Block {
                    call_bound: calls.len(),
                    next: ir::Next::Branch(condition.unwrap(), then_index.get(), else_index.get()),
                });
                let num_variables = variables.num_alive();
                for ast::WithExtraTokens {
                    content: ast_statement,
                    extra_tokens_pos,
                } in ast_then_block.0
                {
                    if let Some(extra_tokens_pos) = extra_tokens_pos {
                        logger.extra_tokens(extra_tokens_pos);
                    }
                    self.translate_statement(
                        ast_statement,
                        function_uses,
                        calls,
                        blocks,
                        variables,
                        num_outer_variables,
                        break_index,
                        continue_index,
                        exports,
                        logger,
                    );
                }
                variables.free_and_remove(num_variables, function_uses, calls, self);
                assert_eq!(blocks.len(), else_index.get());
                blocks.push(ir::Block {
                    call_bound: calls.len(),
                    next: ir::Next::Jump(end_index.get()),
                });
                if let Some(ast::ElseBlock {
                    keyword_else_pos,
                    extra_tokens_pos,
                    block: ast_block,
                }) = ast_else_block
                {
                    for ast::WithExtraTokens {
                        content: ast_statement,
                        extra_tokens_pos,
                    } in ast_block.0
                    {
                        self.translate_statement(
                            ast_statement,
                            function_uses,
                            calls,
                            blocks,
                            variables,
                            num_outer_variables,
                            break_index,
                            continue_index,
                            exports,
                            logger,
                        );
                    }
                    variables.free_and_remove(num_variables, function_uses, calls, self);
                }
                assert_eq!(blocks.len(), end_index.get());
                blocks.push(ir::Block {
                    call_bound: calls.len(),
                    next: ir::Next::Jump(end_index.get()),
                });
            }
            ast::Statement::While {
                keyword_while_pos,
                condition_index,
                condition: ast_condition,
                extra_tokens_pos,
                do_index,
                do_block: ast_do_block,
                end_index,
            } => {
                assert_eq!(blocks.len(), condition_index.get());
                blocks.push(ir::Block {
                    call_bound: calls.len(),
                    next: ir::Next::Jump(condition_index.get()),
                });
                let condition = if let Some(ast_condition) = ast_condition {
                    let condition_pos = ast_condition.pos.clone();
                    self.translate_expression_or_import(
                        ast_condition,
                        false,
                        function_uses,
                        calls,
                        exports,
                        logger,
                    )
                    .and_then(|condition| match condition {
                        ExpressionOrImport::Expression(condition) => Ok(condition),
                        _ => {
                            logger.expected_expression(condition_pos);
                            Err(())
                        }
                    })
                } else {
                    logger.missing_while_condition(keyword_while_pos);
                    Err(())
                };
                assert_eq!(blocks.len(), do_index.get());
                blocks.push(ir::Block {
                    call_bound: calls.len(),
                    next: ir::Next::Branch(condition.unwrap(), do_index.get(), end_index.get()),
                });
                let num_variables = variables.num_alive();
                for ast::WithExtraTokens {
                    content: stmt,
                    extra_tokens_pos,
                } in ast_do_block.0
                {
                    if let Some(extra_tokens_pos) = extra_tokens_pos {
                        logger.extra_tokens(extra_tokens_pos);
                    }
                    self.translate_statement(
                        stmt,
                        function_uses,
                        calls,
                        blocks,
                        variables,
                        num_variables,
                        Some(end_index.get()),
                        Some(condition_index.get()),
                        exports,
                        logger,
                    );
                }
                variables.free_and_remove(num_variables, function_uses, calls, self);
                assert_eq!(blocks.len(), end_index.get());
                blocks.push(ir::Block {
                    call_bound: calls.len(),
                    next: ir::Next::Jump(condition_index.get()),
                });
            }
            ast::Statement::Break => {
                variables.free(num_outer_variables, function_uses, calls);
                blocks.push(ir::Block {
                    call_bound: calls.len(),
                    next: ir::Next::Jump(break_index.unwrap()),
                });
            }
            ast::Statement::Continue => {
                variables.free(num_outer_variables, function_uses, calls);
                blocks.push(ir::Block {
                    call_bound: calls.len(),
                    next: ir::Next::Jump(continue_index.unwrap()),
                });
            }
            ast::Statement::Return { value } => {
                let value = match value {
                    Some(value) => {
                        match self.translate_expression_or_import(
                            value,
                            false,
                            function_uses,
                            calls,
                            exports,
                            logger,
                        ) {
                            Ok(ExpressionOrImport::Expression(value)) => value,
                            _ => todo!(),
                        }
                    }
                    None => todo!(),
                };
                variables.free(0, function_uses, calls);
                blocks.push(ir::Block {
                    call_bound: calls.len(),
                    next: ir::Next::Return(value),
                });
            }
        }
    }

    fn translate_expression_or_import(
        &self,
        ast::TermWithPos {
            term: ast_term,
            pos,
        }: ast::TermWithPos,
        reference: bool,
        function_uses: &mut Vec<ir::FunctionUse>,
        calls: &mut Vec<ir::Call>,
        exports: &[Context],
        logger: &mut log::Logger,
    ) -> Result<ExpressionOrImport, ()> {
        match ast_term {
            ast::Term::NumericLiteral(value) => {
                if value.chars().all(|ch| matches!(ch, '0'..='9')) {
                    match value.parse() {
                        Ok(value) => Ok(ExpressionOrImport::Expression(ir::Expression::Integer(
                            value,
                        ))),
                        Err(err) => {
                            logger.cannot_parse_integer(pos, err);
                            Err(())
                        }
                    }
                } else {
                    match value.parse() {
                        Ok(value) => {
                            Ok(ExpressionOrImport::Expression(ir::Expression::Float(value)))
                        }
                        Err(err) => {
                            logger.cannot_parse_float(pos, err);
                            Err(())
                        }
                    }
                }
            }
            ast::Term::StringLiteral(ast_components) => {
                let mut components = Vec::new();
                for ast_component in ast_components {
                    match ast_component {
                        ast::StringLiteralComponent::String(value) => {
                            components.push(ir::Expression::String(value));
                        }
                        ast::StringLiteralComponent::PlaceHolder { format, value } => {
                            if let Some(value) = value {
                                if let Ok(ExpressionOrImport::Expression(value)) = self
                                    .translate_expression_or_import(
                                        value,
                                        false,
                                        function_uses,
                                        calls,
                                        exports,
                                        logger,
                                    )
                                {
                                    let function_use_index = function_uses.len();
                                    let call_index = calls.len();
                                    function_uses.push(ir::FunctionUse {
                                        candidates: vec![ir::Function::Method(
                                            ir::Class::ToString,
                                            0,
                                        )],
                                        used_by: Some(call_index),
                                    });
                                    calls.push(ir::Call {
                                        function: ir::Expression::FunctionUse(function_use_index),
                                        arguments: vec![value],
                                        used_by: None,
                                    });
                                    components.push(ir::Expression::Call(call_index));
                                }
                            }
                        }
                    }
                }
                Ok(ExpressionOrImport::Expression(
                    components
                        .into_iter()
                        .reduce(|left, right| {
                            let function_use_index = function_uses.len();
                            let call_index = calls.len();
                            function_uses.push(ir::FunctionUse {
                                candidates: vec![ir::Function::ConcatenateString],
                                used_by: Some(call_index),
                            });
                            calls.push(ir::Call {
                                function: ir::Expression::FunctionUse(function_use_index),
                                arguments: vec![left, right],
                                used_by: None,
                            });
                            ir::Expression::Call(call_index)
                        })
                        .unwrap_or(ir::Expression::String(String::new())),
                ))
            }
            ast::Term::Identity => {
                let function = ir::Expression::FunctionUse(function_uses.len());
                function_uses.push(ir::FunctionUse {
                    candidates: vec![ir::Function::Identity],
                    used_by: None,
                });
                Ok(ExpressionOrImport::Expression(function))
            }
            ast::Term::Identifier(name) => {
                self.get_expression_or_import(&name, reference, function_uses, calls, logger)
            }
            ast::Term::FieldByName { term_left, name } => {
                match self.translate_expression_or_import(
                    *term_left,
                    reference,
                    function_uses,
                    calls,
                    exports,
                    logger,
                )? {
                    ExpressionOrImport::Expression(expression) => {
                        todo!();
                    }
                    ExpressionOrImport::Import(index) => exports[index].get_expression_or_import(
                        &name,
                        reference,
                        function_uses,
                        calls,
                        logger,
                    ),
                }
            }
            ast::Term::FunctionCall {
                function: ast_function,
                arguments: ast_arguments,
            } => {
                let function = self.translate_expression_or_import(
                    *ast_function,
                    false,
                    function_uses,
                    calls,
                    exports,
                    logger,
                );
                let mut arguments = Vec::new();
                let mut call_indices = Vec::new();
                let mut function_use_indices = Vec::new();
                if let Ok(ExpressionOrImport::Expression(ref function)) = function {
                    if let &ir::Expression::FunctionUse(index) = function {
                        function_use_indices.push(index);
                    } else if let &ir::Expression::Call(index) = function {
                        call_indices.push(index);
                    }
                }
                for ast_argument in ast_arguments {
                    let ast_argument = match ast_argument {
                        ast::ListElement::Empty { comma_pos } => {
                            logger.empty_argument(comma_pos);
                            continue;
                        }
                        ast::ListElement::NonEmpty(ast_argument) => ast_argument,
                    };
                    let argument_pos = ast_argument.pos.clone();
                    let Ok(argument) = self.translate_expression_or_import(
                        ast_argument,
                        false,
                        function_uses,
                        calls,
                        exports,
                        logger,
                    ) else {
                        continue;
                    };
                    match argument {
                        ExpressionOrImport::Expression(argument) => {
                            if let ir::Expression::FunctionUse(index) = argument {
                                function_use_indices.push(index);
                            } else if let ir::Expression::Call(index) = argument {
                                call_indices.push(index);
                            }
                            arguments.push(argument);
                        }
                        _ => {
                            logger.expected_expression(argument_pos);
                        }
                    }
                }
                match function {
                    Ok(ExpressionOrImport::Expression(function)) => {
                        let call_index = calls.len();
                        for &index in &call_indices {
                            calls[index].used_by = Some(call_index);
                        }
                        for &index in &function_use_indices {
                            function_uses[index].used_by = Some(call_index);
                        }
                        calls.push(ir::Call {
                            function,
                            arguments,
                            used_by: None,
                        });
                        Ok(ExpressionOrImport::Expression(ir::Expression::Call(
                            call_index,
                        )))
                    }
                    _ => todo!(),
                }
            }
            ast::Term::Assignment {
                left_hand_side: ast_left_hand_side,
                operator_name,
                operator_pos,
                right_hand_side: ast_right_hand_side,
            } => {
                let left_hand_side = match ast_left_hand_side {
                    Some(ast_left_hand_side) => {
                        let left_hand_side_pos = ast_left_hand_side.pos.clone();
                        self.translate_expression_or_import(
                            *ast_left_hand_side,
                            true,
                            function_uses,
                            calls,
                            exports,
                            logger,
                        )
                        .and_then(|term| match term {
                            ExpressionOrImport::Expression(expr) => Ok(expr),
                            _ => {
                                logger.expected_expression(left_hand_side_pos);
                                Err(())
                            }
                        })
                    }
                    None => {
                        logger.empty_left_operand(operator_pos.clone());
                        Err(())
                    }
                };
                let right_hand_side = match ast_right_hand_side {
                    Some(ast_right_hand_side) => {
                        let right_hand_side_pos = ast_right_hand_side.pos.clone();
                        self.translate_expression_or_import(
                            *ast_right_hand_side,
                            false,
                            function_uses,
                            calls,
                            exports,
                            logger,
                        )
                        .and_then(|term| match term {
                            ExpressionOrImport::Expression(expr) => Ok(expr),
                            _ => {
                                logger.expected_expression(right_hand_side_pos);
                                Err(())
                            }
                        })
                    }
                    None => {
                        logger.empty_right_operand(operator_pos);
                        Err(())
                    }
                };
                todo!();
                //let candidates = self
                //    .methods
                //    .get(operator_name)
                //    .cloned()
                //    .unwrap_or_else(Vec::new);
                //let function_use_index = function_uses.len();
                //let call_index = calls.len();
                //function_uses.push(ir::FunctionUse {
                //    candidates,
                //    used_by: Some(call_index),
                //});
                //calls.push(ir::Call {
                //    function: ir::Expression::FunctionUse(function_use_index),
                //    arguments: vec![left_hand_side?, right_hand_side?],
                //    used_by: None,
                //});
                //Ok(ExpressionOrImport::Expression(ir::Expression::Call(
                //    call_index,
                //)))
            }
            ast::Term::BinaryOperation {
                left_operand: ast_left_operand,
                operator_class,
                operator_index,
                operator_pos,
                right_operand: ast_right_operand,
            } => {
                let left_operand = match ast_left_operand {
                    Some(ast_left_operand) => {
                        let left_operand_pos = ast_left_operand.pos.clone();
                        self.translate_expression_or_import(
                            *ast_left_operand,
                            true,
                            function_uses,
                            calls,
                            exports,
                            logger,
                        )
                        .and_then(|term| match term {
                            ExpressionOrImport::Expression(expr) => Ok(expr),
                            _ => {
                                logger.expected_expression(left_operand_pos);
                                Err(())
                            }
                        })
                    }
                    None => {
                        logger.empty_left_operand(operator_pos.clone());
                        Err(())
                    }
                };
                let right_operand = match ast_right_operand {
                    Some(ast_right_operand) => {
                        let right_operand_pos = ast_right_operand.pos.clone();
                        self.translate_expression_or_import(
                            *ast_right_operand,
                            false,
                            function_uses,
                            calls,
                            exports,
                            logger,
                        )
                        .and_then(|term| match term {
                            ExpressionOrImport::Expression(expr) => Ok(expr),
                            _ => {
                                logger.expected_expression(right_operand_pos);
                                Err(())
                            }
                        })
                    }
                    None => {
                        logger.empty_right_operand(operator_pos);
                        Err(())
                    }
                };
                let candidates: Vec<_> = self
                    .submodules
                    .iter()
                    .map(|&module_index| {
                        exports[module_index]
                            .get_instances(&operator_class)
                            .iter()
                            .cloned()
                    })
                    .flatten()
                    .collect();
                let function_use_index = function_uses.len();
                let call_index = calls.len();
                function_uses.push(ir::FunctionUse {
                    candidates,
                    used_by: Some(call_index),
                });
                calls.push(ir::Call {
                    function: ir::Expression::FunctionUse(function_use_index),
                    arguments: vec![left_operand?, right_operand?],
                    used_by: None,
                });
                Ok(ExpressionOrImport::Expression(ir::Expression::Call(
                    call_index,
                )))
            }
            _ => todo!(),
        }
    }

    fn get_expression_or_import(
        &self,
        name: &str,
        reference: bool,
        function_uses: &mut Vec<ir::FunctionUse>,
        calls: &mut Vec<ir::Call>,
        logger: &mut log::Logger,
    ) -> Result<ExpressionOrImport, ()> {
        match self.items.get(name) {
            Some((_, Item::Function(candidates))) => {
                if reference {
                    todo!();
                }
                let function = ir::Expression::FunctionUse(function_uses.len());
                function_uses.push(ir::FunctionUse {
                    candidates: candidates.clone(),
                    used_by: None,
                });
                Ok(ExpressionOrImport::Expression(function))
            }
            Some(&(_, Item::Variable(storage, index))) => {
                let variable = ir::Expression::Variable(storage, index);
                if reference {
                    Ok(ExpressionOrImport::Expression(variable))
                } else {
                    let function_use_index = function_uses.len();
                    let call_index = calls.len();
                    function_uses.push(ir::FunctionUse {
                        candidates: vec![ir::Function::Dereference],
                        used_by: Some(call_index),
                    });
                    calls.push(ir::Call {
                        function: ir::Expression::FunctionUse(function_use_index),
                        arguments: vec![variable],
                        used_by: None,
                    });
                    Ok(ExpressionOrImport::Expression(ir::Expression::Call(
                        call_index,
                    )))
                }
            }
            _ => todo!(),
        }
    }

    fn translate_ty(
        &self,
        ast::TermWithPos {
            term: ast_term,
            pos,
        }: ast::TermWithPos,
        exports: &[Context],
        logger: &mut log::Logger,
    ) -> Result<ir::Constant, ()> {
        match ast_term {
            ast::Term::IntegerTy => return Ok(ir::Constant::Integer),
            ast::Term::FloatTy => return Ok(ir::Constant::Float),
            ast::Term::Identifier(name) => self.get_ty(&name, logger),
            ast::Term::FieldByName { term_left, name } => {
                let index = self.translate_import(*term_left, exports, logger)?;
                exports[index].get_ty(&name, logger)
            }
            _ => todo!(),
        }
    }

    fn get_ty(&self, name: &str, logger: &mut log::Logger) -> Result<ir::Constant, ()> {
        match self.items.get(name) {
            Some((_, Item::Constant(ty))) => Ok(ty.clone()),
            _ => Err(()),
        }
    }

    fn translate_import(
        &self,
        ast::TermWithPos {
            term: ast_term,
            pos,
        }: ast::TermWithPos,
        exports: &[Context],
        logger: &mut log::Logger,
    ) -> Result<usize, ()> {
        match ast_term {
            ast::Term::Identifier(name) => self.get_import(&name, logger),
            ast::Term::FieldByName { term_left, name } => {
                let index = self.translate_import(*term_left, exports, logger)?;
                exports[index].get_import(&name, logger)
            }
            _ => todo!(),
        }
    }

    fn get_import(&self, name: &str, logger: &mut log::Logger) -> Result<usize, ()> {
        match self.items.get(name) {
            Some(&(_, Item::Import(index))) => Ok(index),
            _ => Err(()),
        }
    }

    pub fn translate_constant(
        &mut self,
        ast::TermWithPos {
            term: ast_term,
            pos,
        }: ast::TermWithPos,
        depth: usize,
        exports: &[Context],
        logger: &mut log::Logger,
    ) -> Result<ir::Constant, ()> {
        match ast_term {
            ast::Term::Ty => Ok(ir::Constant::Ty),
            ast::Term::Identifier(name) => self.get_constant(&name, depth, logger),
            ast::Term::TypeParameters {
                term_left: ast_constructor,
                parameters: ast_parameters,
            } => {
                let constructor = self.translate_constant(*ast_constructor, depth, exports, logger);
                let mut parameters = Vec::new();
                for ast_parameter in ast_parameters {
                    let ast_parameter = match ast_parameter {
                        ast::ListElement::Empty { comma_pos } => todo!(),
                        ast::ListElement::NonEmpty(parameter) => parameter,
                    };
                    match self.translate_constant(ast_parameter, depth, exports, logger) {
                        Ok(parameter) => parameters.push(parameter),
                        Err(_) => todo!(),
                    }
                }
                Ok(ir::Constant::Application(
                    Box::new(constructor?),
                    parameters,
                ))
            }
            ast::Term::FieldByName { term_left, name } => {
                let index = self.translate_import(*term_left, exports, logger)?;
                exports[index].get_constant(&name, depth, logger)
            }
            _ => todo!(),
        }
    }

    fn get_constant(
        &self,
        name: &str,
        depth: usize,
        logger: &mut log::Logger,
    ) -> Result<ir::Constant, ()> {
        match self.items.get(name) {
            Some((_, Item::Constant(constant))) => Ok(constant.clone()),
            Some((_, Item::Parameter(d, i))) => Ok(ir::Constant::Parameter(depth - d, *i)),
            _ => Err(()),
        }
    }

    fn get_instances(&self, class: &Class) -> &[ir::Function] {
        match *class {
            Class::Add => &self.add_instances,
            Class::Eq => &self.eq_instances,
            Class::UserDefined(index) => &self.instances[index],
            _ => todo!(),
        }
    }
}

enum ExpressionOrImport {
    Expression(ir::Expression),
    Import(usize),
}
