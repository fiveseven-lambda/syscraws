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

/*!
 * Defines [`Context`] used to translate AST into IR.
 */

use std::collections::HashMap;

use super::{BlockBuilder, Item, Variables, ast};
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
    /**
     * A mapping from method names to the list of functions associated with them.
     * Methods can be accessed by their name alone,
     * even from files that import the defining file.
     */
    pub methods: HashMap<String, Vec<ir::Function>>,
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
        files: &[log::File],
        logger: &mut log::Logger,
    ) -> (ir::TyKind, ir::Structure) {
        let mut ty_parameters_name = Vec::new();
        let kind = if let Some(ast_ty_parameters) = ast_ty_parameters {
            for ast_ty_parameter in ast_ty_parameters {
                let ast_ty_parameter = match ast_ty_parameter {
                    ast::ListElement::Empty { comma_pos } => {
                        logger.empty_ty_parameter(comma_pos, &files);
                        continue;
                    }
                    ast::ListElement::NonEmpty(ast_ty_parameter) => ast_ty_parameter,
                };
                match ast_ty_parameter.term {
                    ast::Term::Identifier(name) => match self.items.entry(name.clone()) {
                        std::collections::hash_map::Entry::Occupied(mut entry) => {
                            if let (Some(pos), _) = entry.get() {
                                logger.duplicate_definition(
                                    ast_ty_parameter.pos,
                                    pos.clone(),
                                    &files,
                                );
                            } else {
                                entry.insert((
                                    Some(ast_ty_parameter.pos),
                                    Item::Ty(ir::Ty::Parameter(ty_parameters_name.len())),
                                ));
                                ty_parameters_name.push(name);
                            }
                        }
                        std::collections::hash_map::Entry::Vacant(entry) => {
                            entry.insert((
                                Some(ast_ty_parameter.pos),
                                Item::Ty(ir::Ty::Parameter(ty_parameters_name.len())),
                            ));
                            ty_parameters_name.push(name);
                        }
                    },
                    _ => {
                        logger.invalid_ty_parameter(ast_ty_parameter.pos, &files);
                    }
                }
            }
            ir::TyKind::Abstraction {
                parameters: (0..ty_parameters_name.len()).fold(ir::TyListKind::Nil, |tail, _| {
                    ir::TyListKind::Cons(Box::new(ir::TyKind::Ty), Box::new(tail))
                }),
                ret: Box::new(ir::TyKind::Ty),
            }
        } else {
            ir::TyKind::Ty
        };
        if let Some(extra_tokens_pos) = extra_tokens_pos {
            logger.extra_tokens(extra_tokens_pos, &files);
        }
        let mut fields_ty = Vec::new();
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
                    match self.translate_term(*ast_field_ty, false, &exports, &files, logger) {
                        Ok(Term::Ty(field_ty)) => fields_ty.push(field_ty),
                        Ok(_) => logger.expected_ty(field_ty_pos, &files),
                        Err(()) => {}
                    }
                }
                _ => {
                    logger.invalid_structure_field(ast_field.pos, &files);
                }
            }
            if let Some(extra_tokens_pos) = extra_tokens_pos {
                logger.extra_tokens(extra_tokens_pos, &files);
            }
        }
        for ty_parameter_name in &ty_parameters_name {
            self.items.remove(ty_parameter_name);
        }
        (
            kind,
            ir::Structure {
                num_ty_parameters: ty_parameters_name.len(),
                fields_ty,
            },
        )
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
        files: &[log::File],
        logger: &mut log::Logger,
    ) -> Option<(ir::FunctionTy, ir::FunctionDefinition)> {
        let mut ty_parameters_name = Vec::new();
        if let Some(ast_ty_parameters) = ast_ty_parameters {
            for ast_ty_parameter in ast_ty_parameters {
                let ast_ty_parameter = match ast_ty_parameter {
                    ast::ListElement::Empty { comma_pos } => {
                        logger.empty_ty_parameter(comma_pos, &files);
                        continue;
                    }
                    ast::ListElement::NonEmpty(ast_ty_parameter) => ast_ty_parameter,
                };
                if let ast::Term::Identifier(name) = ast_ty_parameter.term {
                    match self.items.entry(name.clone()) {
                        std::collections::hash_map::Entry::Occupied(mut entry) => {
                            if let (Some(pos), _) = entry.get() {
                                logger.duplicate_definition(
                                    ast_ty_parameter.pos,
                                    pos.clone(),
                                    &files,
                                );
                            } else {
                                entry.insert((
                                    Some(ast_ty_parameter.pos),
                                    Item::Ty(ir::Ty::Parameter(ty_parameters_name.len())),
                                ));
                                ty_parameters_name.push(name);
                            }
                        }
                        std::collections::hash_map::Entry::Vacant(entry) => {
                            entry.insert((
                                Some(ast_ty_parameter.pos),
                                Item::Ty(ir::Ty::Parameter(ty_parameters_name.len())),
                            ));
                            ty_parameters_name.push(name);
                        }
                    }
                } else {
                    logger.invalid_ty_parameter(ast_ty_parameter.pos, &files);
                }
            }
        }
        let mut builder = BlockBuilder::new();
        let mut local_variables = Variables::new(ir::Storage::Local);
        let mut parameters_ty = Vec::new();
        match ast_parameters {
            Ok(ast_parameters) => {
                for ast_parameter in ast_parameters {
                    let ast_parameter = match ast_parameter {
                        ast::ListElement::Empty { comma_pos } => {
                            logger.empty_parameter(comma_pos, &files);
                            continue;
                        }
                        ast::ListElement::NonEmpty(ast_parameter) => ast_parameter,
                    };
                    match ast_parameter.term {
                        ast::Term::TypeAnnotation {
                            term_left: ast_parameter_name,
                            colon_pos,
                            term_right: ast_parameter_ty,
                        } => {
                            match ast_parameter_name.term {
                                ast::Term::Identifier(name) => {
                                    match self.items.entry(name.clone()) {
                                        std::collections::hash_map::Entry::Occupied(mut entry) => {
                                            if let (Some(pos), _) = entry.get() {
                                                logger.duplicate_definition(
                                                    ast_parameter.pos,
                                                    pos.clone(),
                                                    &files,
                                                );
                                            } else {
                                                let item = local_variables.add(name);
                                                entry.insert((Some(ast_parameter_name.pos), item));
                                            }
                                        }
                                        std::collections::hash_map::Entry::Vacant(entry) => {
                                            let item = local_variables.add(name);
                                            entry.insert((Some(ast_parameter_name.pos), item));
                                        }
                                    }
                                }
                                _ => {
                                    logger.invalid_parameter(ast_parameter.pos, &files);
                                }
                            }
                            if let Some(ast_parameter_ty) = ast_parameter_ty {
                                let parameter_pos = ast_parameter_ty.pos.clone();
                                match self.translate_term(
                                    *ast_parameter_ty,
                                    false,
                                    &exports,
                                    &files,
                                    logger,
                                ) {
                                    Ok(Term::Ty(parameter_ty)) => parameters_ty.push(parameter_ty),
                                    Ok(_) => logger.expected_ty(parameter_pos, &files),
                                    Err(()) => {}
                                }
                            } else {
                                logger.missing_ty(colon_pos, &files);
                            }
                        }
                        _ => {
                            logger.invalid_parameter(ast_parameter.pos, &files);
                        }
                    }
                }
            }
            Err(signature_pos) => {
                logger.missing_parameter_list(signature_pos, &files);
            }
        }
        let return_ty = if let Some(ast_return_ty) = ast_return_ty {
            if let Some(ast_return_ty) = ast_return_ty.ty {
                let return_ty_pos = ast_return_ty.pos.clone();
                match self.translate_term(ast_return_ty, false, &exports, &files, logger) {
                    Ok(Term::Ty(return_ty)) => return_ty,
                    Ok(_) => {
                        logger.expected_ty(return_ty_pos, &files);
                        return None;
                    }
                    Err(()) => return None,
                }
            } else {
                logger.missing_ty(ast_return_ty.colon_pos, &files);
                return None;
            }
        } else {
            ir::Ty::Application {
                constructor: Box::new(ir::Ty::Constructor(ir::TyConstructor::Tuple)),
                arguments: vec![],
            }
        };
        if let Some(extra_tokens_pos) = extra_tokens_pos {
            logger.extra_tokens(extra_tokens_pos, &files);
        }
        for ast::WithExtraTokens {
            content: ast_statement,
            extra_tokens_pos,
        } in ast_body
        {
            if let Some(extra_tokens_pos) = extra_tokens_pos {
                logger.extra_tokens(extra_tokens_pos, &files);
            }
            self.translate_statement(
                ast_statement,
                &mut builder,
                &mut local_variables,
                0,
                &exports,
                &files,
                logger,
            );
        }
        local_variables.free_and_remove(0, &mut builder, self);
        let body = builder.finish();
        for ty_parameter_name in &ty_parameters_name {
            self.items.remove(ty_parameter_name);
        }
        Some((
            ir::FunctionTy {
                num_ty_parameters: ty_parameters_name.len(),
                parameters_ty,
                return_ty,
            },
            ir::FunctionDefinition {
                num_local_variables: local_variables.num_total(),
                body,
            },
        ))
    }

    pub fn translate_statement(
        &mut self,
        statement: ast::Statement,
        builder: &mut BlockBuilder,
        variables: &mut Variables,
        num_outer_variables: usize,
        exports: &[Context],
        files: &[log::File],
        logger: &mut log::Logger,
    ) {
        match statement {
            ast::Statement::Term(term) => {
                let term_pos = term.pos.clone();
                let Ok(term) = self.translate_term(term, false, exports, files, logger) else {
                    return;
                };
                match term {
                    Term::Expression(expr) => builder.add_expression(expr),
                    _ => logger.expected_expression(term_pos, files),
                }
            }
            ast::Statement::VariableDeclaration {
                keyword_var_pos,
                term,
            } => {
                let Some(ast_name) = term else {
                    logger.missing_variable_name(keyword_var_pos, files);
                    return;
                };
                match ast_name.term {
                    ast::Term::Identifier(name) => match self.items.entry(name.clone()) {
                        std::collections::hash_map::Entry::Occupied(mut entry) => {
                            if let (Some(pos), _) = entry.get() {
                                logger.duplicate_definition(ast_name.pos, pos.clone(), files);
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
                        logger.invalid_variable_name(ast_name.pos, files);
                        return;
                    }
                }
            }
            ast::Statement::If {
                keyword_if_pos,
                extra_tokens_pos,
                condition: ast_condition,
                then_block: ast_then_block,
                else_block: ast_else_block,
            } => {
                let condition = if let Some(ast_condition) = ast_condition {
                    let condition_pos = ast_condition.pos.clone();
                    self.translate_term(ast_condition, false, exports, files, logger)
                        .and_then(|term| match term {
                            Term::Expression(condition) => Ok(condition),
                            _ => {
                                logger.expected_expression(condition_pos, files);
                                Err(())
                            }
                        })
                } else {
                    logger.missing_if_condition(keyword_if_pos, files);
                    Err(())
                };
                let mut then_builder = BlockBuilder::new();
                let num_variables = variables.num_alive();
                for ast::WithExtraTokens {
                    content: stmt,
                    extra_tokens_pos,
                } in ast_then_block
                {
                    if let Some(extra_tokens_pos) = extra_tokens_pos {
                        logger.extra_tokens(extra_tokens_pos, files);
                    }
                    self.translate_statement(
                        stmt,
                        &mut then_builder,
                        variables,
                        num_outer_variables,
                        exports,
                        files,
                        logger,
                    );
                }
                variables.free_and_remove(num_variables, &mut then_builder, self);
                let then_block = then_builder.finish();
                let mut else_builder = BlockBuilder::new();
                if let Some(ast::ElseBlock {
                    keyword_else_pos,
                    extra_tokens_pos,
                    block: ast_block,
                }) = ast_else_block
                {
                    for ast::WithExtraTokens {
                        content: stmt,
                        extra_tokens_pos,
                    } in ast_block
                    {
                        self.translate_statement(
                            stmt,
                            &mut else_builder,
                            variables,
                            num_outer_variables,
                            exports,
                            files,
                            logger,
                        );
                    }
                    variables.free_and_remove(num_variables, &mut else_builder, self);
                }
                let else_block = else_builder.finish();
                builder.add_if_statement(condition.unwrap(), then_block, else_block);
            }
            ast::Statement::While {
                keyword_while_pos,
                condition: ast_condition,
                extra_tokens_pos,
                do_block: ast_do_block,
            } => {
                let condition = if let Some(ast_condition) = ast_condition {
                    let condition_pos = ast_condition.pos.clone();
                    self.translate_term(ast_condition, false, exports, files, logger)
                        .and_then(|term| match term {
                            Term::Expression(condition) => Ok(condition),
                            _ => {
                                logger.expected_expression(condition_pos, files);
                                Err(())
                            }
                        })
                } else {
                    logger.missing_while_condition(keyword_while_pos, files);
                    Err(())
                };
                let mut do_builder = BlockBuilder::new();
                let num_variables = variables.num_alive();
                for ast::WithExtraTokens {
                    content: stmt,
                    extra_tokens_pos,
                } in ast_do_block
                {
                    if let Some(extra_tokens_pos) = extra_tokens_pos {
                        logger.extra_tokens(extra_tokens_pos, files);
                    }
                    self.translate_statement(
                        stmt,
                        &mut do_builder,
                        variables,
                        num_variables,
                        exports,
                        files,
                        logger,
                    );
                }
                variables.free_and_remove(num_variables, &mut do_builder, self);
                let do_block = do_builder.finish();
                builder.add_while_statement(condition.unwrap(), do_block);
            }
            ast::Statement::Break => {
                variables.free(num_outer_variables, builder);
                builder.add_break();
            }
            ast::Statement::Continue => {
                variables.free(num_outer_variables, builder);
                builder.add_continue();
            }
            ast::Statement::Return { value } => {
                let value = match value {
                    Some(value) => {
                        match self.translate_term(value, false, exports, files, logger) {
                            Ok(Term::Expression(value)) => value,
                            _ => todo!(),
                        }
                    }
                    None => todo!(),
                };
                variables.free(0, builder);
                builder.add_return(value);
            }
        }
    }

    fn translate_term(
        &self,
        ast::TermWithPos {
            term: ast_term,
            pos,
        }: ast::TermWithPos,
        reference: bool,
        exports: &[Context],
        files: &[log::File],
        logger: &mut log::Logger,
    ) -> Result<Term, ()> {
        match ast_term {
            ast::Term::IntegerTy => {
                return Ok(Term::Ty(ir::Ty::Constructor(ir::TyConstructor::Integer)));
            }
            ast::Term::FloatTy => {
                return Ok(Term::Ty(ir::Ty::Constructor(ir::TyConstructor::Float)));
            }
            ast::Term::NumericLiteral(value) => {
                if value.chars().all(|ch| matches!(ch, '0'..='9')) {
                    match value.parse() {
                        Ok(value) => {
                            if reference {
                                logger.expected_lvalue(pos, files);
                                Err(())
                            } else {
                                Ok(Term::Expression(ir::Expression::Integer(value)))
                            }
                        }
                        Err(err) => {
                            logger.cannot_parse_integer(pos, err, files);
                            Err(())
                        }
                    }
                } else {
                    match value.parse() {
                        Ok(value) => {
                            if reference {
                                logger.expected_lvalue(pos, files);
                                Err(())
                            } else {
                                Ok(Term::Expression(ir::Expression::Float(value)))
                            }
                        }
                        Err(err) => {
                            logger.cannot_parse_float(pos, err, files);
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
                                if let Ok(Term::Expression(expression)) =
                                    self.translate_term(value, reference, exports, files, logger)
                                {
                                    components.push(ir::Expression::Function {
                                        candidates: vec![ir::Function::IntegerToString],
                                        calls: vec![ir::Call {
                                            arguments: vec![expression],
                                        }],
                                    });
                                }
                            }
                        }
                    }
                }
                Ok(Term::Expression(
                    components
                        .into_iter()
                        .reduce(|left, right| ir::Expression::Function {
                            candidates: vec![ir::Function::ConcatenateString],
                            calls: vec![ir::Call {
                                arguments: vec![left, right],
                            }],
                        })
                        .unwrap_or(ir::Expression::String(String::new())),
                ))
            }
            ast::Term::Identity => Ok(Term::Expression(ir::Expression::Function {
                candidates: vec![ir::Function::Identity],
                calls: vec![],
            })),
            ast::Term::Identifier(name) => match self.items.get(&name) {
                Some((_, named_item)) => {
                    self.translate_named_item(named_item, reference, pos, files, logger)
                }
                None => {
                    logger.undefined_variable(pos, files);
                    Err(())
                }
            },
            ast::Term::FieldByName {
                term_left: ast_term_left,
                name,
            } => match self.translate_term(*ast_term_left, reference, exports, files, logger)? {
                Term::Expression(expr) => Ok(Term::Expression(ir::Expression::Function {
                    candidates: vec![],
                    calls: vec![ir::Call {
                        arguments: vec![expr],
                    }],
                })),
                Term::Ty(_) => {
                    todo!();
                }
                Term::Import(file_index) => match exports[file_index].items.get(&name) {
                    Some((_, named_item)) => {
                        self.translate_named_item(named_item, reference, pos, files, logger)
                    }
                    None => {
                        logger.undefined_item(&name, pos, file_index, files);
                        Err(())
                    }
                },
            },
            ast::Term::FunctionCall {
                function: ast_function,
                arguments: ast_arguments,
            } => {
                let function_pos = ast_function.pos.clone();
                let function = self
                    .translate_term(*ast_function, false, exports, files, logger)
                    .and_then(|term| match term {
                        Term::Expression(function) => match function {
                            ir::Expression::Function { candidates, calls } => {
                                Ok((candidates, calls))
                            }
                            _ => {
                                logger.expected_function(function_pos, files);
                                Err(())
                            }
                        },
                        _ => {
                            logger.expected_expression(function_pos, files);
                            Err(())
                        }
                    });
                let mut arguments = Vec::new();
                for ast_argument in ast_arguments {
                    let ast_argument = match ast_argument {
                        ast::ListElement::Empty { comma_pos } => {
                            logger.empty_argument(comma_pos, files);
                            continue;
                        }
                        ast::ListElement::NonEmpty(ast_argument) => ast_argument,
                    };
                    let argument_pos = ast_argument.pos.clone();
                    let Ok(argument) =
                        self.translate_term(ast_argument, false, exports, files, logger)
                    else {
                        continue;
                    };
                    match argument {
                        Term::Expression(argument) => {
                            arguments.push(argument);
                        }
                        _ => {
                            logger.expected_expression(argument_pos, files);
                        }
                    }
                }
                let (candidates, mut calls) = function?;
                calls.push(ir::Call { arguments });
                Ok(Term::Expression(ir::Expression::Function {
                    candidates,
                    calls,
                }))
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
                        self.translate_term(*ast_left_hand_side, true, exports, files, logger)
                            .and_then(|term| match term {
                                Term::Expression(expr) => Ok(expr),
                                _ => {
                                    logger.expected_expression(left_hand_side_pos, files);
                                    Err(())
                                }
                            })
                    }
                    None => {
                        logger.empty_left_operand(operator_pos.clone(), files);
                        Err(())
                    }
                };
                let right_hand_side = match ast_right_hand_side {
                    Some(ast_right_hand_side) => {
                        let right_hand_side_pos = ast_right_hand_side.pos.clone();
                        self.translate_term(*ast_right_hand_side, false, exports, files, logger)
                            .and_then(|term| match term {
                                Term::Expression(expr) => Ok(expr),
                                _ => {
                                    logger.expected_expression(right_hand_side_pos, files);
                                    Err(())
                                }
                            })
                    }
                    None => {
                        logger.empty_right_operand(operator_pos, files);
                        Err(())
                    }
                };
                let candidates = self
                    .methods
                    .get(operator_name)
                    .cloned()
                    .unwrap_or_else(Vec::new);
                Ok(Term::Expression(ir::Expression::Function {
                    candidates,
                    calls: vec![ir::Call {
                        arguments: vec![left_hand_side?, right_hand_side?],
                    }],
                }))
            }
            ast::Term::TypeParameters {
                term_left: ast_term_left,
                parameters: ast_parameters,
            } => {
                let term_left_pos = ast_term_left.pos.clone();
                let term_left = self
                    .translate_term(*ast_term_left, false, exports, files, logger)
                    .and_then(|term_left| match term_left {
                        Term::Ty(ty) => Ok(ty),
                        _ => {
                            logger.expected_ty(term_left_pos, files);
                            Err(())
                        }
                    });
                let mut parameters = Vec::new();
                for ast_parameter in ast_parameters {
                    let ast_parameter = match ast_parameter {
                        ast::ListElement::Empty { comma_pos } => {
                            logger.empty_ty_parameter(comma_pos, files);
                            continue;
                        }
                        ast::ListElement::NonEmpty(ast_parameter) => ast_parameter,
                    };
                    let parameter_pos = ast_parameter.pos.clone();
                    let parameter = self
                        .translate_term(ast_parameter, false, exports, files, logger)
                        .and_then(|term_left| match term_left {
                            Term::Ty(ty) => Ok(ty),
                            _ => {
                                logger.expected_ty(parameter_pos, files);
                                Err(())
                            }
                        });
                    if logger.num_errors == 0 {
                        parameters.push(parameter.unwrap());
                    }
                }
                return if logger.num_errors == 0 {
                    Ok(Term::Ty(ir::Ty::Application {
                        constructor: Box::new(term_left?),
                        arguments: parameters,
                    }))
                } else {
                    Err(())
                };
            }
            _ => todo!(),
        }
    }

    fn translate_named_item(
        &self,
        named_item: &Item,
        reference: bool,
        pos: log::Pos,
        files: &[log::File],
        logger: &mut log::Logger,
    ) -> Result<Term, ()> {
        match *named_item {
            Item::Function(ref candidates) => {
                if reference {
                    logger.expected_lvalue(pos, files);
                    Err(())
                } else {
                    Ok(Term::Expression(ir::Expression::Function {
                        candidates: candidates.clone(),
                        calls: vec![],
                    }))
                }
            }
            Item::Variable(storage, index) => {
                let expr = ir::Expression::Variable(storage, index);
                Ok(Term::Expression(if reference {
                    expr
                } else {
                    ir::Expression::Function {
                        candidates: vec![ir::Function::Dereference],
                        calls: vec![ir::Call {
                            arguments: vec![expr],
                        }],
                    }
                }))
            }
            Item::Import(index) => Ok(Term::Import(index)),
            Item::Ty(ref ty) => Ok(Term::Ty(ty.clone())),
        }
    }
}

enum Term {
    Import(usize),
    Ty(ir::Ty),
    Expression(ir::Expression),
}
