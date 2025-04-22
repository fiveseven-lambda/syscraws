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
 * Translates ASTs into intermediate representations in backend.
 */

use std::collections::HashMap;

use super::ast;
use crate::{backend, log};

pub struct Block {
    block: backend::Block,
    expressions: Vec<backend::Expression>,
}

impl Block {
    pub fn new() -> Block {
        Block {
            block: backend::Block {
                statements: Vec::new(),
                size: 0,
            },
            expressions: Vec::new(),
        }
    }
    pub fn add_expression(&mut self, expression: backend::Expression) {
        self.expressions.push(expression);
    }
    pub fn get(mut self) -> backend::Block {
        if !self.expressions.is_empty() {
            self.block
                .statements
                .push(backend::Statement::Expr(self.expressions));
            self.block.size += 1;
        }
        self.block
    }
}

pub struct Variables {
    pub num: usize,
    pub name_and_indices: Vec<(String, usize)>,
}

pub struct Context {
    pub items: HashMap<String, (log::Pos, Item)>,
    pub methods: HashMap<String, Vec<backend::Function>>,
}

#[derive(Clone)]
pub enum Item {
    Import(usize),
    Ty(backend::TyBuilder),
    Function(Vec<backend::Function>),
    Variable(backend::LocalOrGlobal, usize),
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
    ) -> (backend::TyKind, backend::Structure) {
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
                        std::collections::hash_map::Entry::Occupied(entry) => {
                            logger.duplicate_definition(
                                ast_ty_parameter.pos,
                                entry.get().0.clone(),
                                &files,
                            );
                        }
                        std::collections::hash_map::Entry::Vacant(entry) => {
                            entry.insert((
                                ast_ty_parameter.pos,
                                Item::Ty(backend::TyBuilder::Parameter(ty_parameters_name.len())),
                            ));
                            ty_parameters_name.push(name);
                        }
                    },
                    _ => {
                        logger.invalid_ty_parameter(ast_ty_parameter.pos, &files);
                    }
                }
            }
            backend::TyKind::Abstraction {
                parameters: (0..ty_parameters_name.len()).fold(
                    backend::TyListKind::Nil,
                    |tail, _| {
                        backend::TyListKind::Cons(Box::new(backend::TyKind::Ty), Box::new(tail))
                    },
                ),
                ret: Box::new(backend::TyKind::Ty),
            }
        } else {
            backend::TyKind::Ty
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
            backend::Structure {
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
    ) -> Option<(backend::FunctionTy, backend::FunctionDefinition)> {
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
                        std::collections::hash_map::Entry::Occupied(entry) => {
                            logger.duplicate_definition(
                                ast_ty_parameter.pos,
                                entry.get().0.clone(),
                                &files,
                            );
                        }
                        std::collections::hash_map::Entry::Vacant(entry) => {
                            entry.insert((
                                ast_ty_parameter.pos,
                                Item::Ty(backend::TyBuilder::Parameter(ty_parameters_name.len())),
                            ));
                            ty_parameters_name.push(name);
                        }
                    }
                } else {
                    logger.invalid_ty_parameter(ast_ty_parameter.pos, &files);
                }
            }
        }
        let mut body = Block::new();
        let mut local_variables = Variables {
            num: 0,
            name_and_indices: Vec::new(),
        };
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
                                        std::collections::hash_map::Entry::Occupied(entry) => {
                                            logger.duplicate_definition(
                                                ast_parameter.pos,
                                                entry.get().0.clone(),
                                                &files,
                                            );
                                        }
                                        std::collections::hash_map::Entry::Vacant(entry) => {
                                            local_variables
                                                .name_and_indices
                                                .push((name, local_variables.num));
                                            entry.insert((
                                                ast_parameter_name.pos,
                                                Item::Variable(
                                                    backend::LocalOrGlobal::Local,
                                                    local_variables.num,
                                                ),
                                            ));
                                            local_variables.num += 1;
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
            backend::TyBuilder::Application {
                constructor: Box::new(backend::TyBuilder::Constructor(
                    backend::TyConstructor::Tuple,
                )),
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
                &mut body,
                &mut local_variables,
                0,
                backend::LocalOrGlobal::Local,
                &exports,
                &files,
                logger,
            );
        }
        for (name, index) in local_variables.name_and_indices.into_iter().rev() {
            let item = self.items.remove(&name);
            assert!(
                matches!(item, Some((_, Item::Variable(backend::LocalOrGlobal::Local, stored_index))) if stored_index == index)
            );
            body.expressions.push(backend::Expression::Function {
                candidates: vec![backend::Function::Delete],
                calls: vec![backend::Call {
                    arguments: vec![backend::Expression::Variable(
                        backend::LocalOrGlobal::Local,
                        index,
                    )],
                }],
            });
        }
        let body = body.get();
        for ty_parameter_name in &ty_parameters_name {
            self.items.remove(ty_parameter_name);
        }
        Some((
            backend::FunctionTy {
                num_ty_parameters: ty_parameters_name.len(),
                parameters_ty,
                return_ty,
            },
            backend::FunctionDefinition {
                num_local_variables: local_variables.num,
                body,
            },
        ))
    }

    pub fn translate_statement(
        &mut self,
        statement: ast::Statement,
        target: &mut Block,
        variables: &mut Variables,
        num_outer_variables: usize,
        local_or_global: backend::LocalOrGlobal,
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
                    Term::Expression(expr) => target.expressions.push(expr),
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
                        std::collections::hash_map::Entry::Occupied(entry) => {
                            logger.duplicate_definition(ast_name.pos, entry.get().0.clone(), files);
                        }
                        std::collections::hash_map::Entry::Vacant(entry) => {
                            variables.name_and_indices.push((name, variables.num));
                            entry.insert((
                                ast_name.pos,
                                Item::Variable(local_or_global, variables.num),
                            ));
                            variables.num += 1;
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
                let mut then_block = Block::new();
                let num_alive_variables = variables.name_and_indices.len();
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
                        &mut then_block,
                        variables,
                        num_outer_variables,
                        local_or_global,
                        exports,
                        files,
                        logger,
                    );
                }
                for (name, index) in variables
                    .name_and_indices
                    .split_off(num_alive_variables)
                    .into_iter()
                    .rev()
                {
                    let item = self.items.remove(&name);
                    assert!(matches!(
                        item,
                        Some((_, Item::Variable(stored_local_or_global, stored_index)))
                        if stored_index == index
                        && stored_local_or_global == local_or_global
                    ));
                    then_block.add_expression(backend::Expression::Function {
                        candidates: vec![backend::Function::Delete],
                        calls: vec![backend::Call {
                            arguments: vec![backend::Expression::Variable(local_or_global, index)],
                        }],
                    });
                }
                if !then_block.expressions.is_empty() {
                    then_block.block.size += 1;
                    then_block
                        .block
                        .statements
                        .push(backend::Statement::Expr(then_block.expressions));
                }
                let mut else_block = Block {
                    block: backend::Block {
                        statements: Vec::new(),
                        size: 0,
                    },
                    expressions: Vec::new(),
                };
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
                            &mut else_block,
                            variables,
                            num_outer_variables,
                            local_or_global,
                            exports,
                            files,
                            logger,
                        );
                    }
                    for (name, index) in variables
                        .name_and_indices
                        .split_off(num_alive_variables)
                        .into_iter()
                        .rev()
                    {
                        let item = self.items.remove(&name);
                        assert!(matches!(
                            item.unwrap().1,
                            Item::Variable(stored_local_or_global, stored_index)
                            if stored_index == index
                            && stored_local_or_global == local_or_global
                        ));
                        else_block.expressions.push(backend::Expression::Function {
                            candidates: vec![backend::Function::Delete],
                            calls: vec![backend::Call {
                                arguments: vec![backend::Expression::Variable(
                                    local_or_global,
                                    index,
                                )],
                            }],
                        });
                    }
                    if !else_block.expressions.is_empty() {
                        else_block.block.size += 1;
                        else_block
                            .block
                            .statements
                            .push(backend::Statement::Expr(else_block.expressions));
                    }
                }
                target.block.size += then_block.block.size + else_block.block.size + 1;
                target.block.statements.push(backend::Statement::If {
                    antecedents: std::mem::take(&mut target.expressions),
                    condition: condition.unwrap(),
                    then_block: then_block.block,
                    else_block: else_block.block,
                });
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
                let mut do_block = Block {
                    block: backend::Block {
                        statements: Vec::new(),
                        size: 0,
                    },
                    expressions: Vec::new(),
                };
                let num_alive_variables = variables.name_and_indices.len();
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
                        &mut do_block,
                        variables,
                        num_alive_variables,
                        local_or_global,
                        exports,
                        files,
                        logger,
                    );
                }
                for (name, index) in variables
                    .name_and_indices
                    .split_off(num_alive_variables)
                    .into_iter()
                    .rev()
                {
                    let item = self.items.remove(&name);
                    assert!(matches!(
                        item.unwrap().1,
                        Item::Variable(stored_local_or_global, stored_index)
                        if stored_index == index
                        && stored_local_or_global == local_or_global
                    ));
                    do_block.expressions.push(backend::Expression::Function {
                        candidates: vec![backend::Function::Delete],
                        calls: vec![backend::Call {
                            arguments: vec![backend::Expression::Variable(local_or_global, index)],
                        }],
                    });
                }
                if !target.expressions.is_empty() {
                    target.block.size += 1;
                    target
                        .block
                        .statements
                        .push(backend::Statement::Expr(std::mem::take(
                            &mut target.expressions,
                        )));
                }
                if !do_block.expressions.is_empty() {
                    do_block.block.size += 1;
                    do_block
                        .block
                        .statements
                        .push(backend::Statement::Expr(do_block.expressions));
                }
                target.block.size += do_block.block.size + 1;
                target.block.statements.push(backend::Statement::While {
                    condition: condition.unwrap(),
                    do_block: do_block.block,
                });
            }
            ast::Statement::Break => {
                for &(_, index) in variables.name_and_indices[num_outer_variables..]
                    .iter()
                    .rev()
                {
                    target.expressions.push(backend::Expression::Function {
                        candidates: vec![backend::Function::Delete],
                        calls: vec![backend::Call {
                            arguments: vec![backend::Expression::Variable(local_or_global, index)],
                        }],
                    });
                }
                target
                    .block
                    .statements
                    .push(backend::Statement::Break(std::mem::take(
                        &mut target.expressions,
                    )));
                target.block.size += 1;
            }
            ast::Statement::Continue => {
                for &(_, index) in variables.name_and_indices[num_outer_variables..]
                    .iter()
                    .rev()
                {
                    target.expressions.push(backend::Expression::Function {
                        candidates: vec![backend::Function::Delete],
                        calls: vec![backend::Call {
                            arguments: vec![backend::Expression::Variable(local_or_global, index)],
                        }],
                    });
                }
                target
                    .block
                    .statements
                    .push(backend::Statement::Continue(std::mem::take(
                        &mut target.expressions,
                    )));
                target.block.size += 1;
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
                return Ok(Term::Ty(backend::TyBuilder::Constructor(
                    backend::TyConstructor::Integer,
                )))
            }
            ast::Term::FloatTy => {
                return Ok(Term::Ty(backend::TyBuilder::Constructor(
                    backend::TyConstructor::Float,
                )))
            }
            ast::Term::NumericLiteral(value) => {
                if value.chars().all(|ch| matches!(ch, '0'..='9')) {
                    match value.parse() {
                        Ok(value) => {
                            if reference {
                                logger.expected_lvalue(pos, files);
                                Err(())
                            } else {
                                Ok(Term::Expression(backend::Expression::Integer(value)))
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
                                Ok(Term::Expression(backend::Expression::Float(value)))
                            }
                        }
                        Err(err) => {
                            logger.cannot_parse_float(pos, err, files);
                            Err(())
                        }
                    }
                }
            }
            ast::Term::Identity => Ok(Term::Expression(backend::Expression::Function {
                candidates: vec![backend::Function::Identity],
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
                Term::Expression(expr) => Ok(Term::Expression(backend::Expression::Function {
                    candidates: vec![],
                    calls: vec![backend::Call {
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
                            backend::Expression::Function { candidates, calls } => {
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
                calls.push(backend::Call { arguments });
                Ok(Term::Expression(backend::Expression::Function {
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
                Ok(Term::Expression(backend::Expression::Function {
                    candidates,
                    calls: vec![backend::Call {
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
                    Ok(Term::Ty(backend::TyBuilder::Application {
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
                    Ok(Term::Expression(backend::Expression::Function {
                        candidates: candidates.clone(),
                        calls: vec![],
                    }))
                }
            }
            Item::Variable(local_or_global, index) => {
                let expr = backend::Expression::Variable(local_or_global, index);
                Ok(Term::Expression(if reference {
                    expr
                } else {
                    backend::Expression::Function {
                        candidates: vec![backend::Function::Deref],
                        calls: vec![backend::Call {
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
    Ty(backend::TyBuilder),
    Expression(backend::Expression),
}
