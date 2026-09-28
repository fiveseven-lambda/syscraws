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

use super::{Item, ast};
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
    pub items: HashMap<String, Item>,
}

impl Context {
    pub fn translate_structure_definition(
        &mut self,
        structure_definition: ast::StructureDefinition,
        exports: &[Context],
        logger: &mut log::Logger,
    ) -> Result<(ir::Constant, ir::StructureDefinition), ()> {
        let mut parameters_list = Vec::new();
        assert!(
            self.translate_constant_declaration(
                structure_definition.signature.unwrap(),
                &mut parameters_list,
                0,
                exports,
                logger,
            )
            .is_empty()
        );
        let mut ty = ir::Constant::Ty;
        while let Some(parameters) = parameters_list.pop() {
            ty = ir::Constant::Product(
                parameters
                    .into_iter()
                    .map(|(name, ty)| {
                        self.items.remove(&name);
                        ty
                    })
                    .collect(),
                Box::new(ty),
            );
        }
        Ok((ty, ir::StructureDefinition {}))
    }

    pub fn translate_constant_declaration(
        &mut self,
        ast::TermWithPos {
            term: ast_term,
            pos,
        }: ast::TermWithPos,
        constants: &mut Vec<Vec<(String, ir::Constant)>>,
        depth: usize,
        exports: &[Context],
        logger: &mut log::Logger,
    ) -> String {
        match ast_term {
            ast::Term::Identifier(name) => name,
            ast::Term::TypeParameters {
                term_left: ast_function,
                parameters: ast_arguments,
            } => {
                let ret = self.translate_constant_declaration(
                    *ast_function,
                    constants,
                    depth + 1,
                    exports,
                    logger,
                );
                let mut parameters = Vec::new();
                for (argument_index, ast_argument) in ast_arguments.into_iter().enumerate() {
                    let ast::ListElement::NonEmpty(ast_argument) = ast_argument else {
                        todo!();
                    };
                    let ast::Term::TypeAnnotation {
                        term_left: ast_name_and_parameters,
                        colon_pos,
                        term_right: Some(ast_ret),
                    } = ast_argument.term
                    else {
                        todo!();
                    };
                    let mut parameters_list = Vec::new();
                    let parameter_name = self.translate_constant_declaration(
                        *ast_name_and_parameters,
                        &mut parameters_list,
                        depth,
                        exports,
                        logger,
                    );
                    let mut parameter_ty = self
                        .translate_constant(*ast_ret, depth, exports, logger)
                        .unwrap();
                    while let Some(parameters) = parameters_list.pop() {
                        parameter_ty = ir::Constant::Product(
                            parameters
                                .into_iter()
                                .map(|(name, ty)| {
                                    self.items.remove(&name);
                                    ty
                                })
                                .collect(),
                            Box::new(parameter_ty),
                        );
                    }
                    parameters.push((parameter_name.clone(), parameter_ty));
                    self.items
                        .insert(parameter_name, Item::Parameter(depth, argument_index));
                }
                constants.push(parameters);
                ret
            }
            _ => todo!(),
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
            Some(Item::Constant(constant)) => Ok(constant.clone()),
            Some(Item::Parameter(d, i)) => Ok(ir::Constant::Parameter(d - depth, *i)),
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
            Some(&Item::Import(index)) => Ok(index),
            _ => Err(()),
        }
    }
}
