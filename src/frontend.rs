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

mod ast;
mod chars_peekable;
mod tests;

use std::collections::{HashMap, HashSet};
use std::io::Read;
use std::path::{Path, PathBuf};

use crate::{backend, log};
use chars_peekable::CharsPeekable;

/**
 * Reads the file specified by `root_file_path` and any other files it
 * imports, and passes them to `backend`.
 */
pub fn read_input(
    root_file_path: &Path,
    logger: &mut log::Logger,
) -> Result<backend::Definitions, ()> {
    let root_file_path = root_file_path.with_extension("sysc");
    let root_file_path = match root_file_path.canonicalize() {
        Ok(path) => path,
        Err(err) => {
            logger.root_file_not_found(&root_file_path, err);
            return Err(());
        }
    };
    let mut reader = Reader {
        num_structures: 0,
        num_functions: 0,
        definitions: backend::Definitions {
            structures: Vec::new(),
            functions: Vec::new(),
            num_global_variables: 0,
        },
        global_block: Block {
            block: backend::Block {
                statements: Vec::new(),
                size: 0,
            },
            expressions: Vec::new(),
        },
        global_variables: Variables {
            num: 0,
            name_and_indices: Vec::new(),
        },
        exports: Vec::new(),
        files: Vec::new(),
        file_indices: HashMap::new(),
        import_chain: HashSet::from([root_file_path.clone()]),
    };
    if let Err(err) = reader.read_file(&root_file_path, logger) {
        logger.cannot_read_root_file(&root_file_path, err);
    }
    if logger.num_errors > 0 {
        logger.aborting();
        return Err(());
    }
    for (_, index) in reader.global_variables.name_and_indices.into_iter().rev() {
        reader
            .global_block
            .expressions
            .push(backend::Expression::Function {
                candidates: vec![backend::Function::Delete],
                calls: vec![backend::Call {
                    arguments: vec![backend::Expression::Variable(
                        backend::LocalOrGlobal::Global,
                        index,
                    )],
                }],
            });
    }
    if !reader.global_block.expressions.is_empty() {
        reader
            .global_block
            .block
            .statements
            .push(backend::Statement::Expr(reader.global_block.expressions));
        reader.global_block.block.size += 1;
    }
    reader.definitions.functions.push((
        backend::FunctionTy {
            num_ty_parameters: 0,
            parameters_ty: Vec::new(),
            return_ty: backend::TyBuilder::Application {
                constructor: Box::new(backend::TyBuilder::Constructor(
                    backend::TyConstructor::Tuple,
                )),
                arguments: vec![],
            },
        },
        backend::FunctionDefinition {
            num_local_variables: 0,
            body: reader.global_block.block,
        },
    ));
    Ok(reader.definitions)
}

/**
 * A structure used in [`read_input`].
 */
struct Reader {
    /**
     * Total number of structures defined in all files. Used and updated by
     * [`Reader::register_structure_name`].
     */
    num_structures: usize,
    /**
     * Total number of functions defined in all files. Used and updated by
     * [`Reader::register_function_name`].
     */
    num_functions: usize,
    /**
     * The target which [`Reader::read_file`] stores the results in.
     */
    definitions: backend::Definitions,
    /**
     * Block of global statements.
     */
    global_block: Block,
    /**
     *
     */
    global_variables: Variables,
    /**
     *
     */
    exports: Vec<Context>,
    files: Vec<log::File>,
    /**
     * Used in [`Reader::read_file`] to avoid reading the same file multiple
     * times.
     */
    file_indices: HashMap<PathBuf, Option<usize>>,
    /**
     * Used in [`Reader::import_file`] to detect circular imports.
     */
    import_chain: HashSet<PathBuf>,
}

struct Block {
    block: backend::Block,
    expressions: Vec<backend::Expression>,
}

struct Variables {
    num: usize,
    name_and_indices: Vec<(String, usize)>,
}

struct Context {
    items: HashMap<String, (log::Pos, NamedItem)>,
    methods: HashMap<String, Vec<backend::Function>>,
}

impl Reader {
    /**
     * Reads the file specified by `path`.
     */
    fn read_file(
        &mut self,
        path: &Path,
        logger: &mut log::Logger,
    ) -> Result<Option<usize>, std::io::Error> {
        if let Some(&index) = self.file_indices.get(path) {
            /* The file was already read. Since circular imports should have been
             * detected in `Reader::import_file`, this is not circular imports but
             * diamond imports. */
            return Ok(index);
        }
        let mut file = std::fs::File::open(path)?;
        let mut content = String::new();
        file.read_to_string(&mut content)?;
        let mut chars_peekable = CharsPeekable::new(&content);
        let preorder_index = self.files.len();
        let result = ast::parse_file(&mut chars_peekable, preorder_index);
        self.files.push(log::File {
            path: path.to_path_buf(),
            lines: chars_peekable.lines(),
            content,
        });
        let ast_file = match result {
            Ok(ast) => ast,
            Err(err) => {
                logger.parse_error(err, &self.files);
                self.file_indices.insert(path.to_path_buf(), None);
                return Ok(None);
            }
        };
        let mut context = Context {
            items: HashMap::new(),
            methods: HashMap::new(),
        };
        for ast::WithExtraTokens {
            content: ast_import,
            extra_tokens_pos,
        } in ast_file.imports
        {
            if let Some(extra_tokens_pos) = extra_tokens_pos {
                logger.extra_tokens(extra_tokens_pos, &self.files);
            }
            if let Ok((name, pos, Some(index))) =
                self.import_file(ast_import, path.parent().unwrap(), logger)
            {
                match context.items.entry(name) {
                    std::collections::hash_map::Entry::Occupied(entry) => {
                        let (prev_pos, _) = entry.get();
                        logger.duplicate_definition(pos, prev_pos.clone(), &self.files);
                    }
                    std::collections::hash_map::Entry::Vacant(entry) => {
                        entry.insert((pos, NamedItem::Import(index)));
                        for (name, exported_candidates) in &self.exports[index].methods {
                            context
                                .methods
                                .entry(name.clone())
                                .or_insert_with(Vec::new)
                                .extend_from_slice(exported_candidates);
                        }
                    }
                }
            }
            for candidates in context.methods.values_mut() {
                candidates.sort();
                candidates.dedup();
            }
        }
        for name in ast_file.structure_names {
            self.register_structure_name(name, &mut context, logger);
        }
        for name in ast_file.function_names {
            self.register_function_name(name, &mut context, logger);
        }
        for ast::WithExtraTokens {
            content: statement,
            extra_tokens_pos,
        } in ast_file.top_level_statements
        {
            if let Some(extra_tokens_pos) = extra_tokens_pos {
                logger.extra_tokens(extra_tokens_pos, &self.files);
            }
            match statement {
                ast::TopLevelStatement::StructureDefinition(structure_definition) => {
                    let (kind, definition) = self.translate_structure_definition(
                        structure_definition,
                        &mut context,
                        logger,
                    );
                    self.definitions.structures.push((kind, definition));
                }
                ast::TopLevelStatement::FunctionDefinition(function_definition) => {
                    let num_current_items = context.items.len();
                    if let Some((ty, definition)) = self.translate_function_definition(
                        function_definition,
                        &mut context,
                        logger,
                    ) {
                        self.definitions.functions.push((ty, definition));
                    }
                    assert_eq!(context.items.len(), num_current_items);
                }
                ast::TopLevelStatement::Statement(statement) => {
                    context.translate_statement(
                        statement,
                        &mut self.global_block,
                        &mut self.global_variables,
                        0,
                        backend::LocalOrGlobal::Global,
                        &self.exports,
                        &self.files,
                        logger,
                    );
                }
            }
        }
        let postorder_index = self.exports.len();
        self.exports.push(context);
        self.file_indices
            .insert(path.to_path_buf(), Some(postorder_index));
        Ok(Some(postorder_index))
    }

    fn import_file(
        &mut self,
        ast::Import {
            keyword_import_pos,
            target,
        }: ast::Import,
        parent_directory: &Path,
        logger: &mut log::Logger,
    ) -> Result<(String, log::Pos, Option<usize>), ()> {
        let Some(target) = target else {
            logger.missing_import_target(keyword_import_pos, &self.files);
            return Err(());
        };
        let (name, path, name_pos, path_pos) = match target.term {
            ast::Term::Identifier(name) => {
                let path = parent_directory.join(&name);
                (name, path, target.pos.clone(), target.pos)
            }
            ast::Term::FunctionCall {
                function,
                arguments,
            } => {
                let name = match function.term {
                    ast::Term::Identifier(name) => name,
                    _ => {
                        logger.invalid_import_target(target.pos, &self.files);
                        return Err(());
                    }
                };
                let (path, path_pos) = match arguments.into_iter().next() {
                    Some(ast::ListElement::NonEmpty(argument)) => match argument.term {
                        ast::Term::StringLiteral(components) => {
                            let mut path = String::new();
                            for component in components {
                                match component {
                                    ast::StringLiteralComponent::PlaceHolder { .. } => {
                                        logger.placeholder_in_import_path(
                                            argument.pos.clone(),
                                            &self.files,
                                        );
                                        return Err(());
                                    }
                                    ast::StringLiteralComponent::String(value) => {
                                        path.push_str(&value);
                                    }
                                }
                            }
                            (parent_directory.join(&path), argument.pos)
                        }
                        _ => {
                            logger.invalid_import_target(target.pos, &self.files);
                            return Err(());
                        }
                    },
                    Some(ast::ListElement::Empty { comma_pos }) => {
                        logger.empty_argument(comma_pos, &self.files);
                        return Err(());
                    }
                    None => {
                        logger.missing_import_target(target.pos, &self.files);
                        return Err(());
                    }
                };
                (name, path, function.pos, path_pos)
            }
            _ => {
                logger.invalid_import_target(target.pos, &self.files);
                return Err(());
            }
        };
        let path = path.with_extension("sysc");
        let path = match path.canonicalize() {
            Ok(path) => path,
            Err(err) => {
                logger.cannot_read_file(path_pos, &path, err, &self.files);
                return Err(());
            }
        };
        if self.import_chain.insert(path.clone()) {
            let result = self.read_file(&path, logger);
            self.import_chain.remove(&path);
            match result {
                Ok(n) => Ok((name, name_pos, n)),
                Err(err) => {
                    logger.cannot_read_file(path_pos, &path, err, &self.files);
                    Err(())
                }
            }
        } else {
            logger.circular_imports(path_pos, &path, &self.files);
            Err(())
        }
    }

    fn register_structure_name(
        &mut self,
        ast::StructureName {
            keyword_struct_pos,
            name_and_pos: name,
        }: ast::StructureName,
        context: &mut Context,
        logger: &mut log::Logger,
    ) {
        let Some((name, pos)) = name else {
            logger.missing_structure_name(keyword_struct_pos, &self.files);
            return;
        };
        match context.items.entry(name) {
            std::collections::hash_map::Entry::Occupied(entry) => {
                logger.duplicate_definition(pos, entry.get().0.clone(), &self.files);
            }
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert((
                    pos,
                    NamedItem::Ty(backend::TyBuilder::Constructor(
                        backend::TyConstructor::Structure(self.num_structures),
                    )),
                ));
                self.num_structures += 1;
            }
        }
    }

    fn register_function_name(
        &mut self,
        ast::FunctionName {
            keyword_pos,
            name_and_pos: name,
            is_method,
        }: ast::FunctionName,
        context: &mut Context,
        logger: &mut log::Logger,
    ) {
        let Some((name, pos)) = name else {
            logger.missing_function_name(keyword_pos, &self.files);
            return;
        };
        if is_method {
            context
                .methods
                .entry(name)
                .or_insert_with(Vec::new)
                .push(backend::Function::UserDefined(self.num_functions));
        } else {
            match context.items.entry(name) {
                std::collections::hash_map::Entry::Occupied(mut entry) => {
                    let (prev_pos, item) = entry.get_mut();
                    if let NamedItem::Function(functions) = item {
                        functions.push(backend::Function::UserDefined(self.num_functions));
                    } else {
                        logger.duplicate_definition(pos, prev_pos.clone(), &self.files);
                    }
                }
                std::collections::hash_map::Entry::Vacant(entry) => {
                    entry.insert((
                        pos,
                        NamedItem::Function(vec![backend::Function::UserDefined(
                            self.num_functions,
                        )]),
                    ));
                }
            }
        }
        self.num_functions += 1;
    }

    fn translate_structure_definition(
        &mut self,
        ast::StructureDefinition {
            ty_parameters: ast_ty_parameters,
            fields: ast_fields,
            extra_tokens_pos,
        }: ast::StructureDefinition,
        context: &mut Context,
        logger: &mut log::Logger,
    ) -> (backend::TyKind, backend::Structure) {
        let mut ty_parameters_name = Vec::new();
        let kind = if let Some(ast_ty_parameters) = ast_ty_parameters {
            for ast_ty_parameter in ast_ty_parameters {
                let ast_ty_parameter = match ast_ty_parameter {
                    ast::ListElement::Empty { comma_pos } => {
                        logger.empty_ty_parameter(comma_pos, &self.files);
                        continue;
                    }
                    ast::ListElement::NonEmpty(ast_ty_parameter) => ast_ty_parameter,
                };
                match ast_ty_parameter.term {
                    ast::Term::Identifier(name) => match context.items.entry(name.clone()) {
                        std::collections::hash_map::Entry::Occupied(entry) => {
                            logger.duplicate_definition(
                                ast_ty_parameter.pos,
                                entry.get().0.clone(),
                                &self.files,
                            );
                        }
                        std::collections::hash_map::Entry::Vacant(entry) => {
                            entry.insert((
                                ast_ty_parameter.pos,
                                NamedItem::Ty(backend::TyBuilder::Parameter(
                                    ty_parameters_name.len(),
                                )),
                            ));
                            ty_parameters_name.push(name);
                        }
                    },
                    _ => {
                        logger.invalid_ty_parameter(ast_ty_parameter.pos, &self.files);
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
            logger.extra_tokens(extra_tokens_pos, &self.files);
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
                    match context.translate_term(
                        *ast_field_ty,
                        false,
                        &self.exports,
                        &self.files,
                        logger,
                    ) {
                        Ok(TranslatedTerm::Ty(field_ty)) => fields_ty.push(field_ty),
                        Ok(_) => logger.expected_ty(field_ty_pos, &self.files),
                        Err(()) => {}
                    }
                }
                _ => {
                    logger.invalid_structure_field(ast_field.pos, &self.files);
                }
            }
            if let Some(extra_tokens_pos) = extra_tokens_pos {
                logger.extra_tokens(extra_tokens_pos, &self.files);
            }
        }
        for ty_parameter_name in &ty_parameters_name {
            context.items.remove(ty_parameter_name);
        }
        (
            kind,
            backend::Structure {
                num_ty_parameters: ty_parameters_name.len(),
                fields_ty,
            },
        )
    }

    fn translate_function_definition(
        &mut self,
        ast::FunctionDefinition {
            ty_parameters: ast_ty_parameters,
            parameters: ast_parameters,
            return_ty: ast_return_ty,
            body: ast_body,
            extra_tokens_pos,
        }: ast::FunctionDefinition,
        context: &mut Context,
        logger: &mut log::Logger,
    ) -> Option<(backend::FunctionTy, backend::FunctionDefinition)> {
        let mut ty_parameters_name = Vec::new();
        if let Some(ast_ty_parameters) = ast_ty_parameters {
            for ast_ty_parameter in ast_ty_parameters {
                let ast_ty_parameter = match ast_ty_parameter {
                    ast::ListElement::Empty { comma_pos } => {
                        logger.empty_ty_parameter(comma_pos, &self.files);
                        continue;
                    }
                    ast::ListElement::NonEmpty(ast_ty_parameter) => ast_ty_parameter,
                };
                if let ast::Term::Identifier(name) = ast_ty_parameter.term {
                    match context.items.entry(name.clone()) {
                        std::collections::hash_map::Entry::Occupied(entry) => {
                            logger.duplicate_definition(
                                ast_ty_parameter.pos,
                                entry.get().0.clone(),
                                &self.files,
                            );
                        }
                        std::collections::hash_map::Entry::Vacant(entry) => {
                            entry.insert((
                                ast_ty_parameter.pos,
                                NamedItem::Ty(backend::TyBuilder::Parameter(
                                    ty_parameters_name.len(),
                                )),
                            ));
                            ty_parameters_name.push(name);
                        }
                    }
                } else {
                    logger.invalid_ty_parameter(ast_ty_parameter.pos, &self.files);
                }
            }
        }
        let mut body = Block {
            block: backend::Block {
                statements: Vec::new(),
                size: 0,
            },
            expressions: Vec::new(),
        };
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
                            logger.empty_parameter(comma_pos, &self.files);
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
                                    match context.items.entry(name.clone()) {
                                        std::collections::hash_map::Entry::Occupied(entry) => {
                                            logger.duplicate_definition(
                                                ast_parameter.pos,
                                                entry.get().0.clone(),
                                                &self.files,
                                            );
                                        }
                                        std::collections::hash_map::Entry::Vacant(entry) => {
                                            local_variables
                                                .name_and_indices
                                                .push((name, local_variables.num));
                                            entry.insert((
                                                ast_parameter_name.pos,
                                                NamedItem::Variable(
                                                    backend::LocalOrGlobal::Local,
                                                    local_variables.num,
                                                ),
                                            ));
                                            local_variables.num += 1;
                                        }
                                    }
                                }
                                _ => {
                                    logger.invalid_parameter(ast_parameter.pos, &self.files);
                                }
                            }
                            if let Some(ast_parameter_ty) = ast_parameter_ty {
                                let parameter_pos = ast_parameter_ty.pos.clone();
                                match context.translate_term(
                                    *ast_parameter_ty,
                                    false,
                                    &self.exports,
                                    &self.files,
                                    logger,
                                ) {
                                    Ok(TranslatedTerm::Ty(parameter_ty)) => {
                                        parameters_ty.push(parameter_ty)
                                    }
                                    Ok(_) => logger.expected_ty(parameter_pos, &self.files),
                                    Err(()) => {}
                                }
                            } else {
                                logger.missing_ty(colon_pos, &self.files);
                            }
                        }
                        _ => {
                            logger.invalid_parameter(ast_parameter.pos, &self.files);
                        }
                    }
                }
            }
            Err(signature_pos) => {
                logger.missing_parameter_list(signature_pos, &self.files);
            }
        }
        let return_ty = if let Some(ast_return_ty) = ast_return_ty {
            if let Some(ast_return_ty) = ast_return_ty.ty {
                let return_ty_pos = ast_return_ty.pos.clone();
                match context.translate_term(
                    ast_return_ty,
                    false,
                    &self.exports,
                    &self.files,
                    logger,
                ) {
                    Ok(TranslatedTerm::Ty(return_ty)) => return_ty,
                    Ok(_) => {
                        logger.expected_ty(return_ty_pos, &self.files);
                        return None;
                    }
                    Err(()) => return None,
                }
            } else {
                logger.missing_ty(ast_return_ty.colon_pos, &self.files);
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
            logger.extra_tokens(extra_tokens_pos, &self.files);
        }
        for ast::WithExtraTokens {
            content: ast_statement,
            extra_tokens_pos,
        } in ast_body
        {
            if let Some(extra_tokens_pos) = extra_tokens_pos {
                logger.extra_tokens(extra_tokens_pos, &self.files);
            }
            context.translate_statement(
                ast_statement,
                &mut body,
                &mut local_variables,
                0,
                backend::LocalOrGlobal::Local,
                &self.exports,
                &self.files,
                logger,
            );
        }
        for (name, index) in local_variables.name_and_indices.into_iter().rev() {
            let item = context.items.remove(&name);
            assert!(
                matches!(item, Some((_, NamedItem::Variable(backend::LocalOrGlobal::Local, stored_index))) if stored_index == index)
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
        if !body.expressions.is_empty() {
            body.block
                .statements
                .push(backend::Statement::Expr(body.expressions));
            body.block.size += 1;
        }
        for ty_parameter_name in &ty_parameters_name {
            context.items.remove(ty_parameter_name);
        }
        Some((
            backend::FunctionTy {
                num_ty_parameters: ty_parameters_name.len(),
                parameters_ty,
                return_ty,
            },
            backend::FunctionDefinition {
                num_local_variables: local_variables.num,
                body: body.block,
            },
        ))
    }
}

impl Context {
    fn translate_statement(
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
                    TranslatedTerm::Expression(expr) => target.expressions.push(expr),
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
                                NamedItem::Variable(local_or_global, variables.num),
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
                            TranslatedTerm::Expression(condition) => Ok(condition),
                            _ => {
                                logger.expected_expression(condition_pos, files);
                                Err(())
                            }
                        })
                } else {
                    logger.missing_if_condition(keyword_if_pos, files);
                    Err(())
                };
                let mut then_block = Block {
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
                        Some((_, NamedItem::Variable(stored_local_or_global, stored_index)))
                        if stored_index == index
                        && stored_local_or_global == local_or_global
                    ));
                    then_block.expressions.push(backend::Expression::Function {
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
                            NamedItem::Variable(stored_local_or_global, stored_index)
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
                            TranslatedTerm::Expression(condition) => Ok(condition),
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
                        NamedItem::Variable(stored_local_or_global, stored_index)
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
    ) -> Result<TranslatedTerm, ()> {
        match ast_term {
            ast::Term::IntegerTy => {
                return Ok(TranslatedTerm::Ty(backend::TyBuilder::Constructor(
                    backend::TyConstructor::Integer,
                )))
            }
            ast::Term::FloatTy => {
                return Ok(TranslatedTerm::Ty(backend::TyBuilder::Constructor(
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
                                Ok(TranslatedTerm::Expression(backend::Expression::Integer(
                                    value,
                                )))
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
                                Ok(TranslatedTerm::Expression(backend::Expression::Float(
                                    value,
                                )))
                            }
                        }
                        Err(err) => {
                            logger.cannot_parse_float(pos, err, files);
                            Err(())
                        }
                    }
                }
            }
            ast::Term::Identity => Ok(TranslatedTerm::Expression(backend::Expression::Function {
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
                TranslatedTerm::Expression(expr) => {
                    Ok(TranslatedTerm::Expression(backend::Expression::Function {
                        candidates: vec![],
                        calls: vec![backend::Call {
                            arguments: vec![expr],
                        }],
                    }))
                }
                TranslatedTerm::Ty(_) => {
                    todo!();
                }
                TranslatedTerm::Import(file_index) => match exports[file_index].items.get(&name) {
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
                        TranslatedTerm::Expression(function) => match function {
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
                        TranslatedTerm::Expression(argument) => {
                            arguments.push(argument);
                        }
                        _ => {
                            logger.expected_expression(argument_pos, files);
                        }
                    }
                }
                let (candidates, mut calls) = function?;
                calls.push(backend::Call { arguments });
                Ok(TranslatedTerm::Expression(backend::Expression::Function {
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
                                TranslatedTerm::Expression(expr) => Ok(expr),
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
                                TranslatedTerm::Expression(expr) => Ok(expr),
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
                Ok(TranslatedTerm::Expression(backend::Expression::Function {
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
                        TranslatedTerm::Ty(ty) => Ok(ty),
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
                            TranslatedTerm::Ty(ty) => Ok(ty),
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
                    Ok(TranslatedTerm::Ty(backend::TyBuilder::Application {
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
        named_item: &NamedItem,
        reference: bool,
        pos: log::Pos,
        files: &[log::File],
        logger: &mut log::Logger,
    ) -> Result<TranslatedTerm, ()> {
        match *named_item {
            NamedItem::Function(ref candidates) => {
                if reference {
                    logger.expected_lvalue(pos, files);
                    Err(())
                } else {
                    Ok(TranslatedTerm::Expression(backend::Expression::Function {
                        candidates: candidates.clone(),
                        calls: vec![],
                    }))
                }
            }
            NamedItem::Variable(local_or_global, index) => {
                let expr = backend::Expression::Variable(local_or_global, index);
                Ok(TranslatedTerm::Expression(if reference {
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
            NamedItem::Import(index) => Ok(TranslatedTerm::Import(index)),
            NamedItem::Ty(ref ty) => Ok(TranslatedTerm::Ty(ty.clone())),
        }
    }
}

enum TranslatedTerm {
    Import(usize),
    Ty(backend::TyBuilder),
    Expression(backend::Expression),
}

#[derive(Clone)]
enum NamedItem {
    Import(usize),
    Ty(backend::TyBuilder),
    Function(Vec<backend::Function>),
    Variable(backend::LocalOrGlobal, usize),
}
