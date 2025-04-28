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
mod context;
mod parser;
mod tests;
mod variables;

use std::collections::{HashMap, HashSet};
use std::io::Read;
use std::path::{Path, PathBuf};

use crate::{backend, log};
use chars_peekable::CharsPeekable;
use context::Context;
use variables::Variables;

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
            functions_ty: Vec::new(),
            function_definitions: Vec::new(),
            num_global_variables: 0,
        },
        global_builder: backend::BlockBuilder::new(),
        global_variables: Variables::new(false),
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
    reader
        .global_variables
        .truncate(0, &mut reader.global_builder);
    let body = reader.global_builder.finish();
    reader.definitions.functions_ty.push(backend::FunctionTy {
        num_ty_parameters: 0,
        parameters_ty: Vec::new(),
        return_ty: backend::TyBuilder::Application {
            constructor: Box::new(backend::TyBuilder::Constructor(
                backend::TyConstructor::Tuple,
            )),
            arguments: vec![],
        },
    });
    reader
        .definitions
        .function_definitions
        .push(backend::FunctionDefinition {
            num_local_variables: 0,
            body,
        });
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
    global_builder: backend::BlockBuilder,
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

#[derive(Clone)]
pub enum Item {
    Import(usize),
    Ty(backend::TyBuilder),
    Function(Vec<backend::Function>),
    GlobalVariable(usize),
    LocalVariable(usize),
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
        let result = parser::parse_file(&mut chars_peekable, preorder_index);
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
            items: HashMap::from([(
                String::from("print"),
                (None, Item::Function(vec![backend::Function::Print])),
            )]),
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
                    std::collections::hash_map::Entry::Occupied(mut entry) => {
                        if let (Some(prev_pos), _) = entry.get() {
                            logger.duplicate_definition(pos, prev_pos.clone(), &self.files);
                        } else {
                            entry.insert((Some(pos), Item::Import(index)));
                        }
                    }
                    std::collections::hash_map::Entry::Vacant(entry) => {
                        entry.insert((Some(pos), Item::Import(index)));
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
                    let (kind, definition) = context.translate_structure_definition(
                        structure_definition,
                        &self.exports,
                        &self.files,
                        logger,
                    );
                    self.definitions.structures.push((kind, definition));
                }
                ast::TopLevelStatement::FunctionDefinition(function_definition) => {
                    let num_current_items = context.items.len();
                    if let Some((ty, definition)) = context.translate_function_definition(
                        function_definition,
                        &self.exports,
                        &self.files,
                        logger,
                    ) {
                        self.definitions.functions_ty.push(ty);
                        self.definitions.function_definitions.push(definition);
                    }
                    assert_eq!(context.items.len(), num_current_items);
                }
                ast::TopLevelStatement::Statement(statement) => {
                    context.translate_statement(
                        statement,
                        &mut self.global_builder,
                        &mut self.global_variables,
                        0,
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
            std::collections::hash_map::Entry::Occupied(mut entry) => {
                if let (Some(prev_pos), _) = entry.get() {
                    logger.duplicate_definition(pos, prev_pos.clone(), &self.files);
                } else {
                    entry.insert((
                        Some(pos),
                        Item::Ty(backend::TyBuilder::Constructor(
                            backend::TyConstructor::Structure(self.num_structures),
                        )),
                    ));
                }
            }
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert((
                    Some(pos),
                    Item::Ty(backend::TyBuilder::Constructor(
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
                    if let Item::Function(functions) = item {
                        functions.push(backend::Function::UserDefined(self.num_functions));
                    } else {
                        logger.duplicate_definition(pos, prev_pos.clone().unwrap(), &self.files);
                    }
                }
                std::collections::hash_map::Entry::Vacant(entry) => {
                    entry.insert((
                        Some(pos),
                        Item::Function(vec![backend::Function::UserDefined(self.num_functions)]),
                    ));
                }
            }
        }
        self.num_functions += 1;
    }
}
