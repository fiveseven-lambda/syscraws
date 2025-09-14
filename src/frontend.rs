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
 * Parses source files and emits intermediate representation ([`ir`]).
 */

mod ast;
mod block_builder;
mod chars_peekable;
mod context;
mod parser;
mod tests;
mod variables;

use std::collections::{HashMap, HashSet};
use std::io::Read;
use std::path::{Path, PathBuf};

use crate::{ir, log};
use block_builder::BlockBuilder;
use chars_peekable::CharsPeekable;
use context::Context;
use variables::Variables;

/**
 * Reads the file specified by `root_file_path` and any other files it imports,
 * and emits intermediate representation defined in [`ir`].
 */
pub fn read_input(root_file_path: &Path, mut config: log::Config) -> Result<ir::Program, ()> {
    let root_file_path = root_file_path.with_extension("sysc");
    let root_file_path = match root_file_path.canonicalize() {
        Ok(path) => path,
        Err(err) => {
            config.root_file_not_found(&root_file_path, err);
            return Err(());
        }
    };
    let mut reader = Reader {
        num_structures: 0,
        num_functions: 0,
        ir_program: ir::Program {
            structures: Vec::new(),
            functions_ty: Vec::new(),
            function_definitions: Vec::new(),
            num_global_variables: 0,
        },
        global_builder: BlockBuilder::new(),
        global_variables: Variables::new(ir::Storage::Global),
        exports: Vec::new(),
        logger: log::Logger::new(config),
        file_indices: HashMap::new(),
        import_chain: HashSet::from([root_file_path.clone()]),
    };
    if let Err(err) = reader.read_file(&root_file_path) {
        reader.logger.cannot_read_root_file(&root_file_path, err);
    }
    if reader.logger.num_errors > 0 {
        reader.logger.aborting();
        return Err(());
    }
    reader.global_variables.free(0, &mut reader.global_builder);
    let body = reader.global_builder.finish();
    reader.ir_program.functions_ty.push(ir::FunctionTy {
        num_ty_parameters: 0,
        parameters_ty: Vec::new(),
        return_ty: ir::Ty::Application {
            constructor: Box::new(ir::Ty::Constructor(ir::TyConstructor::Tuple)),
            arguments: vec![],
        },
    });
    reader
        .ir_program
        .function_definitions
        .push(ir::FunctionDefinition {
            num_local_variables: 0,
            body,
        });
    reader.ir_program.num_global_variables = reader.global_variables.num_total();
    Ok(reader.ir_program)
}

/**
 * A structure to read files recursively and convert them to intermediate representation (IR) defined in [`ir`].
 */
struct Reader {
    /**
     * Total number of structures defined in all files. Used and updated by
     * [`declare_structure`](Reader::declare_structure) method.
     */
    num_structures: usize,
    /**
     * Total number of functions defined in all files. Used and updated by
     * [`declare_function`](Reader::declare_function) method.
     */
    num_functions: usize,
    /**
     * The target which [`read_file`](Reader::read_file) stores the results in.
     */
    ir_program: ir::Program,
    /**
     * Holds the global statements, which are later added as a single entry-point function.
     */
    global_builder: BlockBuilder,
    /**
     * List of global variables alive.
     * Variables in a block (e.g. if) are removed at the end of the block.
     * After all files are read, any remaining global variables are freed.
     */
    global_variables: Variables,
    /**
     * Items exported from each file, in postorder.
     */
    exports: Vec<Context>,
    /**
     * Logger to report errors.
     */
    logger: log::Logger,
    /**
     * Maps each file path to its postorder index (`None` if a parse error occurred).
     * Used to resolve imports and to avoid reading the same file multiple times.
     */
    file_indices: HashMap<PathBuf, Option<usize>>,
    /**
     * The paths of all files currently being imported (from root to current).
     * Used in [`import_file`](Reader::import_file) to detect circular imports.
     */
    import_chain: HashSet<PathBuf>,
}

/**
 * Various kinds of named entities.
 */
#[derive(Clone)]
pub enum Item {
    /**
     * References another file by its postorder index.
     */
    Import(usize),
    /**
     * A type definition.
     */
    Ty(ir::Ty),
    /**
     * One or more function definitions.
     */
    Function(Vec<ir::Function>),
    /**
     * A variable definition.
     */
    Variable(ir::Storage, usize),
}

impl Reader {
    /**
     * Reads the file specified by `path`.
     */
    fn read_file(&mut self, path: &Path) -> Result<Option<usize>, std::io::Error> {
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
        let preorder_index = self.logger.files.len();
        let result = parser::parse_file(&mut chars_peekable, preorder_index);
        self.logger.files.push(log::File {
            path: path.to_path_buf(),
            lines: chars_peekable.lines(),
            content,
        });
        let ast_file = match result {
            Ok(ast) => ast,
            Err(err) => {
                self.logger.parse_error(err);
                self.file_indices.insert(path.to_path_buf(), None);
                return Ok(None);
            }
        };
        let mut context = Context {
            items: HashMap::from([(
                String::from("print"),
                (None, Item::Function(vec![ir::Function::Print])),
            )]),
            methods: HashMap::from([(String::from("assign"), vec![ir::Function::Assign])]),
        };
        for ast::WithExtraTokens {
            content: ast_import,
            extra_tokens_pos,
        } in ast_file.imports
        {
            if let Some(extra_tokens_pos) = extra_tokens_pos {
                self.logger.extra_tokens(extra_tokens_pos);
            }
            if let Ok((name, pos, Some(index))) =
                self.import_file(ast_import, path.parent().unwrap())
            {
                match context.items.entry(name) {
                    std::collections::hash_map::Entry::Occupied(mut entry) => {
                        if let (Some(prev_pos), _) = entry.get() {
                            self.logger.duplicate_definition(pos, prev_pos.clone());
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
            self.declare_structure(name, &mut context);
        }
        for name in ast_file.function_names {
            self.declare_function(name, &mut context);
        }
        for ast::WithExtraTokens {
            content: statement,
            extra_tokens_pos,
        } in ast_file.top_level_statements
        {
            if let Some(extra_tokens_pos) = extra_tokens_pos {
                self.logger.extra_tokens(extra_tokens_pos);
            }
            match statement {
                ast::TopLevelStatement::StructureDefinition(structure_definition) => {
                    let (kind, definition) = context.translate_structure_definition(
                        structure_definition,
                        &self.exports,
                        &mut self.logger,
                    );
                    self.ir_program.structures.push((kind, definition));
                }
                ast::TopLevelStatement::FunctionDefinition(function_definition) => {
                    let num_current_items = context.items.len();
                    if let Some((ty, definition)) = context.translate_function_definition(
                        function_definition,
                        &self.exports,
                        &mut self.logger,
                    ) {
                        self.ir_program.functions_ty.push(ty);
                        self.ir_program.function_definitions.push(definition);
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
                        &mut self.logger,
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
    ) -> Result<(String, log::Pos, Option<usize>), ()> {
        let Some(target) = target else {
            self.logger.missing_import_target(keyword_import_pos);
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
                        self.logger.invalid_import_target(target.pos);
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
                                        self.logger
                                            .placeholder_in_import_path(argument.pos.clone());
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
                            self.logger.invalid_import_target(target.pos);
                            return Err(());
                        }
                    },
                    Some(ast::ListElement::Empty { comma_pos }) => {
                        self.logger.empty_argument(comma_pos);
                        return Err(());
                    }
                    None => {
                        self.logger.missing_import_target(target.pos);
                        return Err(());
                    }
                };
                (name, path, function.pos, path_pos)
            }
            _ => {
                self.logger.invalid_import_target(target.pos);
                return Err(());
            }
        };
        let path = path.with_extension("sysc");
        let path = match path.canonicalize() {
            Ok(path) => path,
            Err(err) => {
                self.logger.cannot_read_file(path_pos, &path, err);
                return Err(());
            }
        };
        if self.import_chain.insert(path.clone()) {
            let result = self.read_file(&path);
            self.import_chain.remove(&path);
            match result {
                Ok(n) => Ok((name, name_pos, n)),
                Err(err) => {
                    self.logger.cannot_read_file(path_pos, &path, err);
                    Err(())
                }
            }
        } else {
            self.logger.circular_imports(path_pos, &path);
            Err(())
        }
    }

    fn declare_structure(
        &mut self,
        ast::StructureName {
            keyword_struct_pos,
            name_and_pos: name,
        }: ast::StructureName,
        context: &mut Context,
    ) {
        let Some((name, pos)) = name else {
            self.logger.missing_structure_name(keyword_struct_pos);
            return;
        };
        match context.items.entry(name) {
            std::collections::hash_map::Entry::Occupied(mut entry) => {
                if let (Some(prev_pos), _) = entry.get() {
                    self.logger.duplicate_definition(pos, prev_pos.clone());
                } else {
                    entry.insert((
                        Some(pos),
                        Item::Ty(ir::Ty::Constructor(ir::TyConstructor::Structure(
                            self.num_structures,
                        ))),
                    ));
                }
            }
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert((
                    Some(pos),
                    Item::Ty(ir::Ty::Constructor(ir::TyConstructor::Structure(
                        self.num_structures,
                    ))),
                ));
                self.num_structures += 1;
            }
        }
    }

    fn declare_function(
        &mut self,
        ast::FunctionName {
            keyword_pos,
            name_and_pos: name,
            is_method,
        }: ast::FunctionName,
        context: &mut Context,
    ) {
        let Some((name, pos)) = name else {
            self.logger.missing_function_name(keyword_pos);
            return;
        };
        if is_method {
            context
                .methods
                .entry(name)
                .or_insert_with(Vec::new)
                .push(ir::Function::UserDefined(self.num_functions));
        } else {
            match context.items.entry(name) {
                std::collections::hash_map::Entry::Occupied(mut entry) => {
                    let (prev_pos, item) = entry.get_mut();
                    if let Item::Function(functions) = item {
                        functions.push(ir::Function::UserDefined(self.num_functions));
                    } else {
                        self.logger
                            .duplicate_definition(pos, prev_pos.clone().unwrap());
                    }
                }
                std::collections::hash_map::Entry::Vacant(entry) => {
                    entry.insert((
                        Some(pos),
                        Item::Function(vec![ir::Function::UserDefined(self.num_functions)]),
                    ));
                }
            }
        }
        self.num_functions += 1;
    }
}
