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
 * Parses source files and emits intermediate representation ([`ir`]).
 */

mod ast;
mod chars_peekable;
mod context;
mod parser;

use std::collections::{HashMap, HashSet};
use std::io::Read;
use std::path::{Path, PathBuf};

use crate::{ir, log};
use chars_peekable::CharsPeekable;
use context::Context;

/**
 * Reads the file specified by `root_file_path` and any other files it imports,
 * and emits intermediate representation defined in [`ir`].
 */
pub fn read_input(root_file_path: &Path) -> Result<ir::Program, ()> {
    let root_file_path = root_file_path.with_extension("sysc");
    let root_file_path = match root_file_path.canonicalize() {
        Ok(path) => path,
        Err(err) => {
            log::root_file_not_found(&root_file_path, err);
            return Err(());
        }
    };
    let mut reader = Reader {
        num_structures: 0,
        ir_program: ir::Program {
            structure_tys: Vec::new(),
            structure_definitions: Vec::new(),
        },
        exports: Vec::new(),
        logger: log::Logger::new(Box::new(std::io::stderr())),
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
     * The target which [`read_file`](Reader::read_file) stores the results in.
     */
    ir_program: ir::Program,
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
pub enum Item {
    /**
     * References another file by its postorder index.
     */
    Import(usize),
    Constant(ir::Constant),
    Parameter(usize, usize),
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
        let result = parser::parse_file(&mut chars_peekable);
        self.logger.files.push(log::File {
            path: path.to_path_buf(),
            lines: chars_peekable.lines(),
            content,
        });
        let ast_file = match result {
            Ok(ast) => ast,
            Err(err) => todo!(),
        };
        let mut context = Context {
            items: HashMap::new(),
        };
        for ast::WithExtraTokens {
            content: ast_import,
            extra_tokens_pos,
        } in ast_file.imports
        {
            if let Ok((name, Some(index))) = self.import_file(ast_import, path.parent().unwrap()) {
                match context.items.entry(name) {
                    std::collections::hash_map::Entry::Occupied(mut entry) => todo!(),
                    std::collections::hash_map::Entry::Vacant(entry) => {
                        entry.insert(Item::Import(index));
                    }
                }
            }
        }
        for name in ast_file.structure_names {
            self.declare_structure(name, &mut context);
        }
        for ast::WithExtraTokens {
            content: ast_statement,
            extra_tokens_pos,
        } in ast_file.top_level_statements
        {
            match ast_statement {
                ast::TopLevelStatement::StructureDefinition(structure_definition) => {
                    let (ty, definition) = context
                        .translate_structure_definition(
                            structure_definition,
                            &self.exports,
                            &mut self.logger,
                        )
                        .unwrap();
                    self.ir_program.structure_tys.push(ty);
                    self.ir_program.structure_definitions.push(definition);
                }
                ast::TopLevelStatement::FunctionDefinition(function_definition) => todo!(),
                ast::TopLevelStatement::Statement(statement) => todo!(),
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
    ) -> Result<(String, Option<usize>), ()> {
        let Some(target) = target else {
            todo!();
        };
        let (name, path) = match target.term {
            ast::Term::Identifier(name) => {
                let path = parent_directory.join(&name);
                (name, path)
            }
            ast::Term::FunctionCall {
                function,
                arguments,
            } => {
                let name = match function.term {
                    ast::Term::Identifier(name) => name,
                    _ => todo!(),
                };
                let (path, path_pos) = match arguments.into_iter().next() {
                    Some(ast::ListElement::NonEmpty(argument)) => match argument.term {
                        ast::Term::StringLiteral(components) => {
                            let mut path = String::new();
                            for component in components {
                                match component {
                                    ast::StringLiteralComponent::PlaceHolder { .. } => todo!(),
                                    ast::StringLiteralComponent::String(value) => {
                                        path.push_str(&value);
                                    }
                                }
                            }
                            (parent_directory.join(&path), argument.pos)
                        }
                        _ => todo!(),
                    },
                    _ => todo!(),
                };
                (name, path)
            }
            _ => todo!(),
        };
        let path = path.with_extension("sysc");
        let path = match path.canonicalize() {
            Ok(path) => path,
            Err(err) => todo!(),
        };
        if self.import_chain.insert(path.clone()) {
            let result = self.read_file(&path);
            self.import_chain.remove(&path);
            match result {
                Ok(n) => Ok((name, n)),
                Err(err) => todo!(),
            }
        } else {
            todo!();
        }
    }

    fn declare_structure(
        &mut self,
        ast::StructureName {
            keyword_struct_pos,
            name_and_pos,
        }: ast::StructureName,
        context: &mut Context,
    ) {
        let Some((name, pos)) = name_and_pos else {
            todo!();
        };
        match context.items.entry(name) {
            std::collections::hash_map::Entry::Occupied(_) => todo!(),
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(Item::Constant(ir::Constant::Structure(self.num_structures)));
                self.num_structures += 1;
            }
        }
    }
}
