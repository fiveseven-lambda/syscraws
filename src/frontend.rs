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
use ast::*;

use std::collections::{HashMap, HashSet};
use std::io::Read;
use std::path::{Path, PathBuf};

use crate::log;
use chars_peekable::CharsPeekable;

/**
 * Reads the file specified by `root_file_path` and any other files it
 * imports, and passes them to `backend`.
 */
pub fn read_input(root_file_path: &Path) {
    let root_file_path = root_file_path.with_extension("sysc");
    let root_file_path = match root_file_path.canonicalize() {
        Ok(path) => path,
        Err(err) => {
            log::root_file_not_found(&root_file_path, err);
            return;
        }
    };
    let mut reader = Reader {
        num_functions: 0,
        exported_items: Vec::new(),
        files: Vec::new(),
        file_indices: HashMap::new(),
        import_chain: HashSet::from([root_file_path.clone()]),
        num_errors: 0,
    };
    if let Err(err) = reader.read_file(&root_file_path) {
        log::cannot_read_root_file(&root_file_path, err);
        reader.num_errors += 1;
    }
    if reader.num_errors > 0 {
        log::aborting(reader.num_errors);
        return;
    }
}

/**
 * A structure to read files recursively.
 */
struct Reader {
    num_functions: usize,
    exported_items: Vec<HashMap<String, Item>>,
    files: Vec<log::File>,
    /**
     * Used in [`Reader::read_file`] to avoid reading the same file multiple
     * times.
     */
    file_indices: HashMap<PathBuf, usize>,
    /**
     * Used in [`Parser::parse_import`] to detect circular imports.
     */
    import_chain: HashSet<PathBuf>,
    num_errors: u32,
}

impl Reader {
    fn read_file(&mut self, path: &Path) -> Result<usize, std::io::Error> {
        if let Some(&index) = self.file_indices.get(path) {
            // The file specified by `path` was already read.
            // Since circular imports should have been detected in `parse_imports`,
            // this is not circular imports but diamond imports.
            return Ok(index);
        }
        let mut file = std::fs::File::open(path)?;
        let mut content = String::new();
        file.read_to_string(&mut content)?;
        let mut chars_peekable = CharsPeekable::new(&content);
        let result = ast::parse_file(&mut chars_peekable);
        let file = log::File {
            path: path.to_path_buf(),
            lines: chars_peekable.lines(),
            content,
        };
        match result {
            Ok(File {
                imports,
                structure_definitions,
                function_names,
                top_level_statements,
            }) => {
                let mut named_items = HashMap::new();
                for import in imports {
                    self.handle_import(import, path.parent().unwrap(), &mut named_items, &file);
                }
                for structure in structure_definitions {}
                for name in function_names {
                    self.handle_function_name(name, &mut named_items, &file);
                }
                let mut global = Context {
                    variables: HashMap::new(),
                    num_variables: 0,
                };
                let mut global_scope = Vec::new();
                for statement in top_level_statements {
                    match statement {
                        TopLevelStatement::FunctionDefinition {
                            type_parameters: opt_type_parameters,
                            parameters: opt_parameters,
                            return_ty: opt_ret_ty,
                            body,
                        } => {
                            let mut local = Context {
                                variables: HashMap::new(),
                                num_variables: 0,
                            };
                            let mut local_scope = Vec::new();
                            for statement in body {
                                local.translate_statement(
                                    statement,
                                    &mut local_scope,
                                    Some(&global.variables),
                                    &named_items,
                                    &self.exported_items,
                                );
                            }
                        }
                        TopLevelStatement::Statement(statement) => {
                            global.translate_statement(
                                statement,
                                &mut global_scope,
                                None,
                                &named_items,
                                &self.exported_items,
                            );
                        }
                    }
                }
                for (name, index) in global.variables {
                    named_items.insert(name, Item::GlobalVariable(index));
                }
                self.exported_items.push(named_items);
                self.files.push(file);
            }
            Err(err) => {
                err.eprint(&file);
                self.num_errors += 1;
            }
        };
        let new_index = self.file_indices.len();
        self.file_indices.insert(path.to_path_buf(), new_index);
        Ok(new_index)
    }

    fn handle_import(
        &mut self,
        Import {
            keyword_import_pos,
            target,
        }: Import,
        parent_directory: &Path,
        named_items: &mut HashMap<String, Item>,
        file: &log::File,
    ) {
        let Some(target) = target else {
            log::missing_import_name(keyword_import_pos, &file);
            self.num_errors += 1;
            return;
        };
        let (name, path) = match target.term {
            Term::Identifier(name) => {
                let path = parent_directory.join(&name);
                (name, path)
            }
            Term::FunctionCall {
                function,
                arguments,
            } => {
                let name = match function.term {
                    Term::Identifier(name) => name,
                    _ => {
                        log::invalid_import_target(target.pos, file);
                        self.num_errors += 1;
                        return;
                    }
                };
                let path = match arguments.into_iter().next() {
                    Some(ListElement::NonEmpty(argument)) => match argument.term {
                        Term::StringLiteral(components) => {
                            let mut path = String::new();
                            for component in components {
                                match component {
                                    StringLiteralComponent::PlaceHolder { .. } => {
                                        log::invalid_import_target(target.pos, file);
                                        self.num_errors += 1;
                                        return;
                                    }
                                    StringLiteralComponent::String(value) => {
                                        path.push_str(&value);
                                    }
                                }
                            }
                            parent_directory.join(&path)
                        }
                        _ => {
                            log::invalid_import_target(target.pos, file);
                            self.num_errors += 1;
                            return;
                        }
                    },
                    Some(ListElement::Empty { comma_pos }) => {
                        log::empty_element(comma_pos, file);
                        self.num_errors += 1;
                        return;
                    }
                    None => {
                        log::invalid_import_target(target.pos, file);
                        self.num_errors += 1;
                        return;
                    }
                };
                (name, path)
            }
            _ => {
                log::invalid_import_target(target.pos, file);
                self.num_errors += 1;
                return;
            }
        };
        let path = path.with_extension("sysc");
        let path = match path.canonicalize() {
            Ok(path) => path,
            Err(err) => {
                log::cannot_read_file(&path, file, err);
                self.num_errors += 1;
                return;
            }
        };
        if self.import_chain.insert(path.clone()) {
            match self.read_file(&path) {
                Ok(n) => {
                    named_items.insert(name, Item::Import(n));
                }
                Err(err) => {
                    log::cannot_read_file(&path, &file, err);
                    self.num_errors += 1;
                }
            }
            self.import_chain.remove(&path);
        } else {
            log::circular_imports(target.pos, &file);
            self.num_errors += 1;
        }
    }

    fn handle_function_name(
        &mut self,
        name: Option<String>,
        named_items: &mut HashMap<String, Item>,
        file: &log::File,
    ) {
        let Some(name) = name else {
            todo!("Missing function name");
        };
        if let Item::Function(functions) = named_items
            .entry(name)
            .or_insert_with(|| Item::Function(Vec::new()))
        {
            functions.push(self.num_functions);
        } else {
            self.num_errors += 1;
            todo!("Duplicate definition");
        }
        self.num_functions += 1;
    }
}

struct Context {
    variables: HashMap<String, usize>,
    num_variables: usize,
}

impl Context {
    fn translate_statement(
        &mut self,
        statement: Statement,
        scope: &mut Vec<(String, Option<usize>)>,
        global_variables: Option<&HashMap<String, usize>>,
        named_items: &HashMap<String, Item>,
        exported_items: &Vec<HashMap<String, Item>>,
    ) {
        match statement {
            Statement::Expression(term) => {
                let expr = match global_variables {
                    Some(global_variables) => translate_expression(
                        term,
                        Some(&self.variables),
                        global_variables,
                        named_items,
                        exported_items,
                    ),
                    None => translate_expression(
                        term,
                        None,
                        &self.variables,
                        named_items,
                        exported_items,
                    ),
                };
            }
            Statement::VariableDeclaration(name) => {
                let Term::Identifier(name) = name.term else {
                    todo!();
                };
                let new_index = self.num_variables;
                let prev_index = self.variables.insert(name.clone(), new_index);
                scope.push((name, prev_index));
                self.num_variables += 1;
            }
            Statement::While {
                keyword_while_pos,
                condition,
                body,
            } => {
                let mut body_scope = Vec::new();
                for stmt in body {
                    self.translate_statement(
                        stmt,
                        &mut body_scope,
                        global_variables,
                        named_items,
                        exported_items,
                    );
                }
                for (name, prev_index) in body_scope.into_iter().rev() {
                    match prev_index {
                        Some(prev_index) => self.variables.insert(name, prev_index),
                        None => self.variables.remove(&name),
                    };
                }
            }
        }
    }
}

fn translate_expression(
    term: TermWithPos,
    local_variables: Option<&HashMap<String, usize>>,
    global_variables: &HashMap<String, usize>,
    named_items: &HashMap<String, Item>,
    exported_items: &Vec<HashMap<String, Item>>,
) {
    match term.term {
        Term::Identifier(name) => {}
        _ => todo!(),
    }
}

#[derive(Debug)]
enum Item {
    Import(usize),
    Function(Vec<usize>),
    Type(usize),
    GlobalVariable(usize),
}

enum Type {
    Builtin,
    Struct(usize),
}
