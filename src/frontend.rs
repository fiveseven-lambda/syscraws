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

use std::collections::{HashMap, HashSet};
use std::io::Read;
use std::path::{Path, PathBuf};

use crate::{backend, log};
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
        definitions: backend::Definitions {
            structures: Vec::new(),
            functions: vec![
                (
                    backend::FunctionTy {
                        num_ty_parameters: 0,
                        parameters_ty: vec![
                            backend::Ty::Application {
                                constructor: backend::TyConstructor::Integer,
                                arguments: vec![],
                            },
                            backend::Ty::Application {
                                constructor: backend::TyConstructor::Integer,
                                arguments: vec![],
                            },
                        ],
                        return_ty: backend::Ty::Application {
                            constructor: backend::TyConstructor::Integer,
                            arguments: vec![],
                        },
                    },
                    backend::Function::IAdd,
                ),
                (
                    backend::FunctionTy {
                        num_ty_parameters: 1,
                        parameters_ty: vec![backend::Ty::Application {
                            constructor: backend::TyConstructor::Reference,
                            arguments: vec![backend::Ty::Parameter(0)],
                        }],
                        return_ty: backend::Ty::Parameter(0),
                    },
                    backend::Function::Deref,
                ),
            ],
            function_definitions: Vec::new(),
            num_global_variables: 0,
        },
        num_functions: 0,
        exported_items: Vec::new(),
        methods: HashMap::from([(String::from("add"), 0)]),
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
    definitions: backend::Definitions,
    num_functions: usize,
    exported_items: Vec<HashMap<String, Item>>,
    methods: HashMap<String, usize>,
    files: Vec<log::File>,
    /**
     * Used in [`Reader::read_file`] to avoid reading the same file multiple
     * times.
     */
    file_indices: HashMap<PathBuf, usize>,
    /**
     * Used in [`Reader::handle_import`] to detect circular imports.
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
            Ok(ast::File {
                imports,
                structure_definitions,
                function_names,
                top_level_statements,
            }) => {
                let mut named_items = HashMap::new();
                for import in imports {
                    self.translate_import(import, path.parent().unwrap(), &mut named_items, &file);
                }
                for structure_definition in structure_definitions {
                    self.translate_structure_definition(
                        structure_definition,
                        &mut named_items,
                        &file,
                    );
                }
                for name in function_names {
                    self.translate_function_name(name, &mut named_items, &file);
                }
                let mut global = Context {
                    variables: HashMap::new(),
                    num_variables: 0,
                };
                let mut global_scope = Vec::new();
                for statement in top_level_statements {
                    match statement {
                        ast::TopLevelStatement::FunctionDefinition(function_definition) => {
                            self.translate_function_definition(
                                function_definition,
                                &global.variables,
                                &mut named_items,
                            );
                        }
                        ast::TopLevelStatement::Statement(statement) => {
                            self.translate_statement(
                                &mut global,
                                statement,
                                &mut global_scope,
                                &HashMap::new(),
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

    fn translate_import(
        &mut self,
        ast::Import {
            keyword_import_pos,
            target,
        }: ast::Import,
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
                    _ => {
                        log::invalid_import_target(target.pos, file);
                        self.num_errors += 1;
                        return;
                    }
                };
                let path = match arguments.into_iter().next() {
                    Some(ast::ListElement::NonEmpty(argument)) => match argument.term {
                        ast::Term::StringLiteral(components) => {
                            let mut path = String::new();
                            for component in components {
                                match component {
                                    ast::StringLiteralComponent::PlaceHolder { .. } => {
                                        log::invalid_import_target(target.pos, file);
                                        self.num_errors += 1;
                                        return;
                                    }
                                    ast::StringLiteralComponent::String(value) => {
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
                    Some(ast::ListElement::Empty { comma_pos }) => {
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

    fn translate_structure_definition(
        &mut self,
        ast::StructureDefinition {
            keyword_struct_pos,
            name,
            fields,
        }: ast::StructureDefinition,
        named_items: &mut HashMap<String, Item>,
        file: &log::File,
    ) {
        let Some(name) = name else {
            todo!("Missing structure name");
        };
        let mut ty_parameters = HashMap::new();
        let name = match name.term {
            ast::Term::Identifier(name) => name,
            ast::Term::TypeParameters {
                term_left,
                parameters,
            } => {
                let name = match term_left.term {
                    ast::Term::Identifier(name) => name,
                    _ => todo!(),
                };
                for parameter in parameters {
                    match parameter {
                        ast::ListElement::Empty { comma_pos } => todo!(),
                        ast::ListElement::NonEmpty(parameter) => match parameter.term {
                            ast::Term::Identifier(name) => {
                                let new_index = ty_parameters.len();
                                ty_parameters.insert(name, new_index);
                            }
                            _ => todo!(),
                        },
                    }
                }
                name
            }
            _ => todo!(),
        };
        let new_index = self.definitions.structures.len();
        self.definitions.structures.push(backend::Structure {
            num_ty_parameters: ty_parameters.len(),
            fields_ty: Vec::new(),
        });
        named_items.insert(
            name,
            if ty_parameters.is_empty() {
                Item::Ty(backend::Ty::Application {
                    constructor: backend::TyConstructor::UserDefined(new_index),
                    arguments: vec![],
                })
            } else {
                Item::TyConstructor(backend::TyConstructor::UserDefined(new_index))
            },
        );
    }

    fn translate_function_name(
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

    fn translate_function_definition(
        &mut self,
        ast::FunctionDefinition {
            ty_parameters,
            parameters,
            return_ty,
            body,
        }: ast::FunctionDefinition,
        global_variables: &HashMap<String, usize>,
        named_items: &mut HashMap<String, Item>,
    ) {
        let mut local = Context {
            variables: HashMap::new(),
            num_variables: 0,
        };
        let mut local_scope = Vec::new();
        let mut ty_parameters_name = HashMap::new();
        if let Some(ty_parameters) = ty_parameters {
            for (i, ty_parameter) in ty_parameters.into_iter().enumerate() {
                match ty_parameter {
                    ast::ListElement::NonEmpty(ty_parameter) => {
                        if let ast::Term::Identifier(name) = ty_parameter.term {
                            ty_parameters_name.insert(name, i);
                        }
                    }
                    ast::ListElement::Empty { comma_pos } => {
                        todo!();
                    }
                }
            }
        }
        for statement in body {
            self.translate_statement(
                &mut local,
                statement,
                &mut local_scope,
                &ty_parameters_name,
                Some(global_variables),
                &named_items,
                &self.exported_items,
            );
        }
    }

    fn translate_statement(
        &self,
        context: &mut Context,
        statement: ast::Statement,
        scope: &mut Vec<(String, Option<usize>)>,
        ty_parameters: &HashMap<String, usize>,
        global_variables: Option<&HashMap<String, usize>>,
        named_items: &HashMap<String, Item>,
        exported_items: &Vec<HashMap<String, Item>>,
    ) {
        match statement {
            ast::Statement::Term(term) => {
                let expr = match global_variables {
                    Some(global_variables) => self.translate_item(
                        term,
                        named_items,
                        ty_parameters,
                        Some(&context.variables),
                        global_variables,
                        exported_items,
                    ),
                    None => self.translate_item(
                        term,
                        named_items,
                        ty_parameters,
                        None,
                        &context.variables,
                        exported_items,
                    ),
                };
            }
            ast::Statement::VariableDeclaration(name) => {
                let Some(name) = name else {
                    todo!();
                };
                let ast::Term::Identifier(name) = name.term else {
                    todo!();
                };
                let new_index = context.num_variables;
                let prev_index = context.variables.insert(name.clone(), new_index);
                scope.push((name, prev_index));
                context.num_variables += 1;
            }
            ast::Statement::While {
                keyword_while_pos,
                condition,
                body,
            } => {
                let mut body_scope = Vec::new();
                for stmt in body {
                    self.translate_statement(
                        context,
                        stmt,
                        &mut body_scope,
                        ty_parameters,
                        global_variables,
                        named_items,
                        exported_items,
                    );
                }
                for (name, prev_index) in body_scope.into_iter().rev() {
                    match prev_index {
                        Some(prev_index) => context.variables.insert(name, prev_index),
                        None => context.variables.remove(&name),
                    };
                }
            }
        }
    }

    fn translate_item(
        &self,
        item: ast::TermWithPos,
        named_items: &HashMap<String, Item>,
        ty_parameters: &HashMap<String, usize>,
        local_variables: Option<&HashMap<String, usize>>,
        global_variables: &HashMap<String, usize>,
        exported_items: &Vec<HashMap<String, Item>>,
    ) -> Result<Item, ()> {
        match item.term {
            ast::Term::Identifier(name) => {
                if let Some(local_variables) = local_variables {
                    if let Some(&index) = local_variables.get(&name) {
                        return Ok(Item::LValue(backend::Expression::LocalVariable(index)));
                    }
                }
                if let Some(&index) = global_variables.get(&name) {
                    return Ok(Item::LValue(backend::Expression::GlobalVariable(index)));
                }
                if let Some(&index) = ty_parameters.get(&name) {
                    return Ok(Item::Ty(backend::Ty::Parameter(index)));
                }
                if let Some(item) = named_items.get(&name) {
                    return Ok(item.clone());
                }
                Err(())
            }
            ast::Term::IntegerTy => Ok(Item::Ty(backend::Ty::Application {
                constructor: backend::TyConstructor::Integer,
                arguments: vec![],
            })),
            ast::Term::FloatTy => Ok(Item::Ty(backend::Ty::Application {
                constructor: backend::TyConstructor::Float,
                arguments: vec![],
            })),
            ast::Term::TypeParameters {
                term_left,
                parameters,
            } => {
                let term_left = self.translate_item(
                    *term_left,
                    named_items,
                    ty_parameters,
                    local_variables,
                    global_variables,
                    exported_items,
                );
                let mut translated_parameters = Some(Vec::new());
                for parameter in parameters {
                    let translated_parameter = match parameter {
                        ast::ListElement::NonEmpty(parameter) => self.translate_item(
                            parameter,
                            named_items,
                            ty_parameters,
                            local_variables,
                            global_variables,
                            exported_items,
                        ),
                        ast::ListElement::Empty { comma_pos } => todo!(),
                    };
                    if let (Some(translated_parameters), Ok(translated_parameter)) =
                        (&mut translated_parameters, translated_parameter)
                    {
                        if let Item::Ty(ty) = translated_parameter {
                            translated_parameters.push(ty);
                        }
                    }
                }
                match (term_left, translated_parameters) {
                    (Ok(Item::TyConstructor(constructor)), Some(arguments)) => {
                        Ok(Item::Ty(backend::Ty::Application {
                            constructor,
                            arguments,
                        }))
                    }
                    _ => Err(()),
                }
            }
            ast::Term::FieldByName { term_left, name } => {
                let term_left = self.translate_item(
                    *term_left,
                    named_items,
                    ty_parameters,
                    local_variables,
                    global_variables,
                    exported_items,
                )?;
                match term_left {
                    Item::Import(index) => {
                        if let Some(item) = exported_items[index].get(&name) {
                            Ok(item.clone())
                        } else {
                            Err(())
                        }
                    }
                    _ => todo!(),
                }
            }
            _ => todo!(),
        }
    }
}

struct Context {
    variables: HashMap<String, usize>,
    num_variables: usize,
}

impl Context {}

#[derive(Clone)]
enum Item {
    Import(usize),
    Function(Vec<usize>),
    Ty(backend::Ty),
    TyConstructor(backend::TyConstructor),
    GlobalVariable(usize),
    LValue(backend::Expression),
    RValue(backend::Expression),
}
