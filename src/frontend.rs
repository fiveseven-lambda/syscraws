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
pub fn read_input(root_file_path: &Path) -> Result<backend::Definitions, ()> {
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
        num_functions: 0,
        definitions: backend::Definitions::builtin(),
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
        return Err(());
    }
    Ok(reader.definitions)
}

/**
 * A structure used in [`read_input`].
 */
struct Reader {
    /**
     * Total number of structures defined in all files. Used and updated by
     * [`register_structure_name`].
     */
    num_structures: usize,
    /**
     * Total number of functions defined in all files. Used and updated by
     * [`register_function_name`].
     */
    num_functions: usize,
    /**
     * The target which [`Reader::read_file`] stores the results in.
     */
    definitions: backend::Definitions,
    /**
     * Items exported from each file.
     */
    exported_items: Vec<HashMap<String, Item>>,
    /**
     * Debug information of each file.
     */
    files: Vec<log::File>,
    /**
     * Used in [`Reader::read_file`] to avoid reading the same file multiple
     * times.
     */
    file_indices: HashMap<PathBuf, usize>,
    /**
     * Used in [`Reader::import_file`] to detect circular imports.
     */
    import_chain: HashSet<PathBuf>,
    /**
     * Number of errors while reading files.
     */
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
            Ok(ast) => {
                let mut named_items = HashMap::new();
                for import in ast.imports {
                    if let Ok((name, index)) =
                        self.import_file(import, path.parent().unwrap(), &file)
                    {
                        named_items.insert(name, Item::Import(index));
                    }
                }
                for name in ast.structure_names {
                    register_structure_name(
                        name,
                        &mut self.num_structures,
                        &mut named_items,
                        &file,
                        &mut self.num_errors,
                    );
                }
                for name in ast.function_names {
                    register_function_name(
                        name,
                        &mut self.num_functions,
                        &mut named_items,
                        &file,
                        &mut self.num_errors,
                    );
                }
                let mut global_variables = HashMap::new();
                let mut num_global_variables = 0;
                let mut global_scope = Vec::new();
                let global_ty_parameters = HashMap::new();
                let mut global_statements = Ok(Vec::new());
                for statement in ast.top_level_statements {
                    match statement {
                        ast::TopLevelStatement::StructureDefinition(structure_definition) => {
                            let (kind, definition) = translate_structure_definition(
                                structure_definition,
                                &mut named_items,
                                &self.exported_items,
                                &file,
                                &mut self.num_errors,
                            );
                            let new_index = self.definitions.structures.len();
                            self.definitions
                                .tys_kind
                                .insert(backend::TyConstructor::Structure(new_index), kind);
                            self.definitions.structures.push(definition);
                        }
                        ast::TopLevelStatement::FunctionDefinition(function_definition) => {
                            if let Some((ty, definition)) = translate_function_definition(
                                function_definition,
                                &global_variables,
                                &named_items,
                                &self.exported_items,
                                &file,
                                &mut self.num_errors,
                            ) {
                                let new_index = self.definitions.functions.len();
                                self.definitions
                                    .functions_ty
                                    .insert(backend::Function::UserDefined(new_index), ty);
                                self.definitions.functions.push(definition);
                            }
                        }
                        ast::TopLevelStatement::Statement(statement) => {
                            match translate_statement(
                                statement,
                                &mut global_variables,
                                &mut num_global_variables,
                                &mut global_scope,
                                &global_ty_parameters,
                                None,
                                &named_items,
                                &self.exported_items,
                                &file,
                                &mut self.num_errors,
                            ) {
                                Some(stmt) => {
                                    if let Some(stmt) = stmt {
                                        if let Ok(global_statements) = &mut global_statements {
                                            global_statements.push(stmt);
                                        }
                                    }
                                }
                                None => global_statements = Err(()),
                            }
                        }
                    }
                }
                for (name, index) in global_variables {
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

    fn import_file(
        &mut self,
        ast::Import {
            keyword_import_pos,
            target,
            extra_tokens_pos,
        }: ast::Import,
        parent_directory: &Path,
        file: &log::File,
    ) -> Result<(String, usize), ()> {
        let Some(target) = target else {
            eprintln!("Missing import target after `import` at {keyword_import_pos}.");
            file.quote_pos(keyword_import_pos);
            self.num_errors += 1;
            return Err(());
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
                        eprintln!("Invalid import target at {}.", target.pos);
                        file.quote_pos(target.pos);
                        self.num_errors += 1;
                        return Err(());
                    }
                };
                let path = match arguments.into_iter().next() {
                    Some(ast::ListElement::NonEmpty(argument)) => match argument.term {
                        ast::Term::StringLiteral(components) => {
                            let mut path = String::new();
                            for component in components {
                                match component {
                                    ast::StringLiteralComponent::PlaceHolder { .. } => {
                                        eprintln!("Import path must not contain a placeholder.");
                                        file.quote_pos(argument.pos);
                                        self.num_errors += 1;
                                        return Err(());
                                    }
                                    ast::StringLiteralComponent::String(value) => {
                                        path.push_str(&value);
                                    }
                                }
                            }
                            parent_directory.join(&path)
                        }
                        _ => {
                            eprintln!("Invalid import target at {}.", target.pos);
                            file.quote_pos(target.pos);
                            self.num_errors += 1;
                            return Err(());
                        }
                    },
                    Some(ast::ListElement::Empty { comma_pos }) => {
                        eprintln!("Empty argument before comma at {comma_pos}.");
                        file.quote_pos(comma_pos);
                        self.num_errors += 1;
                        return Err(());
                    }
                    None => {
                        eprintln!("Missing import path at {}.", target.pos);
                        file.quote_pos(target.pos);
                        self.num_errors += 1;
                        return Err(());
                    }
                };
                (name, path)
            }
            _ => {
                eprintln!("Invalid import target at {}.", target.pos);
                file.quote_pos(target.pos);
                self.num_errors += 1;
                return Err(());
            }
        };
        if let Some(extra_tokens_pos) = extra_tokens_pos {
            eprintln!("Extra tokens at {}.", extra_tokens_pos);
            file.quote_pos(extra_tokens_pos);
            self.num_errors += 1;
            return Err(());
        }
        let path = path.with_extension("sysc");
        let path = match path.canonicalize() {
            Ok(path) => path,
            Err(err) => {
                eprintln!("Cannot read file `{}`. {}", path.display(), err);
                file.quote_line(keyword_import_pos.line());
                self.num_errors += 1;
                return Err(());
            }
        };
        if self.import_chain.insert(path.clone()) {
            let result = self.read_file(&path);
            self.import_chain.remove(&path);
            match result {
                Ok(n) => Ok((name, n)),
                Err(err) => {
                    eprintln!("Cannot read file `{}`. {}", path.display(), err);
                    file.quote_line(keyword_import_pos.line());
                    self.num_errors += 1;
                    Err(())
                }
            }
        } else {
            eprintln!("Circular imports of `{}`.", path.display());
            file.quote_line(keyword_import_pos.line());
            self.num_errors += 1;
            Err(())
        }
    }
}

fn register_structure_name(
    ast::StructureName {
        keyword_struct_pos,
        name,
        extra_tokens_pos,
    }: ast::StructureName,
    num_structures: &mut usize,
    named_items: &mut HashMap<String, Item>,
    file: &log::File,
    num_errors: &mut u32,
) {
    let Some(name) = name else {
        eprintln!(
            "Missing structure name after `struct` at {}.",
            keyword_struct_pos
        );
        file.quote_pos(keyword_struct_pos);
        *num_errors += 1;
        return;
    };
    match named_items.entry(name) {
        std::collections::hash_map::Entry::Occupied(entry) => {
            eprintln!("Duplicate definition of `{}`.", entry.key());
            file.quote_line(keyword_struct_pos.line());
            *num_errors += 1;
        }
        std::collections::hash_map::Entry::Vacant(entry) => {
            entry.insert(Item::Ty(backend::TyBuilder::Constructor(
                backend::TyConstructor::Structure(*num_structures),
            )));
            *num_structures += 1;
        }
    }
    if let Some(extra_tokens_pos) = extra_tokens_pos {
        eprintln!("Extra tokens at {}.", extra_tokens_pos);
        file.quote_pos(extra_tokens_pos);
        *num_errors += 1;
    }
}

fn register_function_name(
    ast::FunctionName {
        keyword_func_pos,
        name,
        extra_tokens_pos,
    }: ast::FunctionName,
    num_functions: &mut usize,
    named_items: &mut HashMap<String, Item>,
    file: &log::File,
    num_errors: &mut u32,
) {
    let Some(name) = name else {
        eprintln!(
            "Missing structure name after `func` at {}.",
            keyword_func_pos
        );
        file.quote_pos(keyword_func_pos);
        *num_errors += 1;
        return;
    };
    match named_items.entry(name) {
        std::collections::hash_map::Entry::Occupied(mut entry) => {
            if let Item::Function(functions) = entry.get_mut() {
                functions.push(backend::Function::UserDefined(*num_functions));
            } else {
                eprintln!("Duplicate definition of `{}`.", entry.key());
                file.quote_line(keyword_func_pos.line());
                *num_errors += 1;
            }
        }
        std::collections::hash_map::Entry::Vacant(entry) => {
            entry.insert(Item::Function(vec![backend::Function::UserDefined(
                *num_functions,
            )]));
        }
    }
    *num_functions += 1;
    if let Some(extra_tokens_pos) = extra_tokens_pos {
        eprintln!("Extra tokens at {}.", extra_tokens_pos);
        file.quote_pos(extra_tokens_pos);
        *num_errors += 1;
    }
}

fn translate_structure_definition(
    ast::StructureDefinition {
        ty_parameters,
        fields,
        extra_tokens_pos,
    }: ast::StructureDefinition,
    named_items: &HashMap<String, Item>,
    exported_items: &Vec<HashMap<String, Item>>,
    file: &log::File,
    num_errors: &mut u32,
) -> (backend::TyKind, backend::Structure) {
    let mut ty_parameters_name = HashMap::new();
    let kind = if let Some(ty_parameters) = ty_parameters {
        for ty_parameter in ty_parameters {
            match ty_parameter {
                ast::ListElement::NonEmpty(name) => match name.term {
                    ast::Term::Identifier(name) => {
                        let new_index = ty_parameters_name.len();
                        ty_parameters_name.insert(name, new_index);
                    }
                    _ => {
                        eprintln!("Invalid type parameter at {}.", name.pos);
                        file.quote_pos(name.pos);
                        *num_errors += 1;
                    }
                },
                ast::ListElement::Empty { comma_pos } => {
                    eprintln!("Empty type parameter before comma at {}.", comma_pos);
                    file.quote_pos(comma_pos);
                    *num_errors += 1;
                }
            }
        }
        backend::TyKind::Abstraction {
            parameters: (0..ty_parameters_name.len()).fold(backend::TyListKind::Nil, |tail, _| {
                backend::TyListKind::Cons(Box::new(backend::TyKind::Ty), Box::new(tail))
            }),
            ret: Box::new(backend::TyKind::Ty),
        }
    } else {
        backend::TyKind::Ty
    };
    let mut translated_fields_ty = Vec::new();
    for ast::StructureField {
        field,
        extra_tokens_pos,
    } in fields
    {
        match field.term {
            ast::Term::TypeAnnotation {
                term_left: _,
                colon_pos: _,
                term_right: Some(field_ty),
            } => {
                if let Some(ty) = translate_ty(
                    *field_ty,
                    named_items,
                    &ty_parameters_name,
                    &exported_items,
                    file,
                    num_errors,
                ) {
                    translated_fields_ty.push(ty);
                }
            }
            _ => {
                eprintln!("Invalid structure field at {}.", field.pos);
                file.quote_pos(field.pos);
            }
        }
        if let Some(extra_tokens_pos) = extra_tokens_pos {
            eprintln!("Extra tokens at {}.", extra_tokens_pos);
            file.quote_pos(extra_tokens_pos);
            *num_errors += 1;
        }
    }
    if let Some(extra_tokens_pos) = extra_tokens_pos {
        eprintln!("Extra tokens at {}.", extra_tokens_pos);
        file.quote_pos(extra_tokens_pos);
        *num_errors += 1;
    }
    (
        kind,
        backend::Structure {
            num_ty_parameters: ty_parameters_name.len(),
            fields_ty: translated_fields_ty,
        },
    )
}

fn translate_function_definition(
    ast::FunctionDefinition {
        ty_parameters,
        parameters,
        return_ty,
        body,
        extra_tokens_pos,
    }: ast::FunctionDefinition,
    global_variables: &HashMap<String, usize>,
    named_items: &HashMap<String, Item>,
    exported_items: &Vec<HashMap<String, Item>>,
    file: &log::File,
    num_errors: &mut u32,
) -> Option<(backend::FunctionTy, backend::FunctionDefinition)> {
    let mut ty_parameters_name = HashMap::new();
    if let Some(ty_parameters) = ty_parameters {
        for (i, ty_parameter) in ty_parameters.into_iter().enumerate() {
            match ty_parameter {
                ast::ListElement::NonEmpty(ty_parameter) => {
                    if let ast::Term::Identifier(name) = ty_parameter.term {
                        ty_parameters_name.insert(name, i);
                    } else {
                        eprintln!("Invalid type parameter at {}.", ty_parameter.pos);
                        file.quote_pos(ty_parameter.pos);
                        *num_errors += 1;
                    }
                }
                ast::ListElement::Empty { comma_pos } => {
                    eprintln!("Empty type parameter before comma at {}.", comma_pos);
                    file.quote_pos(comma_pos);
                    *num_errors += 1;
                }
            }
        }
    }
    let mut local_variables = HashMap::new();
    let mut num_local_variables = 0;
    let mut local_scope = Vec::new();
    let mut parameters_ty = Vec::new();
    if let Some(parameters) = parameters {
        for parameter in parameters {
            match parameter {
                ast::ListElement::NonEmpty(parameter) => match parameter.term {
                    ast::Term::TypeAnnotation {
                        term_left: parameter_name,
                        colon_pos,
                        term_right: parameter_ty,
                    } => {
                        match parameter_name.term {
                            ast::Term::Identifier(name) => {
                                match local_variables.entry(name.clone()) {
                                    std::collections::hash_map::Entry::Occupied(_) => {
                                        eprintln!(
                                            "Duplicate parameter name at {}.",
                                            parameter_name.pos
                                        );
                                        file.quote_pos(parameter_name.pos);
                                    }
                                    std::collections::hash_map::Entry::Vacant(entry) => {
                                        entry.insert(num_local_variables);
                                        local_scope.push((name, None));
                                        num_local_variables += 1;
                                    }
                                }
                            }
                            _ => {
                                eprintln!("Invalid parameter name at {}.", parameter_name.pos);
                                file.quote_pos(parameter_name.pos);
                                *num_errors += 1;
                            }
                        }
                        if let Some(parameter_ty) = parameter_ty {
                            if let Some(ty) = translate_ty(
                                *parameter_ty,
                                named_items,
                                &ty_parameters_name,
                                &exported_items,
                                file,
                                num_errors,
                            ) {
                                parameters_ty.push(ty);
                            }
                        } else {
                            eprintln!("Missing type after colon at {}.", colon_pos);
                            file.quote_pos(colon_pos);
                            *num_errors += 1;
                        }
                    }
                    _ => {
                        eprintln!("Invalid parameter at {}.", parameter.pos);
                        file.quote_pos(parameter.pos);
                        *num_errors += 1;
                    }
                },
                ast::ListElement::Empty { comma_pos } => {
                    eprintln!("Empty parameter before comma at {}.", comma_pos);
                    file.quote_pos(comma_pos);
                    *num_errors += 1;
                }
            }
        }
    } else {
        eprintln!("Missing parameter list.");
        *num_errors += 1;
    }
    let return_ty = if let Some(return_ty) = return_ty {
        if let Some(return_ty) = return_ty.ty {
            match translate_ty(
                return_ty,
                named_items,
                &ty_parameters_name,
                &exported_items,
                file,
                num_errors,
            ) {
                Some(ty) => ty,
                None => return None,
            }
        } else {
            eprintln!(
                "Missing return type after colon at {}.",
                return_ty.colon_pos
            );
            file.quote_pos(return_ty.colon_pos);
            *num_errors += 1;
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
        eprintln!("Extra tokens at {}.", extra_tokens_pos);
        file.quote_pos(extra_tokens_pos);
        *num_errors += 1;
    }
    let mut translated_body = Some(Vec::new());
    for statement in body {
        let translated_statement = translate_statement(
            statement,
            &mut local_variables,
            &mut num_local_variables,
            &mut local_scope,
            &ty_parameters_name,
            Some(global_variables),
            named_items,
            exported_items,
            file,
            num_errors,
        );
        match translated_statement {
            Some(Some(statement)) => {
                if let Some(translated_body) = &mut translated_body {
                    translated_body.push(statement);
                }
            }
            Some(None) => {}
            None => translated_body = None,
        }
    }
    Some((
        backend::FunctionTy {
            num_ty_parameters: ty_parameters_name.len(),
            parameters_ty,
            return_ty,
        },
        backend::FunctionDefinition {
            num_local_variables,
            body: translated_body?,
        },
    ))
}

fn translate_statement(
    statement: ast::Statement,
    variables: &mut HashMap<String, usize>,
    num_variables: &mut usize,
    scope: &mut Vec<(String, Option<usize>)>,
    ty_parameters: &HashMap<String, usize>,
    global_variables: Option<&HashMap<String, usize>>,
    named_items: &HashMap<String, Item>,
    exported_items: &Vec<HashMap<String, Item>>,
    file: &log::File,
    num_errors: &mut u32,
) -> Option<Option<backend::Statement>> {
    match statement {
        ast::Statement::Term(term) => {
            let term_pos = term.pos.clone();
            let expr = match global_variables {
                Some(global_variables) => translate_expression(
                    term,
                    named_items,
                    ty_parameters,
                    Some(&variables),
                    global_variables,
                    exported_items,
                    file,
                    num_errors,
                ),
                None => translate_expression(
                    term,
                    named_items,
                    ty_parameters,
                    None,
                    &variables,
                    exported_items,
                    file,
                    num_errors,
                ),
            };
            Some(expr.map(backend::Statement::Expr))
        }
        ast::Statement::VariableDeclaration {
            keyword_var_pos,
            term,
        } => {
            let Some(name) = term else {
                eprintln!("Missing variable name after `var` at {}.", keyword_var_pos);
                file.quote_pos(keyword_var_pos);
                return None;
            };
            match name.term {
                ast::Term::Identifier(name) => {
                    let prev_index = variables.insert(name.clone(), *num_variables);
                    scope.push((name, prev_index));
                    *num_variables += 1;
                    Some(None)
                }
                _ => {
                    eprintln!("Expected a variable name at {}.", name.pos);
                    file.quote_pos(name.pos);
                    return None;
                }
            }
        }
        ast::Statement::While {
            keyword_while_pos,
            condition,
            body,
        } => {
            let condition = if let Some(condition) = condition {
                let condition_pos = condition.pos.clone();
                match global_variables {
                    Some(global_variables) => translate_expression(
                        condition,
                        named_items,
                        ty_parameters,
                        Some(&variables),
                        global_variables,
                        exported_items,
                        file,
                        num_errors,
                    ),
                    None => translate_expression(
                        condition,
                        named_items,
                        ty_parameters,
                        None,
                        &variables,
                        exported_items,
                        file,
                        num_errors,
                    ),
                }
            } else {
                eprintln!("Missing condition after `while` at {}", keyword_while_pos);
                file.quote_pos(keyword_while_pos);
                None
            };
            let mut body_scope = Vec::new();
            let mut translated_stmts = Some(Vec::new());
            for stmt in body {
                match translate_statement(
                    stmt,
                    variables,
                    num_variables,
                    &mut body_scope,
                    ty_parameters,
                    global_variables,
                    named_items,
                    exported_items,
                    file,
                    num_errors,
                ) {
                    Some(stmt) => {
                        if let Some(stmt) = stmt {
                            if let Some(translated_stmts) = &mut translated_stmts {
                                translated_stmts.push(stmt);
                            }
                        }
                    }
                    None => translated_stmts = None,
                }
            }
            for (name, prev_index) in body_scope.into_iter().rev() {
                match prev_index {
                    Some(prev_index) => variables.insert(name, prev_index),
                    None => variables.remove(&name),
                };
            }
            (|| {
                Some(Some(backend::Statement::While(
                    condition?,
                    translated_stmts?,
                )))
            })()
        }
    }
}

fn translate_import(
    import: ast::TermWithPos,
    named_items: &HashMap<String, Item>,
    exported_items: &Vec<HashMap<String, Item>>,
    file: &log::File,
    num_errors: &mut u32,
) -> Option<usize> {
    let item = match import.term {
        ast::Term::Identifier(name) => match named_items.get(&name) {
            Some(item) => item,
            None => return None,
        },
        ast::Term::FieldByName { term_left, name } => {
            let file_index =
                translate_import(*term_left, named_items, exported_items, file, num_errors)?;
            match exported_items[file_index].get(&name) {
                Some(item) => item,
                None => return None,
            }
        }
        _ => return None,
    };
    match *item {
        Item::Import(n) => Some(n),
        _ => None,
    }
}

fn translate_ty(
    ty: ast::TermWithPos,
    named_items: &HashMap<String, Item>,
    ty_parameters: &HashMap<String, usize>,
    exported_items: &Vec<HashMap<String, Item>>,
    file: &log::File,
    num_errors: &mut u32,
) -> Option<backend::TyBuilder> {
    let item = match ty.term {
        ast::Term::IntegerTy => {
            return Some(backend::TyBuilder::Constructor(
                backend::TyConstructor::Integer,
            ))
        }
        ast::Term::FloatTy => {
            return Some(backend::TyBuilder::Constructor(
                backend::TyConstructor::Float,
            ))
        }
        ast::Term::Identifier(name) => {
            if let Some(&index) = ty_parameters.get(&name) {
                return Some(backend::TyBuilder::Parameter(index));
            }
            match named_items.get(&name) {
                Some(item) => item,
                None => return None,
            }
        }
        ast::Term::FieldByName { term_left, name } => {
            let file_index =
                translate_import(*term_left, named_items, exported_items, file, num_errors)?;
            match exported_items[file_index].get(&name) {
                Some(item) => item,
                None => return None,
            }
        }
        ast::Term::TypeParameters {
            term_left,
            parameters,
        } => {
            let term_left = translate_ty(
                *term_left,
                named_items,
                ty_parameters,
                exported_items,
                file,
                num_errors,
            );
            let mut translated_parameters = Some(Vec::new());
            for parameter in parameters {
                let translated_parameter = match parameter {
                    ast::ListElement::NonEmpty(parameter) => translate_ty(
                        parameter,
                        named_items,
                        ty_parameters,
                        exported_items,
                        file,
                        num_errors,
                    ),
                    ast::ListElement::Empty { comma_pos } => {
                        eprintln!("Empty type parameter before comma at {comma_pos}");
                        None
                    }
                };
                match translated_parameter {
                    Some(parameter) => {
                        if let Some(translated_parameters) = &mut translated_parameters {
                            translated_parameters.push(parameter);
                        };
                    }
                    None => translated_parameters = None,
                }
            }
            return (|| {
                Some(backend::TyBuilder::Application {
                    constructor: Box::new(term_left?),
                    arguments: translated_parameters?,
                })
            })();
        }
        _ => return None,
    };
    match item {
        Item::Ty(ty) => Some(ty.clone()),
        _ => None,
    }
}

fn translate_expression(
    expression: ast::TermWithPos,
    named_items: &HashMap<String, Item>,
    ty_parameters: &HashMap<String, usize>,
    local_variables: Option<&HashMap<String, usize>>,
    global_variables: &HashMap<String, usize>,
    exported_items: &Vec<HashMap<String, Item>>,
    file: &log::File,
    num_errors: &mut u32,
) -> Option<backend::Expression> {
    let item = match expression.term {
        ast::Term::Identifier(name) => {
            if let Some(local_variables) = local_variables {
                if let Some(&index) = local_variables.get(&name) {
                    return Some(backend::Expression::Function {
                        candidates: vec![backend::Function::Deref],
                        calls: vec![backend::Call {
                            arguments: vec![backend::Expression::LocalVariable(index)],
                        }],
                    });
                }
            }
            if let Some(&index) = global_variables.get(&name) {
                return Some(backend::Expression::Function {
                    candidates: vec![backend::Function::Deref],
                    calls: vec![backend::Call {
                        arguments: vec![backend::Expression::GlobalVariable(index)],
                    }],
                });
            }
            match named_items.get(&name) {
                Some(item) => item,
                None => return None,
            }
        }
        ast::Term::FunctionCall {
            function,
            arguments,
        } => {
            if let ast::Term::FieldByName { term_left, name } = function.term {
                let function_pos = term_left.pos.clone();
                let translated_function = translate_expression(
                    *term_left,
                    named_items,
                    ty_parameters,
                    local_variables,
                    global_variables,
                    exported_items,
                    file,
                    num_errors,
                );
                let mut translated_arguments = Vec::new();
                for argument in arguments {
                    match argument {
                        ast::ListElement::NonEmpty(argument) => {
                            if let Some(expression) = translate_expression(
                                argument,
                                named_items,
                                ty_parameters,
                                local_variables,
                                global_variables,
                                exported_items,
                                file,
                                num_errors,
                            ) {
                                translated_arguments.push(expression);
                            }
                        }
                        ast::ListElement::Empty { comma_pos } => {
                            eprintln!("Empty argument before comma at {comma_pos}");
                        }
                    }
                }
                let mut ret = translated_function?;
                if let backend::Expression::Function {
                    candidates: _,
                    calls,
                } = &mut ret
                {
                    calls.push(backend::Call {
                        arguments: translated_arguments,
                    });
                    return Some(ret);
                } else {
                    eprintln!("Not a function");
                    file.quote_pos(function_pos);
                    return None;
                }
            } else {
                todo!();
            }
        }
        ast::Term::TypeAnnotation {
            term_left,
            colon_pos,
            term_right,
        } => {
            translate_expression(
                *term_left,
                named_items,
                ty_parameters,
                local_variables,
                global_variables,
                exported_items,
                file,
                num_errors,
            );
            if let Some(ty) = term_right {
                translate_ty(
                    *ty,
                    named_items,
                    ty_parameters,
                    exported_items,
                    file,
                    num_errors,
                );
            } else {
                eprintln!("Missing type after colon at {colon_pos}");
                return None;
            }
            todo!();
        }
        _ => todo!(),
    };
    match item {
        Item::Function(candidates) => Some(backend::Expression::Function {
            candidates: candidates.clone(),
            calls: vec![],
        }),
        _ => todo!(),
    }
}

fn translate_reference(
    expression: ast::TermWithPos,
    named_items: &HashMap<String, Item>,
    ty_parameters: &HashMap<String, usize>,
    local_variables: Option<&HashMap<String, usize>>,
    global_variables: &HashMap<String, usize>,
    exported_items: &Vec<HashMap<String, Item>>,
    file: &log::File,
    num_errors: &mut u32,
) -> Option<backend::Expression> {
    let item = match expression.term {
        ast::Term::Identifier(name) => {
            if let Some(local_variables) = local_variables {
                if let Some(&index) = local_variables.get(&name) {
                    return Some(backend::Expression::LocalVariable(index));
                }
            }
            if let Some(&index) = global_variables.get(&name) {
                return Some(backend::Expression::GlobalVariable(index));
            }
            match named_items.get(&name) {
                Some(item) => item,
                None => return None,
            }
        }
        ast::Term::FieldByName { term_left, name } => {
            todo!();
        }
        _ => todo!(),
    };
    todo!();
}

enum ImportOrExpression {
    Import(usize),
    Expression(backend::Expression),
}

fn translate_import_or_expression(
    expression: ast::TermWithPos,
    named_items: &HashMap<String, Item>,
    ty_parameters: &HashMap<String, usize>,
    local_variables: Option<&HashMap<String, usize>>,
    global_variables: &HashMap<String, usize>,
    exported_items: &Vec<HashMap<String, Item>>,
    file: &log::File,
    num_errors: &mut u32,
) -> Option<ImportOrExpression> {
    let item = match expression.term {
        ast::Term::Identifier(name) => {
            if let Some(local_variables) = local_variables {
                if let Some(&index) = local_variables.get(&name) {
                    return Some(ImportOrExpression::Expression(
                        backend::Expression::LocalVariable(index),
                    ));
                }
            }
            if let Some(&index) = global_variables.get(&name) {
                return Some(ImportOrExpression::Expression(
                    backend::Expression::GlobalVariable(index),
                ));
            }
            match named_items.get(&name) {
                Some(item) => item,
                None => return None,
            }
        }
        ast::Term::FieldByName { term_left, name } => {
            todo!();
        }
        _ => todo!(),
    };
    match item {
        Item::Import(index) => Some(ImportOrExpression::Import(*index)),
        Item::Function(candidates) => Some(ImportOrExpression::Expression(
            backend::Expression::Function {
                candidates: candidates.clone(),
                calls: vec![],
            },
        )),
        Item::GlobalVariable(index) => Some(ImportOrExpression::Expression(
            backend::Expression::GlobalVariable(*index),
        )),
        _ => todo!(),
    }
}

fn translate_import_or_reference(
    expression: ast::TermWithPos,
    named_items: &HashMap<String, Item>,
    ty_parameters: &HashMap<String, usize>,
    local_variables: Option<&HashMap<String, usize>>,
    global_variables: &HashMap<String, usize>,
    exported_items: &Vec<HashMap<String, Item>>,
    file: &log::File,
    num_errors: &mut u32,
) -> Option<ImportOrExpression> {
    let item = match expression.term {
        ast::Term::Identifier(name) => {
            if let Some(local_variables) = local_variables {
                if let Some(&index) = local_variables.get(&name) {
                    return Some(ImportOrExpression::Expression(
                        backend::Expression::LocalVariable(index),
                    ));
                }
            }
            if let Some(&index) = global_variables.get(&name) {
                return Some(ImportOrExpression::Expression(
                    backend::Expression::GlobalVariable(index),
                ));
            }
            match named_items.get(&name) {
                Some(item) => item,
                None => return None,
            }
        }
        ast::Term::FieldByName { term_left, name } => {
            todo!();
        }
        _ => todo!(),
    };
    match item {
        Item::Import(index) => Some(ImportOrExpression::Import(*index)),
        Item::Function(candidates) => Some(ImportOrExpression::Expression(
            backend::Expression::Function {
                candidates: candidates.clone(),
                calls: vec![],
            },
        )),
        Item::GlobalVariable(index) => Some(ImportOrExpression::Expression(
            backend::Expression::GlobalVariable(*index),
        )),
        _ => todo!(),
    }
}

#[derive(Clone)]
enum Item {
    Import(usize),
    Ty(backend::TyBuilder),
    Function(Vec<backend::Function>),
    GlobalVariable(usize),
}
