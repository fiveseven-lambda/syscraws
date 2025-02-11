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

struct Reader {
    num_structures: usize,
    num_functions: usize,
    definitions: backend::Definitions,
    exported_items: Vec<HashMap<String, Item>>,
    files: Vec<log::File>,
    file_indices: HashMap<PathBuf, usize>,
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
            Ok(ast) => {
                let mut named_items = HashMap::new();
                for import in ast.imports {
                    if let Ok((name, index)) =
                        self.translate_import(import, path.parent().unwrap(), &file)
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
                let mut global = Context {
                    variables: HashMap::new(),
                    num_variables: 0,
                };
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
                            if let Ok((ty, definition)) = translate_function_definition(
                                function_definition,
                                &global.variables,
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
                                &mut global,
                                &mut global_scope,
                                &global_ty_parameters,
                                None,
                                &named_items,
                                &self.exported_items,
                                &file,
                                &mut self.num_errors,
                            ) {
                                Ok(stmt) => {
                                    if let Some(stmt) = stmt {
                                        if let Ok(global_statements) = &mut global_statements {
                                            global_statements.push(stmt);
                                        }
                                    }
                                }
                                Err(()) => global_statements = Err(()),
                            }
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
            entry.insert(Item::Ty(backend::Ty::Constructor(
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
                functions.push(*num_functions);
            } else {
                eprintln!("Duplicate definition of `{}`.", entry.key());
                file.quote_line(keyword_func_pos.line());
                *num_errors += 1;
            }
        }
        std::collections::hash_map::Entry::Vacant(entry) => {
            entry.insert(Item::Function(vec![*num_functions]));
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
            parameters: (0..ty_parameters_name.len())
                .map(|_| backend::TyKind::Ty)
                .collect(),
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
                let field_ty_pos = field_ty.pos.clone();
                match translate_item(
                    *field_ty,
                    named_items,
                    &ty_parameters_name,
                    None,
                    &HashMap::new(),
                    &exported_items,
                ) {
                    Ok(Item::Ty(ty)) => {
                        translated_fields_ty.push(ty);
                    }
                    Ok(_) => {
                        eprintln!("Expected a type at {field_ty_pos}.");
                        file.quote_pos(field_ty_pos);
                        *num_errors += 1;
                    }
                    Err(()) => {}
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
) -> Result<(backend::FunctionTy, backend::FunctionDefinition), ()> {
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
    let mut local = Context {
        variables: HashMap::new(),
        num_variables: 0,
    };
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
                                match local.variables.entry(name.clone()) {
                                    std::collections::hash_map::Entry::Occupied(_) => {
                                        eprintln!(
                                            "Duplicate parameter name at {}.",
                                            parameter_name.pos
                                        );
                                        file.quote_pos(parameter_name.pos);
                                    }
                                    std::collections::hash_map::Entry::Vacant(entry) => {
                                        entry.insert(local.num_variables);
                                        local_scope.push((name, None));
                                        local.num_variables += 1;
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
                            let field_ty_pos = parameter_ty.pos.clone();
                            match translate_item(
                                *parameter_ty,
                                named_items,
                                &ty_parameters_name,
                                None,
                                &HashMap::new(),
                                &exported_items,
                            ) {
                                Ok(Item::Ty(ty)) => {
                                    parameters_ty.push(ty);
                                }
                                Ok(_) => {
                                    eprintln!("Expected a type at {field_ty_pos}.");
                                    file.quote_pos(field_ty_pos);
                                    *num_errors += 1;
                                }
                                Err(()) => {}
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
            let return_ty_pos = return_ty.pos.clone();
            match translate_item(
                return_ty,
                named_items,
                &ty_parameters_name,
                None,
                &HashMap::new(),
                &exported_items,
            ) {
                Ok(Item::Ty(ty)) => ty,
                Ok(_) => {
                    eprintln!("Expected a type at {}.", return_ty_pos);
                    file.quote_pos(return_ty_pos);
                    *num_errors += 1;
                    return Err(());
                }
                Err(()) => {
                    return Err(());
                }
            }
        } else {
            eprintln!(
                "Missing return type after colon at {}.",
                return_ty.colon_pos
            );
            file.quote_pos(return_ty.colon_pos);
            *num_errors += 1;
            return Err(());
        }
    } else {
        backend::Ty::Application {
            constructor: Box::new(backend::Ty::Constructor(backend::TyConstructor::Tuple)),
            arguments: vec![],
        }
    };
    if let Some(extra_tokens_pos) = extra_tokens_pos {
        eprintln!("Extra tokens at {}.", extra_tokens_pos);
        file.quote_pos(extra_tokens_pos);
        *num_errors += 1;
    }
    let mut translated_body = Ok(Vec::new());
    for statement in body {
        match translate_statement(
            statement,
            &mut local,
            &mut local_scope,
            &ty_parameters_name,
            Some(global_variables),
            named_items,
            exported_items,
            file,
            num_errors,
        ) {
            Ok(Some(statement)) => {
                if let Ok(translated_body) = &mut translated_body {
                    translated_body.push(statement);
                }
            }
            Ok(None) => {}
            Err(()) => translated_body = Err(()),
        }
    }
    Ok((
        backend::FunctionTy {
            num_ty_parameters: ty_parameters_name.len(),
            parameters_ty,
            return_ty,
        },
        backend::FunctionDefinition {
            num_local_variables: local.num_variables,
            body: translated_body?,
        },
    ))
}

fn translate_statement(
    statement: ast::Statement,
    context: &mut Context,
    scope: &mut Vec<(String, Option<usize>)>,
    ty_parameters: &HashMap<String, usize>,
    global_variables: Option<&HashMap<String, usize>>,
    named_items: &HashMap<String, Item>,
    exported_items: &Vec<HashMap<String, Item>>,
    file: &log::File,
    num_errors: &mut u32,
) -> Result<Option<backend::Statement>, ()> {
    match statement {
        ast::Statement::Term(term) => {
            let term_pos = term.pos.clone();
            let expr = match global_variables {
                Some(global_variables) => translate_item(
                    term,
                    named_items,
                    ty_parameters,
                    Some(&context.variables),
                    global_variables,
                    exported_items,
                ),
                None => translate_item(
                    term,
                    named_items,
                    ty_parameters,
                    None,
                    &context.variables,
                    exported_items,
                ),
            };
            match expr? {
                Item::RValue(expr) => Ok(Some(backend::Statement::Expr(expr))),
                Item::LValue(expr) => Ok(Some(backend::Statement::Expr(expr))),
                _ => {
                    eprintln!("Expected an expression at {}.", term_pos);
                    file.quote_pos(term_pos);
                    Err(())
                }
            }
        }
        ast::Statement::VariableDeclaration {
            keyword_var_pos,
            term,
        } => {
            let Some(name) = term else {
                eprintln!("Missing variable name after `var` at {}.", keyword_var_pos);
                file.quote_pos(keyword_var_pos);
                return Err(());
            };
            match name.term {
                ast::Term::Identifier(name) => {
                    let new_index = context.num_variables;
                    let prev_index = context.variables.insert(name.clone(), new_index);
                    scope.push((name, prev_index));
                    context.num_variables += 1;
                    Ok(None)
                }
                _ => {
                    eprintln!("Expected a variable name at {}.", name.pos);
                    file.quote_pos(name.pos);
                    return Err(());
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
                let expr = match global_variables {
                    Some(global_variables) => translate_item(
                        condition,
                        named_items,
                        ty_parameters,
                        Some(&context.variables),
                        global_variables,
                        exported_items,
                    ),
                    None => translate_item(
                        condition,
                        named_items,
                        ty_parameters,
                        None,
                        &context.variables,
                        exported_items,
                    ),
                };
                match expr {
                    Ok(Item::RValue(condition)) => Ok(condition),
                    Ok(Item::LValue(_)) => todo!("deref to obtain condition"),
                    Ok(_) => {
                        eprintln!("Expected an expression at {}", condition_pos);
                        Err(())
                    }
                    Err(()) => Err(()),
                }
            } else {
                eprintln!("Missing condition after `while` at {}", keyword_while_pos);
                file.quote_pos(keyword_while_pos);
                Err(())
            };
            let mut body_scope = Vec::new();
            let mut translated_stmts = Ok(Vec::new());
            for stmt in body {
                match translate_statement(
                    stmt,
                    context,
                    &mut body_scope,
                    ty_parameters,
                    global_variables,
                    named_items,
                    exported_items,
                    file,
                    num_errors,
                ) {
                    Ok(stmt) => {
                        if let Some(stmt) = stmt {
                            if let Ok(translated_stmts) = &mut translated_stmts {
                                translated_stmts.push(stmt);
                            }
                        }
                    }
                    Err(()) => translated_stmts = Err(()),
                }
            }
            for (name, prev_index) in body_scope.into_iter().rev() {
                match prev_index {
                    Some(prev_index) => context.variables.insert(name, prev_index),
                    None => context.variables.remove(&name),
                };
            }
            if let (Ok(condition), Ok(translated_stmts)) = (condition, translated_stmts) {
                Ok(Some(backend::Statement::While(condition, translated_stmts)))
            } else {
                Err(())
            }
        }
    }
}

fn translate_item(
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
        ast::Term::IntegerTy => Ok(Item::Ty(backend::Ty::Constructor(
            backend::TyConstructor::Integer,
        ))),
        ast::Term::FloatTy => Ok(Item::Ty(backend::Ty::Constructor(
            backend::TyConstructor::Float,
        ))),
        ast::Term::TypeParameters {
            term_left,
            parameters,
        } => {
            let term_left = translate_item(
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
                    ast::ListElement::NonEmpty(parameter) => translate_item(
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
                (Ok(Item::Ty(constructor)), Some(arguments)) => {
                    Ok(Item::Ty(backend::Ty::Application {
                        constructor: Box::new(constructor),
                        arguments,
                    }))
                }
                _ => Err(()),
            }
        }
        ast::Term::FieldByName { term_left, name } => {
            let term_left = translate_item(
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

struct Context {
    variables: HashMap<String, usize>,
    num_variables: usize,
}

#[derive(Clone)]
enum Item {
    Import(usize),
    Function(Vec<usize>),
    Ty(backend::Ty),
    GlobalVariable(usize),
    LValue(backend::Expression),
    RValue(backend::Expression),
}
