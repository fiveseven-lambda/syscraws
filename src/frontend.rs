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
        num_global_variables: 0,
        definitions: backend::Definitions {
            structures: Vec::new(),
            functions: Vec::new(),
            num_global_variables: 0,
        },
        global_block: backend::Block {
            statements: Vec::new(),
            size: 0,
        },
        global_expressions: Vec::new(),
        exported_items: Vec::new(),
        methods: HashMap::from([(
            String::from("assign"),
            vec![Method {
                file_index: None,
                function: backend::Function::IAssign,
            }],
        )]),
        files: Vec::new(),
        file_indices: HashMap::new(),
        import_chain: HashSet::from([root_file_path.clone()]),
        imported_file_indices: Vec::new(),
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
    if !reader.global_expressions.is_empty() {
        reader
            .global_block
            .statements
            .push(backend::Statement::Expr(reader.global_expressions));
        reader.global_block.size += 1;
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
            body: reader.global_block,
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
     * [`register_structure_name`].
     */
    num_structures: usize,
    /**
     * Total number of functions defined in all files. Used and updated by
     * [`register_function_name`].
     */
    num_functions: usize,
    num_global_variables: usize,
    /**
     * The target which [`Reader::read_file`] stores the results in.
     */
    definitions: backend::Definitions,
    global_block: backend::Block,
    global_expressions: Vec<backend::Expression>,
    /**
     * Items exported from each file.
     */
    exported_items: Vec<HashMap<String, Item>>,
    methods: HashMap<String, Vec<Method>>,
    /**
     * Debug information of each file.
     */
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
    imported_file_indices: Vec<HashSet<usize>>,
    /**
     * Number of errors while reading files.
     */
    num_errors: u32,
}

struct Method {
    file_index: Option<usize>,
    function: backend::Function,
}

impl Reader {
    /**
     * Reads the file specified by `path`.
     */
    fn read_file(&mut self, path: &Path) -> Result<Option<usize>, std::io::Error> {
        if let Some(&index) = self.file_indices.get(path) {
            /* The file specified by `path` was already read. Since circular imports
             * should have been detected in `parse_imports`, this is not circular imports
             * but diamond imports. */
            return Ok(index);
        }
        let mut file = std::fs::File::open(path)?;
        let mut content = String::new();
        file.read_to_string(&mut content)?;
        let mut chars_peekable = CharsPeekable::new(&content);
        let ast_file = ast::parse_file(&mut chars_peekable);
        let file = log::File {
            path: path.to_path_buf(),
            lines: chars_peekable.lines(),
            content,
        };
        match ast_file {
            Ok(ast_file) => {
                let mut named_items = HashMap::new();
                let mut imported_file_indices = HashSet::new();
                for ast::WithExtraTokens {
                    content: ast_import,
                    extra_tokens_pos,
                } in ast_file.imports
                {
                    if let Some(extra_tokens_pos) = extra_tokens_pos {
                        log::extra_tokens(extra_tokens_pos, &file);
                        self.num_errors += 1;
                    }
                    if let Ok((name, Some(index))) =
                        self.import_file(ast_import, path.parent().unwrap(), &file)
                    {
                        named_items.insert(name, Item::Import(index));
                        for &imported_file_index in &self.imported_file_indices[index] {
                            imported_file_indices.insert(imported_file_index);
                        }
                    }
                }
                let current_file_index = self.files.len();
                assert_eq!(self.exported_items.len(), current_file_index);
                assert_eq!(self.imported_file_indices.len(), current_file_index);
                imported_file_indices.insert(current_file_index);
                for name in ast_file.structure_names {
                    register_structure_name(
                        name,
                        &mut self.num_structures,
                        &mut named_items,
                        &file,
                        &mut self.num_errors,
                    );
                }
                for name in ast_file.function_names {
                    register_function_name(
                        name,
                        &mut self.num_functions,
                        &mut named_items,
                        &mut self.methods,
                        current_file_index,
                        &file,
                        &mut self.num_errors,
                    );
                }
                let mut global_variables = HashMap::new();
                let mut global_scope = Vec::new();
                let global_ty_parameters = HashMap::new();
                for ast::WithExtraTokens {
                    content: statement,
                    extra_tokens_pos,
                } in ast_file.top_level_statements
                {
                    if let Some(extra_tokens_pos) = extra_tokens_pos {
                        log::extra_tokens(extra_tokens_pos, &file);
                        self.num_errors += 1;
                    }
                    match statement {
                        ast::TopLevelStatement::StructureDefinition(structure_definition) => {
                            let (kind, definition) = translate_structure_definition(
                                structure_definition,
                                &mut named_items,
                                &self.exported_items,
                                &file,
                                &mut self.num_errors,
                            );
                            self.definitions.structures.push((kind, definition));
                        }
                        ast::TopLevelStatement::FunctionDefinition(function_definition) => {
                            if let Some((ty, definition)) = translate_function_definition(
                                function_definition,
                                &global_variables,
                                &named_items,
                                &self.methods,
                                &imported_file_indices,
                                &self.exported_items,
                                &file,
                                &mut self.num_errors,
                            ) {
                                self.definitions.functions.push((ty, definition));
                            }
                        }
                        ast::TopLevelStatement::Statement(statement) => {
                            translate_statement(
                                statement,
                                &mut self.global_block,
                                &mut self.global_expressions,
                                &mut global_variables,
                                &mut self.num_global_variables,
                                &mut global_scope,
                                &global_ty_parameters,
                                None,
                                &named_items,
                                &self.methods,
                                &imported_file_indices,
                                &self.exported_items,
                                &file,
                                &mut self.num_errors,
                            );
                        }
                    }
                }
                for (name, index) in global_variables {
                    named_items.insert(name, Item::GlobalVariable(index));
                }
                assert_eq!(self.files.len(), current_file_index);
                self.files.push(file);
                assert_eq!(self.exported_items.len(), current_file_index);
                self.exported_items.push(named_items);
                assert_eq!(self.imported_file_indices.len(), current_file_index);
                self.imported_file_indices.push(imported_file_indices);
                Ok(Some(current_file_index))
            }
            Err(err) => {
                err.eprint(&file);
                self.num_errors += 1;
                self.file_indices.insert(path.to_path_buf(), None);
                Ok(None)
            }
        }
    }

    fn import_file(
        &mut self,
        ast::Import {
            keyword_import_pos,
            target,
        }: ast::Import,
        parent_directory: &Path,
        file: &log::File,
    ) -> Result<(String, Option<usize>), ()> {
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
}

fn register_function_name(
    ast::FunctionName {
        keyword_pos,
        name,
        is_method,
    }: ast::FunctionName,
    num_functions: &mut usize,
    named_items: &mut HashMap<String, Item>,
    methods: &mut HashMap<String, Vec<Method>>,
    file_index: usize,
    file: &log::File,
    num_errors: &mut u32,
) {
    let Some(name) = name else {
        eprintln!("Missing function name after keyword at {}.", keyword_pos);
        file.quote_pos(keyword_pos);
        *num_errors += 1;
        return;
    };
    if is_method {
        methods.entry(name).or_insert_with(Vec::new).push(Method {
            file_index: Some(file_index),
            function: backend::Function::UserDefined(*num_functions),
        });
    } else {
        match named_items.entry(name) {
            std::collections::hash_map::Entry::Occupied(mut entry) => {
                if let Item::Function(functions) = entry.get_mut() {
                    functions.push(backend::Function::UserDefined(*num_functions));
                } else {
                    eprintln!("Duplicate definition of `{}`.", entry.key());
                    file.quote_line(keyword_pos.line());
                    *num_errors += 1;
                }
            }
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(Item::Function(vec![backend::Function::UserDefined(
                    *num_functions,
                )]));
            }
        }
    }
    *num_functions += 1;
}

fn translate_structure_definition(
    ast::StructureDefinition {
        ty_parameters: ast_ty_parameters,
        fields: ast_fields,
        extra_tokens_pos,
    }: ast::StructureDefinition,
    named_items: &HashMap<String, Item>,
    exported_items: &Vec<HashMap<String, Item>>,
    file: &log::File,
    num_errors: &mut u32,
) -> (backend::TyKind, backend::Structure) {
    let mut ty_parameters_name = HashMap::new();
    let kind = if let Some(ast_ty_parameters) = ast_ty_parameters {
        for ast_ty_parameter in ast_ty_parameters {
            let ast_ty_parameter = match ast_ty_parameter {
                ast::ListElement::Empty { comma_pos } => {
                    eprintln!("Empty type parameter before comma at {}.", comma_pos);
                    file.quote_pos(comma_pos);
                    *num_errors += 1;
                    continue;
                }
                ast::ListElement::NonEmpty(ast_ty_parameter) => ast_ty_parameter,
            };
            match ast_ty_parameter.term {
                ast::Term::Identifier(name) => {
                    let new_index = ty_parameters_name.len();
                    ty_parameters_name.insert(name, new_index);
                }
                _ => {
                    eprintln!("Invalid type parameter at {}.", ast_ty_parameter.pos);
                    file.quote_pos(ast_ty_parameter.pos);
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
    if let Some(extra_tokens_pos) = extra_tokens_pos {
        log::extra_tokens(extra_tokens_pos, file);
        *num_errors += 1;
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
                if let Ok(ty) = translate_ty(
                    *ast_field_ty,
                    named_items,
                    &ty_parameters_name,
                    &exported_items,
                    file,
                    num_errors,
                ) {
                    fields_ty.push(ty);
                }
            }
            _ => {
                eprintln!("Invalid structure field at {}.", ast_field.pos);
                file.quote_pos(ast_field.pos);
            }
        }
        if let Some(extra_tokens_pos) = extra_tokens_pos {
            log::extra_tokens(extra_tokens_pos, file);
            *num_errors += 1;
        }
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
    ast::FunctionDefinition {
        ty_parameters: ast_ty_parameters,
        parameters: ast_parameters,
        return_ty: ast_return_ty,
        body: ast_body,
        extra_tokens_pos,
    }: ast::FunctionDefinition,
    global_variables: &HashMap<String, usize>,
    named_items: &HashMap<String, Item>,
    methods: &HashMap<String, Vec<Method>>,
    imported_file_indices: &HashSet<usize>,
    exported_items: &Vec<HashMap<String, Item>>,
    file: &log::File,
    num_errors: &mut u32,
) -> Option<(backend::FunctionTy, backend::FunctionDefinition)> {
    let mut ty_parameters_name = HashMap::new();
    if let Some(ast_ty_parameters) = ast_ty_parameters {
        for (i, ast_ty_parameter) in ast_ty_parameters.into_iter().enumerate() {
            let ast_ty_parameter = match ast_ty_parameter {
                ast::ListElement::Empty { comma_pos } => {
                    eprintln!("Empty type parameter before comma at {}.", comma_pos);
                    file.quote_pos(comma_pos);
                    *num_errors += 1;
                    continue;
                }
                ast::ListElement::NonEmpty(ast_ty_parameter) => ast_ty_parameter,
            };
            if let ast::Term::Identifier(name) = ast_ty_parameter.term {
                ty_parameters_name.insert(name, i);
            } else {
                eprintln!("Invalid type parameter at {}.", ast_ty_parameter.pos);
                file.quote_pos(ast_ty_parameter.pos);
                *num_errors += 1;
            }
        }
    }
    let mut local_variables = HashMap::new();
    let mut num_local_variables = 0;
    let mut local_scope = Vec::new();
    let mut parameters_ty = Vec::new();
    if let Some(ast_parameters) = ast_parameters {
        for ast_parameter in ast_parameters {
            let ast_parameter = match ast_parameter {
                ast::ListElement::Empty { comma_pos } => {
                    eprintln!("Empty parameter before comma at {}.", comma_pos);
                    file.quote_pos(comma_pos);
                    *num_errors += 1;
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
                        ast::Term::Identifier(name) => match local_variables.entry(name.clone()) {
                            std::collections::hash_map::Entry::Occupied(_) => {
                                eprintln!(
                                    "Duplicate parameter name at {}.",
                                    ast_parameter_name.pos
                                );
                                file.quote_pos(ast_parameter_name.pos);
                            }
                            std::collections::hash_map::Entry::Vacant(entry) => {
                                entry.insert(num_local_variables);
                                local_scope.push((name, None));
                                num_local_variables += 1;
                            }
                        },
                        _ => {
                            eprintln!("Invalid parameter name at {}.", ast_parameter_name.pos);
                            file.quote_pos(ast_parameter_name.pos);
                            *num_errors += 1;
                        }
                    }
                    if let Some(ast_parameter_ty) = ast_parameter_ty {
                        if let Ok(parameter_ty) = translate_ty(
                            *ast_parameter_ty,
                            named_items,
                            &ty_parameters_name,
                            &exported_items,
                            file,
                            num_errors,
                        ) {
                            parameters_ty.push(parameter_ty);
                        }
                    } else {
                        eprintln!("Missing type after colon at {}.", colon_pos);
                        file.quote_pos(colon_pos);
                        *num_errors += 1;
                    }
                }
                _ => {
                    eprintln!("Invalid parameter at {}.", ast_parameter.pos);
                    file.quote_pos(ast_parameter.pos);
                    *num_errors += 1;
                }
            }
        }
    } else {
        eprintln!("Missing parameter list.");
        *num_errors += 1;
    }
    let return_ty = if let Some(ast_return_ty) = ast_return_ty {
        if let Some(ast_return_ty) = ast_return_ty.ty {
            match translate_ty(
                ast_return_ty,
                named_items,
                &ty_parameters_name,
                &exported_items,
                file,
                num_errors,
            ) {
                Ok(return_ty) => return_ty,
                Err(()) => return None,
            }
        } else {
            eprintln!(
                "Missing return type after colon at {}.",
                ast_return_ty.colon_pos
            );
            file.quote_pos(ast_return_ty.colon_pos);
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
        log::extra_tokens(extra_tokens_pos, file);
        *num_errors += 1;
    }
    let mut body = backend::Block {
        statements: Vec::new(),
        size: 0,
    };
    let mut expressions = Vec::new();
    for ast::WithExtraTokens {
        content: ast_statement,
        extra_tokens_pos,
    } in ast_body
    {
        if let Some(extra_tokens_pos) = extra_tokens_pos {
            log::extra_tokens(extra_tokens_pos, file);
            *num_errors += 1;
        }
        translate_statement(
            ast_statement,
            &mut body,
            &mut expressions,
            &mut local_variables,
            &mut num_local_variables,
            &mut local_scope,
            &ty_parameters_name,
            Some(global_variables),
            named_items,
            methods,
            imported_file_indices,
            exported_items,
            file,
            num_errors,
        );
    }
    if !expressions.is_empty() {
        body.statements.push(backend::Statement::Expr(expressions));
        body.size += 1;
    }
    Some((
        backend::FunctionTy {
            num_ty_parameters: ty_parameters_name.len(),
            parameters_ty,
            return_ty,
        },
        backend::FunctionDefinition {
            num_local_variables,
            body,
        },
    ))
}

fn translate_statement(
    statement: ast::Statement,
    target_block: &mut backend::Block,
    target_expressions: &mut Vec<backend::Expression>,
    variables: &mut HashMap<String, usize>,
    num_variables: &mut usize,
    scope: &mut Vec<(String, Option<usize>)>,
    ty_parameters: &HashMap<String, usize>,
    global_variables: Option<&HashMap<String, usize>>,
    named_items: &HashMap<String, Item>,
    methods: &HashMap<String, Vec<Method>>,
    imported_file_indices: &HashSet<usize>,
    exported_items: &Vec<HashMap<String, Item>>,
    file: &log::File,
    num_errors: &mut u32,
) {
    match statement {
        ast::Statement::Term(term) => {
            let term_pos = term.pos.clone();
            let Ok(term) = translate_expression_or_import(
                term,
                false,
                named_items,
                methods,
                imported_file_indices,
                ty_parameters,
                global_variables.map(|_| &*variables),
                global_variables.unwrap_or(&variables),
                exported_items,
                file,
                num_errors,
            ) else {
                return;
            };
            match term {
                ImportOrExpression::Expression(expr) => target_expressions.push(expr),
                _ => {
                    eprintln!("Not an expression");
                    *num_errors += 1;
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
                return;
            };
            match name.term {
                ast::Term::Identifier(name) => {
                    let prev_index = variables.insert(name.clone(), *num_variables);
                    scope.push((name, prev_index));
                    *num_variables += 1;
                }
                _ => {
                    eprintln!("Expected a variable name at {}.", name.pos);
                    file.quote_pos(name.pos);
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
            let condition = if let Some(expr) = ast_condition {
                translate_expression_or_import(
                    expr,
                    false,
                    named_items,
                    methods,
                    imported_file_indices,
                    ty_parameters,
                    global_variables.map(|_| &*variables),
                    global_variables.unwrap_or(&variables),
                    exported_items,
                    file,
                    num_errors,
                )
                .and_then(|term| match term {
                    ImportOrExpression::Expression(condition) => Ok(condition),
                    _ => {
                        eprintln!("Not an expression");
                        *num_errors += 1;
                        Err(())
                    }
                })
            } else {
                eprintln!("Empty condition");
                *num_errors += 1;
                Err(())
            };
            let mut then_scope = Vec::new();
            let mut then_block = backend::Block {
                statements: Vec::new(),
                size: 0,
            };
            let mut then_expressions = Vec::new();
            for ast::WithExtraTokens {
                content: stmt,
                extra_tokens_pos,
            } in ast_then_block
            {
                if let Some(extra_tokens_pos) = extra_tokens_pos {
                    log::extra_tokens(extra_tokens_pos, file);
                    *num_errors += 1;
                }
                translate_statement(
                    stmt,
                    &mut then_block,
                    &mut then_expressions,
                    variables,
                    num_variables,
                    &mut then_scope,
                    ty_parameters,
                    global_variables,
                    named_items,
                    methods,
                    imported_file_indices,
                    exported_items,
                    file,
                    num_errors,
                );
            }
            if !then_expressions.is_empty() {
                then_block.size += 1;
                then_block
                    .statements
                    .push(backend::Statement::Expr(then_expressions));
            }
            let mut else_scope = Vec::new();
            let mut else_block = backend::Block {
                statements: Vec::new(),
                size: 0,
            };
            let mut else_expressions = Vec::new();
            if let Some(ast_else_block) = ast_else_block {
                for ast::WithExtraTokens {
                    content: stmt,
                    extra_tokens_pos,
                } in ast_else_block.block
                {
                    translate_statement(
                        stmt,
                        &mut else_block,
                        &mut else_expressions,
                        variables,
                        num_variables,
                        &mut else_scope,
                        ty_parameters,
                        global_variables,
                        named_items,
                        methods,
                        imported_file_indices,
                        exported_items,
                        file,
                        num_errors,
                    );
                }
                if !else_expressions.is_empty() {
                    else_block.size += 1;
                    else_block
                        .statements
                        .push(backend::Statement::Expr(else_expressions));
                }
            }
            target_block.size += then_block.size + else_block.size + 1;
            target_block.statements.push(backend::Statement::If {
                antecedents: std::mem::take(target_expressions),
                condition: condition.unwrap(),
                then_block,
                else_block,
            });
        }
        ast::Statement::While {
            keyword_while_pos,
            condition,
            extra_tokens_pos,
            do_block: body,
        } => {
            let condition = if let Some(condition) = condition {
                translate_expression_or_import(
                    condition,
                    false,
                    named_items,
                    methods,
                    imported_file_indices,
                    ty_parameters,
                    global_variables.map(|_| &*variables),
                    global_variables.unwrap_or(&variables),
                    exported_items,
                    file,
                    num_errors,
                )
                .and_then(|term| match term {
                    ImportOrExpression::Expression(condition) => Ok(condition),
                    _ => {
                        eprintln!("Not an expression");
                        *num_errors += 1;
                        Err(())
                    }
                })
            } else {
                eprintln!("Missing condition after `while` at {}", keyword_while_pos);
                file.quote_pos(keyword_while_pos);
                Err(())
            };
            let mut body_scope = Vec::new();
            let mut translated_body = backend::Block {
                statements: Vec::new(),
                size: 0,
            };
            let mut translated_expressions = Vec::new();
            for ast::WithExtraTokens {
                content: stmt,
                extra_tokens_pos,
            } in body
            {
                if let Some(extra_tokens_pos) = extra_tokens_pos {
                    log::extra_tokens(extra_tokens_pos, file);
                    *num_errors += 1;
                }
                translate_statement(
                    stmt,
                    &mut translated_body,
                    &mut translated_expressions,
                    variables,
                    num_variables,
                    &mut body_scope,
                    ty_parameters,
                    global_variables,
                    named_items,
                    methods,
                    imported_file_indices,
                    exported_items,
                    file,
                    num_errors,
                );
            }
            for (name, prev_index) in body_scope.into_iter().rev() {
                match prev_index {
                    Some(prev_index) => variables.insert(name, prev_index),
                    None => variables.remove(&name),
                };
            }
            if !target_expressions.is_empty() {
                target_block.size += 1;
                target_block
                    .statements
                    .push(backend::Statement::Expr(std::mem::take(target_expressions)));
            }
            if !translated_expressions.is_empty() {
                translated_body.size += 1;
                translated_body
                    .statements
                    .push(backend::Statement::Expr(translated_expressions));
            }
            target_block.size += translated_body.size + 1;
            target_block.statements.push(backend::Statement::While {
                condition: condition.unwrap(),
                do_block: translated_body,
            });
        }
        ast::Statement::Break => {
            todo!();
        }
        ast::Statement::Continue => {
            todo!();
        }
    }
}

fn translate_import(
    import: ast::TermWithPos,
    named_items: &HashMap<String, Item>,
    exported_items: &Vec<HashMap<String, Item>>,
    file: &log::File,
    num_errors: &mut u32,
) -> Result<usize, ()> {
    let item = match import.term {
        ast::Term::Identifier(name) => match named_items.get(&name) {
            Some(item) => item,
            None => return Err(()),
        },
        ast::Term::FieldByName { term_left, name } => {
            let file_index =
                translate_import(*term_left, named_items, exported_items, file, num_errors)?;
            match exported_items[file_index].get(&name) {
                Some(item) => item,
                None => return Err(()),
            }
        }
        _ => return Err(()),
    };
    match *item {
        Item::Import(n) => Ok(n),
        _ => Err(()),
    }
}

fn translate_ty(
    ty: ast::TermWithPos,
    named_items: &HashMap<String, Item>,
    ty_parameters: &HashMap<String, usize>,
    exported_items: &Vec<HashMap<String, Item>>,
    file: &log::File,
    num_errors: &mut u32,
) -> Result<backend::TyBuilder, ()> {
    let item = match ty.term {
        ast::Term::IntegerTy => {
            return Ok(backend::TyBuilder::Constructor(
                backend::TyConstructor::Integer,
            ))
        }
        ast::Term::FloatTy => {
            return Ok(backend::TyBuilder::Constructor(
                backend::TyConstructor::Float,
            ))
        }
        ast::Term::Identifier(name) => {
            if let Some(&index) = ty_parameters.get(&name) {
                return Ok(backend::TyBuilder::Parameter(index));
            }
            match named_items.get(&name) {
                Some(item) => item,
                None => return Err(()),
            }
        }
        ast::Term::FieldByName { term_left, name } => {
            let file_index =
                translate_import(*term_left, named_items, exported_items, file, num_errors)?;
            match exported_items[file_index].get(&name) {
                Some(item) => item,
                None => return Err(()),
            }
        }
        ast::Term::TypeParameters {
            term_left: ast_term_left,
            parameters: ast_parameters,
        } => {
            let term_left = translate_ty(
                *ast_term_left,
                named_items,
                ty_parameters,
                exported_items,
                file,
                num_errors,
            );
            let mut parameters = Vec::new();
            for ast_parameter in ast_parameters {
                let ast_parameter = match ast_parameter {
                    ast::ListElement::Empty { comma_pos } => {
                        eprintln!("Empty type parameter before comma at {comma_pos}");
                        continue;
                    }
                    ast::ListElement::NonEmpty(ast_parameter) => ast_parameter,
                };
                let parameter = translate_ty(
                    ast_parameter,
                    named_items,
                    ty_parameters,
                    exported_items,
                    file,
                    num_errors,
                );
                if *num_errors == 0 {
                    parameters.push(parameter?);
                }
            }
            return if *num_errors == 0 {
                Ok(backend::TyBuilder::Application {
                    constructor: Box::new(term_left?),
                    arguments: parameters,
                })
            } else {
                Err(())
            };
        }
        _ => return Err(()),
    };
    match item {
        Item::Ty(ty) => Ok(ty.clone()),
        _ => Err(()),
    }
}

fn translate_expression_or_import(
    ast::TermWithPos {
        term: ast_term,
        pos,
    }: ast::TermWithPos,
    reference: bool,
    named_items: &HashMap<String, Item>,
    methods: &HashMap<String, Vec<Method>>,
    imported_file_indices: &HashSet<usize>,
    ty_parameters: &HashMap<String, usize>,
    local_variables: Option<&HashMap<String, usize>>,
    global_variables: &HashMap<String, usize>,
    exported_items: &Vec<HashMap<String, Item>>,
    file: &log::File,
    num_errors: &mut u32,
) -> Result<ImportOrExpression, ()> {
    match ast_term {
        ast::Term::NumericLiteral(value) => {
            if value.chars().all(|ch| matches!(ch, '0'..='9')) {
                match value.parse() {
                    Ok(value) => {
                        if reference {
                            eprintln!("Cannot dereference literal");
                            *num_errors += 1;
                            Err(())
                        } else {
                            Ok(ImportOrExpression::Expression(
                                backend::Expression::Integer(value),
                            ))
                        }
                    }
                    Err(err) => {
                        eprintln!("{err}");
                        *num_errors += 1;
                        Err(())
                    }
                }
            } else {
                match value.parse() {
                    Ok(value) => {
                        if reference {
                            eprintln!("Cannot dereference literal");
                            *num_errors += 1;
                            Err(())
                        } else {
                            Ok(ImportOrExpression::Expression(backend::Expression::Float(
                                value,
                            )))
                        }
                    }
                    Err(err) => {
                        eprintln!("{err}");
                        *num_errors += 1;
                        Err(())
                    }
                }
            }
        }
        ast::Term::Identity => Ok(ImportOrExpression::Expression(
            backend::Expression::Function {
                candidates: vec![backend::Function::Identity],
                calls: vec![],
            },
        )),
        ast::Term::Identifier(name) => {
            if let Some(local_variables) = local_variables {
                if let Some(&index) = local_variables.get(&name) {
                    let expr = backend::Expression::LocalVariable(index);
                    return Ok(ImportOrExpression::Expression(if reference {
                        expr
                    } else {
                        backend::Expression::Function {
                            candidates: vec![backend::Function::Deref],
                            calls: vec![backend::Call {
                                arguments: vec![expr],
                            }],
                        }
                    }));
                }
            }
            if let Some(&index) = global_variables.get(&name) {
                let expr = backend::Expression::GlobalVariable(index);
                return Ok(ImportOrExpression::Expression(if reference {
                    expr
                } else {
                    backend::Expression::Function {
                        candidates: vec![backend::Function::Deref],
                        calls: vec![backend::Call {
                            arguments: vec![expr],
                        }],
                    }
                }));
            }
            match named_items.get(&name) {
                Some(Item::Function(candidates)) => {
                    if reference {
                        eprintln!("Cannot dereference a function");
                        *num_errors += 1;
                        Err(())
                    } else {
                        Ok(ImportOrExpression::Expression(
                            backend::Expression::Function {
                                candidates: candidates.clone(),
                                calls: vec![],
                            },
                        ))
                    }
                }
                Some(Item::GlobalVariable(index)) => {
                    let expr = backend::Expression::GlobalVariable(*index);
                    Ok(ImportOrExpression::Expression(if reference {
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
                Some(Item::Import(index)) => Ok(ImportOrExpression::Import(*index)),
                Some(Item::Ty(_)) => {
                    eprintln!("Note an expression");
                    *num_errors += 1;
                    Err(())
                }
                None => {
                    eprintln!("Undefined variable at {pos}");
                    *num_errors += 1;
                    Err(())
                }
            }
        }
        ast::Term::FieldByName {
            term_left: ast_term_left,
            name,
        } => {
            match translate_expression_or_import(
                *ast_term_left,
                reference,
                named_items,
                methods,
                imported_file_indices,
                ty_parameters,
                local_variables,
                global_variables,
                exported_items,
                file,
                num_errors,
            )? {
                ImportOrExpression::Expression(expr) => Ok(ImportOrExpression::Expression(
                    backend::Expression::Function {
                        candidates: vec![],
                        calls: vec![backend::Call {
                            arguments: vec![expr],
                        }],
                    },
                )),
                ImportOrExpression::Import(file_index) => {
                    match exported_items[file_index].get(&name) {
                        Some(Item::Function(candidates)) => {
                            if reference {
                                eprintln!("Cannot dereference a function");
                                *num_errors += 1;
                                Err(())
                            } else {
                                Ok(ImportOrExpression::Expression(
                                    backend::Expression::Function {
                                        candidates: candidates.clone(),
                                        calls: vec![],
                                    },
                                ))
                            }
                        }
                        Some(Item::GlobalVariable(index)) => {
                            let expr = backend::Expression::GlobalVariable(*index);
                            Ok(ImportOrExpression::Expression(if reference {
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
                        Some(Item::Import(index)) => Ok(ImportOrExpression::Import(*index)),
                        Some(Item::Ty(_)) => {
                            eprintln!("Note an expression");
                            *num_errors += 1;
                            Err(())
                        }
                        None => Err(()),
                    }
                }
            }
        }
        ast::Term::FunctionCall {
            function: ast_function,
            arguments: ast_arguments,
        } => {
            let function_pos = ast_function.pos.clone();
            let function = translate_expression_or_import(
                *ast_function,
                false,
                named_items,
                methods,
                imported_file_indices,
                ty_parameters,
                local_variables,
                global_variables,
                exported_items,
                file,
                num_errors,
            )
            .and_then(|term| match term {
                ImportOrExpression::Expression(function) => match function {
                    backend::Expression::Function { candidates, calls } => Ok((candidates, calls)),
                    _ => {
                        eprintln!("Not a function");
                        *num_errors += 1;
                        Err(())
                    }
                },
                _ => {
                    eprintln!("Not an expression");
                    *num_errors += 1;
                    Err(())
                }
            });
            let mut arguments = Vec::new();
            for ast_argument in ast_arguments {
                let ast_argument = match ast_argument {
                    ast::ListElement::Empty { comma_pos } => {
                        eprintln!("Empty argument before comma at {comma_pos}");
                        file.quote_pos(comma_pos);
                        continue;
                    }
                    ast::ListElement::NonEmpty(ast_argument) => ast_argument,
                };
                let Ok(argument) = translate_expression_or_import(
                    ast_argument,
                    false,
                    named_items,
                    methods,
                    imported_file_indices,
                    ty_parameters,
                    local_variables,
                    global_variables,
                    exported_items,
                    file,
                    num_errors,
                ) else {
                    continue;
                };
                match argument {
                    ImportOrExpression::Expression(argument) => {
                        arguments.push(argument);
                    }
                    ImportOrExpression::Import(_) => {
                        eprintln!("Not an expression");
                        *num_errors += 1;
                    }
                }
            }
            let (candidates, mut calls) = function?;
            calls.push(backend::Call { arguments });
            Ok(ImportOrExpression::Expression(
                backend::Expression::Function { candidates, calls },
            ))
        }
        ast::Term::Assignment {
            left_hand_side: ast_left_hand_side,
            operator_name,
            operator_pos,
            right_hand_side: ast_right_hand_side,
        } => {
            let left_hand_side = match ast_left_hand_side {
                Some(ast_left_hand_side) => translate_expression_or_import(
                    *ast_left_hand_side,
                    true,
                    named_items,
                    methods,
                    imported_file_indices,
                    ty_parameters,
                    local_variables,
                    global_variables,
                    exported_items,
                    file,
                    num_errors,
                )
                .and_then(|expression_or_import| match expression_or_import {
                    ImportOrExpression::Expression(expr) => Ok(expr),
                    ImportOrExpression::Import(import) => {
                        eprintln!("Not an expression ");
                        Err(())
                    }
                }),
                None => {
                    eprintln!("Empty left hand side");
                    *num_errors += 1;
                    Err(())
                }
            };
            let right_hand_side = match ast_right_hand_side {
                Some(ast_right_hand_side) => translate_expression_or_import(
                    *ast_right_hand_side,
                    false,
                    named_items,
                    methods,
                    imported_file_indices,
                    ty_parameters,
                    local_variables,
                    global_variables,
                    exported_items,
                    file,
                    num_errors,
                )
                .and_then(|expression_or_import| match expression_or_import {
                    ImportOrExpression::Expression(expr) => Ok(expr),
                    ImportOrExpression::Import(import) => {
                        eprintln!("Not an expression ");
                        Err(())
                    }
                }),
                None => {
                    eprintln!("Empty right hand side");
                    *num_errors += 1;
                    Err(())
                }
            };
            let candidates: Vec<_> = methods
                .get(operator_name)
                .into_iter()
                .flatten()
                .filter_map(
                    |Method {
                         file_index,
                         function,
                     }| {
                        file_index
                            .is_none_or(|file_index| imported_file_indices.contains(&file_index))
                            .then_some(function.clone())
                    },
                )
                .collect();
            Ok(ImportOrExpression::Expression(
                backend::Expression::Function {
                    candidates,
                    calls: vec![backend::Call {
                        arguments: vec![left_hand_side?, right_hand_side?],
                    }],
                },
            ))
        }
        _ => todo!(),
    }
}

enum ImportOrExpression {
    Import(usize),
    Expression(backend::Expression),
}

#[derive(Clone)]
enum Item {
    Import(usize),
    Ty(backend::TyBuilder),
    Function(Vec<backend::Function>),
    GlobalVariable(usize),
}
