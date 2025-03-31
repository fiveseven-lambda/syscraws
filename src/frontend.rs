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

use std::collections::hash_map::{self, HashMap};
use std::collections::HashSet;
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
    files: Vec<File>,
    /**
     * Used in [`Reader::read_file`] to avoid reading the same file multiple
     * times.
     */
    file_indices: HashMap<PathBuf, Option<usize>>,
    /**
     * Used in [`Reader::import_file`] to detect circular imports.
     */
    import_chain: HashSet<PathBuf>,
    /**
     * Number of errors while reading files.
     */
    num_errors: u32,
}

struct Block {
    block: backend::Block,
    expressions: Vec<backend::Expression>,
}

struct Variables {
    num: usize,
    name_and_indices: Vec<(String, usize)>,
}

struct File {
    items: HashMap<String, NamedItem>,
    methods: HashMap<String, Vec<backend::Function>>,
    log: log::File,
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
        let ast_file = ast::parse_file(&mut chars_peekable);
        let file = log::File {
            path: path.to_path_buf(),
            lines: chars_peekable.lines(),
            content,
        };
        match ast_file {
            Ok(ast_file) => {
                let mut file = File {
                    items: HashMap::new(),
                    methods: HashMap::new(),
                    log: file,
                };
                for ast::WithExtraTokens {
                    content: ast_import,
                    extra_tokens_pos,
                } in ast_file.imports
                {
                    if let Some(extra_tokens_pos) = extra_tokens_pos {
                        log::extra_tokens(extra_tokens_pos, &file.log);
                        self.num_errors += 1;
                    }
                    if let Ok((name, pos, Some(index))) =
                        self.import_file(ast_import, path.parent().unwrap(), &file.log)
                    {
                        match file.items.entry(name) {
                            std::collections::hash_map::Entry::Occupied(entry) => {
                                duplicate_definition(entry, pos, &file.log);
                                self.num_errors += 1;
                            }
                            std::collections::hash_map::Entry::Vacant(entry) => {
                                entry.insert(NamedItem::Import(pos, index));
                                for (name, exported_candidates) in &self.files[index].methods {
                                    let candidates =
                                        file.methods.entry(name.clone()).or_insert_with(Vec::new);
                                    candidates.extend_from_slice(exported_candidates);
                                    candidates.sort();
                                    candidates.dedup();
                                }
                            }
                        }
                    }
                }
                for name in ast_file.structure_names {
                    self.register_structure_name(name, &mut file);
                }
                for name in ast_file.function_names {
                    self.register_function_name(name, &mut file);
                }
                for ast::WithExtraTokens {
                    content: statement,
                    extra_tokens_pos,
                } in ast_file.top_level_statements
                {
                    if let Some(extra_tokens_pos) = extra_tokens_pos {
                        log::extra_tokens(extra_tokens_pos, &file.log);
                        self.num_errors += 1;
                    }
                    match statement {
                        ast::TopLevelStatement::StructureDefinition(structure_definition) => {
                            let (kind, definition) = self
                                .translate_structure_definition(structure_definition, &mut file);
                            self.definitions.structures.push((kind, definition));
                        }
                        ast::TopLevelStatement::FunctionDefinition(function_definition) => {
                            let num_current_items = file.items.len();
                            if let Some((ty, definition)) =
                                self.translate_function_definition(function_definition, &mut file)
                            {
                                self.definitions.functions.push((ty, definition));
                            }
                            assert_eq!(file.items.len(), num_current_items);
                        }
                        ast::TopLevelStatement::Statement(statement) => {
                            file.translate_statement(
                                statement,
                                &mut self.global_block,
                                &mut self.global_variables,
                                0,
                                backend::LocalOrGlobal::Global,
                                &self.files,
                                &mut self.num_errors,
                            );
                        }
                    }
                }
                let current_file_index = self.files.len();
                self.files.push(file);
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
    ) -> Result<(String, log::Pos, Option<usize>), ()> {
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
                Ok(n) => Ok((name, target.pos, n)),
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

impl Reader {
    fn register_structure_name(
        &mut self,
        ast::StructureName {
            keyword_struct_pos,
            name,
        }: ast::StructureName,
        file: &mut File,
    ) {
        let Some((name, pos)) = name else {
            eprintln!(
                "Missing structure name after `struct` at {}.",
                keyword_struct_pos
            );
            file.log.quote_pos(keyword_struct_pos);
            self.num_errors += 1;
            return;
        };
        match file.items.entry(name) {
            std::collections::hash_map::Entry::Occupied(entry) => {
                duplicate_definition(entry, pos, &file.log);
                self.num_errors += 1;
            }
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(NamedItem::Ty(
                    pos,
                    backend::TyBuilder::Constructor(backend::TyConstructor::Structure(
                        self.num_structures,
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
            name,
            is_method,
        }: ast::FunctionName,
        file: &mut File,
    ) {
        let Some((name, pos)) = name else {
            eprintln!("Missing function name after keyword at {}.", keyword_pos);
            file.log.quote_pos(keyword_pos);
            self.num_errors += 1;
            return;
        };
        if is_method {
            file.methods
                .entry(name)
                .or_insert_with(Vec::new)
                .push(backend::Function::UserDefined(self.num_functions));
        } else {
            match file.items.entry(name) {
                std::collections::hash_map::Entry::Occupied(mut entry) => {
                    if let NamedItem::Function(functions) = entry.get_mut() {
                        functions.push((pos, backend::Function::UserDefined(self.num_functions)));
                    } else {
                        duplicate_definition(entry, pos, &file.log);
                        self.num_errors += 1;
                    }
                }
                std::collections::hash_map::Entry::Vacant(entry) => {
                    entry.insert(NamedItem::Function(vec![(
                        pos,
                        backend::Function::UserDefined(self.num_functions),
                    )]));
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
        file: &mut File,
    ) -> (backend::TyKind, backend::Structure) {
        let mut ty_parameters_name = Vec::new();
        let kind = if let Some(ast_ty_parameters) = ast_ty_parameters {
            for ast_ty_parameter in ast_ty_parameters {
                let ast_ty_parameter = match ast_ty_parameter {
                    ast::ListElement::Empty { comma_pos } => {
                        eprintln!("Empty type parameter before comma at {}.", comma_pos);
                        file.log.quote_pos(comma_pos);
                        self.num_errors += 1;
                        continue;
                    }
                    ast::ListElement::NonEmpty(ast_ty_parameter) => ast_ty_parameter,
                };
                match ast_ty_parameter.term {
                    ast::Term::Identifier(name) => match file.items.entry(name.clone()) {
                        std::collections::hash_map::Entry::Occupied(entry) => {
                            duplicate_definition(entry, ast_ty_parameter.pos, &file.log);
                            self.num_errors += 1;
                        }
                        std::collections::hash_map::Entry::Vacant(entry) => {
                            entry.insert(NamedItem::Ty(
                                ast_ty_parameter.pos,
                                backend::TyBuilder::Parameter(ty_parameters_name.len()),
                            ));
                            ty_parameters_name.push(name);
                        }
                    },
                    _ => {
                        eprintln!("Invalid type parameter at {}.", ast_ty_parameter.pos);
                        file.log.quote_pos(ast_ty_parameter.pos);
                        self.num_errors += 1;
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
            log::extra_tokens(extra_tokens_pos, &file.log);
            self.num_errors += 1;
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
                    if let Ok(ty) =
                        file.translate_ty(*ast_field_ty, &self.files, &mut self.num_errors)
                    {
                        fields_ty.push(ty);
                    }
                }
                _ => {
                    eprintln!("Invalid structure field at {}.", ast_field.pos);
                    file.log.quote_pos(ast_field.pos);
                }
            }
            if let Some(extra_tokens_pos) = extra_tokens_pos {
                log::extra_tokens(extra_tokens_pos, &file.log);
                self.num_errors += 1;
            }
        }
        for ty_parameter_name in &ty_parameters_name {
            file.items.remove(ty_parameter_name);
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
        file: &mut File,
    ) -> Option<(backend::FunctionTy, backend::FunctionDefinition)> {
        let mut ty_parameters_name = Vec::new();
        if let Some(ast_ty_parameters) = ast_ty_parameters {
            for ast_ty_parameter in ast_ty_parameters {
                let ast_ty_parameter = match ast_ty_parameter {
                    ast::ListElement::Empty { comma_pos } => {
                        eprintln!("Empty type parameter before comma at {}.", comma_pos);
                        file.log.quote_pos(comma_pos);
                        self.num_errors += 1;
                        continue;
                    }
                    ast::ListElement::NonEmpty(ast_ty_parameter) => ast_ty_parameter,
                };
                if let ast::Term::Identifier(name) = ast_ty_parameter.term {
                    match file.items.entry(name.clone()) {
                        std::collections::hash_map::Entry::Occupied(entry) => {
                            duplicate_definition(entry, ast_ty_parameter.pos, &file.log);
                            self.num_errors += 1;
                        }
                        std::collections::hash_map::Entry::Vacant(entry) => {
                            entry.insert(NamedItem::Ty(
                                ast_ty_parameter.pos,
                                backend::TyBuilder::Parameter(ty_parameters_name.len()),
                            ));
                            ty_parameters_name.push(name);
                        }
                    }
                } else {
                    eprintln!("Invalid type parameter at {}.", ast_ty_parameter.pos);
                    file.log.quote_pos(ast_ty_parameter.pos);
                    self.num_errors += 1;
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
        if let Some(ast_parameters) = ast_parameters {
            for ast_parameter in ast_parameters {
                let ast_parameter = match ast_parameter {
                    ast::ListElement::Empty { comma_pos } => {
                        eprintln!("Empty parameter before comma at {}.", comma_pos);
                        file.log.quote_pos(comma_pos);
                        self.num_errors += 1;
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
                            ast::Term::Identifier(name) => match file.items.entry(name.clone()) {
                                std::collections::hash_map::Entry::Occupied(entry) => {
                                    duplicate_definition(entry, ast_parameter.pos, &file.log);
                                    self.num_errors += 1;
                                }
                                std::collections::hash_map::Entry::Vacant(entry) => {
                                    local_variables
                                        .name_and_indices
                                        .push((name, local_variables.num));
                                    entry.insert(NamedItem::Variable(
                                        ast_parameter_name.pos,
                                        backend::LocalOrGlobal::Local,
                                        local_variables.num,
                                    ));
                                    local_variables.num += 1;
                                }
                            },
                            _ => {
                                eprintln!("Invalid parameter name at {}.", ast_parameter_name.pos);
                                file.log.quote_pos(ast_parameter_name.pos);
                                self.num_errors += 1;
                            }
                        }
                        if let Some(ast_parameter_ty) = ast_parameter_ty {
                            if let Ok(parameter_ty) = file.translate_ty(
                                *ast_parameter_ty,
                                &self.files,
                                &mut self.num_errors,
                            ) {
                                parameters_ty.push(parameter_ty);
                            }
                        } else {
                            eprintln!("Missing type after colon at {}.", colon_pos);
                            file.log.quote_pos(colon_pos);
                            self.num_errors += 1;
                        }
                    }
                    _ => {
                        eprintln!("Invalid parameter at {}.", ast_parameter.pos);
                        file.log.quote_pos(ast_parameter.pos);
                        self.num_errors += 1;
                    }
                }
            }
        } else {
            eprintln!("Missing parameter list.");
            self.num_errors += 1;
        }
        let return_ty = if let Some(ast_return_ty) = ast_return_ty {
            if let Some(ast_return_ty) = ast_return_ty.ty {
                match file.translate_ty(ast_return_ty, &self.files, &mut self.num_errors) {
                    Ok(return_ty) => return_ty,
                    Err(()) => return None,
                }
            } else {
                eprintln!(
                    "Missing return type after colon at {}.",
                    ast_return_ty.colon_pos
                );
                file.log.quote_pos(ast_return_ty.colon_pos);
                self.num_errors += 1;
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
            log::extra_tokens(extra_tokens_pos, &file.log);
            self.num_errors += 1;
        }
        for ast::WithExtraTokens {
            content: ast_statement,
            extra_tokens_pos,
        } in ast_body
        {
            if let Some(extra_tokens_pos) = extra_tokens_pos {
                log::extra_tokens(extra_tokens_pos, &file.log);
                self.num_errors += 1;
            }
            file.translate_statement(
                ast_statement,
                &mut body,
                &mut local_variables,
                0,
                backend::LocalOrGlobal::Local,
                &self.files,
                &mut self.num_errors,
            );
        }
        for (name, index) in local_variables.name_and_indices.into_iter().rev() {
            let item = file.items.remove(&name);
            assert!(matches!(
                    item,
                    Some(NamedItem::Variable(_, backend::LocalOrGlobal::Local, stored_index))
                    if stored_index == index
            ));
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

impl File {
    fn translate_statement(
        &mut self,
        statement: ast::Statement,
        target: &mut Block,
        variables: &mut Variables,
        num_outer_variables: usize,
        local_or_global: backend::LocalOrGlobal,
        exported_items: &Vec<File>,
        num_errors: &mut u32,
    ) {
        match statement {
            ast::Statement::Term(term) => {
                let term_pos = term.pos.clone();
                let Ok(term) = self.translate_term(term, false, exported_items, num_errors) else {
                    return;
                };
                match term {
                    TranslatedTerm::Expression(expr) => target.expressions.push(expr),
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
                let Some(ast_name) = term else {
                    eprintln!("Missing variable name after `var` at {}.", keyword_var_pos);
                    self.log.quote_pos(keyword_var_pos);
                    return;
                };
                match ast_name.term {
                    ast::Term::Identifier(name) => match self.items.entry(name.clone()) {
                        std::collections::hash_map::Entry::Occupied(entry) => {
                            duplicate_definition(entry, ast_name.pos, &self.log);
                            *num_errors += 1;
                        }
                        std::collections::hash_map::Entry::Vacant(entry) => {
                            variables.name_and_indices.push((name, variables.num));
                            entry.insert(NamedItem::Variable(
                                ast_name.pos,
                                local_or_global,
                                variables.num,
                            ));
                            variables.num += 1;
                        }
                    },
                    _ => {
                        eprintln!("Expected a variable name at {}.", ast_name.pos);
                        self.log.quote_pos(ast_name.pos);
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
                    self.translate_term(expr, false, exported_items, num_errors)
                        .and_then(|term| match term {
                            TranslatedTerm::Expression(condition) => Ok(condition),
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
                        log::extra_tokens(extra_tokens_pos, &self.log);
                        *num_errors += 1;
                    }
                    self.translate_statement(
                        stmt,
                        &mut then_block,
                        variables,
                        num_outer_variables,
                        local_or_global,
                        exported_items,
                        num_errors,
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
                        Some(NamedItem::Variable(_, stored_local_or_global, stored_index))
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
                            exported_items,
                            num_errors,
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
                            Some(NamedItem::Variable(_, stored_local_or_global, stored_index))
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
                condition,
                extra_tokens_pos,
                do_block: ast_do_block,
            } => {
                let condition = if let Some(condition) = condition {
                    self.translate_term(condition, false, exported_items, num_errors)
                        .and_then(|term| match term {
                            TranslatedTerm::Expression(condition) => Ok(condition),
                            _ => {
                                eprintln!("Not an expression");
                                *num_errors += 1;
                                Err(())
                            }
                        })
                } else {
                    eprintln!("Missing condition after `while` at {}", keyword_while_pos);
                    self.log.quote_pos(keyword_while_pos);
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
                        log::extra_tokens(extra_tokens_pos, &self.log);
                        *num_errors += 1;
                    }
                    self.translate_statement(
                        stmt,
                        &mut do_block,
                        variables,
                        num_alive_variables,
                        local_or_global,
                        exported_items,
                        num_errors,
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
                        Some(NamedItem::Variable(_, stored_local_or_global, stored_index))
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

    fn translate_ty(
        &self,
        term_with_pos: ast::TermWithPos,
        exported_items: &Vec<File>,
        num_errors: &mut u32,
    ) -> Result<backend::TyBuilder, ()> {
        match self.translate_term(term_with_pos, false, exported_items, num_errors)? {
            TranslatedTerm::Ty(ty) => Ok(ty),
            _ => {
                eprintln!("Not a type");
                *num_errors += 1;
                Err(())
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
        files: &Vec<File>,
        num_errors: &mut u32,
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
                                log::not_lvalue(pos, &self.log);
                                *num_errors += 1;
                                Err(())
                            } else {
                                Ok(TranslatedTerm::Expression(backend::Expression::Integer(
                                    value,
                                )))
                            }
                        }
                        Err(err) => {
                            log::cannot_parse_integer(pos, err, &self.log);
                            *num_errors += 1;
                            Err(())
                        }
                    }
                } else {
                    match value.parse() {
                        Ok(value) => {
                            if reference {
                                log::not_lvalue(pos, &self.log);
                                *num_errors += 1;
                                Err(())
                            } else {
                                Ok(TranslatedTerm::Expression(backend::Expression::Float(
                                    value,
                                )))
                            }
                        }
                        Err(err) => {
                            log::cannot_parse_float(pos, err, &self.log);
                            *num_errors += 1;
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
                Some(named_item) => {
                    self.translate_named_item(named_item, reference, pos, num_errors)
                }
                None => {
                    eprintln!("Undefined variable at {pos}");
                    self.log.quote_pos(pos);
                    *num_errors += 1;
                    Err(())
                }
            },
            ast::Term::FieldByName {
                term_left: ast_term_left,
                name,
            } => match self.translate_term(*ast_term_left, reference, files, num_errors)? {
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
                TranslatedTerm::Import(file_index) => match files[file_index].items.get(&name) {
                    Some(named_item) => {
                        self.translate_named_item(named_item, reference, pos, num_errors)
                    }
                    None => {
                        eprintln!(
                            "`{name}` is not defined in {}",
                            files[file_index].log.path.display()
                        );
                        self.log.quote_pos(pos);
                        *num_errors += 1;
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
                    .translate_term(*ast_function, false, files, num_errors)
                    .and_then(|term| match term {
                        TranslatedTerm::Expression(function) => match function {
                            backend::Expression::Function { candidates, calls } => {
                                Ok((candidates, calls))
                            }
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
                            self.log.quote_pos(comma_pos);
                            continue;
                        }
                        ast::ListElement::NonEmpty(ast_argument) => ast_argument,
                    };
                    let Ok(argument) = self.translate_term(ast_argument, false, files, num_errors)
                    else {
                        continue;
                    };
                    match argument {
                        TranslatedTerm::Expression(argument) => {
                            arguments.push(argument);
                        }
                        _ => {
                            eprintln!("Not an expression");
                            *num_errors += 1;
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
                    Some(ast_left_hand_side) => self
                        .translate_term(*ast_left_hand_side, true, files, num_errors)
                        .and_then(|term| match term {
                            TranslatedTerm::Expression(expr) => Ok(expr),
                            _ => {
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
                    Some(ast_right_hand_side) => self
                        .translate_term(*ast_right_hand_side, false, files, num_errors)
                        .and_then(|term| match term {
                            TranslatedTerm::Expression(expr) => Ok(expr),
                            _ => {
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
                let term_left = self
                    .translate_term(*ast_term_left, false, files, num_errors)
                    .and_then(|term_left| match term_left {
                        TranslatedTerm::Ty(ty) => Ok(ty),
                        _ => {
                            eprintln!("Not a type");
                            *num_errors += 1;
                            Err(())
                        }
                    });
                let mut parameters = Vec::new();
                for ast_parameter in ast_parameters {
                    let ast_parameter = match ast_parameter {
                        ast::ListElement::Empty { comma_pos } => {
                            eprintln!("Empty type parameter before comma at {comma_pos}");
                            continue;
                        }
                        ast::ListElement::NonEmpty(ast_parameter) => ast_parameter,
                    };
                    let parameter = self
                        .translate_term(ast_parameter, false, files, num_errors)
                        .and_then(|term_left| match term_left {
                            TranslatedTerm::Ty(ty) => Ok(ty),
                            _ => {
                                eprintln!("Not a type");
                                *num_errors += 1;
                                Err(())
                            }
                        });
                    if *num_errors == 0 {
                        parameters.push(parameter.unwrap());
                    }
                }
                return if *num_errors == 0 {
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
        num_errors: &mut u32,
    ) -> Result<TranslatedTerm, ()> {
        match *named_item {
            NamedItem::Function(ref candidates) => {
                if reference {
                    log::not_lvalue(pos, &self.log);
                    *num_errors += 1;
                    Err(())
                } else {
                    Ok(TranslatedTerm::Expression(backend::Expression::Function {
                        candidates: candidates
                            .iter()
                            .map(|(_, candidate)| candidate.clone())
                            .collect(),
                        calls: vec![],
                    }))
                }
            }
            NamedItem::Variable(_, local_or_global, index) => {
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
            NamedItem::Import(_, index) => Ok(TranslatedTerm::Import(index)),
            NamedItem::Ty(_, ref ty) => Ok(TranslatedTerm::Ty(ty.clone())),
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
    Import(log::Pos, usize),
    Ty(log::Pos, backend::TyBuilder),
    Function(Vec<(log::Pos, backend::Function)>),
    Variable(log::Pos, backend::LocalOrGlobal, usize),
}

fn duplicate_definition(
    entry: hash_map::OccupiedEntry<String, NamedItem>,
    pos: log::Pos,
    file: &log::File,
) {
    eprintln!("Duplicate definition of `{}` at {}", entry.key(), pos);
    file.quote_pos(pos);
    match entry.get() {
        NamedItem::Import(pos, _) => {
            eprintln!("Previously imported at {pos}");
            file.quote_pos(pos.clone());
        }
        NamedItem::Ty(pos, _) => {
            eprintln!("Previously defined at {pos}");
            file.quote_pos(pos.clone());
        }
        NamedItem::Variable(pos, _, _) => {
            eprintln!("Previously defined at {pos}");
            file.quote_pos(pos.clone());
        }
        NamedItem::Function(candidates) => {
            for (pos, _) in candidates {
                eprintln!("Previously defined at {pos}");
                file.quote_pos(pos.clone());
            }
        }
    }
}
