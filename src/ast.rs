/*
 * Copyright (c) 2023 Atsushi Komaba
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

use crate::range::Range;
use num::BigInt;

pub enum ExprWithSymbol {
    Identifier(String),
    Decl(String, Option<Box<PExprWithSymbol>>),
    Integer(BigInt),
    Float(f64),
    String(String),
    Operator(Operator),
    Call(Box<PExprWithSymbol>, Vec<PExprWithSymbol>),
}
pub struct PExprWithSymbol {
    pos: Range,
    expr: ExprWithSymbol,
}
impl PExprWithSymbol {
    pub fn new(pos: Range, expr: ExprWithSymbol) -> PExprWithSymbol {
        PExprWithSymbol { pos, expr }
    }
}

pub enum StmtWithSymbol {
    Expr(Option<PExprWithSymbol>),
    Return(Option<PExprWithSymbol>),
    If(
        PExprWithSymbol,
        Box<PStmtWithSymbol>,
        Option<Box<PStmtWithSymbol>>,
    ),
    While(PExprWithSymbol, Box<PStmtWithSymbol>),
    Block(Vec<PStmtWithSymbol>),
    Def {
        name: String,
        args: Vec<String>,
        body: Vec<PStmtWithSymbol>,
    },
}

pub struct PStmtWithSymbol {
    pos: Range,
    stmt: StmtWithSymbol,
}

impl PStmtWithSymbol {
    pub fn new(pos: Range, stmt: StmtWithSymbol) -> PStmtWithSymbol {
        PStmtWithSymbol { pos, stmt }
    }
}

pub enum Expr {
    Variable(usize),
    Global(usize),
    Func(usize),
    Integer(BigInt),
    Float(f64),
    String(String),
    Operator(Operator),
    Call(Box<Expr>, Vec<Expr>),
}

#[derive(Debug, Clone)]
pub enum Operator {
    Plus,
    Minus,
    LogicalNot,
    BitNot,
    PreInc,
    PreDec,
    PostInc,
    PostDec,
    ForwardShift,
    BackwardShift,
    Mul,
    Div,
    Rem,
    Add,
    Sub,
    RightShift,
    LeftShift,
    BitAnd,
    BitXor,
    BitOr,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Equal,
    NotEqual,
    LogicalAnd,
    LogicalOr,
    Cast,
    Assign,
    ForwardShiftAssign,
    BackwardShiftAssign,
    MulAssign,
    DivAssign,
    RemAssign,
    AddAssign,
    SubAssign,
    RightShiftAssign,
    LeftShiftAssign,
    BitAndAssign,
    BitXorAssign,
    BitOrAssign,
}

pub enum Stmt {
    Expr(Expr),
    Return(Option<Expr>),
    If(Expr, Vec<Stmt>, Vec<Stmt>),
    While(Expr, Vec<Stmt>),
}

pub struct FuncDef {
    num_args: usize,
    num_locals: usize,
    body: Vec<Stmt>,
}

use std::collections::HashMap;
struct Variables<'a> {
    names: HashMap<&'a str, Vec<usize>>,
    num: usize,
}
impl<'a> Variables<'a> {
    fn new() -> Variables<'a> {
        Variables {
            names: HashMap::new(),
            num: 0,
        }
    }
    fn insert(&mut self, name: &'a str) -> usize {
        let id = self.num;
        self.num += 1;
        self.names.entry(name).or_insert_with(Vec::new).push(id);
        id
    }
    fn remove(&mut self, name: &str) -> usize {
        self.names.get_mut(name).unwrap().pop().unwrap()
    }
    fn get(&self, name: &str) -> Option<usize> {
        self.names.get(name).and_then(|v| v.last()).copied()
    }
}
struct Funcs<'a> {
    names: HashMap<&'a str, usize>,
    defs: Vec<Vec<FuncDef>>,
}
impl<'a> Funcs<'a> {
    fn new() -> Funcs<'a> {
        Funcs {
            names: HashMap::new(),
            defs: Vec::new(),
        }
    }
    fn insert(&mut self, name: &'a str) -> usize {
        *self.names.entry(name).or_insert_with(|| {
            let id = self.defs.len();
            self.defs.push(Vec::new());
            id
        })
    }
}
impl PExprWithSymbol {
    fn resolve_symbol<'a>(
        &'a self,
        variables: &mut Variables<'a>,
        funcs: &mut Funcs<'a>,
        globals: Option<&Variables<'a>>,
        variables_in_current_scope: &mut Vec<&'a str>,
    ) -> Expr {
        match &self.expr {
            ExprWithSymbol::Decl(name, right_hand_side) => {
                // TODO: 右辺のない宣言に対応
                let right_hand_side = right_hand_side.as_ref().unwrap().resolve_symbol(
                    variables,
                    funcs,
                    globals,
                    variables_in_current_scope,
                );
                let id = variables.insert(name);
                variables_in_current_scope.push(name);
                Expr::Call(
                    Expr::Operator(Operator::Assign).into(),
                    vec![Expr::Variable(id), right_hand_side],
                )
            }
            ExprWithSymbol::Identifier(name) => {
                if let Some(id) = variables.get(name) {
                    Expr::Variable(id)
                } else if let Some(id) = globals.and_then(|variables| variables.get(name)) {
                    Expr::Global(id)
                } else {
                    let id = funcs.insert(name);
                    Expr::Func(id)
                }
            }
            ExprWithSymbol::Integer(value) => Expr::Integer(value.clone()), // TODO: 識別子のライフタイムだけ慎重に扱ってこの clone を不要に
            ExprWithSymbol::Float(value) => Expr::Float(*value),
            ExprWithSymbol::String(value) => Expr::String(value.clone()),
            ExprWithSymbol::Operator(operator) => Expr::Operator(operator.clone()),
            ExprWithSymbol::Call(func, args) => {
                let func =
                    func.resolve_symbol(variables, funcs, globals, variables_in_current_scope);
                let args = args
                    .iter()
                    .map(|arg| {
                        arg.resolve_symbol(variables, funcs, globals, variables_in_current_scope)
                    })
                    .collect();
                Expr::Call(func.into(), args)
            }
        }
    }
}
impl PStmtWithSymbol {
    fn resolve_symbol<'a>(
        &'a self,
        variables: &mut Variables<'a>,
        funcs: &mut Funcs<'a>,
        globals: Option<&Variables<'a>>,
        variables_in_current_scope: &mut Vec<&'a str>,
        is_toplevel: bool,
    ) -> Vec<Stmt> {
        match &self.stmt {
            StmtWithSymbol::Expr(Some(expr)) => vec![Stmt::Expr(expr.resolve_symbol(
                variables,
                funcs,
                globals,
                variables_in_current_scope,
            ))],
            StmtWithSymbol::Expr(None) => vec![],
            StmtWithSymbol::Def { name, args, body } => {
                if !is_toplevel {
                    // TODO: push error
                    return vec![];
                }
                let id = funcs.insert(name);
                let mut variables_in_current_scope = Vec::new();
                let mut locals = Variables::new();
                for arg in args {
                    locals.insert(arg);
                }
                let body = body
                    .iter()
                    .map(|stmt| {
                        stmt.resolve_symbol(
                            &mut locals,
                            funcs,
                            Some(variables),
                            &mut variables_in_current_scope,
                            false,
                        )
                    })
                    .flatten()
                    .collect();
                funcs.defs[id].push(FuncDef {
                    num_args: args.len(),
                    num_locals: locals.num,
                    body,
                });
                vec![]
            }
            StmtWithSymbol::Block(stmts) => {
                let mut variables_in_current_scope = Vec::new();
                let stmts = stmts
                    .iter()
                    .map(|stmt| {
                        stmt.resolve_symbol(
                            variables,
                            funcs,
                            globals,
                            &mut variables_in_current_scope,
                            false,
                        )
                    })
                    .flatten()
                    .collect();
                for variable in variables_in_current_scope {
                    variables.remove(variable);
                }
                stmts
            }
            StmtWithSymbol::If(cond, stmt_then, stmt_else) => {
                let cond =
                    cond.resolve_symbol(variables, funcs, globals, variables_in_current_scope);
                let mut variables_in_current_scope = Vec::new();
                let stmt_then = stmt_then.resolve_symbol(
                    variables,
                    funcs,
                    globals,
                    &mut variables_in_current_scope,
                    false,
                );
                for variable in variables_in_current_scope {
                    variables.remove(variable);
                }
                let stmt_else = match stmt_else {
                    Some(stmt_else) => {
                        let mut variables_in_current_scope = Vec::new();
                        let stmt_else = stmt_else.resolve_symbol(
                            variables,
                            funcs,
                            globals,
                            &mut variables_in_current_scope,
                            false,
                        );
                        for variable in variables_in_current_scope {
                            variables.remove(variable);
                        }
                        stmt_else
                    }
                    None => {
                        vec![]
                    }
                };
                vec![Stmt::If(cond, stmt_then, stmt_else)]
            }
            StmtWithSymbol::While(cond, stmt) => {
                let cond =
                    cond.resolve_symbol(variables, funcs, globals, variables_in_current_scope);
                let mut variables_in_current_scope = Vec::new();
                let stmt = stmt.resolve_symbol(
                    variables,
                    funcs,
                    globals,
                    &mut variables_in_current_scope,
                    false,
                );
                for variable in variables_in_current_scope {
                    variables.remove(variable);
                }
                vec![Stmt::While(cond, stmt)]
            }
            StmtWithSymbol::Return(expr) => {
                let expr = expr.as_ref().map(|expr| {
                    expr.resolve_symbol(variables, funcs, globals, variables_in_current_scope)
                });
                vec![Stmt::Return(expr)]
            }
        }
    }
}
pub fn resolve_symbol(stmts: Vec<PStmtWithSymbol>) -> (Vec<Stmt>, Vec<Vec<FuncDef>>) {
    let mut globals = Variables::new();
    let mut funcs = Funcs::new();
    let mut variables = Vec::new();
    let stmts = stmts
        .iter()
        .map(|stmt| stmt.resolve_symbol(&mut globals, &mut funcs, None, &mut variables, true))
        .flatten()
        .collect();
    for (&name, &index) in &funcs.names {
        if funcs.defs[index].is_empty() {
            eprintln!("`{name}` is not defined");
        }
    }
    (stmts, funcs.defs)
}

impl Expr {
    pub fn debug_print(&self, depth: usize) {
        let indent = "  ".repeat(depth);
        match self {
            Expr::Variable(id) => println!("{indent}variable({id})"),
            Expr::Global(id) => println!("{indent}global({id})"),
            Expr::Func(id) => println!("{indent}func({id})"),
            Expr::Integer(value) => println!("{indent}integer({value})"),
            Expr::Float(value) => println!("{indent}float({value})"),
            Expr::String(value) => println!("{indent}string({value})"),
            Expr::Operator(operator) => println!("{indent}operator({operator:?})"),
            Expr::Call(func, args) => {
                println!("{indent}call");
                func.debug_print(depth + 1);
                for arg in args {
                    arg.debug_print(depth + 1);
                }
            }
        }
    }
}
impl Stmt {
    pub fn debug_print(&self, depth: usize) {
        let indent = "  ".repeat(depth);
        match self {
            Stmt::Expr(expr) => {
                println!("{indent}expression statement");
                expr.debug_print(depth + 1);
            }
            Stmt::Return(expr) => {
                println!("{indent}return statement");
                if let Some(expr) = expr {
                    expr.debug_print(depth + 1);
                }
            }
            Stmt::If(cond, stmts_then, stmts_else) => {
                println!("{indent}if statement");
                cond.debug_print(depth + 1);
                println!("{indent}then");
                for stmt in stmts_then {
                    stmt.debug_print(depth + 1);
                }
                println!("{indent}else");
                for stmt in stmts_else {
                    stmt.debug_print(depth + 1);
                }
            }
            Stmt::While(cond, stmts) => {
                println!("{indent}while statement");
                cond.debug_print(depth + 1);
                println!("{indent}do");
                for stmt in stmts {
                    stmt.debug_print(depth + 1);
                }
            }
        }
    }
}
impl FuncDef {
    pub fn debug_print(&self) {
        println!("{} args, {} locals", self.num_args, self.num_locals);
        for stmt in &self.body {
            stmt.debug_print(0);
        }
    }
}
