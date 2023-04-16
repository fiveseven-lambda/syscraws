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

use enum_iterator::Sequence;
use std::iter;

use super::Operator;
use crate::{ast, ir, ty};
use std::collections::HashMap;

pub struct Variables<'id> {
    names: HashMap<&'id str, Vec<usize>>,
    tys: Vec<Option<ty::Ty>>,
}

impl<'id> Variables<'id> {
    pub fn new() -> Variables<'id> {
        Variables {
            names: HashMap::new(),
            tys: Vec::new(),
        }
    }
    pub fn insert(&mut self, name: &'id str, ty: Option<ty::Ty>) -> usize {
        let id = self.tys.len();
        self.tys.push(ty);
        self.names.entry(name).or_insert_with(Vec::new).push(id);
        id
    }
    pub fn remove(&mut self, name: &str) -> usize {
        self.names.get_mut(name).unwrap().pop().unwrap()
    }
    pub fn remove_all(&mut self, names: Vec<&str>) {
        for name in names {
            self.remove(name);
        }
    }
    pub fn get(&self, name: &str) -> Option<usize> {
        self.names.get(name).and_then(|v| v.last()).copied()
    }
    pub fn tys(self) -> Vec<Option<ty::Ty>> {
        self.tys
    }
}

pub struct Funcs<'id> {
    names: HashMap<&'id str, usize>,
    overloads: Vec<Vec<ir::Func>>,
    defs: Vec<ast::FuncDef>,
}

impl<'id> Funcs<'id> {
    pub fn new() -> Funcs<'id> {
        let mut ret = Funcs {
            names: HashMap::new(),
            overloads: iter::repeat_with(Vec::new)
                .take(Operator::CARDINALITY)
                .collect(),
            defs: Vec::new(),
        };
        ret.overloads[Operator::Plus as usize].push(ir::Func::Builtin(ir::BuiltinFunc::PlusFloat));
        ret.overloads[Operator::Plus as usize]
            .push(ir::Func::Builtin(ir::BuiltinFunc::PlusInteger));
        ret.overloads[Operator::Minus as usize]
            .push(ir::Func::Builtin(ir::BuiltinFunc::MinusFloat));
        ret.overloads[Operator::Minus as usize]
            .push(ir::Func::Builtin(ir::BuiltinFunc::MinusInteger));
        ret.overloads[Operator::Recip as usize]
            .push(ir::Func::Builtin(ir::BuiltinFunc::RecipFloat));
        ret.overloads[Operator::LogicalNot as usize]
            .push(ir::Func::Builtin(ir::BuiltinFunc::NotBoolean));
        ret.overloads[Operator::Add as usize].push(ir::Func::Builtin(ir::BuiltinFunc::AddFloat));
        ret.overloads[Operator::Add as usize].push(ir::Func::Builtin(ir::BuiltinFunc::AddInteger));
        ret.overloads[Operator::Sub as usize].push(ir::Func::Builtin(ir::BuiltinFunc::SubFloat));
        ret.overloads[Operator::Sub as usize].push(ir::Func::Builtin(ir::BuiltinFunc::SubInteger));
        ret.overloads[Operator::Mul as usize].push(ir::Func::Builtin(ir::BuiltinFunc::MulFloat));
        ret.overloads[Operator::Mul as usize].push(ir::Func::Builtin(ir::BuiltinFunc::MulInteger));
        ret.overloads[Operator::Div as usize].push(ir::Func::Builtin(ir::BuiltinFunc::DivFloat));
        ret.overloads[Operator::Div as usize].push(ir::Func::Builtin(ir::BuiltinFunc::DivInteger));
        ret.overloads[Operator::Rem as usize].push(ir::Func::Builtin(ir::BuiltinFunc::RemFloat));
        ret.overloads[Operator::Rem as usize].push(ir::Func::Builtin(ir::BuiltinFunc::RemInteger));
        ret.overloads[Operator::Equal as usize]
            .push(ir::Func::Builtin(ir::BuiltinFunc::EqualInteger));
        ret.overloads[Operator::NotEqual as usize]
            .push(ir::Func::Builtin(ir::BuiltinFunc::NotEqualInteger));
        ret.overloads[Operator::Greater as usize]
            .push(ir::Func::Builtin(ir::BuiltinFunc::GreaterInteger));
        ret.overloads[Operator::GreaterEqual as usize]
            .push(ir::Func::Builtin(ir::BuiltinFunc::GreaterEqualInteger));
        ret.overloads[Operator::Less as usize]
            .push(ir::Func::Builtin(ir::BuiltinFunc::LessInteger));
        ret.overloads[Operator::LessEqual as usize]
            .push(ir::Func::Builtin(ir::BuiltinFunc::LessEqualInteger));
        ret.overloads[Operator::Assign as usize]
            .push(ir::Func::Builtin(ir::BuiltinFunc::AssignFloat));
        ret.overloads[Operator::Assign as usize]
            .push(ir::Func::Builtin(ir::BuiltinFunc::AssignInteger));
        let print = ret.get_or_insert("print");
        ret.overloads[print].push(ir::Func::Builtin(ir::BuiltinFunc::PrintFloat));
        ret.overloads[print].push(ir::Func::Builtin(ir::BuiltinFunc::PrintInteger));
        ret.overloads[print].push(ir::Func::Builtin(ir::BuiltinFunc::PrintBoolean));
        ret.overloads[print].push(ir::Func::Builtin(ir::BuiltinFunc::PrintString));
        ret
    }
    pub fn get_or_insert(&mut self, name: &'id str) -> usize {
        *self.names.entry(name).or_insert_with(|| {
            let symbol_id = self.overloads.len();
            self.overloads.push(Vec::new());
            symbol_id
        })
    }
    pub fn add_def(&mut self, symbol_id: usize, def: ast::FuncDef) {
        let entity_id = self.defs.len();
        self.overloads[symbol_id].push(ir::Func::UserDefined(entity_id));
        self.defs.push(def);
    }
    pub fn into_inner(
        self,
    ) -> (
        HashMap<&'id str, usize>,
        Vec<Vec<ir::Func>>,
        Vec<ast::FuncDef>,
    ) {
        (self.names, self.overloads, self.defs)
    }
}
