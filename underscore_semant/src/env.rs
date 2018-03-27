use constraints::{MetaVar, TyCon, Type};
use std::rc::Rc;
use syntax::ast::{Ident, Sign, Size};
use util::symbol::{FactoryMap, Table};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Entry {
    TyCon(TyCon),
    Ty(Type),
}

#[derive(Debug, Clone)]
pub struct Env {
    types: Table<Ident, Entry>,
    vars: Table<Ident, Type>,
    pub metavars: HashMap<MetaVar, Type>,
}

trait GetIdent {
    fn ident(&mut self, name: &str) -> Ident;
}

impl GetIdent for Table<Ident, Type> {
    fn ident(&mut self, name: &str) -> Ident {
        for (key, value) in self.strings.mappings.borrow().iter() {
            if value == name {
                return *key;
            }
        }
        let symbol = Ident(*self.strings.next.borrow());
        self.strings
            .mappings
            .borrow_mut()
            .insert(symbol, name.to_owned());
        *self.strings.next.borrow_mut() += 1;
        symbol
    }
}

impl Env {
    pub fn new(strings: &Rc<FactoryMap<Ident>>) -> Self {
        let mut types = Table::new(strings.clone());
        let string_ident = types.ident("str");
        let i8_ident = types.ident("i8");
        let u8_ident = types.ident("u8");
        let i32_ident = types.ident("i32");
        let u32_ident = types.ident("u32");
        let i64_ident = types.ident("i64");
        let u64_ident = types.ident("u64");

        let nil_ident = types.ident("nil");
        let bool_ident = types.ident("bool");
        
        types.enter(
            i8_ident,
            Type::App(TyCon::Int(Sign::Signed, Size::Bit8), vec![]),
        );

        types.enter(
            u8_ident,
            Type::App(TyCon::Int(Sign::Unsigned, Size::Bit8), vec![]),
        );

        types.enter(
            i32_ident,
            Type::App(TyCon::Int(Sign::Signed, Size::Bit32), vec![]),
        );
        types.enter(
            u32_ident,
            Type::App(TyCon::Int(Sign::Unsigned, Size::Bit32), vec![]),
        );

        types.enter(
            i64_ident,
            Type::App(TyCon::Int(Sign::Signed, Size::Bit64), vec![]),
        );
        types.enter(
            u64_ident,
            Type::App(TyCon::Int(Sign::Unsigned, Size::Bit64), vec![]),
        );

        types.enter(bool_ident, Type::App(TyCon::Bool, vec![]));
        types.enter(nil_ident, Type::App(TyCon::Void, vec![]));
        types.enter(string_ident, Type::App(TyCon::String, vec![]));

        Env {
            metavars: HashMap::new(),
            types: Table::new(Rc::clone(&strings)),
            vars: Table::new(Rc::clone(&strings)),
        }
    }

    pub fn begin_scope(&mut self) {
        self.types.begin_scope();
        self.vars.begin_scope();
    }

    pub fn end_scope(&mut self) {
        self.types.end_scope();
        self.vars.end_scope();
    }

    pub fn look_type(&self, ident: Ident) -> Option<&Entry> {
        self.types.look(ident)
    }

    pub fn look_meta(&self, ident: MetaVar) -> Option<&Type> {
        self.metavars.get(&ident)
    }

    pub fn look_var(&self, ident: Ident) -> Option<&Type> {
        self.vars.look(ident)
    }

    pub fn add_type(&mut self, ident: Ident, data: Entry) {
        self.types.enter(ident, data)
    }

    pub fn add_var(&mut self, ident: Ident, data: Type) {
        self.vars.enter(ident, data)
    }

    pub fn name(&self, ident: Ident) -> String {
        self.vars.name(ident)
    }
}
