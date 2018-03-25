use util::symbol::{FactoryMap, Table};
use constraints::{TyCon, Type};
use syntax::ast::{Ident, Sign, Size};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Entry {
    TyCon(TyCon),
    Ty(Type),
}

#[derive(Debug, Clone)]
pub struct Env {
    types: Table<Ident, Entry>,
    vars: Table<Ident, Type>,
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
        let char_ident = types.ident("char");
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
        types.enter(
            char_ident,
            Type::App(TyCon::Int(Sign::Unsigned, Size::Bit8), vec![]),
        );

        Env {
            types: Table::new(Rc::clone(&strings)),
            vars: Table::new(Rc::clone(&strings)),
        }
    }

    pub fn begin_scope(&mut self) {
        self.types.begin_scope();
        self.vars.begin_scope();
    }
    pub fn look_type(&self, ident: Ident) -> Option<&Entry> {
        self.types.look(ident)
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
