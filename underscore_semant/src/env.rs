use types::{TyCon, Type};

use std::rc::Rc;
use syntax::ast::{Sign, Size};
use util::symbol::{Symbol, SymbolMap, Symbols};

#[derive(Debug, Clone)]
pub enum Entry {
    TyCon(TyCon),
    Ty(Type),
}

#[derive(Debug, Clone)]
pub struct Env {
    types: Symbols<Entry>,
    vars: Symbols<Type>,
    pub escapes: Symbols<(u32, bool)>,
}

impl Env {
    pub fn new(strings: &Rc<SymbolMap<Symbol>>) -> Self {
        let mut types = Symbols::new(strings.clone());
        let string_ident = types.symbol("str");
        let i8_ident = types.symbol("i8");
        let u8_ident = types.symbol("u8");
        let i32_ident = types.symbol("i32");
        let u32_ident = types.symbol("u32");
        let i64_ident = types.symbol("i64");
        let u64_ident = types.symbol("u64");

        let nil_ident = types.symbol("nil");
        let bool_ident = types.symbol("bool");

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
        types.enter(nil_ident, Type::Nil);
        types.enter(string_ident, Type::App(TyCon::String, vec![]));

        Env {
            types: Symbols::new(Rc::clone(strings)),
            vars: Symbols::new(Rc::clone(strings)),
            escapes: Symbols::new(Rc::clone(strings)),
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

    pub fn look_type(&self, ident: Symbol) -> Option<&Entry> {
        self.types.look(ident)
    }

    pub fn replace_type(&mut self, ident: Symbol, data: Entry) {
        self.types.replace(ident, data)
    }

    pub fn look_var(&self, ident: Symbol) -> Option<&Type> {
        self.vars.look(ident)
    }

    pub fn add_type(&mut self, ident: Symbol, data: Entry) {
        self.types.enter(ident, data)
    }

    pub fn add_var(&mut self, ident: Symbol, data: Type) {
        self.vars.enter(ident, data)
    }

    pub fn name(&self, ident: Symbol) -> String {
        self.vars.name(ident)
    }
}
