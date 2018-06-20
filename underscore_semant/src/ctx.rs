use env::{Entry, VarEntry, VarType};
use std::collections::HashMap;
use std::rc::Rc;
use syntax::ast::{Sign, Size};
use types::{TyCon, Type, TypeVar};
use util::emitter::Reporter;
use util::pos::Span;
use util::symbol::{Symbol, SymbolMap, Symbols};

pub struct CompileCtx<'a> {
    types: Symbols<Entry>,
    tvars: HashMap<TypeVar, VarType>,
    vars: Symbols<VarEntry>,
    escapes: Symbols<(u32, bool)>,
    temps: Symbols<::ir::Temp>,
    reporter: &'a mut Reporter,
}

impl<'a> CompileCtx<'a> {
    pub fn new(strings: &Rc<SymbolMap<Symbol>>, reporter: &'a mut Reporter) -> Self {
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

        CompileCtx {
            types: Symbols::new(strings.clone()),
            tvars: HashMap::new(),
            vars: Symbols::new(strings.clone()),
            escapes: Symbols::new(strings.clone()),
            temps: Symbols::new(strings.clone()),
            reporter,
        }
    }

    pub fn symbol(&mut self, name: &str) -> Symbol {
        self.types.symbol(name); // Adds it to both type and symbol
        self.vars.symbol(name)
    }

    pub fn begin_scope(&mut self) {
        self.types.begin_scope();
        self.vars.begin_scope();
        self.escapes.begin_scope();
        self.temps.begin_scope();
    }

    pub fn end_scope(&mut self) {
        self.types.end_scope();
        self.vars.end_scope();
        self.escapes.end_scope();
        self.temps.end_scope();
    }

    pub fn error<M: Into<String>>(&mut self, msg: M, span: Span) {
        self.reporter.error(msg, span)
    }

    pub fn remove_error(&mut self) {
        self.reporter.pop_error();
    }

    pub fn look_type(&self, ident: Symbol) -> Option<&Entry> {
        self.types.look(ident)
    }

    pub fn replace_type(&mut self, ident: Symbol, data: Entry) {
        self.types.replace(ident, data)
    }

    pub fn look_var(&self, ident: Symbol) -> Option<&VarEntry> {
        self.vars.look(ident)
    }

    pub fn look_tvar(&self, ident: TypeVar) -> Option<&VarType> {
        self.tvars.get(&ident)
    }

    pub fn look_escape(&self, ident: Symbol) -> Option<&(u32, bool)> {
        self.escapes.look(ident)
    }

    pub fn look_temp(&self, ident: Symbol) -> Option<&::ir::Temp> {
        self.temps.look(ident)
    }

    pub fn add_type(&mut self, ident: Symbol, data: Entry) {
        self.types.enter(ident, data)
    }

    pub fn add_temp(&mut self, ident: Symbol, data: ::ir::Temp) {
        self.temps.enter(ident, data)
    }

    pub fn add_var(&mut self, ident: Symbol, data: VarEntry) {
        self.vars.enter(ident, data)
    }

    pub fn add_tvar(&mut self, ident: TypeVar, data: VarType) {
        self.tvars.insert(ident, data);
    }

    pub fn add_escape(&mut self, ident: Symbol, data: (u32, bool)) {
        self.escapes.enter(ident, data)
    }

    pub fn name(&self, ident: Symbol) -> String {
        self.vars.name(ident)
    }
}
