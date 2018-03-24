use util::symbol::Table;
use constraints::{TyCon, Type, TypeVar};
use syntax::ast::Ident;

#[derive(Debug, Clone)]
pub enum Entry {
    TyCon(TyCon),
    Ty(Type),
}

#[derive(Debug, Clone)]
pub struct Env {
    types: Table<Ident, Type>,
    vars: Table<Ident, Entry>,
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
    pub fn look_type(&self, ident: Ident) -> Option<&Type> {
        self.types.look(ident)
    }

    pub fn look_var(&self, ident: Ident) -> Option<&Entry> {
        self.vars.look(ident)
    }

    pub fn name(&self, ident: Ident) -> String {
        self.vars.name(ident)
    }
}
