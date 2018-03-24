use util::symbol::{FactoryMap, Table};
use constraints::{TyCon, Type};
use syntax::ast::Ident;
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
        Env {
            types: Table::new(Rc::clone(&strings)),
            vars: Table::new(Rc::clone(&strings)),
        }
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

    pub fn name(&self, ident: Ident) -> String {
        self.vars.name(ident)
    }
}
