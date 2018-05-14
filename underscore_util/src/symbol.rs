//! This module provides a Symbols which keeps a track of the mappings between a
//! `Symbol` and a `String`

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{self, Display};
use std::hash::Hash;
use std::rc::Rc;

#[derive(Hash, Debug, Copy, Clone, PartialEq, Eq, Default)]
pub struct Symbol(pub u32);

#[derive(Debug, Clone, Default)]
/// Maps any T to a string
pub struct SymbolMap<T: Copy + Eq + Hash + Default> {
    pub next: RefCell<u32>,
    pub mappings: RefCell<HashMap<T, String>>,
}

#[derive(Debug, Clone)]
/// A Scoped Map that takes any K and V
pub struct Symbols<V: Clone> {
    pub strings: Rc<SymbolMap<Symbol>>,
    pub table: HashMap<Symbol, Vec<V>>,
    scopes: Vec<Option<Symbol>>,
}

impl<V: Clone> Symbols<V> {
    /// A new Symbols Instance
    pub fn new(strings: Rc<SymbolMap<Symbol>>) -> Self {
        Symbols {
            strings,
            table: HashMap::new(),
            scopes: vec![],
        }
    }

    /// Adds a new scope to the table
    pub fn begin_scope(&mut self) {
        self.scopes.push(None);
    }
    /// Recursivly destorys the scopes
    pub fn end_scope(&mut self) {
        while let Some(Some(symbol)) = self.scopes.pop() {
            let mapping = self.table.get_mut(&symbol).expect("Symbol not in table");
            mapping.pop();
        }
    }

    /// Enters a peice of data into the current scope
    pub fn enter(&mut self, symbol: Symbol, data: V) {
        let mapping = self.table.entry(symbol).or_insert_with(Vec::new);
        mapping.push(data);

        self.scopes.push(Some(symbol));
    }

    /// Looks in the table for the `Symbol` and if found returns the top element in
    /// the stack of Vec<T>
    pub fn look(&self, symbol: Symbol) -> Option<&V> {
        self.table.get(&symbol).and_then(|vec| vec.last())
    }

    /// Finds the name given to a `Symbol`
    pub fn name(&self, symbol: Symbol) -> String {
        self.strings.mappings.borrow()[&symbol].to_owned()
    }

    // /// Checks if the given name allready exists within the table
    // /// a new `Symbol` is returned else the previous `Symbol`
    pub fn symbol(&mut self, name: &str) -> Symbol {
        for (key, value) in self.strings.mappings.borrow().iter() {
            if value == name {
                return *key;
            }
        }
        let symbol = Symbol(*self.strings.next.borrow());
        self.strings
            .mappings
            .borrow_mut()
            .insert(symbol, name.to_owned());
        *self.strings.next.borrow_mut() += 1;
        symbol
    }
    /// Inserts the `Symbol` into the table
    pub fn replace(&mut self, symbol: Symbol, data: V) {
        let bindings = self.table.entry(symbol).or_insert_with(Vec::new);
        bindings.pop().expect("Call enter() before replace()");
        bindings.push(data);
    }
}

impl<T: Copy + Eq + Hash + Default> SymbolMap<T> {
    pub fn new() -> SymbolMap<T> {
        Self::default()
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "s{}", self.0)
    }
}
