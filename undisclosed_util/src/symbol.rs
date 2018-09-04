//! This module provides a SymbolMap which keeps a track of the mappings between a
//! `Symbol` and a `String`

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{self, Display};
use std::hash::Hash;
use std::rc::Rc;

#[derive(Hash, Debug, Copy, Clone, PartialEq, Eq, Default, PartialOrd)]
pub struct Symbol(pub u32);

#[derive(Debug, Clone, Default)]
/// Maps any T to a string
pub struct Hasher<T: Copy + Eq + Hash> {
    pub next: RefCell<u32>,
    pub mappings: RefCell<HashMap<T, String>>,
}

#[derive(Debug, Clone)]
/// A Scoped Map that takes any K and
pub struct ScopedMap<K: Eq + Hash, V>
where
    K: Eq + Hash,
{
    table: HashMap<K, Vec<V>>,
    scopes: Vec<Option<K>>,
}

impl<K: Eq + Hash + Copy, V> ScopedMap<K, V> {
    pub fn new() -> Self {
        Self {
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

    /// Enters value into the current scope
    pub fn insert(&mut self, key: K, value: V) {
        let mapping = self.table.entry(key).or_insert_with(Vec::new);
        mapping.push(value);
        self.scopes.push(Some(key));
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        self.table.get(key).and_then(|vec| vec.last())
    }

    pub fn replace(&mut self, key: K, value: V) {
        let bindings = self.table.entry(key).or_insert_with(Vec::new);
        bindings.pop().expect("Call enter() before replace()");
        bindings.push(value);
    }
}

#[derive(Debug, Clone)]
pub struct SymbolMap<V: Clone> {
    hasher: Rc<Hasher<Symbol>>,
    map: ScopedMap<Symbol, V>,
}

impl<V: Clone> SymbolMap<V> {
    /// A new SymbolMap Instance
    pub fn new(hasher: Rc<Hasher<Symbol>>) -> Self {
        SymbolMap {
            hasher,
            map: ScopedMap::new(),
        }
    }

    /// Adds a new scope to the table
    pub fn begin_scope(&mut self) {
        self.map.begin_scope()
    }
    /// Recursivly destorys the scopes
    pub fn end_scope(&mut self) {
        self.map.end_scope()
    }

    /// Enters a peice of value into the current scope
    pub fn insert(&mut self, symbol: Symbol, value: V) {
        self.map.insert(symbol, value)
    }

    /// Looks in the table for the `Symbol` and if found returns the top element in
    /// the stack of Vec<T>
    pub fn get(&self, symbol: &Symbol) -> Option<&V> {
        self.map.get(symbol)
    }

    /// Finds the name given to a `Symbol`
    pub fn name(&self, symbol: Symbol) -> String {
        self.hasher.mappings.borrow()[&symbol].to_owned()
    }

    // /// Checks if the given name allready exists within the table
    // /// a new `Symbol` is returned else the previous `Symbol`
    pub fn symbol(&mut self, name: &str) -> Symbol {
        for (key, value) in self.hasher.mappings.borrow().iter() {
            if value == name {
                return *key;
            }
        }
        let symbol = Symbol(*self.hasher.next.borrow());
        self.hasher
            .mappings
            .borrow_mut()
            .insert(symbol, name.to_owned());
        *self.hasher.next.borrow_mut() += 1;
        symbol
    }
    /// Inserts the `Symbol` into the table
    pub fn replace(&mut self, symbol: Symbol, value: V) {
        self.map.replace(symbol, value)
    }
}

impl<T: Copy + Eq + Hash + Default> Hasher<T> {
    pub fn new() -> Hasher<T> {
        Self::default()
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "s{}", self.0)
    }
}
