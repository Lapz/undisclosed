//! This module provides a Table which keeps a track of the mappings between a
//! `T` and a `String`

use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::Hash;
use std::rc::Rc;

#[derive(Debug, Clone, Default)]
/// Maps any T to a string
pub struct FactoryMap<T: Copy + Eq + Hash + Default> {
    pub next: RefCell<u32>,
    pub mappings: RefCell<HashMap<T, String>>,
}

#[derive(Debug, Clone)]
/// A Scoped Map that takes any K and V
pub struct Table<K: Clone + Hash + Eq + Copy + Default, V: Clone> {
    pub strings: Rc<FactoryMap<K>>,
    pub table: HashMap<K, Vec<V>>,
    scopes: Vec<Option<K>>,
}

impl<K: Clone + Hash + Eq + Copy + Default, V: Clone> Table<K, V> {
    /// A new Table Instance
    pub fn new(strings: Rc<FactoryMap<K>>) -> Self {
        Table {
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
    pub fn enter(&mut self, symbol: K, data: V) {
        let mapping = self.table.entry(symbol).or_insert_with(Vec::new);
        mapping.push(data);

        self.scopes.push(Some(symbol));
    }

    /// Looks in the table for the `Symbol` and if found returns the top element in
    /// the stack of Vec<T>
    pub fn look(&self, symbol: K) -> Option<&V> {
        self.table.get(&symbol).and_then(|vec| vec.last())
    }

    /// Finds the name given to a `Symbol`
    pub fn name(&self, symbol: K) -> String {
        self.strings.mappings.borrow()[&symbol].to_owned()
    }

    // /// Checks if the given name allready exists within the table
    // /// a new `Symbol` is returned else the previous `Symbol`
    // pub fn symbol(&mut self, name: &str) -> K {
    //     for (key, value) in self.strings.mappings.borrow().iter() {
    //         if value == name {
    //             return *key;
    //         }
    //     }
    //     let symbol = K(*self.strings.next.borrow());
    //     self.strings
    //         .mappings
    //         .borrow_mut()
    //         .insert(symbol, name.to_owned());
    //     *self.strings.next.borrow_mut() += 1;
    //     symbol
    // }
    /// Inserts the `Symbol` into the table
    pub fn replace(&mut self, symbol: K, data: V) {
        let bindings = self.table.entry(symbol).or_insert_with(Vec::new);
        bindings.pop().expect("Call enter() before replace()");
        bindings.push(data);
    }
}

impl<T: Copy + Eq + Hash + Default> FactoryMap<T> {
    pub fn new() -> FactoryMap<T> {
        Self::default()
    }
}
