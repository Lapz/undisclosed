use std::collections::HashMap;
use temp::Temp;

#[derive(Debug)]
pub struct Scopes {
    scopes: Vec<HashMap<Temp, i32>>,
}

impl Scopes {
    pub fn new() -> Self {
        Scopes { scopes: vec![] }
    }

    pub fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn end_scope(&mut self) {
        self.scopes.pop().expect("Call push() before end()");
    }

    pub fn enter(&mut self, temp: Temp, data: i32) {
        let len = self.scopes.len() - 1;
        self.scopes[len].insert(temp, data);
    }

    pub fn locals(&self) -> usize {
        let len = self.scopes.len() - 1;
        self.scopes[len].len()
    }
}
