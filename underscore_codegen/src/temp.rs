// use syntax::ast::Ident;
use std::fmt::{self, Debug, Display};
use util::symbol::{Symbol, Symbols};
// use std::io::{self, Write};

/// A Label represents an address in assembly language.
pub type Label = Symbol;

/// A Temporary address in assembly language.
#[derive(Clone, Copy, PartialEq, PartialOrd, Hash, Eq)]
pub struct Temp(pub u32);

static mut TEMP_COUNT: u32 = 1;
static mut LABEL_COUNT: u32 = 0;
pub fn new_label<T: Clone>(symbol: Symbol, symbols: &mut Symbols<T>) -> Symbol {
    let name = symbols.name(symbol);
    symbols.symbol(&format!("l_{}", name))
}

pub fn new_named_label<T: Clone>(name: &str, symbols: &mut Symbols<T>) -> Symbol {
    unsafe {
        let label = symbols.symbol(&format!("l_{}_{}", name, LABEL_COUNT));

        LABEL_COUNT += 1;
        label
    }
}

pub fn new_label_pair<T: Clone>(
    name: &str,
    name2: &str,
    symbols: &mut Symbols<T>,
) -> (Symbol, Symbol) {
    unsafe {
        let label1 = symbols.symbol(&format!("l_{}_{}", name, LABEL_COUNT));
        let label2 = symbols.symbol(&format!("l_{}_{}", name2, LABEL_COUNT));
        LABEL_COUNT += 1;
        (label1, label2)
    }
}

impl Temp {
    /// Makes a new temp with a given Ident.
    /// Warning: avoid repeated calls with the same name.
    pub fn new() -> Self {
        let value = unsafe { TEMP_COUNT };
        unsafe { TEMP_COUNT += 1 };
        Temp(value)
    }
}

impl Debug for Temp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "t{}", self.0)
    }
}

impl Display for Temp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "t{}", self.0)
    }
}
