// use syntax::ast::Ident;
use std::fmt::{Debug, Display,self};
use util::symbol::{Symbols,Symbol};
// use std::io::{self, Write};


/// A Label represents an address in assembly language.
pub type Label = Symbol;

/// A Temporary address in assembly language.
#[derive(Clone, Copy, PartialEq, PartialOrd, Hash)]
pub struct Temp(pub u32);

static mut TEMP_COUNT: u32 = 0;


impl Temp {
    /// Makes a new temp with a given Ident.
    /// Warning: avoid repeated calls with the same name.
    pub fn new() -> Self {
        let value = unsafe {TEMP_COUNT };
        unsafe {TEMP_COUNT += 1 };
        Temp(value)
    }
}

impl Debug for Temp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "r{}", self.0)
    }
}

impl Display for Temp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "r{}", self.0)
    }
}

