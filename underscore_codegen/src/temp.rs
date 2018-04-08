use std::fmt;
use std::fmt::{Debug, Display};
use std::io::{self, Write};
use syntax::ast::Ident;
/// A Label represents an address in assembly language.
#[derive(Clone, Copy, Hash)]
pub struct Label(pub u32, Ident);

/// A Temporary address in assembly language.
#[derive(Clone, Copy, PartialEq, PartialOrd, Hash)]
pub struct Temp(pub u32);

static mut LABEL_COUNT: u32 = 0;
static mut Temp_COUNT: u32 = 0;

impl Label {
    /// Makes a new label with a given Ident.
    /// Warning: avoid repeated calls with the same name.
    pub fn new_with_sym(sym: Ident) -> Self {
        let value = unsafe { LABEL_COUNT };
        unsafe { LABEL_COUNT += 1 };
        Label(value, sym)
    }

    pub fn new() -> Self {
        let value = unsafe { LABEL_COUNT };
        unsafe { LABEL_COUNT += 1 };
        Label(value, Ident(value))
    }
}

impl Temp {
    /// Makes a new label with a given Ident.
    /// Warning: avoid repeated calls with the same name.
    pub fn new() -> Self {
        let value = unsafe { Temp_COUNT };
        unsafe { Temp_COUNT += 1 };
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

impl Debug for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "l{}", self.0)
    }
}

impl Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "l{}", self.0)
    }
}
