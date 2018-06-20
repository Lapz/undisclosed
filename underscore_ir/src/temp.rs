use std::fmt::{self, Debug, Display};
use util::symbol::{Symbol, Symbols};

// pub type Label = Symbol;

/// A Temporary address in assembly language.
#[derive(Clone, Copy, PartialEq, PartialOrd, Hash, Eq)]
pub struct Temp(pub u32);

static mut TEMP_COUNT: u32 = 1;
static mut LABEL_COUNT: u32 = 0;

/// A Label represents an address in assembly language.
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Label {
    Int(u32),
    Named(String),
}

impl Label {
    pub fn new() -> Self {
        unsafe {
            let label = Label::Int(LABEL_COUNT);

            LABEL_COUNT += 1;
            label
        }
    }

    pub fn named(name: &str) -> Label {
        Label::Named(name.into())
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

impl Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Label::Int(ref i) => write!(f, "l{}", i),
            Label::Named(ref n) => write!(f, "l{}", n),
        }
    }
}
