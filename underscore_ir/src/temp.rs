use std::fmt::{self, Debug, Display};

// pub type Label = Symbol;

/// A Temporary address in assembly language.
#[derive(Clone, Copy, PartialEq, PartialOrd, Hash, Eq)]
pub struct Temp(pub u32);

static mut TEMP_COUNT: u32 = 1;
static mut LABEL_COUNT: u32 = 0;

/// A Label represents an address in assembly language.
#[derive(Debug, Clone, PartialEq, PartialOrd, Copy)]
pub struct Label(pub u32);

impl Label {
    pub fn new() -> Self {
        unsafe {
            let label = Label(LABEL_COUNT);

            LABEL_COUNT += 1;
            label
        }
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
        write!(f, "l{}", self.0)
    }
}
