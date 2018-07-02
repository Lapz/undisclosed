use std::fmt::{self, Display};

#[derive(Debug, Clone)]
pub enum Register {
    RDI,
    RSI,
    RDX,
    RCX,
    R8,
    R9,
    RAX,
    RBP(i32),
}

impl Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Register::RDI => write!(f, "%rdi"),
            Register::RSI => write!(f, "%rsi"),
            Register::RDX => write!(f, "%rdx"),
            Register::RCX => write!(f, "%rcx"),
            Register::R8 => write!(f, "%r8"),
            Register::R9 => write!(f, "%r9"),
            Register::RAX => write!(f, "%rax"),
            Register::RBP(ref i) => write!(f, "{}(%rbp)",i),
        }
    }
}
