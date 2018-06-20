use std::fmt::{self, Display};
use syntax::ast::Linkage;
use syntax::ast::{Sign, Size};
use temp::{Label, Temp};
use util::symbol::Symbol;

#[derive(Debug)]
pub struct Program {
    pub functions: Vec<Function>,
}

#[derive(Debug)]
pub struct Function {
    pub name: Symbol,
    pub body: Vec<Instruction>,
    pub linkage: Linkage,
}

#[derive(Debug)]
pub enum Instruction {
    /// Store a value into a register
    Store(Temp, Value),
    /// Copy the contents of x into y
    /// i.e x = y
    Copy(Temp, Value),
    /// Jump to a label
    Jump(Label),
    /// CAST the expresion to a different type treating it a
    Cast(Temp, Sign, Size), //TODO take into account sign

    /// Binary operation and store in Temp
    BinOp(Temp, BinOp, Value, Value),

    /// Unary Op store in Temp
    UnOp(Temp, UnOp, Value),

    /// Evaluate l1, l2 compare using CmpOp and then got to L or R
    CJump(Temp, CmpOp, Temp, Label, Label),
    /// A Value
    Value(Value),
    /// Call a function with arguments
    Call(Temp, Label, Vec<Temp>),
    /// Empty Label
    Label(Label),
    /// Return
    Return(Value),
    /// Load
    Load(Value),

    /// Block
    Block(Temp, Vec<Value>),
}

#[derive(Debug)]
pub enum Value {
    /// Integer Constant
    Const(u64, Sign, Size),
    /// A named variable
    Name(Label),
    /// A Temporary similar to a register
    Temp(Temp),
    //  Contents of a word of memory at address
    Mem(Vec<u8>),
}

#[derive(Debug)]
pub enum BinOp {
    Plus,
    Minus,
    Mul,
    Div,
    And,
    Or,
}

#[derive(Debug)]
pub enum UnOp {
    Bang,
    Minus,
}

#[derive(Debug)]
pub enum CmpOp {
    LT,
    GT,
    LTE,
    GTE,
    EQ,
    NE,
}

impl Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for func in &self.functions {
            write!(f, "{}", func)?;
            writeln!(f)?;
        }

        Ok(())
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "function {}", self.name)?;

        write!(f, "(")?;

        write!(f, ")")?;

        write!(f, " {{")?;

        writeln!(f)?;

        for instruction in &self.body {
            writeln!(f, "    {:?}", instruction)?;
        }

        write!(f, "}}")
    }
}

impl Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            BinOp::Plus => write!(f, "+"),
            BinOp::Minus => write!(f, "-"),
            BinOp::Mul => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
            BinOp::And => write!(f, "and"),
            BinOp::Or => write!(f, "or"),
        }
    }
}

impl Display for CmpOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            CmpOp::LT => write!(f, "<"),
            CmpOp::LTE => write!(f, "<="),
            CmpOp::GT => write!(f, ">"),
            CmpOp::GTE => write!(f, ">="),
            CmpOp::NE => write!(f, "!="),
            CmpOp::EQ => write!(f, "=="),
        }
    }
}

impl Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            UnOp::Bang => write!(f, "!"),
            UnOp::Minus => write!(f, "-"),
        }
    }
}
