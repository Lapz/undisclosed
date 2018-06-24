use std::collections::HashMap;
use std::fmt::{self, Display};
use syntax::ast::Linkage;
use syntax::ast::{Sign, Size};
use temp::{Label, Temp};
#[derive(Debug)]
pub struct Program {
    pub functions: Vec<Function>,
}

#[derive(Debug)]
pub struct Function {
    pub name: Label,
    pub body: Vec<Instruction>,
    pub linkage: Linkage,
    pub locals: HashMap<Temp, i32>,
}

#[derive(Debug, Clone)]
pub enum Instruction {
    /// Store a value into a register
    /// i.e x = y
    Store(Temp, Value),
    /// Jump to a label
    Jump(Label),
    /// Jump to a specfic label depending on the cond
    JumpOp(CmpOp, Label),
    /// CAST the expresion to a different type treating it a
    Cast(Temp, Sign, Size), //TODO take into account sign
    /// Binary operation and store in Temp
    BinOp(Temp, BinOp, Box<Instruction>, Box<Instruction>),
    /// Unary Op store in Temp
    UnOp(Temp, UnOp, Box<Instruction>),
    /// Call a function with arguments
    Call(Label),
    /// Empty Label
    Label(Label),
    /// Return
    Return(Box<Instruction>),
    /// Load
    Load(Temp),
    /// Block
    Block(Label, Vec<Instruction>),
}

impl Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Instruction::Store(_, ref value) => write!(f, "{}", value),
            Instruction::BinOp(ref t1, ref op, ref v1, ref v2) => {
                write!(f, "{} := {} {} {}", t1, v1, op, v2)
            }

            Instruction::Block(ref temp, ref temps) => {
                write!(f, "{} := [", temp)?;

                for (i, temp) in temps.iter().enumerate() {
                    if i + 1 == temps.len() {
                        write!(f, "{}", temp)?
                    } else {
                        write!(f, "{},", temp)?
                    }
                }

                write!(f, "]")
            }

            Instruction::Load(ref temp) => write!(f, "load {}", temp),
            Instruction::UnOp(ref t1, ref op, ref t2) => write!(f, "{} := {} {}", t1, op, t2),
            Instruction::Cast(ref t1, ref sign, ref size) => {
                write!(f, "t1 := {}:{}{}", t1, sign, size)
            }
            Instruction::Call(ref label) => write!(f, "{}()", label),
            Instruction::Jump(ref label) => write!(f, "jump {}", label),
            Instruction::JumpOp(ref op, ref label) => write!(f, "jump {} {}", op, label),
            Instruction::Label(ref label) => write!(f, "label {}", label),
            Instruction::Return(ref ret) => write!(f, "ret {}", ret),
        }
    }
}

#[derive(Debug, Clone)]
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

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Const(ref v, sn, si) => write!(f, "{}{}{}", v, sn, si),
            Value::Name(ref l) => write!(f, "{}", l),
            Value::Temp(ref temp) => write!(f, "{}", temp),
            Value::Mem(ref bytes) => write!(f, "{:?}", bytes),
        }
    }
}

#[derive(Debug, Clone)]
pub enum BinOp {
    Plus,
    Minus,
    Mul,
    Div,
    Or,
    And,
    LT,
    GT,
    LTE,
    GTE,
    EQ,
    NE,
}

#[derive(Debug, Clone)]
pub enum UnOp {
    Bang,
    Minus,
}

#[derive(Debug, Clone)]
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
            BinOp::LT => write!(f, "<"),
            BinOp::LTE => write!(f, "<="),
            BinOp::GT => write!(f, ">"),
            BinOp::GTE => write!(f, ">="),
            BinOp::NE => write!(f, "!="),
            BinOp::EQ => write!(f, "=="),
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
