use std::fmt::{self, Display};
use syntax::ast::{Sign, Size};
use temp::{Label, Temp};
use util;
#[derive(Debug)]
pub enum Instruction {
    /// Store a value into a register
    Store(Temp, Value),
    /// Copy the contents of x into y
    /// i.e x = y
    Copy(Temp, Temp),
    /// Jump to a label
    Jump(Label),

    /// Jump to a label witha temp
    TJump(Temp, Label),

    /// CAST the expresion to a different type treating it a
    Cast(Temp, Sign, Size), //TODO take into account sign

    /// Binary operation and store in Temp
    BinOp(BinOp, Temp, Temp, Temp),

    /// Unary Op store in Temp
    UnOp(UnOp, Temp, Temp),

    /// Evaluate l1, l2 compare using CmpOp and then got to L or R
    CJump(CmpOp, Value, Value, Label, Label),
    /// A Value
    Value(Value),
    /// A sequence  of instructions
    Block(Value, Vec<Instruction>),
    /// Call a function with arguments
    Call(Temp, Label, Vec<Temp>),
    /// Empty Label
    Label(Label),
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

impl Instruction {
    pub fn fmt<T: Clone>(&self, symbols: &mut util::symbol::Symbols<T>) -> String {
        match *self {
            Instruction::Store(ref temp, ref value) => format!("\n{} := {}", temp, value),
            Instruction::Value(ref value) => format!("{}", value),
            Instruction::BinOp(ref op, ref v1, ref v2, ref t) => {
                format!("\n{} := {} {} {}", t, v1, op, v2)
            }

            Instruction::Copy(ref t1, ref t2) => format!("\n{} = {}", t1, t2),
            Instruction::UnOp(ref op, ref t1, ref t2) => format!("\n{} := {} {}", t1, op, t2),
            Instruction::Cast(ref t1, ref sign, ref size) => {
                format!("\nt1 := {}:{}{}", t1, sign, size)
            }
            Instruction::Call(ref t1, ref label, ref temps) => {
                let mut fmt_str = format!("\n{} := {}.call(", t1, symbols.name(*label));

                for (i, temp) in temps.iter().enumerate() {
                    if i + 1 == temps.len() {
                        fmt_str.push_str(&format!("{}", temp))
                    } else {
                        fmt_str.push_str(&format!("{},", temp));
                    }
                }

                fmt_str.push_str(")");

                fmt_str
            }
            Instruction::TJump(ref temp,ref label) => format!("\ntjump {} {}",temp,symbols.name(*label)),
            Instruction::Label(ref label) => format!("\nlabel {}",symbols.name(*label)),
            ref e_ => unimplemented!("{:?}", e_),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Const(ref v, ref sign, ref size) => write!(f, "{}:{}{}", v, sign, size),
            Value::Name(ref name) => write!(f, "{:?}", name),
            Value::Temp(ref temp) => write!(f, "{}", temp),
            Value::Mem(ref bytes) => {
                write!(f, "[");

                for (i, byte) in bytes.iter().enumerate() {
                    if i + 1 == bytes.len() {
                        write!(f, "{}", byte)?;
                    } else {
                        write!(f, "{},", byte)?;
                    }
                }

                write!(f, "]")
            }
        }
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

impl Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            UnOp::Bang => write!(f, "!"),
            UnOp::Minus => write!(f, "-"),
        }
    }
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
    // signed
    LT,
    GT,
    LTE,
    GTE,
    // signed  or unsigned
    EQ,
    NE,
}
