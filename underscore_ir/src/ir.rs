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

#[derive(Debug,Clone)]
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
    BinOp(Temp, BinOp, Box<Instruction>, Box<Instruction>),

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
    Block(Temp, Vec<Temp>),
}

impl Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Instruction::Store(ref temp, ref value) => write!(f, "{} := {}", temp, value),
            Instruction::Value(ref value) => write!(f, "{}", value),
            Instruction::BinOp(ref t1, ref op, ref v1, ref v2) => {
                write!(f, "{} := {} {} {}", t1, op, v1, v2)
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

            Instruction::Load(ref temp) => write!(f, "load{}", temp),

            Instruction::Copy(ref t1, ref t2) => write!(f, "{} := {}", t1, t2),
            Instruction::UnOp(ref t1, ref op, ref t2) => write!(f, "{} := {} {}", t1, op, t2),
            Instruction::Cast(ref t1, ref sign, ref size) => {
                write!(f, "t1 := {}:{}{}", t1, sign, size)
            }
            Instruction::Call(ref t1, ref label, ref temps) => {
                write!(f, "{} := {}.call(", t1, label)?;

                for (i, temp) in temps.iter().enumerate() {
                    if i + 1 == temps.len() {
                        write!(f, "{}", temp)?
                    } else {
                        write!(f, "{},", temp)?
                    }
                }

                write!(f, ")")
            }
            Instruction::Jump(ref label) => write!(f, "jump {}", label),
            Instruction::CJump(ref t1, ref op, ref t2, ref ltrue, ref lfalse) => {
                write!(f, "if {} {} {} then {} else {}", t1, op, t2, ltrue, lfalse)
            }
            Instruction::Label(ref label) => write!(f, "label {}", label),
            Instruction::Return(ref ret) => write!(f, "ret {}", ret),
        }
    }
}

#[derive(Debug,Clone)]
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

#[derive(Debug,Clone)]
pub enum BinOp {
    Plus,
    Minus,
    Mul,
    Div,
    And,
    Or,
}

#[derive(Debug,Clone)]
pub enum UnOp {
    Bang,
    Minus,
}

#[derive(Debug,Clone)]
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
            writeln!(f, "    {}", instruction)?;
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
