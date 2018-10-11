use syntax::ast::{Sign, Size,Linkage};
use util::symbol::Symbol;
use std::fmt::{Display,Debug,self};

static mut LABEL_COUNT: u32 = 0;

static mut TEMP_COUNT: u32 = 0;

/// A label in the code.
#[derive(Debug, Clone, Hash, PartialEq)]
pub enum Label {
    Named(String),
    Int(u32),
}

/// A temp label that can either be a temp location or a register
#[derive(Clone, Copy, Hash, PartialEq,Default)]
pub struct Temp(u32);

#[derive(Debug)]
pub struct Program {
    pub functions: Vec<Function>,
}

#[derive(Debug)]
pub struct Function {
    pub name: Symbol,
    pub params:Vec<Label>,
    pub body: Vec<Instruction>,
    pub linkage: Linkage,
}

impl Label {
    pub fn named(name: String) -> Label {
        Label::Named(name)
    }

    pub fn new() -> Label {
        let count = unsafe { LABEL_COUNT };

        let label = Label::Int(count);

        unsafe {
            LABEL_COUNT += 1;
        }

        label
    }
}

impl Temp {
    pub fn new() -> Temp {
        let count = unsafe { TEMP_COUNT };

        let temp = Temp(count);

        unsafe {
            TEMP_COUNT += 1;
        }

        temp
    }
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
/// Instruction used in the IR
/// Instructions are of the form i <- a op b
#[derive(Debug)]
pub enum Instruction {
    Label(Label),
    StatementStart,
    Jump(Label),
    Binary(Temp, Value, BinaryOp, Value),
    /// t1 = val
    Store(Value, Value),

    Cast(Value, Sign, Size),
    /// t1 = op a
    Unary(Value, Value, UnaryOp),

    Return(Label),

    Call(Value, Label, Vec<Value>),

    /// Evaluate l1, l2 compare using CmpOp and then got to L or R
    CJump(Value, CmpOp, Value, Label, Label),
}

#[derive(Debug)]
pub enum BinaryOp {
    Plus,
    Minus,
    Mul,
    Div,
    And,
    Or,
}

#[derive(Debug)]
pub enum UnaryOp {
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

impl Display for Temp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "t{}", self.0)
    }
}

impl Debug for Temp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "t{}", self.0)
    }
}

impl Display for Label {
    fn fmt(&self,f:&mut fmt::Formatter) -> fmt::Result {
        match *self {
            Label::Int(ref i) => write!(f, "l{}",i),
            Label::Named(ref s) => write!(f, "{}",s)
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Const(ref v, ref sign, ref size) => write!(f, "{}{}{}", v, sign, size),
            Value::Name(ref name) => write!(f, "{}", name),
            Value::Temp(ref temp) => write!(f, "{}", temp),
            Value::Mem(ref bytes) => {
                write!(f, "[")?;

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

impl Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            BinaryOp::Plus => write!(f, "+"),
            BinaryOp::Minus => write!(f, "-"),
            BinaryOp::Mul => write!(f, "*"),
            BinaryOp::Div => write!(f, "/"),
            BinaryOp::And => write!(f, "and"),
            BinaryOp::Or => write!(f, "or"),
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

impl Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            UnaryOp::Bang => write!(f, "!"),
            UnaryOp::Minus => write!(f, "-"),
        }
    }
}

#[cfg(test)]
mod test {
    use super::{BinaryOp, Instruction, Label, Sign, Size, Temp, Value};
    #[test]
    fn it_works() {
        // a - 2*b

        let mut insts = vec![];
        let t1 = Temp::new(); // b
        let t2 = Temp::new();
        let t3 = Temp::new();
        let t4 = Temp::new();

        insts.push(Instruction::Store(
            Value::Temp(t1),
            Value::Name(Label::named("b".to_string())),
        )); // t1 <-b

        insts.push(Instruction::Binary(
            t2,
            Value::Const(2, Sign::Unsigned, Size::Bit32),
            BinaryOp::Mul,
            Value::Temp(t1),
        ));  // t2 <- 2 * t1

        insts.push(Instruction::Store(
            Value::Temp(t3),
            Value::Name(Label::Named("b".to_string())),
        )); // t3 <- a;

        insts.push(Instruction::Binary(
            t4,
            Value::Temp(t2),
            BinaryOp::Minus,
            Value::Temp(t3),
        )); // t4 <- t2 - t3
    }
}
