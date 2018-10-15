use std::collections::HashMap;
use std::fmt::{self, Debug, Display};
use syntax::ast::{Sign, Size};
pub use syntax::ast::Linkage;
use util::symbol::Symbol;

static mut LABEL_COUNT: u32 = 0;

static mut REGISTER_COUNT: u32 = 0;

static mut BLOCK_COUNT: u32 = 0;

#[derive(Debug, Clone)]
pub struct LoopDescription {
    start: BlockID,
    end: BlockID,
}

#[derive(Debug, Clone)]
pub enum BlockEnd {
    Jump(BlockID),
    Return(Value),
    End,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub instructions: Vec<Instruction>,
    pub end: BlockEnd,
}

/// A label in the code.
pub struct Label(u32);

/// A temp label that can either be a temp location or a register
#[derive(Clone, Copy, Hash, PartialEq, Default)]
pub struct Register(u32);

#[derive(Debug)]
pub struct Program {
    pub functions: Vec<Function>,
}

#[derive(Debug)]
pub struct Function {
    pub name: Symbol,
    pub params: Vec<Register>,
    pub blocks: HashMap<BlockID, Block>,
    pub body: Vec<Instruction>,
    pub linkage: Linkage,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Copy)]
pub struct BlockID(pub u32);

impl BlockID {
    pub fn new() -> BlockID {
        let count = unsafe { LABEL_COUNT };

        let id = BlockID(count);

        unsafe {
            BLOCK_COUNT += 1;
        }

        id
    }
}

impl Label {
    pub fn new() -> Label {
        let count = unsafe { LABEL_COUNT };

        let label = Label(count);

        unsafe {
            LABEL_COUNT += 1;
        }

        label
    }
}

impl Register {
    pub fn new() -> Register {
        let count = unsafe { REGISTER_COUNT };

        let temp = Register(count);

        unsafe {
            REGISTER_COUNT += 1;
        }

        temp
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    /// Integer Constant
    Const(u64, Sign, Size),
    /// A named variable
    Name(Symbol),
    /// A Registerorary similar to a register
    Register(Register),
    //  Contents of a word of memory at address
    Mem(Vec<u8>),
}
/// Instruction used in the IR
/// Instructions are of the form i <- a op b
#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    /// A stackallocated array of size whatever
    /// Stored at a location
    Array(Value, usize),
    Label(Label),
    StatementStart,
    Jump(Label),
    Binary(Register, Value, BinaryOp, Value),
    /// t1 = val
    Store(Value, Value),

    Cast(Value, Sign, Size),
    /// t1 = op a
    Unary(Value, Value, UnaryOp),

    Return(Value),

    Call(Value, Value, Vec<Value>),

    /// Evaluate l1, l2 compare using CmpOp and then got to L or R
    CJump(Value, CmpOp, Value, Label, Label),

    /// Jumps to a label if the condtion is true
    JumpIf(Value, Label),

    /// Jumps to a label if the condition is false
    JumpNot(Value, Label),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Plus,
    Minus,
    Mul,
    Div,
    Gt,
    Gte,
    Lt,
    Lte,
    Equal,
    NotEqual,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Bang,
    Minus,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CmpOp {
    LT,
    GT,
    LTE,
    GTE,
    EQ,
    NE,
}

impl Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "t{}", self.0)
    }
}

impl Debug for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "t{}", self.0)
    }
}

impl Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "l{}", self.0)
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Const(ref v, ref sign, ref size) => write!(f, "{}{}{}", v, sign, size),
            Value::Name(ref name) => write!(f, "{}", name),
            Value::Register(ref temp) => write!(f, "{}", temp),
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
            BinaryOp::Lt => write!(f, "<"),
            BinaryOp::Gt => write!(f, ">"),
            BinaryOp::Lte => write!(f, "<="),
            BinaryOp::Gte => write!(f, ">="),
            BinaryOp::Equal => write!(f, "=="),
            BinaryOp::NotEqual => write!(f, "!="),
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


impl Display for BlockID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "label_{}",self.0)
    }
}

impl Display for BlockEnd {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            BlockEnd::End => write!(f, "end"),
            BlockEnd::Jump(ref id) => write!(f,"goto {}",id),
            BlockEnd::Return(ref id) => write!(f, "return {}",id),
        }
    }
}


#[cfg(test)]
mod test {
    use super::{BinaryOp, Instruction, Label, Register, Sign, Size, Value};
    #[test]
    fn it_works() {
        // a - 2*b

        let mut insts = vec![];
        let t1 = Register::new(); // b
        let t2 = Register::new();
        let t3 = Register::new();
        let t4 = Register::new();

        insts.push(Instruction::Store(
            Value::Register(t1),
            Value::Name(Symbol(1)),
        )); // t1 <-b

        insts.push(Instruction::Binary(
            t2,
            Value::Const(2, Sign::Unsigned, Size::Bit32),
            BinaryOp::Mul,
            Value::Register(t1),
        )); // t2 <- 2 * t1

        insts.push(Instruction::Store(
            Value::Register(t3),
            Value::Name(Symbol(2)),
        )); // t3 <- a;

        insts.push(Instruction::Binary(
            t4,
            Value::Register(t2),
            BinaryOp::Minus,
            Value::Register(t3),
        )); // t4 <- t2 - t3
    }
}
