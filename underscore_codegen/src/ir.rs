use std::io::{self, Write};
use temp::{Label, Temp};

#[derive(Debug)]
pub enum Instruction {
    /// No operation
    Nop,
    /// Load a value into a register
    Load(Temp, Value),
    /// Store a value into a register
    Store(Temp, Value),
    /// i.e x = y
    /// Copy the contents of x into y
    Copy(Temp, Temp),
    /// Jump to a label
    Jump(Label),

    /// Binary operation and store in Temp
    BinOp(BinOp, Value, Value, Temp),

    /// Unary Op store in Temp
    UnOp(UnOp, Value, Temp),

    /// Evaluate l1, l2 compare using CmpOp and then got to L or R
    CJump(CmpOp, Value, Value, Label, Label),
    /// A Value
    Value(Value),
    /// A sequence  of instructions
    Block(Value, Vec<Instruction>),
}

#[derive(Debug)]
pub enum Value {
    /// Integer Constant
    Const(u32),
    /// A named variable
    Name(Label),
    /// A Temporary similar to a register
    Temp(Temp),
    ///  Contents of a word of memory at address
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
    Plus,
    Minus,
}

#[derive(Debug)]
pub enum CmpOp {
    // signed
    LT,
    GT,
    LTE,
    GTE,

    // unsigned
    ULT,
    UGT,
    ULE,
    UGE,

    // signed  or unsigned
    EQ,
    NE,
}
