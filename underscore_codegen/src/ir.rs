use temp::{Label, Temp};
use std::fmt::{self,Display};

#[derive(Debug)]
pub enum Instruction {
    /// No operation
    Nop,
    /// Store a value into a register
    Store(Temp, Value),
    /// Copy the contents of x into y
    /// i.e x = y
    Copy(Temp, Temp),
    /// Jump to a label
    Jump(Label),

    /// Binary operation and store in Temp
    BinOp(BinOp, Temp,Temp,Temp),

    /// Unary Op store in Temp
    UnOp(UnOp, Temp, Temp),

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
    Const(u64),
    /// A named variable
    Name(Label),
    /// A Temporary similar to a register
    Temp(Temp),
    //  Contents of a word of memory at address
    Mem(Vec<u8>),
}



impl Display for Instruction {
    fn fmt(&self,f:&mut fmt::Formatter) -> fmt::Result {
        match *self {
            Instruction::Store(ref temp,ref value) => {
                write!(f,"\n{} := {}",temp,value)
            },
            Instruction::Value(ref value) => write!(f,"{}",value),
            Instruction::BinOp(ref op,ref v1,ref v2,ref t) => {
                write!(f,"\n{} := {} {:?} {}",t,v1,op,v2)
            },

            Instruction::Copy(ref t1,ref t2) => write!(f,"\n{} = {}",t1,t2),
            Instruction::UnOp(ref op,ref t1,ref t2) => write!(f,"\n{} := {:?} {}",t1,op,t2),
            ref e_ => unimplemented!("{:?}",e_)
        }
    }
}

impl Display for Value {
    fn fmt(&self,f:&mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Const(ref v) => write!(f, "{}",v),
            Value::Name(ref name)  => write!(f,"{:?}",name),
            Value::Temp(ref temp) => write!(f,"{}",temp),
            Value::Mem(ref bytes) => {
                write!(f,"[");
                for byte in bytes {
                    write!(f,"{}",byte)?;
                }
                write!(f,"]")
            }
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