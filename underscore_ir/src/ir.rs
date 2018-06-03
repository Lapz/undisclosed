use std::fmt::{self, Display,Debug};
use syntax::ast::{Sign, Size};
use syntax::ast::Linkage;
use util::symbol::{Symbol, Symbols};


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
    Copy(Temp, Temp),
    /// Jump to a label
    Jump(Label),
    /// CAST the expresion to a different type treating it a
    Cast(Temp, Sign, Size), //TODO take into account sign

    /// Binary operation and store in Temp
    BinOp(Temp,BinOp, Temp, Temp),

    /// Unary Op store in Temp
    UnOp( Temp,UnOp, Temp),

    /// Evaluate l1, l2 compare using CmpOp and then got to L or R
    CJump(Temp, CmpOp, Temp, Label, Label),
    /// A Value
    Value(Value),
    /// Call a function with arguments
    Call(Temp, Label, Vec<Temp>),
    /// Empty Label
    Label(Label),
    /// Return
    Return(Temp),
    /// Load
    Load(Temp),

    /// Block
    Block(Temp, Vec<Temp>),
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
    pub fn fmt<T: Clone>(&self, symbols: &mut Symbols<T>) -> String {
        match *self {
            Instruction::Store(ref temp, ref value) => format!("   {} := {}", temp, value),
            Instruction::Value(ref value) => format!("\n{}", value),
            Instruction::BinOp(ref t1, ref op, ref v1, ref v2) => {
                format!("{} := {} {} {}", t1, op,v1, v2)
            }

            Instruction::Block(ref temp, ref temps) => {
                let mut fmt_str = format!("\n{} := [", temp);

                for (i, temp) in temps.iter().enumerate() {
                    if i + 1 == temps.len() {
                        fmt_str.push_str(&format!("{}", temp))
                    } else {
                        fmt_str.push_str(&format!("{},", temp));
                    }
                }

                fmt_str.push_str("]");

                fmt_str
            }

            Instruction::Load(ref temp) => format!("\nload{}", temp),

            Instruction::Copy(ref t1, ref t2) => format!("\n{} := {}", t1, t2),
            Instruction::UnOp(ref t1, ref op, ref t2) => format!("\n{} := {} {}", t1, op, t2),
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
            Instruction::Jump(ref label) => format!("\njump {}", symbols.name(*label)),
            Instruction::CJump(ref t1, ref op ,ref t2, ref ltrue, ref lfalse) => format!(
                "\nif {} {} {} then {} else {}",
                t1,
                op,
                t2,
                symbols.name(*ltrue),
                symbols.name(*lfalse)
            ),
            Instruction::Label(ref label) => format!("\nlabel {}", symbols.name(*label)),
            Instruction::Return(ref ret) => format!("\nret {}", ret),
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
    LT,
    GT,
    LTE,
    GTE,
    EQ,
    NE,
}


// use syntax::ast::Ident;

// use std::io::{self, Write};

/// A Label represents an address in assembly language.
pub type Label = Symbol;

/// A Temporary address in assembly language.
#[derive(Clone, Copy, PartialEq, PartialOrd, Hash, Eq)]
pub struct Temp(pub u32);

static mut TEMP_COUNT: u32 = 1;
static mut LABEL_COUNT: u32 = 0;
pub fn new_label<T: Clone>(symbol: Symbol, symbols: &mut Symbols<T>) -> Symbol {
    let name = symbols.name(symbol);
    symbols.symbol(&format!("l_{}", name))
}

pub fn new_named_label<T: Clone>(name: &str, symbols: &mut Symbols<T>) -> Symbol {
    unsafe {
        let label = symbols.symbol(&format!("l_{}_{}", name, LABEL_COUNT));

        LABEL_COUNT += 1;
        label
    }
}

pub fn new_label_pair<T: Clone>(
    name: &str,
    name2: &str,
    symbols: &mut Symbols<T>,
) -> (Symbol, Symbol) {
    unsafe {
        let label1 = symbols.symbol(&format!("l_{}_{}", name, LABEL_COUNT));
        let label2 = symbols.symbol(&format!("l_{}_{}", name2, LABEL_COUNT));
        LABEL_COUNT += 1;
        (label1, label2)
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
impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Const(ref v, ref sign, ref size) => write!(f, "{}:{}{}", v, sign, size),
            Value::Name(ref name) => write!(f, "{:?}", name),
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

impl Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Instruction::Store(ref temp, ref value) => write!(f, "{} := {}", temp, value),
            Instruction::Value(ref value) => write!(f, "{}", value),
            Instruction::BinOp(ref op, ref v1, ref v2, ref t) => {
                write!(f, "{} := {} {} {}", t, v1, op, v2)
            }

            Instruction::Block(ref temp, ref temps) => {
                write!(f, "{} := [", temp)?;

                for (i, temp) in temps.iter().enumerate() {
                    if i + 1 == temps.len() {
                        write!(f, "{}", temp)?;
                    } else {
                        write!(f, "{},", temp)?;
                    }
                }

                write!(f, "]")
            }

            Instruction::Load(ref temp) => write!(f, "load{}", temp),

            Instruction::Copy(ref t1, ref t2) => write!(f, "{} := {}", t1, t2),
            Instruction::UnOp(ref op, ref t1, ref t2) => write!(f, "{} := {} {}", t1, op, t2),
            Instruction::Cast(ref t1, ref sign, ref size) => {
                write!(f, "t1 := {}:{}{}", t1, sign, size)
            }
            Instruction::Call(ref t1, ref label, ref temps) => {
                write!(f, "{} := {}.call(", t1, label)?;

                for (i, temp) in temps.iter().enumerate() {
                    if i + 1 == temps.len() {
                        write!(f, "{}", temp)?;
                    } else {
                        write!(f, "{},", temp)?;
                    }
                }

                write!(f, ")")
            }
            Instruction::Jump(ref label) => write!(f, "jump {}", label),
            Instruction::CJump(ref op, ref t1, ref t2, ref ltrue, ref lfalse) => {
                write!(f, "if {} {} {} then {} else {}", t1, op, t2, ltrue, lfalse)
            }
            Instruction::Label(ref label) => write!(f, "label {}", label),
            Instruction::Return(ref ret) => write!(f, "ret {}", ret),
        }
    }
}