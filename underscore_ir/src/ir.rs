use std::fmt::{self, Debug, Display};
use syntax::ast::Linkage;
use syntax::ast::{Sign, Size};
use temp::{Label, Temp};
use util::symbol::{Symbol, Symbols};

#[derive(Debug)]
pub struct Program {
    pub functions: Vec<Function>,
}

#[derive(Debug)]
pub struct Function {
    pub name: Symbol,
    pub body: Vec<()>,
    pub linkage: Linkage,
}

#[derive(Debug)]
pub enum Expr {
    /// Binary operation
    BinOp {
        lhs: Box<Expr>,
        op: BinOp,
        rhs: Box<Expr>,
    },
    /// Integer Constant
    Const(u64, Sign, Size),
    /// A named variable
    Name(Label),
    /// A Temporary similar to a register
    Temp(Temp),
    /// Contents of a word of memory at address
    Mem(Box<Expr>),
    /// Call a function with arguments
    Call(Box<Expr>, Vec<Expr>),
    /// Evaluate S for side effects and then e for the result
    ESeq(Box<Stm>, Box<Expr>),
}
#[derive(Debug)]
pub enum Stm {
    /// Evaluate l1, l2 compare using CmpOp and then got to L or R
    CJump {
        lhs: Expr,
        op: CmpOp,
        rhs: Expr,
        ltrue: Label,
        lfalse: Label,
    },
    /// An Expression
    Exp(Expr),
    /// Jump to a label
    Jump(Label),
    /// Empty Label
    Label(Label),

    Move(Expr, Expr),
    /// Block
    Seq(Box<Stm>, Box<Stm>),
}

pub enum Ir {
    /// An Expression
    Expr(Expr),
    /// An Expression with no result
    NExpr(Stm),
    /// A conditional
    CExpr { ltrue: Label, lfalse: Label },
}

impl Expr {
    pub fn print<T: Clone>(&self, symbols: &mut Symbols<T>) -> String {
        match *self {
            Expr::BinOp {
                ref lhs,
                ref op,
                ref rhs,
            } => format!("{} {} {}", lhs.print(symbols), op, rhs.print(symbols)),
            Expr::Const(ref v, ref sign, ref size) => format!("{}:{}{}", v, sign, size),
            Expr::Name(ref n) => format!("{}", n),
            Expr::Temp(ref t) => format!("{}", t),
            Expr::Mem(ref expr) => expr.print(symbols),
            Expr::Call(ref callee, ref exprs) => {
                let mut fmt_str = format!("{}.call(", callee.print(symbols));

                for (i, expr) in exprs.iter().enumerate() {
                    if i + 1 == exprs.len() {
                        fmt_str.push_str(&format!("{}", expr.print(symbols)))
                    } else {
                        fmt_str.push_str(&format!("{},", expr.print(symbols)));
                    }
                }

                fmt_str.push_str(")");

                fmt_str
            }
            Expr::ESeq(_, ref expr) => expr.print(symbols),
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
