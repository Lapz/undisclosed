use codegen::ir::Instruction;
use std::fmt::{self, Display};
use syntax::ast::{Linkage, Literal, Op, UnaryOp};
use types::{Field, Type};
use util::{pos::Span, symbol::Symbol};

#[derive(Debug)]
pub struct Program {
    pub functions: Vec<Function>,
}

#[derive(Debug)]
pub struct Function {
    pub span: Span,
    pub name: Symbol,
    pub param_types: Vec<Type>,
    pub returns: Type,
    pub body: Vec<Instruction>,
    pub linkage: Linkage,
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

        for (i, param) in self.param_types.iter().enumerate() {
            if i + 1 == self.param_types.len() {
                write!(f, "{}", param)?;
            } else {
                write!(f, "{},", param)?;
            }
        }

        write!(f, ")")?;

        write!(f, " -> {}", self.returns)?;

        write!(f, " {{")?;

        writeln!(f)?;

        for instruction in &self.body {
            writeln!(f, "    {}", instruction)?;
        }

        write!(f, "}}")
    }
}
