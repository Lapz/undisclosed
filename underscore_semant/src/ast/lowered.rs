use codegen::ir::Instruction;
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
