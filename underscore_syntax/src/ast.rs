use std::fmt::{self, Display};
use util::pos::{Span, Spanned};
pub struct Program {
    structs: Vec<Struct>,
    functions: Vec<Function>,
}

#[derive(Hash, Copy, Clone, PartialEq, Eq)]
pub struct Ident(u32);

pub struct ItemName {
    name: Spanned<Ident>,
    type_params: Vec<Spanned<Ident>>,
}

pub struct Struct {
    span: Span,
    name: ItemName,
    fields: Vec<Spanned<Field>>,
}

pub struct Field {
    name: Spanned<Ident>,
    ty: Spanned<Ty>,
}

pub struct Function {
    span: Span,
    name: ItemName,
    params: Vec<Spanned<FunctionParams>>,
    returns: Spanned<Ty>,
    body: Spanned<Statement>,
    linkage: Linkage,
}

pub struct FunctionParams {
    name: Spanned<Ident>,
    ty: Spanned<Ty>,
}

pub enum Linkage {
    Normal,
    External,
}

pub enum Ty {
    Name(Spanned<Ident>),
    Nil,
    I8,
    I32,
    I64,
    U8,
    U32,
    U64,
    Bool,
}

pub enum Statement {
    Block(Vec<Spanned<Expression>>),
    Break,
    Continue,
    Expr(Spanned<Expression>),
    If {
        cond: Spanned<Expression>,
        then: Box<Spanned<Statement>>,
        otherwise: Option<Box<Spanned<Statement>>>,
    },
    Return(Spanned<Expression>),
    While {
        cond: Spanned<Expression>,
        body: Box<Spanned<Statement>>,
    },
    TyAlias {
        alias: Ident,
        ty: Spanned<Ty>,
    },
}

pub enum Expression {
    Array {
        init: Box<Spanned<Expression>>,
        items: Vec<Spanned<Expression>>,
        ty: Spanned<Ty>,
    },
    Assign {
        name: Spanned<Ident>,
        value: Box<Spanned<Expression>>,
    },
    Binary {
        lft: Box<Spanned<Expression>>,
        op: Op,
        rft: Box<Spanned<Expression>>,
    },

    Cast {
        expr: Box<Spanned<Expression>>,
        to: Spanned<Ty>,
    },

    Call {
        callee: Box<Spanned<Expression>>,
        args: Vec<Spanned<Expression>>,
    },

    Grouping {
        expr: Box<Spanned<Expression>>,
    },

    Literal(Literal),

    Unary {
        op: UnaryOp,
        expr: Box<Spanned<Expression>>,
    },

    Var(Spanned<Var>),
}

pub enum Literal {
    Number(Number),
    True(bool),
    False(bool),
    Nil,
    Str(String),
    Char(char),
}

pub enum Var {
    Field {
        expr: Box<Spanned<Expression>>,
        ident: Spanned<Ident>,
    },
    Simple(Spanned<Ident>),
    SubScript {
        expr: Box<Spanned<Expression>>,
        target: Box<Spanned<Expression>>,
    },
}

pub enum Op {
    NEq,
    Equal,
    LT,
    LTE,
    GTE,
    GT,
    Plus,
    Minus,
    Star,
    Slash,
}

#[derive(Debug, PartialOrd, Clone, PartialEq, Hash)]
pub enum UnaryOp {
    Bang,
    Minus,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Number {
    pub value: u64,
    pub ty: Option<(Sign, Size)>, // Option because not all numbers are declared like 10u32
}

#[derive(PartialEq, Debug, Clone)]
pub enum Sign {
    Signed,
    Unsigned,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Size {
    Bit8,
    Bit32,
    Bit64,
}

impl Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)?;

        match self.ty {
            Some((ref sign, ref size)) => write!(f, "{}{}", sign, size),
            None => Ok(()),
        }
    }
}

impl Display for Sign {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Sign::Signed => write!(f, "i"),
            Sign::Unsigned => write!(f, "u"),
        }
    }
}

impl Display for Size {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Size::Bit8 => write!(f, "8"),
            Size::Bit32 => write!(f, "32"),
            Size::Bit64 => write!(f, "64"),
        }
    }
}
