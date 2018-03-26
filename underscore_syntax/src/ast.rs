use std::fmt::{self, Display};
use util::pos::{Span, Spanned};
#[derive(Debug)]
pub struct Program {
    pub structs: Vec<Spanned<Struct>>,
    pub functions: Vec<Spanned<Function>>,
    pub type_alias: Vec<Spanned<TyAlias>>,
}

impl Program {
    pub fn fmt(&mut self) -> String {
        format!("{:#?}", self)
    }
}

#[derive(Hash, Debug, Copy, Clone, PartialEq, Eq)]
pub struct Ident(pub u32);
#[derive(Debug)]
pub struct ItemName {
    pub name: Spanned<Ident>,
    pub type_params: Vec<Spanned<Ident>>,
}
#[derive(Debug)]
pub struct Struct {
    pub span: Span,
    pub name: Spanned<ItemName>,
    pub fields: Spanned<Vec<Spanned<Field>>>,
}

#[derive(Debug)]
pub struct Field {
    pub name: Spanned<Ident>,
    pub ty: Spanned<Ty>,
}

#[derive(Debug)]
pub struct Function {
    pub span: Span,
    pub name: Spanned<ItemName>,
    pub params: Spanned<Vec<Spanned<FunctionParams>>>,
    pub returns: Option<Spanned<Ty>>,
    pub body: Spanned<Statement>,
    pub linkage: Linkage,
}
#[derive(Debug)]
pub struct FunctionParams {
    pub name: Spanned<Ident>,
    pub ty: Spanned<Ty>,
}
#[derive(Debug)]
pub enum Linkage {
    Normal,
    External,
}
#[derive(Debug)]
pub enum Ty {
    Func(Vec<Spanned<Ty>>, Option<Box<Spanned<Ty>>>),
    Poly(Spanned<Ident>, Vec<Spanned<Ty>>),
    Simple(Spanned<Ident>),
    Nil,
    I8,
    I32,
    I64,
    U8,
    U32,
    U64,
    Bool,
    Str,
}

#[derive(Debug)]
pub struct TyAlias {
    pub ident: Spanned<ItemName>,
    pub ty: Spanned<Ty>,
}

#[derive(Debug)]
pub enum Statement {
    Block(Vec<Spanned<Statement>>),
    Break,
    Continue,
    Expr(Spanned<Expression>),
    For {
        init: Option<Box<Spanned<Statement>>>,
        cond: Option<Spanned<Expression>>,
        incr: Option<Spanned<Expression>>,
        body: Box<Spanned<Statement>>,
    },
    If {
        cond: Spanned<Expression>,
        then: Box<Spanned<Statement>>,
        otherwise: Option<Box<Spanned<Statement>>>,
    },
    Let {
        ident: Spanned<Ident>,
        ty: Option<Spanned<Ty>>,
        expr: Option<Spanned<Expression>>,
    },
    Return(Spanned<Expression>),
    While {
        cond: Spanned<Expression>,
        body: Box<Spanned<Statement>>,
    },
}
#[derive(Debug)]
pub enum Expression {
    Assign {
        name: Spanned<Var>,
        value: Box<Spanned<Expression>>,
    },
    Binary {
        lhs: Box<Spanned<Expression>>,
        op: Spanned<Op>,
        rhs: Box<Spanned<Expression>>,
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

    StructLiteral {
        ident: Spanned<Ident>,
        fields: Vec<Spanned<StructLitField>>,
    },

    Unary {
        op: Spanned<UnaryOp>,
        expr: Box<Spanned<Expression>>,
    },

    Var(Spanned<Var>),
}

#[derive(Debug)]
pub struct StructLitField {
    pub ident: Spanned<Ident>,
    pub expr: Spanned<Expression>,
}
#[derive(Debug)]
pub enum Literal {
    Number(Number),
    True(bool),
    False(bool),
    Nil,
    Str(String),
    Char(char),
}
#[derive(Debug)]
pub enum Var {
    Field {
        ident: Spanned<Ident>,
        value: Spanned<Ident>,
    },
    Simple(Spanned<Ident>),
    SubScript {
        expr: Box<Spanned<Expression>>,
        target: Spanned<Ident>,
    },
}
#[derive(Debug)]
pub enum Call {
    Simple{
        callee: Box<Spanned<Expression>>,
        args: Vec<Spanned<Expression>>,
    },
    Instantiation {
        callee: Box<Spanned<Expression>>,
        ty:Spanned<Vec<Ty>>,
        args: Vec<Spanned<Expression>>,
    },
}

#[derive(Debug)]
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
    And,
    Or,
}

#[derive(Debug, PartialOrd, Clone, PartialEq, Hash)]
pub enum UnaryOp {
    Bang,
    Minus,
}

#[derive(Debug, Clone, PartialEq, Hash, Eq, Copy)]
pub struct Number {
    pub value: u64,
    pub ty: Option<(Sign, Size)>, // Option because not all numbers are declared like 10u32
}

#[derive(PartialEq, Debug, Clone, Hash, Eq, Copy)]
pub enum Sign {
    Signed,
    Unsigned,
}

#[derive(PartialEq, Debug, Clone, Hash, Eq, Copy)]
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
