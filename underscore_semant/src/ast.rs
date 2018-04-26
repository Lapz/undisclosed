use syntax::ast::{Linkage, Literal, Op, UnaryOp};
use types::{Field, Type};
use util::{pos::Span, symbol::Symbol};

#[derive(Debug)]
pub struct Program {
    pub structs: Vec<Struct>,
    pub functions: Vec<Function>,
}

#[derive(Debug)]
pub struct Struct {
    pub name: Symbol,
    pub type_params: Vec<Symbol>,
    pub fields: Vec<Field>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub span: Span,
    pub name: Symbol,
    pub generic: bool,
    pub params: Vec<FunctionParam>,
    pub returns: Type,
    pub body: Statement,
    pub linkage: Linkage,
}

#[derive(Debug, Clone)]
pub struct FunctionParam {
    pub name: Symbol,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct TypedExpression {
    pub expr: Box<Expression>,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Block(Vec<Statement>),
    Break,
    Continue,
    Expr(TypedExpression),
    For {
        init: Option<Box<Statement>>,
        cond: Option<TypedExpression>,
        incr: Option<TypedExpression>,
        body: Box<Statement>,
    },
    If {
        cond: TypedExpression,
        then: Box<Statement>,
        otherwise: Option<Box<Statement>>,
    },
    Let {
        ident: Symbol,
        ty: Type,
        expr: Option<TypedExpression>,
    },
    Return(TypedExpression),
    While(TypedExpression, Box<Statement>),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Assign(Symbol, TypedExpression),
    Binary(TypedExpression, Op, TypedExpression),

    Cast(TypedExpression, Type),

    Call(Symbol, Vec<TypedExpression>),
    Closure(Box<Function>),

    Grouping { expr: TypedExpression },

    Literal(Literal),

    StructLit(Symbol, Vec<TypedExpression>),

    Unary(UnaryOp, TypedExpression),

    Var(Symbol, Type),
}
