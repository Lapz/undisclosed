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

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub span: Span,
    pub name: Symbol,
    pub generic: bool,
    pub params: Vec<FunctionParam>,
    pub returns: Type,
    pub body: Statement,
    pub linkage: Linkage,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionParam {
    pub name: Symbol,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedExpression {
    pub expr: Box<Expression>,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Block(Vec<Statement>),
    Break,
    Continue,
    Expr(TypedExpression),
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

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Array(Vec<TypedExpression>),

    Assign(Var, TypedExpression),

    Binary(TypedExpression, Op, TypedExpression),

    Cast(TypedExpression, Type),

    Call(Symbol, Vec<TypedExpression>),

    Closure(Box<Function>),
    /// Field access i.e foo.bar;
    Field(Symbol, Symbol),

    Grouping {
        expr: TypedExpression,
    },

    Index(Symbol, TypedExpression),

    Literal(Literal),

    StructLit(Symbol, Vec<TypedExpression>),

    Unary(UnaryOp, TypedExpression),
    /// Simple var i.e x;
    Var(Var),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Var {
    /// Field access i.e foo.bar;
    Field(Symbol, Symbol, Type),
    /// Simple var i.e x;
    Simple(Symbol, Type),
    /// Index operation i.e a[10];
    SubScript(Symbol, TypedExpression, Type),
}
