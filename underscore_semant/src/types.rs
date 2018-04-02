use syntax::ast::{Ident, Sign, Size};

static mut UNIQUE_COUNT: u32 = 0;

static mut TYPEVAR_COUNT: u32 = 0;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVar(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Default)]
pub struct Unique(pub u32);

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Nil,
    App(TyCon, Vec<Type>),
    Var(TypeVar),
    Poly(Vec<TypeVar>, Box<Type>),
    Unique(TyCon, Unique),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: Ident,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TyCon {
    Int(Sign, Size),
    String,
    Char,
    Void,
    Arrow,
    Bool,
    Struct(Vec<Field>),
    Fun(Vec<TypeVar>, Box<Type>),
    Unique(Box<TyCon>, Unique),
}

impl Unique {
    pub fn new() -> Self {
        let value = unsafe { UNIQUE_COUNT };
        unsafe { UNIQUE_COUNT += 1 };
        Unique(value)
    }
}

impl TypeVar {
    pub fn new() -> Self {
        let value = unsafe { TYPEVAR_COUNT };
        unsafe { TYPEVAR_COUNT += 1 };
        TypeVar(value)
    }
}
