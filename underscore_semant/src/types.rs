use syntax::ast::Ident;

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub struct TypeVar(pub u32);

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    name: TypeVar,
    pub ty: Ty,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
    Nil,
    Int,
    String,
    Char,
    Bool,
    Struct(Vec<Ty>, Vec<Field>),
    Fun(Vec<Ty>, Box<Ty>),
    Var(TypeVar),
    Unique(Box<Ty>, Unique),
}

static mut UNIQUE_COUNT: u64 = 0;

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Unique(pub u64);

impl Unique {
    pub fn new() -> Self {
        let value = unsafe { UNIQUE_COUNT };
        unsafe { UNIQUE_COUNT += 1 };
        Unique(value)
    }
}
