use syntax::ast::Ident;

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub struct TypeVar(pub u32);

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    name: TypeVar,
    ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Struct { fields: Vec<Field> },

    Func(Vec<Type>, Box<Type>),
    Array(Box<Type>),
    Name(Spanned<Ident>, Box<Type>),
    Int,
    Str,
    Bool,
    Nil,
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
