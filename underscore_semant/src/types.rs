use syntax::ast::Ident;

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub struct TypeVar(pub u32);

#[derive(Debug, Clone)]
pub struct Field {
    name: TypeVar,
    ty: Ty,
}

#[derive(Debug, Clone)]
pub enum Ty {
    Nil,
    Int,
    String,
    Struct(Vec<Ty>, Vec<Field>),
    Fun(Vec<Ty>, Box<Ty>),
    Var(TypeVar),
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
