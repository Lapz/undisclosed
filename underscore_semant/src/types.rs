#[derive(Debug, Clone, Copy)]
pub struct TypeVar(pub u32);

#[derive(Debug, Clone)]
pub struct Field {
    name: TypeVar,
    ty: Ty,
}

#[derive(Debug, Clone)]
pub enum Ty {
    Nil,
    App(TyCon, Vec<Ty>),
    Var(TypeVar),
    Poly(Vec<TypeVar>, Box<Ty>),
    Struct(Vec<Field>, Box<Ty>),
    Unique(TyCon, Unique),
    Fun(Vec<TypeVar>, Box<Ty>),
}

#[derive(Debug, Clone)]
pub enum TyCon {
    Int,
    String,
    Nil,
    Arrow,
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
