use env::Env;
use std::fmt::{self, Display};
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
    Struct(Ident, Vec<Field>, Unique), // Name, Fields, Unique
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
    Fun(Vec<TypeVar>, Box<Type>),
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

impl Type {
    pub fn is_int(&self) -> bool {
        match *self {
            Type::App(TyCon::Int(_, _), _) => true,
            _ => false,
        }
    }
}

impl Type {
    pub fn print(&self, env: &Env) -> String {
        match *self {
            Type::Struct(ref name, ref fields, _) => {
                let mut fmt_string = String::new();
                fmt_string.push_str(&format!("{}<", env.name(*name)));

                for (i, field) in fields.iter().enumerate() {
                    if i + 1 == fields.len() {
                        fmt_string.push_str(&format!("{}", field.ty.print(env)));
                    } else {
                        fmt_string.push_str(&format!("{},", field.ty.print(env)));
                    }
                }

                fmt_string.push('>');

                fmt_string
            }
            Type::Nil => "nil".into(),
            Type::App(ref tycon, ref types) => {
                let mut fmt_string = String::new();

                if let TyCon::Arrow = *tycon {
                    fmt_string.push_str("fn(");

                    for i in 0..types.len() - 1 {
                        if i + 1 == types.len() - 1 {
                            fmt_string.push_str(&format!("{}", types[i].print(env)));
                        } else {
                            fmt_string.push_str(&format!("{},", types[i].print(env)));
                        }
                    }

                    fmt_string.push_str(") -> ");

                    fmt_string.push_str(&format!("{}", types.last().unwrap().print(env)));

                    return fmt_string;
                }

                fmt_string.push_str(&format!("{}", tycon));

                for (i, ty) in types.iter().enumerate() {
                    if i + 1 == types.len() {
                        fmt_string.push_str(&ty.print(env))
                    } else {
                        fmt_string.push_str(&format!("{},", ty.print(env)))
                    }
                }

                fmt_string
            }
            Type::Var(ref v) => format!("{}", v.0 as u8 as char),
            Type::Poly(ref vars, ref ret) => {
                let mut fmt_string = String::new();
                fmt_string.push_str("poly<");

                for (i, var) in vars.iter().enumerate() {
                    if i + 1 == vars.len() {
                        fmt_string.push(var.0 as u8 as char);
                    } else {
                        fmt_string.push_str(&format!("{},", var.0 as u8 as char));
                    }
                }

                fmt_string.push('>');

                fmt_string.push_str(&ret.print(env));

                fmt_string
            }
        }
    }
}

impl Display for TyCon {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            TyCon::Int(ref sign, ref size) => write!(f, "{}{}", sign, size),
            TyCon::String => write!(f, "str"),
            TyCon::Char => write!(f, "ch"),
            TyCon::Void => write!(f, "nil"),
            TyCon::Arrow => write!(f, "->"),
            TyCon::Bool => write!(f, "bool"),
            TyCon::Fun(_, ref ret) => write!(f, "fn () -> {:?}", ret),
        }
    }
}
