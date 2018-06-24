use types::{TyCon, Type};
#[derive(Debug, Clone)]

pub enum Entry {
    TyCon(TyCon),
    Ty(Type),
}

#[derive(Debug, Clone, PartialEq)]
pub enum VarType {
    /// A typedvariable mapped to a var
    Int,
    Other,
}

#[derive(Debug, Clone)]
pub enum VarEntry {
    Var(Type),
    Fun { ty: Type },
}

impl VarEntry {
    pub fn get_ty(self) -> Type {
        match self {
            VarEntry::Var(ty) => ty,
            VarEntry::Fun { ty, .. } => ty,
        }
    }
}
