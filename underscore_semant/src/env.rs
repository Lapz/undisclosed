use gen_ir::{Level, TranslateAccess};
use ir::{Frame, Label};
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

#[derive(Clone)]
pub enum VarEntry<T: Frame + Clone> {
    Var(TranslateAccess<T>, Type),
    Fun {
        ty: Type,
        level: Level<T>,
        label: Label,
    },
}

impl <T:Frame+Clone> VarEntry<T> {
    pub fn get_ty(self) -> Type {
        match self {
            VarEntry::Var(_,ty) => ty,
            VarEntry::Fun { ty, .. } => ty,
        }
    }
}
