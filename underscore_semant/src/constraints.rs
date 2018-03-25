use syntax::ast::{Sign, Size};
use util::emitter::Reporter;
use std::collections::HashMap;
use util::pos::Span;

static mut UNIQUE_COUNT: u32 = 0;

static mut TYPEVAR_COUNT: u32 = 0;

pub type InferResult<T> = Result<T, ()>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVar(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Default)]
pub struct Unique(pub u32);

#[derive(Debug, Clone)]
pub enum Type {
    Nil,
    App(TyCon, Vec<Type>),
    Var(TypeVar),
    Poly(Vec<TypeVar>, Box<Type>),
}

#[derive(Debug, Clone)]
pub enum TyCon {
    Int(Sign, Size),
    String,
    Char,
    Void,
    Arrow,
    Bool,
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

#[derive(Debug)]
pub struct Infer {
    reporter: Reporter,
}

impl Infer {
    /// Deals with the subsitution of type variables
    fn subst(&self, ty: &Type, substions: &mut HashMap<TypeVar, Type>) -> Type {
        match *ty {
            Type::Var(ref tvar) => {
                if let Some(tyvar) = substions.get(tvar) {
                    tyvar.clone()
                } else {
                    Type::Var(*tvar)
                }
            }

            Type::Nil => Type::Nil,

            Type::App(TyCon::Fun(ref tvars, ref returns), ref types) => {
                for (tvar, ty) in tvars.iter().zip(types.iter()) {
                    substions.insert(*tvar, ty.clone());
                }

                self.subst(&self.subst(returns, substions), substions)
            }

            Type::App(ref tycon, ref types) => Type::App(
                tycon.clone(),
                types.iter().map(|ty| self.subst(ty, substions)).collect(),
            ),

            Type::Poly(ref tyvars, ref u) => Type::Poly(
                tyvars.iter().map(|_| TypeVar::new()).collect(),
                Box::new(self.subst(u, substions)),
            ),
        }
    }

    fn unify(
        &self,
        lhs: &Type,
        rhs: &Type,
        reporter: &mut Reporter,
        span: Span,
    ) -> InferResult<()> {
        match (lhs, rhs) {
            (&Type::App(_, ref types1), &Type::App(_, ref types2)) => {
                for (a, b) in types1.iter().zip(types2.iter()) {
                    self.unify(a, b, reporter, span)?
                }
                Ok(())
            }

            (
                &Type::App(TyCon::Unique(_, ref z1), ref types1),
                &Type::App(TyCon::Unique(_, ref z2), ref types2),
            ) => {
                if z1 != z2 {
                    return Err(());
                }

                for (a, b) in types1.iter().zip(types2.iter()) {
                    self.unify(a, b, reporter, span)?
                }
                Ok(())
            }

            (&Type::App(TyCon::Fun(ref tyvars, ref ret), ref u), ref t) => {
                let mut mappings = HashMap::new();

                for (var, ty) in tyvars.iter().zip(u) {
                    mappings.insert(*var, ty.clone());
                }

                let lhs = self.subst(ret, &mut mappings);

                self.unify(&lhs, t, reporter, span)?;
                Ok(())
            }

            (ref t, &Type::App(TyCon::Fun(ref tyvars, ref ret), ref u)) => {
                let mut mappings = HashMap::new();

                for (var, ty) in tyvars.iter().zip(u) {
                    mappings.insert(*var, ty.clone());
                }

                let lhs = self.subst(ret, &mut mappings);

                self.unify(&lhs, t, reporter, span)?;
                Ok(())
            }

            (&Type::Poly(ref vars1, ref ret1), &Type::Poly(ref vars2, ref ret2)) => {
                let mut mappings = HashMap::new();

                for var in vars1 {
                    mappings.insert(*var, Type::Var(*var));
                }

                for var in vars2 {
                    mappings.insert(*var, Type::Var(*var));
                }

                self.unify(ret1, &self.subst(ret2, &mut mappings), reporter, span)
            }

            (&Type::Var(ref v1), &Type::Var(ref v2)) => if v1 == v2 {
                Ok(())
            } else {
                Err(())
            },

            (t1, t2) => {
                let msg = format!("Cannot unify {:?} vs {:?}", t1, t2);
                reporter.error(msg, span);
                Err(())
            }
        }
    }

    fn expand(&self, ty: Type) -> Type {
        match ty {
            Type::App(TyCon::Fun(vars, ret), types) => {
                let mut mappings = HashMap::new();

                for (var, ty) in vars.iter().zip(types) {
                    mappings.insert(*var, ty.clone());
                }

                let ty = self.subst(&ret, &mut mappings);

                self.expand(ty)
            }

            Type::App(TyCon::Unique(tycon, _), types) => self.expand(Type::App(*tycon, types)),
            u => u,
        }
    }
}
