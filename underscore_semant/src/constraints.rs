use env::Env;
use std::collections::HashMap;
use syntax::ast::Ident;
use syntax::ast::{Sign, Size};
use util::emitter::Reporter;
use util::pos::Span;

static mut UNIQUE_COUNT: u32 = 0;

static mut TYPEVAR_COUNT: u32 = 0;

pub type InferResult<T> = Result<T, ()>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVar(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MetaVar(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Default)]
pub struct Unique(pub u32);

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Nil,
    App(TyCon, Vec<Type>),
    Var(TypeVar),
    Poly(Vec<TypeVar>, Box<Type>),
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

#[derive(Debug, Default)]
pub struct Infer {}

impl Infer {
    pub fn new() -> Self {
        Self::default()
    }
    /// Deals with the subsitution of type variables
    pub fn subst(
        &self,
        ty: &Type,
        substions: &mut HashMap<TypeVar, Type>,
        meta: &mut HashMap<MetaVar, Type>,
    ) -> Type {
        match *ty {
            Type::Var(ref tvar) => {
                if let Some(ty) = substions.get(tvar) {
                    ty.clone()
                } else {
                    Type::Var(*tvar)
                }
            }

            Type::Nil => Type::Nil,

            Type::App(TyCon::Fun(ref tvars, ref returns), ref types) => {
                for (tvar, ty) in tvars.iter().zip(types.iter()) {
                    substions.insert(*tvar, ty.clone());
                }

                self.subst(&self.subst(returns, substions, meta), substions, meta)
            }

            Type::App(ref tycon, ref types) => Type::App(
                tycon.clone(),
                types
                    .iter()
                    .map(|ty| self.subst(ty, substions, meta))
                    .collect(),
            ),

            Type::Poly(ref tyvars, ref u) => Type::Poly(
                tyvars.iter().map(|_| TypeVar::new()).collect(),
                Box::new(self.subst(u, substions, meta)),
            ),
        }
    }

    pub fn unify(
        &self,
        lhs: &Type,
        rhs: &Type,
        reporter: &mut Reporter,
        span: Span,
        env: &mut Env,
    ) -> InferResult<()> {
        match (lhs, rhs) {
            (
                &Type::App(TyCon::Unique(ref tycon1, ref z1), ref types1),
                &Type::App(TyCon::Unique(ref tycon2, ref z2), ref types2),
            ) => {
                if z1 != z2 {
                    let msg = format!("Cannot unify {:?} vs {:?}", z1, z2);
                    reporter.error(msg, span);
                    return Err(());
                }

                if tycon1 != tycon2 {
                    let msg = format!("Cannot unify {:?} vs {:?}", tycon1, tycon2);
                    reporter.error(msg, span);
                    return Err(());
                }

                for (a, b) in types1.iter().zip(types2.iter()) {
                    self.unify(a, b, reporter, span, env)?
                }
                Ok(())
            }

            (&Type::App(ref tycon1, ref types1), &Type::App(ref tycon2, ref types2)) => {
                if tycon1 != tycon2 {
                    let msg = format!("Cannot unify {:?} vs {:?}", tycon1, tycon2);
                    reporter.error(msg, span);
                    return Err(());
                }

                for (a, b) in types1.iter().zip(types2.iter()) {
                    self.unify(a, b, reporter, span, env)?
                }
                Ok(())
            }

            (&Type::App(TyCon::Fun(ref tyvars, ref ret), ref u), t) => {
                let mut mappings = HashMap::new();

                for (var, ty) in tyvars.iter().zip(u) {
                    mappings.insert(*var, ty.clone());
                }

                let lhs = self.subst(ret, &mut mappings, &mut env.metavars);

                self.unify(&lhs, t, reporter, span, env)?;
                Ok(())
            }

            (t, &Type::App(TyCon::Fun(ref tyvars, ref ret), ref u)) => {
                let mut mappings = HashMap::new();

                for (var, ty) in tyvars.iter().zip(u) {
                    mappings.insert(*var, ty.clone());
                }

                let lhs = self.subst(ret, &mut mappings, &mut env.metavars);

                self.unify(&lhs, t, reporter, span, env)?;
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

                self.unify(
                    ret1,
                    &self.subst(ret2, &mut mappings, &mut env.metavars),
                    reporter,
                    span,
                    env,
                )
            }

            (&Type::Var(ref v1), &Type::Var(ref v2)) => if v1 == v2 {
                Ok(())
            } else {
                Err(())
            },
            (t1, t2) => {
                let msg = format!("Cannot unify  {:?} vs {:?}", t1, t2);
                reporter.error(msg, span);
                Err(())
            }
        }
    }

    pub fn expand(&self, ty: Type, env: &mut Env) -> Type {
        match ty {
            Type::App(TyCon::Fun(vars, ret), types) => {
                let mut mappings = HashMap::new();

                for (var, ty) in vars.iter().zip(types) {
                    mappings.insert(*var, ty.clone());
                }

                let ty = self.subst(&ret, &mut mappings, &mut env.metavars);

                self.expand(ty, env)
            }

            Type::App(TyCon::Unique(tycon, _), types) => self.expand(Type::App(*tycon, types), env),
            u => u,
        }
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
