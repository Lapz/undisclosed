use super::Infer;
use types::{TyCon, Type, TypeVar};
use std::collections::HashMap;

impl Infer {
    /// Deals with the subsitution of type variables
    pub fn subst(&self, ty: &Type, substions: &mut HashMap<TypeVar, Type>) -> Type {
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

            Type::Unique(ref tycon, ref unique) => Type::Unique(tycon.clone(), *unique),
        }
    }
}
