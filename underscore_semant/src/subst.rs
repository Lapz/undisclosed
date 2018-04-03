use super::Infer;
use std::collections::HashMap;
use types::{TyCon, Type, TypeVar,Field};

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
            },
            Type::App(ref tycon,ref types) => {
                Type::App(
                    tycon.clone(),
                    types.iter().map(|ty| self.subst(ty, substions)).collect(),
                    )
            }

            Type::Struct(ref name,ref fields,ref unique) => {
                 let mut new_fields = Vec::new();

                    for field in fields {
                        new_fields.push(Field{
                            name:field.name,
                            ty:self.subst(&field.ty,substions)
                        });
                    }

                    Type::Struct(*name,new_fields,*unique)
            } 

            Type::Poly(ref tyvars, ref u) => Type::Poly(
                tyvars.iter().map(|_| TypeVar::new()).collect(),
                Box::new(self.subst(u, substions)),
            ),
        }
    }
}
