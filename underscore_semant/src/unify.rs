use env::Env;
use std::collections::HashMap;

use types::{TyCon, Type};
use util::emitter::Reporter;
use util::pos::Span;
use super::{Infer, InferResult};

impl Infer {
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

            (&Type::App(TyCon::Void, _), &Type::App(TyCon::Struct(_), _)) => Ok(()),
            (&Type::App(TyCon::Struct(_), _), &Type::App(TyCon::Void, _)) => Ok(()),

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

                let lhs = self.subst(ret, &mut mappings);

                self.unify(&lhs, t, reporter, span, env)?;
                Ok(())
            }

            (t, &Type::App(TyCon::Fun(ref tyvars, ref ret), ref u)) => {
                let mut mappings = HashMap::new();

                for (var, ty) in tyvars.iter().zip(u) {
                    mappings.insert(*var, ty.clone());
                }

                let lhs = self.subst(ret, &mut mappings);

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

                self.unify(ret1, &self.subst(ret2, &mut mappings), reporter, span, env)
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

                let ty = self.subst(&ret, &mut mappings);

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
