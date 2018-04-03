use env::Env;
use std::collections::HashMap;

use super::{Infer, InferResult};
use types::{TyCon, Type};
use util::emitter::Reporter;
use util::pos::Span;

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


            (&Type::Struct(ref name1,ref fields1,ref unique1),&Type::Struct(ref name2,ref fields2,ref unique2)) => {

                if unique1 != unique2 {
                    let msg = format!("Struct `{}` != Struct `{}`",env.name(*name1),env.name(*name2));

                    reporter.error(msg, span);
                    return Err(())
                }


                for (field1,field2) in fields1.iter().zip(fields2) {
                    self.unify(&field1.ty,&field2.ty,reporter,span,env)?;
                }

                Ok(())
            }
          

            

            (&Type::Struct(_,_, _), &Type::App(TyCon::Struct(_), _)) => Ok(()),
            (&Type::Struct(_,_, _), &Type::App(TyCon::Void, _)) => Ok(()),

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
}
