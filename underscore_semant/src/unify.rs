use super::{Infer, InferResult};
use ctx::CompileCtx;
use std::collections::HashMap;
use types::{TyCon, Type};
use util::pos::Span;
impl Infer {
    pub fn unify(
        &self,
        lhs: &Type,
        rhs: &Type,
        span: Span,
        ctx: &mut CompileCtx,
    ) -> InferResult<()> {
        match (lhs, rhs) {
            (
                &Type::Struct(ref name1, ref fields1, ref unique1),
                &Type::Struct(ref name2, ref fields2, ref unique2),
            ) => {
                if unique1 != unique2 {
                    let msg = format!(
                        "struct `{}` != struct `{}`",
                        ctx.name(*name1),
                        ctx.name(*name2)
                    );

                    ctx.error(msg, span);
                    return Err(());
                }

                for (field1, field2) in fields1.iter().zip(fields2) {
                    self.unify(&field1.ty, &field2.ty, span, ctx)?;
                }

                Ok(())
            }

            (&Type::App(TyCon::Void, _), &Type::Struct(_, _, _)) => Ok(()),
            (&Type::Struct(_, _, _), &Type::App(TyCon::Void, _)) => Ok(()),
            (&Type::Array(ref ty, ref len), &Type::Array(ref ty2, ref len2)) => {
                if len != len2 {
                    let msg = format!("Expected array with len `{}` found len `{}`", len, len2);
                    ctx.error(msg, span);
                    return Err(());
                }

                self.unify(ty, ty2, span, ctx)?;

                Ok(())
            }

            (&Type::Array(ref ty, _), ref other) => {
                self.unify(ty, other, span, ctx)?;

                Ok(())
            }

            (ref other, &Type::Array(ref ty, _)) => {
                self.unify(ty, other, span, ctx)?;

                Ok(())
            }

            (&Type::App(ref tycon1, ref types1), &Type::App(ref tycon2, ref types2)) => {
                if tycon1 != tycon2 {
                    let msg = format!("Cannot unify `{}` vs `{}`", lhs.print(ctx), rhs.print(ctx));
                    ctx.error(msg, span);
                    return Err(());
                }

                for (a, b) in types1.iter().zip(types2.iter()) {
                    self.unify(a, b, span, ctx)?
                }
                Ok(())
            }

            (&Type::App(TyCon::Fun(ref tyvars, ref ret), ref u), t) => {
                let mut mappings = HashMap::new();

                for (var, ty) in tyvars.iter().zip(u) {
                    mappings.insert(*var, ty.clone());
                }

                let lhs = self.subst(ret, &mut mappings);

                self.unify(&lhs, t, span, ctx)?;
                Ok(())
            }

            (t, &Type::App(TyCon::Fun(ref tyvars, ref ret), ref u)) => {
                let mut mappings = HashMap::new();

                for (var, ty) in tyvars.iter().zip(u) {
                    mappings.insert(*var, ty.clone());
                }

                let lhs = self.subst(ret, &mut mappings);

                self.unify(&lhs, t, span, ctx)?;
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

                self.unify(ret1, &self.subst(ret2, &mut mappings), span, ctx)
            }

            (&Type::Var(ref v1), &Type::Var(ref v2)) => if v1 == v2 {
                Ok(())
            } else {
                let a = ctx.look_tvar(*v1);
                let b = ctx.look_tvar(*v2);

                if a != b {
                    let msg = format!("Cannot unify `{}` vs `{}`", lhs.print(ctx), rhs.print(ctx));
                    ctx.error(msg, span);
                    return Err(());
                }
                Ok(())
            },

            (&Type::Var(_), &Type::App(TyCon::Int(_, _), _)) => Ok(()),

            (&Type::App(TyCon::Int(_, _), _), &Type::Var(_)) => Ok(()),

            (&Type::Nil, &Type::Nil) => Ok(()),
            (&Type::Nil, &Type::App(TyCon::Void, _)) => Ok(()),
            (t1, t2) => {
                let msg = format!("Cannot unify `{}` vs `{}`", t1.print(ctx), t2.print(ctx));
                ctx.error(msg, span);
                Err(())
            }
        }
    }
}
