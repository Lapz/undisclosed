use super::{Infer, InferResult};
use env::{Entry, Env};
use syntax::ast::{TyAlias};
use util::{emitter::Reporter, pos::Spanned,};
use types::{Type, TypeVar};

impl Infer {
    pub fn infer_alias(
        &self,
        alias: &Spanned<TyAlias>,
        env: &mut Env,
        reporter: &mut Reporter,
    ) -> InferResult<()> {
        if alias.value.ident.value.type_params.is_empty() {
            let ty = self.trans_ty(&alias.value.ty, env, reporter)?;

            env.add_type(alias.value.ident.value.name.value, Entry::Ty(ty));
            return Ok(());
        }

        let mut poly_tvs = Vec::with_capacity(alias.value.ident.value.type_params.len());

        for ident in &alias.value.ident.value.type_params {
            let tv = TypeVar::new();
            env.add_type(ident.value, Entry::Ty(Type::Var(tv)));
            poly_tvs.push(tv);
        }

        let entry = Entry::Ty(Type::Poly(
            poly_tvs,
            Box::new(self.trans_ty(&alias.value.ty, env, reporter)?),
        ));

        env.add_type(alias.value.ident.value.name.value, entry);

        Ok(())
    }

}