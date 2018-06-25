use super::{Infer, InferResult};
use ctx::CompileCtx;
use env::{Entry, VarType};
use syntax::ast::TyAlias;
use types::{Type, TypeVar};
use util::pos::Spanned;

impl Infer {
    pub fn infer_alias(&self, alias: &Spanned<TyAlias>, ctx: &mut CompileCtx) -> InferResult<()> {
        if alias.value.ident.value.type_params.is_empty() {
            let ty = self.trans_ty(&alias.value.ty, ctx)?;

            ctx.add_type(alias.value.ident.value.name.value, Entry::Ty(ty));
            return Ok(());
        }

        let mut poly_tvs = Vec::with_capacity(alias.value.ident.value.type_params.len());

        for ident in &alias.value.ident.value.type_params {
            let tv = TypeVar::new();
            ctx.add_tvar(tv, VarType::Other);
            ctx.add_type(ident.value, Entry::Ty(Type::Var(tv)));
            poly_tvs.push(tv);
        }

        let entry = Entry::Ty(Type::Poly(
            poly_tvs,
            Box::new(self.trans_ty(&alias.value.ty, ctx)?),
        ));

        ctx.add_type(alias.value.ident.value.name.value, entry);

        Ok(())
    }
}
