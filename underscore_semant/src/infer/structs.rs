use super::{Infer, InferResult};
use ast::typed as t;
use env::{Entry, Env, VarType};
use syntax::ast::Struct;
use types::{Field, Type, TypeVar, Unique};
use util::{emitter::Reporter, pos::Spanned};
impl Infer {
    pub fn infer_struct(
        &self,
        struct_def: &Spanned<Struct>,
        env: &mut Env,
        reporter: &mut Reporter,
    ) -> InferResult<t::Struct> {
        let mut poly_tvs = Vec::with_capacity(struct_def.value.name.value.type_params.len());
        let mut type_params = Vec::with_capacity(struct_def.value.name.value.type_params.len());

        for ident in &struct_def.value.name.value.type_params {
            let tv = TypeVar::new();
            env.add_tvar(tv, VarType::Other);
            env.add_type(ident.value, Entry::Ty(Type::Var(tv)));
            poly_tvs.push(tv);
            type_params.push(ident.value)
        }

        let mut type_fields = Vec::with_capacity(struct_def.value.fields.value.len());

        let unique = Unique::new();

        env.add_type(
            struct_def.value.name.value.name.value,
            Entry::Ty(Type::Poly(
                poly_tvs.clone(),
                Box::new(Type::Struct(
                    struct_def.value.name.value.name.value,
                    vec![],
                    unique,
                )),
            )),
        ); // For recursive types we need to add the empty struct

        for field in &struct_def.value.fields.value {
            type_fields.push(Field {
                name: field.value.name.value,
                ty: self.trans_ty(&field.value.ty, env, reporter)?,
            });
        }

        env.add_type(
            struct_def.value.name.value.name.value,
            Entry::Ty(Type::Poly(
                poly_tvs.clone(),
                Box::new(Type::Struct(
                    struct_def.value.name.value.name.value,
                    type_fields.clone(),
                    unique,
                )),
            )),
        );

        Ok(t::Struct {
            name: struct_def.value.name.value.name.value,
            fields: type_fields,
            type_params,
        })
    }
}
