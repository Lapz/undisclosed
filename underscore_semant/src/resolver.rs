use super::InferResult;
use ctx::CompileCtx;
use std::collections::HashSet;
use syntax::ast::{Function, Program, Struct, TyAlias};
use util::{ pos::Spanned, symbol::Symbol};
use ir::Frame;
#[derive(Debug, Default)]
pub struct Resolver {
    values: HashSet<Symbol>,
}

impl Resolver {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn resolve_ast<T:Frame+Clone>(&mut self, program: &Program, ctx: &mut CompileCtx<T>) -> InferResult<()> {
        for alias in &program.type_alias {
            self.resolve_alias(alias, ctx)?;
        }

        for struct_def in &program.structs {
            self.resolve_structs(struct_def, ctx)?;
        }

        for function in &program.functions {
            self.resolve_functions(function, ctx)?;
        }

        Ok(())
    }

    fn resolve_alias<T:Frame+Clone>(&mut self, alias: &Spanned<TyAlias>, ctx: &mut CompileCtx<T>) -> InferResult<()> {
        if !self.values.insert(alias.value.ident.value.name.value) {
            let msg = format!(
                "`{} ` is defined twice",
                ctx.name(alias.value.ident.value.name.value)
            );
            ctx.error(msg, alias.span);
            Err(())
        } else {
            Ok(())
        }
    }

    fn resolve_functions<T:Frame+Clone>(
        &mut self,
        function: &Spanned<Function>,
        ctx: &mut CompileCtx<T>,
    ) -> InferResult<()> {
        if !self.values.insert(function.value.name.value.name.value) {
            let msg = format!(
                "`{} ` is defined twice",
                ctx.name(function.value.name.value.name.value)
            );
            ctx.error(msg, function.span);
            Err(())
        } else {
            Ok(())
        }
    }

    fn resolve_structs<T:Frame+Clone>(
        &mut self,
        struct_def: &Spanned<Struct>,
        ctx: &mut CompileCtx<T>,
    ) -> InferResult<()> {
        if !self.values.insert(struct_def.value.name.value.name.value) {
            let msg = format!(
                "`{} ` is defined twice",
                ctx.name(struct_def.value.name.value.name.value)
            );
            ctx.error(msg, struct_def.span);
            Err(())
        } else {
            Ok(())
        }
    }
}
