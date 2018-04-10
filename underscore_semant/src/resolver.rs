use super::InferResult;
use env::Env;
use std::collections::HashSet;
use syntax::ast::{Function, Program, Struct, TyAlias};
use util::{emitter::Reporter, pos::Spanned, symbol::Symbol};

#[derive(Debug, Default)]
pub struct Resolver {
    values: HashSet<Symbol>,
}

impl Resolver {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn resolve_ast(
        &mut self,
        program: &Program,
        reporter: &mut Reporter,
        env: &Env,
    ) -> InferResult<()> {
        for alias in &program.type_alias {
            self.resolve_alias(alias, reporter, env)?;
        }

        for struct_def in &program.structs {
            self.resolve_structs(struct_def, reporter, env)?;
        }

        for function in &program.functions {
            self.resolve_functions(function, reporter, env)?;
        }

        Ok(())
    }

    fn resolve_alias(
        &mut self,
        alias: &Spanned<TyAlias>,
        reporter: &mut Reporter,
        env: &Env,
    ) -> InferResult<()> {
        if !self.values.insert(alias.value.ident.value.name.value) {
            let msg = format!(
                "`{} ` is defined twice",
                env.name(alias.value.ident.value.name.value)
            );
            reporter.error(msg, alias.span);
            Err(())
        } else {
            Ok(())
        }
    }

    fn resolve_functions(
        &mut self,
        function: &Spanned<Function>,
        reporter: &mut Reporter,
        env: &Env,
    ) -> InferResult<()> {
        if !self.values.insert(function.value.name.value.name.value) {
            let msg = format!(
                "`{} ` is defined twice",
                env.name(function.value.name.value.name.value)
            );
            reporter.error(msg, function.span);
            Err(())
        } else {
            Ok(())
        }
    }

    fn resolve_structs(
        &mut self,
        struct_def: &Spanned<Struct>,
        reporter: &mut Reporter,
        env: &Env,
    ) -> InferResult<()> {
        if !self.values.insert(struct_def.value.name.value.name.value) {
            let msg = format!(
                "`{} ` is defined twice",
                env.name(struct_def.value.name.value.name.value)
            );
            reporter.error(msg, struct_def.span);
            Err(())
        } else {
            Ok(())
        }
    }
}
