extern crate underscore_syntax as syntax;
extern crate underscore_util as util;

mod cast_check;
mod env;
mod resolver;
mod subst;
mod trans;
mod types;
mod unify;

pub use env::Env as TypeEnv;
use env::Env;
use resolver::Resolver;
use syntax::ast::Program;
use util::emitter::Reporter;

pub(crate) type InferResult<T> = Result<T, ()>;

#[derive(Debug, Default)]
pub struct Infer {}

impl Infer {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn infer(
        &self,
        program: &Program,
        env: &mut Env,
        reporter: &mut Reporter,
    ) -> InferResult<()> {
        for alias in &program.type_alias {
            self.trans_alias(alias, env, reporter)?
        }

        for record in &program.structs {
            self.trans_struct(record, env, reporter)?
        }

        for function in &program.functions {
            self.trans_function(function, env, reporter)?
        }

        let mut resolver = Resolver::new();

        resolver.resolve_ast(program, reporter, env)?;

        Ok(())
    }
}
