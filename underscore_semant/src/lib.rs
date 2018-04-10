extern crate underscore_codegen as codegen;
extern crate underscore_syntax as syntax;
extern crate underscore_util as util;

mod cast_check;
mod env;
mod escape;
mod resolver;
mod subst;
mod trans;
mod types;
mod unify;

use codegen::{temp,
              translate::{Level, Translator},
              x86::x86};
pub use env::Env as TypeEnv;
use env::Env;
use escape::FindEscape;
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
        program: &mut Program,
        env: &mut Env,
        reporter: &mut Reporter,
    ) -> InferResult<()> {
        let mut escaper = FindEscape::new();

        // let mut frame = x86::new(temp::new_label(&mut env.escapes),&[]);
        // let mut outermost = Translator::new_level(Level::Top,);

        escaper.find_escape(program, &mut env.escapes)?;

        for alias in &program.type_alias {
            self.trans_alias(alias, env, reporter)?
        }

        for record in &program.structs {
            self.trans_struct(record, env, reporter)?
        }

        for function in &program.functions {
            self.trans_function(function, env, reporter, Level::Top)?
        }

        let mut resolver = Resolver::new();

        resolver.resolve_ast(program, reporter, env)?;

        Ok(())
    }
}
