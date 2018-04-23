extern crate underscore_codegen as codegen;
extern crate underscore_syntax as syntax;
extern crate underscore_util as util;

mod ast;
mod cast_check;
mod env;
mod escape;
mod infer;
mod monomorphize;
mod resolver;
mod statements;
mod subst;
mod trans;
mod types;
mod unify;
use codegen::{gen::Ctx,
              temp,
              translate::{Level, Translator},
              x86::x86};
pub use env::Env as TypeEnv;
use env::Env;
use escape::FindEscape;
use resolver::Resolver;
use syntax::ast::Program;
use types::Type;
use util::emitter::Reporter;
pub(crate) type InferResult<T> = Result<T, ()>;

#[derive(Debug)]
pub struct Infer {
    body: Type, // for function returning
}

impl Infer {
    pub fn new() -> Self {
        Self { body: Type::Nil }
    }

    pub fn infer<'a>(
        &mut self,
        program: &mut Program,
        env: &mut Env,
        ctx: &'a mut Ctx,
        reporter: &mut Reporter,
    ) -> InferResult<ast::Program> {
        let mut new_program = ast::Program {
            functions: vec![],
            structs: vec![],
        };

        for alias in &program.type_alias {
            self.infer_alias(alias, env, reporter)?
        }

        for struct_def in &program.structs {
            new_program
                .structs
                .push(self.infer_struct(struct_def, env, reporter)?)
        }

        for function in &program.functions {
            new_program.functions.push(self.infer_function(
                function,
                &mut Level::Top,
                ctx,
                env,
                reporter,
            )?);
        }

        Ok(new_program)
    }
}
