extern crate underscore_codegen as codegen;
extern crate underscore_syntax as syntax;
extern crate underscore_util as util;

mod ast;
mod cast_check;
mod cfg;
mod env;
mod escape;
mod gen_cfg;
mod gen_ir;
mod infer;
mod monomorphize;
mod resolver;
mod subst;
mod types;
mod unify;

use ast::typed as t;
pub use env::Env as TypeEnv;
use env::Env;
use escape::FindEscape;
pub use gen_ir::Codegen;
use monomorphize::Mono;
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
        reporter: &mut Reporter,
    ) -> InferResult<t::Program> {
        let mut new_program = t::Program {
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
            new_program
                .functions
                .push(self.infer_function(function, env, reporter)?);
        }

        let mut resolver = Resolver::new();

        resolver.resolve_ast(program, reporter, env)?;

        let mut mono = Mono::new();

        let mono_program = mono.monomorphize_program(new_program, env);

        Ok(mono_program)
    }
}
