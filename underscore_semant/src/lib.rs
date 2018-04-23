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
use util::emitter::Reporter;
use types::Type;
pub(crate) type InferResult<T> = Result<T, ()>;

#[derive(Debug)]
pub struct Infer {
    body:Type, // for function returning
}

impl Infer {
    pub fn new() -> Self {
       Self {
           body:Type::Nil
       }
    }

    pub fn new_infer<'a>(
         &mut self,
        program: &mut Program,
        env: &mut Env,
        ctx: &'a mut Ctx,
        reporter: &mut Reporter,
    ) -> InferResult<ast::Program> {

        let mut new_program = ast::Program {
            functions:vec![],
            structs:vec![]    
        };

        for alias in &program.type_alias {
            self.infer_alias(alias, env, reporter)?
        }

        for struct_def in &program.structs {
            
        }

       
        for function in &program.functions {
            new_program.functions.push(self.infer_function(function,&mut Level::Top, ctx,env, reporter, )?);
        }

       

        Ok(new_program)
    }

    pub fn infer<'a>(
        &self,
        program: &mut Program,
        env: &mut Env,
        ctx: &'a mut Ctx,
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
            self.trans_function(function, env, ctx, reporter, Level::Top)?
        }

        let mut resolver = Resolver::new();

        resolver.resolve_ast(program, reporter, env)?;

        Ok(())
    }
}
