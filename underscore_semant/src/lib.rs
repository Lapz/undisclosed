// #![feature(nll)]

extern crate underscore_ir as ir;
extern crate underscore_syntax as syntax;
extern crate underscore_util as util;
extern crate underscore_x86 as x86;

mod ast;
mod cast_check;
mod cfg;
mod ctx;
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
use ctx::CompileCtx;
// use escape::FindEscape;
use gen_ir::Codegen;
use monomorphize::Mono;
use resolver::Resolver;
use std::rc::Rc;
use syntax::ast::Program;
use types::Type;
use util::{
    emitter::Reporter,
    symbol::{Hasher, Symbol},
};
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
        strings: &Rc<Hasher<Symbol>>,
        reporter: &mut Reporter,
    ) -> InferResult<::ir::ir::Program> {
        let mut ctx = CompileCtx::new(strings, reporter);

        let mut new_program = t::Program {
            functions: vec![],
            structs: vec![],
        };

        for alias in &program.type_alias {
            self.infer_alias(alias, &mut ctx)?
        }

        for struct_def in &program.structs {
            new_program
                .structs
                .push(self.infer_struct(struct_def, &mut ctx)?)
        }

        for function in &program.functions {
            new_program
                .functions
                .push(self.infer_function(function, &mut ctx)?);
        }

        let mut resolver = Resolver::new();

        resolver.resolve_ast(program, &mut ctx)?;

        let mut mono = Mono::new();

        let mono_program = mono.monomorphize_program(new_program, &mut ctx);

        let mut codegen = Codegen::new();

        let ir = codegen.gen_program(mono_program, &mut ctx);

        use std::fs::File;
        
        Ok(ir)
    }
}
