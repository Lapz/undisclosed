use ir::{ir, optimize::Optimizer, Label, Register, Scopes, Temp};
use std::collections::HashMap;
use ctx::CompileCtx;
use ast::typed as t;
use util::{symbol::Symbol};

#[derive(Debug)]
pub struct IrBuilder<'a> {
    params: Vec<Register>,
    current_function:Option<Vec<ir::Instruction>>,
    var_registers: HashMap<Symbol,Register>,
    ctx: &'a CompileCtx<'a>,
}

impl <'a> IrBuilder <'a> {
    pub fn new(ctx:&'a mut CompileCtx<'a>) -> Self {
       IrBuilder {
           params:vec![],
           current_function:None,
           var_registers:HashMap::new(),
           ctx
       }
    }

 

    pub fn compile_fn(&mut self, f:&mut t::Function) {
        for (i,param) in f.params.iter().enumerate() {
            let reg = get_register(i);
            self.params.push(reg);
            self.var_registers.insert(param.name,reg);
        }

        
    }

    fn build_statement(&mut self,statement:&mut t::Statement) {

        match *statement {
            t::Statement::Block(ref statements) {
                for statement in statements {

                }

                
            },

            _ => unimplemented!()
        }
    }
}


fn get_register(i: usize) -> Register {
    match i {
        0 => Register::RDI,
        1 => Register::RSI,
        2 => Register::RDX,
        3 => Register::RCX,
        4 => Register::R8,
        5 => Register::R9,
        _ => panic!("To many params"),
    }
}
