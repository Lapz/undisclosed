use ast::typed;
use std::collections::HashMap;
use util::symbol::Symbol;

// struct Mono {
//     program:Program,
//     new_defs:Vec<ast::Function>
// }
// #[derive(Debug)]
// struct Program {
//    pub functions:HashMap<Symbol,Vec<ast::Function>>,
// }

// impl Mono {
//     fn monomorphize_program(&mut self,new_program:&mut Program,program:ast::Program) {

//     for function in program.functions {
//         if function.generic {
//             self.genbody(&function)
//         }else {
//             new_program.functions.insert(function.name,vec![function]);
//         }

//     }

// }

// fn genbody(&mut self,function:&ast::Function)  {

// }

// fn gen_call(&mut self,call:ast::Expression,function:ast::Function) {
//     match call {
//         ast::Expression::Call(symbol,expressions) => {

//             if let Some(ref defs) =  self.program.functions.get(&symbol) {
//                 if defs.len() > 1 {

//                     let mut new_params = vec![];

//                     for (call_param,def_param) in expressions.iter().zip(function.params) {
//                         new_params.push(ast::FunctionParam{
//                             name:def_param.name,
//                             ty:call_param.ty,
//                         })
//                     }
//                     ast::Function {
//                 span:function.span,
//                 name:symbol,
//                 generic:true,
//                 params:params,
//                 returns:function.returns.clone(),
//                 body:function.body.clone()

//             }
//                 }
//             }

//         }

//         _ => ()
//     }
// }
// }
