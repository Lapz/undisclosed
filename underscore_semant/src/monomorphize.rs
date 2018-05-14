use ast::typed as t;
use env::Env;
use std::collections::HashMap;
use types::Type;
use util::symbol::Symbol;

#[derive(Debug, Default)]
pub struct Mono {
    gen_functions: Vec<Symbol>,
    new_defs: HashMap<Symbol, Vec<(Symbol, Vec<Type>)>>,
}

impl Mono {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn monomorphize_program(&mut self, program: &mut t::Program, env: &mut Env) {
        for function in &program.functions {
            if function.generic {
                self.gen_functions.push(function.name);
            }
        }

        for function in &mut program.functions {
            self.mono_body(&mut function.body, env)
        }

        let mut new_defs = vec![];

    
        for function in program.functions.iter() {
            if self.new_defs.get(&function.name).is_some() {
                let defs = self.new_defs.remove(&function.name).unwrap();
                for (new_name, param_types) in defs {
                    let mut params = vec![];
                    for (param, ty) in function.params.iter().zip(param_types.into_iter()) {
                        params.push(t::FunctionParam {
                            name: param.name,
                            ty,
                        });
                    }
                    new_defs.push(t::Function {
                        span: function.span,
                        name: new_name,
                        generic: true,
                        params: params,
                        returns: function.returns.clone(),
                        body: function.body.clone(),
                        linkage: function.linkage,
                    });
                }
            }
        }

        println!("{:?}", new_defs);

        program.functions.extend(new_defs.into_iter());
    }

    fn mono_body(&mut self, body: &mut t::Statement, env: &mut Env) {
        match body {
            t::Statement::Block(ref mut statements) => {
                for mut statement in statements {
                    self.mono_body(&mut statement, env)
                }
            }
            t::Statement::Expr(ref mut texpr) => {
                self.mono_function_call(&mut texpr.expr, env);
            }
            t::Statement::Let { ref mut expr, .. } => {
                if let Some(ref mut expr) = *expr {
                    self.mono_function_call(&mut expr.expr, env);
                }
            }
            _ => (),
        }
    }

    fn mono_function_call(&mut self, call: &mut t::Expression, env: &mut Env) {
        match call {
            t::Expression::Call(mut symbol, expressions) => {
                if self.gen_functions.contains(&symbol) {
                    let mut name = env.name(symbol);

                    for ty in expressions.iter() {
                        name.push_str(&format!("{}", ty.ty))
                    }

                    let new_sym = env.symbol(&name);

                  

                    if self.new_defs.get(&symbol).is_some() {
                        let mut defs = self.new_defs.get_mut(&symbol).unwrap();
                        defs.push((new_sym, expressions.iter().map(|e| e.ty.clone()).collect()))
                    } else {
                        self.new_defs.insert(
                            symbol,
                            vec![(new_sym, expressions.iter().map(|e| e.ty.clone()).collect())],
                        );
                    }
                   

                    
                }
            }

            _ => (),
        }
    }
}

// #[derive(Debug)]
// struct Program {
//    pub functions:HashMap<Symbol,Vec<ast::Function>>,
// }

// impl Mono {
// fn monomorphize_program(&mut self,new_program:&mut Program,program:ast::Program) {

// for function in program.functions {
//     if function.generic {
//         self.genbody(&function)
//     }else {
//         new_program.functions.insert(function.name,vec![function]);
//     }

// }

// }

// fn genbody(&mut self,function:&ast::Function)  {

// }

// fn gen_call(&mut self,call:ast::Expression,function:ast::Function) {
// match call {
//     ast::Expression::Call(symbol,expressions) => {

//         if let Some(ref defs) =  self.program.functions.get(&symbol) {
//             if defs.len() > 1 {

//                 let mut new_params = vec![];

//                 for (call_param,def_param) in expressions.iter().zip(function.params) {
//                     new_params.push(ast::FunctionParam{
//                         name:def_param.name,
//                         ty:call_param.ty,
//                     })
//                 }
//                 ast::Function {
//             span:function.span,
//             name:symbol,
//             generic:true,
//             params:params,
//             returns:function.returns.clone(),
//             body:function.body.clone()

//         }
//             }
//         }

//     }

//     _ => ()
// }
// }
// }
