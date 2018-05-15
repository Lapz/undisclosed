use ast::typed as t;
use env::Env;
use std::collections::HashMap;
use types::Type;
use util::symbol::Symbol;

#[derive(Debug, Default)]
pub struct Mono {
    gen_functions: Vec<Symbol>,
    new_defs: HashMap<Symbol, Vec<(Symbol, Vec<Type>, Type)>>,
}

impl Mono {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn monomorphize_program(&mut self, mut program: t::Program, env: &mut Env) -> t::Program {
        for function in &program.functions {
            if function.generic {
                self.gen_functions.push(function.name);
            }
        }

        for function in &mut program.functions.iter_mut() {
            self.mono_body(&mut function.body, env)
        }

        let mut new_defs = vec![];

        for function in program.functions.iter() {
            if self.new_defs.get(&function.name).is_some() {
                let defs = self.new_defs.remove(&function.name).unwrap();
                for (new_name, param_types, returns) in defs {
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
                        returns,
                        body: self.new_body(function.body.clone(), env),
                        linkage: function.linkage,
                    });
                }
            }
        }

        for function in program.functions {
            if self.new_defs.get(&function.name).is_none() {
                new_defs.push(t::Function {
                    span: function.span,
                    name: function.name,
                    generic: true,
                    params: function.params,
                    returns: function.returns,
                    body: self.new_body(function.body, env),
                    linkage: function.linkage,
                })
            }
        }

        t::Program {
            functions: new_defs,
            structs: program.structs,
        }

        // program.functions.extend(new_defs.into_iter());
    }

    fn mono_body(&mut self, body: &mut t::Statement, env: &mut Env) {
        match body {
            t::Statement::Block(ref mut statements) => {
                for mut statement in statements {
                    self.mono_body(&mut statement, env)
                }
            }
            t::Statement::Expr(ref mut texpr) => {
                self.mono_function_call(texpr, env);
            }
            t::Statement::Let { ref mut expr, .. } => {
                if let Some(ref mut expr) = *expr {
                    self.mono_function_call(expr, env);
                }
            }
            _ => (),
        }
    }

    fn new_body(&mut self, body: t::Statement, env: &mut Env) -> t::Statement {
        match body {
            t::Statement::Block(statements) => {
                let mut new_block = Vec::new();

                for mut statement in statements {
                    new_block.push(self.new_body(statement, env))
                }

                t::Statement::Block(new_block)
            }

            t::Statement::Let { ident, ty, expr } => {
                if let Some(texpr) = expr {
                    t::Statement::Let {
                        ident,
                        ty,
                        expr: Some(self.new_function_call(texpr, env)),
                    }
                } else {
                    t::Statement::Let {
                        ident,
                        ty,
                        expr: None,
                    }
                }
            }

            t::Statement::Expr(texpr) => t::Statement::Expr(self.new_function_call(texpr, env)),
            t::Statement::Return(texpr) => t::Statement::Return(self.new_function_call(texpr, env)),

            ref e => unimplemented!("{:?}", e),
        }
    }

    fn new_function_call(
        &mut self,
        texpr: t::TypedExpression,
        env: &mut Env,
    ) -> t::TypedExpression {
        match &*texpr.expr {
            t::Expression::Call(symbol, expressions) => {
                if self.gen_functions.contains(&symbol) {
                    let mut name = env.name(*symbol);

                    for ty in expressions.iter() {
                        name.push_str(&format!("{}", ty.ty))
                    }

                    let mut new_sym = env.symbol(&name);

                    t::TypedExpression {
                        expr: Box::new(t::Expression::Call(new_sym, expressions.clone())),
                        ty: texpr.ty,
                    }
                } else {
                    t::TypedExpression {
                        expr: Box::new(t::Expression::Call(*symbol, expressions.clone())),
                        ty: texpr.ty,
                    }
                }
            }

            _ => texpr.clone(),
        }
    }

    fn mono_function_call(&mut self, texpr: &mut t::TypedExpression, env: &mut Env) {
        match *texpr.expr {
            t::Expression::Call(ref symbol, ref expressions) => {
                if self.gen_functions.contains(&symbol) {
                    let mut name = env.name(*symbol);

                    for ty in expressions.iter() {
                        name.push_str(&format!("{}", ty.ty))
                    }

                    let mut new_sym = env.symbol(&name);

                    if self.new_defs.get(&symbol).is_some() {
                        let mut defs = self.new_defs.get_mut(&symbol).unwrap();
                        defs.push((
                            new_sym,
                            expressions.iter().map(|e| e.ty.clone()).collect(),
                            texpr.ty.clone(),
                        ))
                    } else {
                        self.new_defs.insert(
                            *symbol,
                            vec![(
                                new_sym,
                                expressions.iter().map(|e| e.ty.clone()).collect(),
                                texpr.ty.clone(),
                            )],
                        );
                    }
                }
            }

            _ => (),
        }
    }
}

// #[derive(Debug, Default)]
// struct Mono2 {
//     new_defs: HashMap<Symbol, Vec<MonoFunction>>,
//     gen_functions: Vec<Symbol>,
// }

// #[derive(Debug)]
// struct MonoFunction {
//     name: Symbol,
//     params: Vec<Type>,
//     returns: Type,
// }

// struct Program {
//     functions: Vec<t::Function>,
// }

// impl Mono2 {
//     pub fn new() -> Self {
//         Self::default()
//     }

//     fn monomorphize_program(&mut self, program: t::Program, env: &mut Env) -> t::Program {
//         let mut new_program = t::Program {
//             functions: Vec::new(),
//         };

//         for function in program.functions {
//             if function.generic {
//                 self.gen_functions.push(function.name);
//             }

//             if new_program.functions.contains(&function) {
//                 if self.new_defs.get(&function.name).is_some() {
//                     let new_defs = self.new_defs.remove(&function.name).unwrap();

//                     for mono_function in new_defs {
//                         let mut params = vec![];
//                         for (param, ty) in
//                             function.params.iter().zip(mono_function.params.into_iter())
//                         {
//                             params.push(t::FunctionParam {
//                                 name: param.name,
//                                 ty,
//                             });
//                         }

//                         new_program.functions.push(t::Function {
//                             span: function.span,
//                             name: mono_function.name,
//                             generic: true,
//                             params: params,
//                             returns: mono_function.returns,
//                             body: self.mono_body(function.body.clone(), env),
//                             linkage: function.linkage,
//                         });
//                     }
//                 }
//             } else {
//                 new_program.functions.push(function)
//             }
//         }

//         new_program
//     }

// }
