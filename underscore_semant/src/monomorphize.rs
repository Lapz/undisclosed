use ast::typed as t;
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
    // pub fn monomorphize_program(&mut self, mut program: t::Program, env: &mut Env) -> t::Program {
    //     // Build up a list of generic functions
    //     for function in &program.functions {
    //         if function.generic {
    //             self.gen_functions.push(function.name);
    //         }
    //     }

    //     // Walk the ast store the types of the generic functions
    //     for function in &mut program.functions.iter_mut() {
    //         self.mono_body(&mut function.body, env)
    //     }

    //     let mut new_defs = vec![];

    //     // Walk through the defs and generate all the new definitions
    //     for function in program.functions.iter() {
    //         if self.new_defs.get(&function.name).is_some() {
    //             let defs = self.new_defs.remove(&function.name).unwrap();
    //             for (new_name, param_types, returns) in defs {
    //                 let mut params = vec![];
    //                 for (param, ty) in function.params.iter().zip(param_types.into_iter()) {
    //                     params.push(t::FunctionParam {
    //                         name: param.name,
    //                         ty,
    //                     });
    //                 }
    //                 new_defs.push(t::Function {
    //                     span: function.span,
    //                     name: new_name,
    //                     generic: true,
    //                     params: params,
    //                     returns,
    //                     body: self.gen_new_body(function.body.clone(), env),
    //                     linkage: function.linkage,
    //                 });
    //             }
    //         }
    //     }

    //     for function in program.functions {
    //         if self.new_defs.get(&function.name).is_none() {
    //             new_defs.push(t::Function {
    //                 span: function.span,
    //                 name: function.name,
    //                 generic: true,
    //                 params: function.params,
    //                 returns: function.returns,
    //                 body: self.gen_new_body(function.body, env),
    //                 linkage: function.linkage,
    //             })
    //         }
    //     }

    //     t::Program {
    //         functions: new_defs,
    //         structs: program.structs,
    //     }
    // }

    // /// Find of generic calls in the body
    // fn mono_body(&mut self, body: &t::Statement, env: &mut Env) {
    //     match body {
    //         t::Statement::Block(ref statements) => {
    //             for mut statement in statements {
    //                 self.mono_body(&mut statement, env)
    //             }
    //         }

    //         t::Statement::Break | t::Statement::Continue => (),
    //         t::Statement::If {
    //             ref cond,
    //             ref then,
    //             ref otherwise,
    //         } => {
    //             self.mono_expr(cond, env);
    //             self.mono_body(then, env);
    //             if let Some(ref otherwise) = *otherwise {
    //                 self.mono_body(then, env);
    //                 self.mono_body(otherwise, env);
    //             }
    //         }

    //         t::Statement::Expr(ref texpr) | t::Statement::Return(ref texpr) => {
    //             self.mono_expr(texpr, env);
    //         }

    //         t::Statement::Let { ref expr, .. } => {
    //             if let Some(ref expr) = *expr {
    //                 self.mono_expr(expr, env);
    //             }
    //         }

    //         t::Statement::While(ref cond, ref body) => {
    //             self.mono_expr(cond, env);
    //             self.mono_body(body, env)
    //         }
    //     }
    // }

    // fn mono_expr(&mut self, texpr: &t::TypedExpression, env: &mut Env) {
    //     match *texpr.expr {
    //         t::Expression::Array(ref texprs) => {
    //             for texpr in texprs {
    //                 self.mono_expr(texpr, env)
    //             }
    //         }

    //         t::Expression::Assign(ref var, ref texpr) => {
    //             match *var {
    //                 t::Var::SubScript(_, ref texpr, _) => self.mono_expr(texpr, env),
    //                 _ => (),
    //             }

    //             self.mono_expr(texpr, env)
    //         }

    //         t::Expression::Binary(ref lhs, _, ref rhs) => {
    //             self.mono_expr(lhs, env);
    //             self.mono_expr(rhs, env)
    //         }

    //         t::Expression::Cast(ref texpr, _) => self.mono_expr(texpr, env),

    //         t::Expression::Call(ref symbol, ref expressions) => {
    //             if self.gen_functions.contains(&symbol) {
    //                 let mut name = env.name(*symbol);

    //                 for ty in expressions.iter() {
    //                     name.push_str(&format!("{}", ty.ty))
    //                 }

    //                 let mut new_sym = env.symbol(&name);

    //                 if self.new_defs.get(&symbol).is_some() {
    //                     let mut defs = self.new_defs.get_mut(&symbol).unwrap();
    //                     defs.push((
    //                         new_sym,
    //                         expressions.iter().map(|e| e.ty.clone()).collect(),
    //                         texpr.ty.clone(),
    //                     ))
    //                 } else {
    //                     self.new_defs.insert(
    //                         *symbol,
    //                         vec![(
    //                             new_sym,
    //                             expressions.iter().map(|e| e.ty.clone()).collect(),
    //                             texpr.ty.clone(),
    //                         )],
    //                     );
    //                 }
    //             }
    //         }

    //         t::Expression::Closure(ref closure) => self.mono_body(&closure.body, env),

    //         t::Expression::Field(_, _) => (),

    //         t::Expression::Grouping { ref expr } => self.mono_expr(expr, env),

    //         t::Expression::Index(_, ref texpr) => self.mono_expr(texpr, env),

    //         t::Expression::Literal(_) => (),

    //         t::Expression::StructLit(_, ref texprs) => {
    //             for texpr in texprs {
    //                 self.mono_expr(texpr, env)
    //             }
    //         }

    //         t::Expression::Unary(_, ref texpr) => self.mono_expr(texpr, env),

    //         t::Expression::Var(ref var) => match *var {
    //             t::Var::SubScript(_, ref texpr, _) => self.mono_expr(texpr, env),
    //             _ => (),
    //         },
    //     }
    // }

    // fn gen_new_expr(&mut self, texpr: t::TypedExpression, env: &mut Env) -> t::TypedExpression {
    //     let t = *texpr.expr; // RUSTC Limitation see https://stackoverflow.com/questions/28466809/collaterally-moved-error-when-deconstructing-a-box-of-pairs
    //     match t {
    //         t::Expression::Array(texprs) => t::TypedExpression {
    //             expr: {
    //                 let mut vec = Vec::with_capacity(texprs.len());

    //                 for texpr in texprs {
    //                     vec.push(self.gen_new_expr(texpr, env));
    //                 }

    //                 Box::new(t::Expression::Array(vec))
    //             },
    //             ty: texpr.ty,
    //         },
    //         t::Expression::Assign(var, value) => t::TypedExpression {
    //             expr: Box::new(t::Expression::Assign(
    //                 self.gen_new_var(var, env),
    //                 self.gen_new_expr(value, env),
    //             )),
    //             ty: texpr.ty,
    //         },
    //         t::Expression::Binary(lhs, op, rhs) => t::TypedExpression {
    //             expr: Box::new(t::Expression::Binary(
    //                 self.gen_new_expr(lhs, env),
    //                 op,
    //                 self.gen_new_expr(rhs, env),
    //             )),
    //             ty: texpr.ty,
    //         },
    //         t::Expression::Cast(expr, ty) => t::TypedExpression {
    //             expr: Box::new(t::Expression::Cast(self.gen_new_expr(expr, env), ty)),
    //             ty: texpr.ty,
    //         },
    //         t::Expression::Call(symbol, expressions) => {
    //             if self.gen_functions.contains(&symbol) {
    //                 let mut name = env.name(symbol);

    //                 for ty in expressions.iter() {
    //                     name.push_str(&format!("{}", ty.ty))
    //                 }

    //                 let mut new_sym = env.symbol(&name);

    //                 t::TypedExpression {
    //                     expr: Box::new(t::Expression::Call(new_sym, expressions.clone())),
    //                     ty: texpr.ty,
    //                 }
    //             } else {
    //                 t::TypedExpression {
    //                     expr: Box::new(t::Expression::Call(symbol, expressions.clone())),
    //                     ty: texpr.ty,
    //                 }
    //             }
    //         }
    //         t::Expression::Closure(closure) => t::TypedExpression {
    //             expr: Box::new(t::Expression::Closure(Box::new(t::Function {
    //                 span: closure.span,
    //                 name: closure.name,
    //                 generic: closure.generic,
    //                 params: closure.params.clone(),
    //                 returns: closure.returns.clone(),
    //                 linkage: closure.linkage,
    //                 body: self.gen_new_body(closure.body, env),
    //             }))),
    //             ty: texpr.ty,
    //         },

    //         t::Expression::Field(sym1, sym2) => t::TypedExpression {
    //             expr: Box::new(t::Expression::Field(sym1, sym2)),
    //             ty: texpr.ty,
    //         },

    //         t::Expression::Grouping { expr } => t::TypedExpression {
    //             expr: Box::new(t::Expression::Grouping {
    //                 expr: self.gen_new_expr(expr, env),
    //             }),
    //             ty: texpr.ty,
    //         },

    //         t::Expression::Index(sym, expr) => t::TypedExpression {
    //             expr: Box::new(t::Expression::Index(sym, self.gen_new_expr(expr, env))),
    //             ty: texpr.ty,
    //         },

    //         t::Expression::Literal(literal) => t::TypedExpression {
    //             expr: Box::new(t::Expression::Literal(literal)),
    //             ty: texpr.ty,
    //         },

    //         t::Expression::StructLit(name, texprs) => t::TypedExpression {
    //             expr: {
    //                 let mut vec = Vec::with_capacity(texprs.len());

    //                 for texpr in texprs {
    //                     vec.push(self.gen_new_expr(texpr, env));
    //                 }

    //                 Box::new(t::Expression::StructLit(name, vec))
    //             },
    //             ty: texpr.ty,
    //         },

    //         t::Expression::Unary(op, expr) => t::TypedExpression {
    //             expr: Box::new(t::Expression::Unary(op, self.gen_new_expr(expr, env))),
    //             ty: texpr.ty,
    //         },

    //         t::Expression::Var(var) => t::TypedExpression {
    //             expr: Box::new(t::Expression::Var(self.gen_new_var(var, env))),
    //             ty: texpr.ty,
    //         },
    //     }
    // }

    // fn gen_new_var(&mut self, var: t::Var, env: &mut Env) -> t::Var {
    //     match var {
    //         t::Var::Field(_, _, _) | t::Var::Simple(_, _) => var,
    //         t::Var::SubScript(symbol, texpr, ty) => {
    //             t::Var::SubScript(symbol, self.gen_new_expr(texpr, env), ty)
    //         }
    //     }
    // }

    // fn gen_new_body(&mut self, body: t::Statement, env: &mut Env) -> t::Statement {
    //     match body {
    //         t::Statement::Block(statements) => {
    //             let mut new_block = Vec::new();

    //             for mut statement in statements {
    //                 new_block.push(self.gen_new_body(statement, env))
    //             }
    //             t::Statement::Block(new_block)
    //         }
    //         t::Statement::Break => t::Statement::Break,
    //         t::Statement::Continue => t::Statement::Continue,
    //         t::Statement::Expr(texpr) => t::Statement::Expr(self.gen_new_expr(texpr, env)),
    //         t::Statement::If {
    //             cond,
    //             then,
    //             otherwise,
    //         } => {
    //             if let Some(otherwise) = otherwise {
    //                 t::Statement::If {
    //                     cond: self.gen_new_expr(cond, env),
    //                     then: Box::new(self.gen_new_body(*then, env)),
    //                     otherwise: Some(Box::new(self.gen_new_body(*otherwise, env))),
    //                 }
    //             } else {
    //                 t::Statement::If {
    //                     cond: self.gen_new_expr(cond, env),
    //                     then: Box::new(self.gen_new_body(*then, env)),
    //                     otherwise: None,
    //                 }
    //             }
    //         }
    //         t::Statement::Let { ident, ty, expr } => {
    //             if let Some(texpr) = expr {
    //                 t::Statement::Let {
    //                     ident,
    //                     ty,
    //                     expr: Some(self.gen_new_expr(texpr, env)),
    //                 }
    //             } else {
    //                 t::Statement::Let {
    //                     ident,
    //                     ty,
    //                     expr: None,
    //                 }
    //             }
    //         }
    //         t::Statement::Return(texpr) => t::Statement::Return(self.gen_new_expr(texpr, env)),
    //         t::Statement::While(cond, body) => t::Statement::While(
    //             self.gen_new_expr(cond, env),
    //             Box::new(self.gen_new_body(*body, env)),
    //         ),
    //     }
    // }
}
