use ast::typed as t;
use ctx::CompileCtx;
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
    pub fn monomorphize_program(
        &mut self,
        mut program: t::Program,
        ctx: &mut CompileCtx,
    ) -> t::Program {
        // Build up a list of generic functions
        for function in &program.functions {
            if function.generic {
                self.gen_functions.push(function.name);
            }
        }

        // Walk the ast store the types of the generic functions
        for function in &mut program.functions.iter_mut() {
            self.mono_body(&mut function.body, ctx)
        }

        let mut new_defs = vec![];

        // Walk through the defs and generate all the new definitions
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
                        body: self.gen_new_body(function.body.clone(), ctx),
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
                    body: self.gen_new_body(function.body, ctx),
                    linkage: function.linkage,
                })
            }
        }

        t::Program {
            functions: new_defs,
            structs: program.structs,
        }
    }

    /// Find of generic calls in the body
    fn mono_body(&mut self, body: &t::Statement, ctx: &mut CompileCtx) {
        match body {
            t::Statement::Block(ref statements) => {
                for mut statement in statements {
                    self.mono_body(&mut statement, ctx)
                }
            }

            t::Statement::Break | t::Statement::Continue => (),
            t::Statement::If {
                ref cond,
                ref then,
                ref otherwise,
            } => {
                self.mono_expr(cond, ctx);
                self.mono_body(then, ctx);
                if let Some(ref otherwise) = *otherwise {
                    self.mono_body(then, ctx);
                    self.mono_body(otherwise, ctx);
                }
            }

            t::Statement::Expr(ref texpr) | t::Statement::Return(ref texpr) => {
                self.mono_expr(texpr, ctx);
            }

            t::Statement::Let { ref expr, .. } => {
                if let Some(ref expr) = *expr {
                    self.mono_expr(expr, ctx);
                }
            }

            t::Statement::While(ref cond, ref body) => {
                self.mono_expr(cond, ctx);
                self.mono_body(body, ctx)
            }
        }
    }

    fn mono_expr(&mut self, texpr: &t::TypedExpression, ctx: &mut CompileCtx) {
        match *texpr.expr {
            t::Expression::Array(ref texprs) => {
                for texpr in texprs {
                    self.mono_expr(texpr, ctx)
                }
            }

            t::Expression::Assign(ref var, ref texpr) => {
                match *var {
                    t::Var::SubScript(_, ref texpr, _) => self.mono_expr(texpr, ctx),
                    _ => (),
                }

                self.mono_expr(texpr, ctx)
            }

            t::Expression::Binary(ref lhs, _, ref rhs) => {
                self.mono_expr(lhs, ctx);
                self.mono_expr(rhs, ctx)
            }

            t::Expression::Cast(ref texpr, _) => self.mono_expr(texpr, ctx),

            t::Expression::Call(ref symbol, ref expressions) => {
                if self.gen_functions.contains(&symbol) {
                    let mut name = ctx.name(*symbol);

                    for ty in expressions.iter() {
                        name.push_str(&format!("{}", ty.ty))
                    }

                    let new_sym = ctx.symbol(&name);

                    if self.new_defs.get(&symbol).is_some() {
                        let defs = self.new_defs.get_mut(&symbol).unwrap();
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

            t::Expression::Closure(ref closure) => self.mono_body(&closure.body, ctx),

            t::Expression::Field(_, _) => (),

            t::Expression::Grouping { ref expr } => self.mono_expr(expr, ctx),

            t::Expression::Index(_, ref texpr) => self.mono_expr(texpr, ctx),

            t::Expression::Literal(_) => (),

            t::Expression::StructLit(_, ref texprs) => {
                for texpr in texprs {
                    self.mono_expr(texpr, ctx)
                }
            }

            t::Expression::Unary(_, ref texpr) => self.mono_expr(texpr, ctx),

            t::Expression::Var(ref var) => match *var {
                t::Var::SubScript(_, ref texpr, _) => self.mono_expr(texpr, ctx),
                _ => (),
            },
        }
    }

    fn gen_new_expr(
        &mut self,
        texpr: t::TypedExpression,
        ctx: &mut CompileCtx,
    ) -> t::TypedExpression {
        let t = *texpr.expr; // RUSTC Limitation see https://stackoverflow.com/questions/28466809/collaterally-moved-error-when-deconstructing-a-box-of-pairs
        match t {
            t::Expression::Array(texprs) => t::TypedExpression {
                expr: {
                    let mut vec = Vec::with_capacity(texprs.len());

                    for texpr in texprs {
                        vec.push(self.gen_new_expr(texpr, ctx));
                    }

                    Box::new(t::Expression::Array(vec))
                },
                ty: texpr.ty,
            },
            t::Expression::Assign(var, value) => t::TypedExpression {
                expr: Box::new(t::Expression::Assign(
                    self.gen_new_var(var, ctx),
                    self.gen_new_expr(value, ctx),
                )),
                ty: texpr.ty,
            },
            t::Expression::Binary(lhs, op, rhs) => t::TypedExpression {
                expr: Box::new(t::Expression::Binary(
                    self.gen_new_expr(lhs, ctx),
                    op,
                    self.gen_new_expr(rhs, ctx),
                )),
                ty: texpr.ty,
            },
            t::Expression::Cast(expr, ty) => t::TypedExpression {
                expr: Box::new(t::Expression::Cast(self.gen_new_expr(expr, ctx), ty)),
                ty: texpr.ty,
            },
            t::Expression::Call(symbol, expressions) => {
                if self.gen_functions.contains(&symbol) {
                    let mut name = ctx.name(symbol);

                    for ty in expressions.iter() {
                        name.push_str(&format!("{}", ty.ty))
                    }

                    let new_sym = ctx.symbol(&name);

                    t::TypedExpression {
                        expr: Box::new(t::Expression::Call(new_sym, expressions.clone())),
                        ty: texpr.ty,
                    }
                } else {
                    t::TypedExpression {
                        expr: Box::new(t::Expression::Call(symbol, expressions.clone())),
                        ty: texpr.ty,
                    }
                }
            }
            t::Expression::Closure(closure) => t::TypedExpression {
                expr: Box::new(t::Expression::Closure(Box::new(t::Function {
                    span: closure.span,
                    name: closure.name,
                    generic: closure.generic,
                    params: closure.params.clone(),
                    returns: closure.returns.clone(),
                    linkage: closure.linkage,
                    body: self.gen_new_body(closure.body, ctx),
                }))),
                ty: texpr.ty,
            },

            t::Expression::Field(sym1, sym2) => t::TypedExpression {
                expr: Box::new(t::Expression::Field(sym1, sym2)),
                ty: texpr.ty,
            },

            t::Expression::Grouping { expr } => t::TypedExpression {
                expr: Box::new(t::Expression::Grouping {
                    expr: self.gen_new_expr(expr, ctx),
                }),
                ty: texpr.ty,
            },

            t::Expression::Index(sym, expr) => t::TypedExpression {
                expr: Box::new(t::Expression::Index(sym, self.gen_new_expr(expr, ctx))),
                ty: texpr.ty,
            },

            t::Expression::Literal(literal) => t::TypedExpression {
                expr: Box::new(t::Expression::Literal(literal)),
                ty: texpr.ty,
            },

            t::Expression::StructLit(name, texprs) => t::TypedExpression {
                expr: {
                    let mut vec = Vec::with_capacity(texprs.len());

                    for texpr in texprs {
                        vec.push(self.gen_new_expr(texpr, ctx));
                    }

                    Box::new(t::Expression::StructLit(name, vec))
                },
                ty: texpr.ty,
            },

            t::Expression::Unary(op, expr) => t::TypedExpression {
                expr: Box::new(t::Expression::Unary(op, self.gen_new_expr(expr, ctx))),
                ty: texpr.ty,
            },

            t::Expression::Var(var) => t::TypedExpression {
                expr: Box::new(t::Expression::Var(self.gen_new_var(var, ctx))),
                ty: texpr.ty,
            },
        }
    }

    fn gen_new_var(&mut self, var: t::Var, ctx: &mut CompileCtx) -> t::Var {
        match var {
            t::Var::Field(_, _, _) | t::Var::Simple(_, _) => var,
            t::Var::SubScript(symbol, texpr, ty) => {
                t::Var::SubScript(symbol, self.gen_new_expr(texpr, ctx), ty)
            }
        }
    }

    fn gen_new_body(&mut self, body: t::Statement, ctx: &mut CompileCtx) -> t::Statement {
        match body {
            t::Statement::Block(statements) => {
                let mut new_block = Vec::new();

                for statement in statements {
                    new_block.push(self.gen_new_body(statement, ctx))
                }
                t::Statement::Block(new_block)
            }
            t::Statement::Break => t::Statement::Break,
            t::Statement::Continue => t::Statement::Continue,
            t::Statement::Expr(texpr) => t::Statement::Expr(self.gen_new_expr(texpr, ctx)),
            t::Statement::If {
                cond,
                then,
                otherwise,
            } => {
                if let Some(otherwise) = otherwise {
                    t::Statement::If {
                        cond: self.gen_new_expr(cond, ctx),
                        then: Box::new(self.gen_new_body(*then, ctx)),
                        otherwise: Some(Box::new(self.gen_new_body(*otherwise, ctx))),
                    }
                } else {
                    t::Statement::If {
                        cond: self.gen_new_expr(cond, ctx),
                        then: Box::new(self.gen_new_body(*then, ctx)),
                        otherwise: None,
                    }
                }
            }
            t::Statement::Let { ident, ty, expr } => {
                if let Some(texpr) = expr {
                    t::Statement::Let {
                        ident,
                        ty,
                        expr: Some(self.gen_new_expr(texpr, ctx)),
                    }
                } else {
                    t::Statement::Let {
                        ident,
                        ty,
                        expr: None,
                    }
                }
            }
            t::Statement::Return(texpr) => t::Statement::Return(self.gen_new_expr(texpr, ctx)),
            t::Statement::While(cond, body) => t::Statement::While(
                self.gen_new_expr(cond, ctx),
                Box::new(self.gen_new_body(*body, ctx)),
            ),
        }
    }
}
