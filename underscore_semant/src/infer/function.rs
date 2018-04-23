use super::{Infer, InferResult};
use cast_check::*;
use codegen::gen::{CodeGen, Ctx};
use codegen::{temp,
              translate::{Level, Translator}};
use env::{Entry, Env, VarEntry};
use std::collections::HashMap;
use syntax::ast::{Call, Expression, Literal,Function, Op, Sign, Size, Statement, StructLit, UnaryOp, Var};
use types::{Field, TyCon, Type, TypeVar, Unique};
use util::{emitter::Reporter, pos::Spanned, symbol::Symbol};

use ast;

impl Infer {
    pub fn infer_function(&mut self,function:&Spanned<Function>, level: &mut Level,
        ctx: &mut Ctx,
        env: &mut Env,
        reporter: &mut Reporter) -> InferResult<ast::Function> {
        let mut poly_tvs = Vec::with_capacity(function.value.name.value.type_params.len());

        for ident in &function.value.name.value.type_params {
            let tv = TypeVar::new();
            env.add_type(ident.value, Entry::Ty(Type::Var(tv)));
            poly_tvs.push(tv);
        }

        let mut param_tys = Vec::with_capacity(function.value.params.value.len());
        let mut params = Vec::with_capacity(function.value.params.value.len());
        let returns = if let Some(ref return_ty) = function.value.returns {
            self.trans_ty(return_ty, env, reporter)?
        } else {
            Type::Nil
        };

        let mut formals = Vec::with_capacity(function.value.params.value.len() + 1);

        for param in &function.value.params.value {
            let ty =self.trans_ty(&param.value.ty, env, reporter)?;
            param_tys.push(ty.clone());
            params.push(ast::FunctionParam {
                name:param.value.name.value,
                ty,
            })
        }

        for param in &function.value.params.value {
            if let Some(ref escape) = env.escapes.look(param.value.name.value) {
                formals.push(escape.1)
            } else {
                formals.push(false)
            }
        }

        param_tys.push(returns.clone()); // Return is the last value

        let label = temp::new_label(&mut env.escapes);
        let mut new_level = Translator::new_level(level.clone(), label, &mut formals);
        env.add_var(
            function.value.name.value.name.value,
            VarEntry::Fun {
                level: new_level.clone(),
                label,
                ty: Type::Poly(
                    poly_tvs,
                    Box::new(Type::App(TyCon::Arrow, param_tys.clone())),
                ),
            },
        );

        env.begin_scope();

        for (param, ident) in param_tys.into_iter().zip(&function.value.params.value) {
            env.add_var(ident.value.name.value, VarEntry::Var(None, param))
        }

        let body = self.infer_statement(&function.value.body, &mut new_level, ctx, env, reporter)?;

        self.unify(&returns, &self.body, reporter, function.value.body.span, env)?;

        env.end_scope();

        Ok(ast::Function {
            span:function.span,
            name:function.value.name.value.name.value,
            params:params,
            returns,
            body,
            linkage:function.value.linkage,
          })
        }
    pub fn infer_statement(
        &mut self,
        statement: &Spanned<Statement>,
        level: &mut Level,
        ctx: &mut Ctx,
        env: &mut Env,
        reporter: &mut Reporter,
    ) -> InferResult<ast::Statement> {
        match statement.value {
            Statement::Block(ref statements) => {
                let mut result = ast::Statement::Expr(ast::TypedExpression {
                    expr: Box::new(ast::Expression::Literal(Literal::Nil)),
                    ty: Type::Nil,
                });
                env.begin_scope();

                for statement in statements {
                    result = self.infer_statement(statement, level, ctx, env, reporter)?;
                }

                env.end_scope();

                

                Ok(result)
            }
            Statement::Break | Statement::Continue => Ok(ast::Statement::Break),
            Statement::Expr(ref expr) => {
                let type_expr = self.infer_expr(
                    expr, level, ctx, env, reporter,
                )?;

             
                Ok(ast::Statement::Expr(type_expr)) // Expressions are given the type of Nil to signify that they return nothing
            }
            Statement::For {
                ref init,
                ref cond,
                ref incr,
                ref body,
            } => {
                if init.is_none() && cond.is_none() && incr.is_none() {
                    let body = self.infer_statement(body, level, ctx, env, reporter)?;
                    return Ok(body);
                }

                let mut init_tyexpr = None;

                if let Some(ref init) = *init {
                    init_tyexpr = Some(Box::new(self.infer_statement(
                        init, level, ctx, env, reporter,
                    )?));
                }

                let mut incr_tyexpr = None;

                if let Some(ref incr) = *incr {
                    let ty = self.infer_expr(incr, level, ctx, env, reporter)?;
                    if !ty.ty.is_int() {
                        // Change
                        let msg = format!("Increment cannot be of type `{}`", ty.ty.print(env));

                        reporter.error(msg, incr.span);
                        return Err(());
                    }

                    incr_tyexpr = Some(ty);
                }

                let mut cond_tyexpr = None;

                if let Some(ref cond) = *cond {
                    let ty = self.infer_expr(cond, level, ctx, env, reporter)?;

                    self.unify(
                        &Type::App(TyCon::Bool, vec![]),
                        &ty.ty,
                        reporter,
                        cond.span,
                        env,
                    )?;

                    cond_tyexpr = Some(ty);
                }

                Ok(ast::Statement::For {
                    init: init_tyexpr,
                    cond: cond_tyexpr,
                    incr: incr_tyexpr,
                    body: Box::new(self.infer_statement(body, level, ctx, env, reporter)?),
                })
            }

            Statement::If {
                ref cond,
                ref then,
                ref otherwise,
            } => {

                let cond_tyexpr = self.infer_expr(cond, level, ctx, env, reporter)?;
                self.unify(
                    &Type::App(TyCon::Bool, vec![]),
                    &cond_tyexpr.ty,
                    reporter,
                    cond.span,
                    env,
                )?;

                let then_tyexpr = Box::new(self.infer_statement(then,level,ctx,env,reporter)?);
                let mut otherwise_tyexpr = None;

                if let Some(ref otherwise) = *otherwise {
                    let tyexpr = Box::new(self.infer_statement(otherwise, level, ctx, env, reporter)?);
                  

                    otherwise_tyexpr = Some(tyexpr)
                } 

                Ok(ast::Statement::If {
                    cond:cond_tyexpr,
                    then:then_tyexpr,
                    otherwise:otherwise_tyexpr,
                })

            }

            Statement::Return(ref expr) => {
                let type_expr = self.infer_expr(
                expr, level, ctx, env, reporter,
            )?;
                   self.body = type_expr.ty.clone();
                    Ok(ast::Statement::Return(type_expr))
            }
            
           ,

            Statement::While { ref cond, ref body } => {
                let expr = self.infer_expr(cond, level, ctx, env, reporter)?;
                self.unify(
                    &Type::App(TyCon::Bool, vec![]),
                    &expr.ty,
                    reporter,
                    cond.span,
                    env,
                )?;

                Ok(ast::Statement::While(
                    expr,
                    Box::new(self.infer_statement(body, level, ctx, env, reporter)?),
                ))
            },

             Statement::Let {
                ref ident,
                ref ty,
                ref expr,
                ref escapes,
            } => {

                if let Some(ref expr) = *expr {
                    let expr_tyexpr = self.infer_expr(expr, level, ctx, env, reporter)?;

                    if let Some(ref ty) = *ty {
                        let t = self.trans_ty(ty, env, reporter)?;

                        self.unify(&expr_tyexpr.ty, &t, reporter, ty.span, env)?;

                        env.add_var(
                            ident.value,
                            VarEntry::Var(
                                Some(Translator::alloc_local(level, *escapes)),
                                t.clone(),
                            ),
                        );

                        return Ok(ast::Statement::Let{ident:ident.value,ty:t,expr:Some(expr_tyexpr)});
                    }

                    env.add_var(
                        ident.value,
                        VarEntry::Var(Some(Translator::alloc_local(level, *escapes)), expr_tyexpr.ty.clone()),
                    );

                     Ok(ast::Statement::Let{ident:ident.value,ty:expr_tyexpr.ty.clone(),expr:Some(expr_tyexpr)})
                } else {
                    if let Some(ref ty) = *ty {
                        let ty = self.trans_ty(ty, env, reporter)?;

                        env.add_var(
                            ident.value,
                            VarEntry::Var(Some(Translator::alloc_local(level, *escapes)), ty.clone()),
                        );

                        return Ok(ast::Statement::Let{ident:ident.value,ty,expr:None})
                    }

                    env.add_var(
                        ident.value,
                        VarEntry::Var(Some(Translator::alloc_local(level, *escapes)), Type::Nil),
                    );

                    Ok(ast::Statement::Let{ident:ident.value,ty:Type::Nil,expr:None})
                }
            }
           
        }
    }
}

impl Infer {
    fn infer_expr(
        &self,
        expr: &Spanned<Expression>,
        level: &mut Level,
        ctx: &mut Ctx,
        env: &mut Env,
        reporter: &mut Reporter,
    ) -> InferResult<ast::TypedExpression> {
        let (typed, ty) = match expr.value {
            Expression::Assign {
                ref name,
                ref value,
            } => {
                let (name, ty) = self.infer_var(name, level, ctx, env, reporter)?;

                let value_ty = self.infer_expr(value, level, ctx, env, reporter)?;

                self.unify(&ty, &value_ty.ty, reporter, expr.span, env)?;

                let ty = value_ty.ty.clone();

                (ast::Expression::Assign(name, value_ty), ty)
            }

            Expression::Binary {
                ref lhs,
                ref op,
                ref rhs,
            } => {
                let span = lhs.span.to(rhs.span);
                let lhs = self.infer_expr(lhs, level, ctx, env, reporter)?;
                let rhs = self.infer_expr(rhs, level, ctx, env, reporter)?;

                match op.value {
                    Op::NEq | Op::Equal => (
                        ast::Expression::Binary(lhs, op.value, rhs),
                        Type::App(TyCon::Bool, vec![]),
                    ),
                    Op::LT | Op::LTE | Op::GT | Op::GTE | Op::And | Op::Or => {
                        self.unify(&lhs.ty, &rhs.ty, reporter, span, env)?;
                        (
                            ast::Expression::Binary(lhs, op.value, rhs),
                            Type::App(TyCon::Bool, vec![]),
                        )
                    }

                    Op::Plus | Op::Slash | Op::Star | Op::Minus => {
                        match self.unify(&lhs.ty, &rhs.ty, reporter, span, env) {
                            Ok(()) => (),
                            Err(_) => match self.unify(
                                &lhs.ty,
                                &Type::App(TyCon::String, vec![]),
                                reporter,
                                span,
                                env,
                            ) {
                                Ok(()) => (),
                                Err(_) => {
                                    reporter.pop_error();
                                    return Err(());
                                }
                            },
                        }

                        let ty = lhs.ty.clone();

                        (ast::Expression::Binary(lhs, op.value, rhs), ty)
                    }
                }
            }

            Expression::Cast { ref from, ref to } => {
                let expr_ty = self.infer_expr(from, level, ctx, env, reporter)?;
                let ty = self.trans_ty(to, env, reporter)?;

                match cast_check(&expr_ty.ty, &ty) {
                    Ok(()) => (ast::Expression::Cast(expr_ty, ty.clone()), ty),
                    Err(_) => {
                        let msg = format!(
                            "Cannot cast `{}` to type `{}`",
                            expr_ty.ty.print(env),
                            ty.print(env)
                        );
                        reporter.error(msg, expr.span);
                        return Err(());
                    }
                }
            }
            Expression::Call(ref call) => self.infer_call(call, level, ctx, env, reporter)?,
            //            Expression::Closure(ref closure) => {
            //                let mut param_tys = Vec::with_capacity(closure.value.params.value.len());
            //
            //                for param in &closure.value.params.value {
            //                    param_tys.push(self.trans_ty(&param.value.ty, env, reporter)?)
            //                }
            //
            //                let label = temp::new_label(&mut env.escapes.clone());
            //
            //                env.add_var(
            //                    closure.value.name.value.name.value,
            //                    VarEntry::Fun {
            //                        level: level.clone(),
            //                        ty: Type::App(TyCon::Arrow, param_tys.clone()),
            //                        label,
            //                    },
            //                );
            //
            //                env.begin_scope();
            //
            //                for (param, ident) in param_tys
            //                    .clone()
            //                    .into_iter()
            //                    .zip(&closure.value.params.value)
            //                {
            //                    env.add_var(ident.value.name.value, VarEntry::Var(None, param))
            //                }
            //
            //                param_tys.push(self.trans_statement(
            //                    &closure.value.body,
            //                    level,
            //                    ctx,
            //                    env,
            //                    reporter,
            //                )?); // Add the return type of the body
            //
            //                env.end_scope();
            //
            //                Ok(Type::Poly(
            //                    Vec::with_capacity(0),
            //                    Box::new(Type::App(TyCon::Arrow, param_tys)),
            //                ))
            //            }
            Expression::Grouping { ref expr } => {
                return self.infer_expr(expr, level, ctx, env, reporter)
            }
            Expression::Literal(ref literal) => {
                let ty = self.infer_literal(literal);
                (ast::Expression::Literal(literal.clone()), ty)
            }

            Expression::StructLit(ref struct_lit) => {
                self.infer_struct_lit(struct_lit, level, ctx, env, reporter)?
            }

            Expression::Unary { ref op, ref expr } => {
                let span = expr.span;
                let expr = self.infer_expr(expr, level, ctx, env, reporter)?;

                match op.value {
                    UnaryOp::Bang => (
                        ast::Expression::Unary(op.value, expr),
                        Type::App(TyCon::Bool, vec![]),
                    ),
                    UnaryOp::Minus => {
                        if !expr.ty.is_int() {
                            let msg =
                                format!("Cannot use `-` operator on type `{}`", expr.ty.print(env));

                            reporter.error(msg, span);
                            return Err(());
                        }

                        let ty = expr.ty.clone();
                        (ast::Expression::Unary(op.value, expr), ty)
                    }
                }
            }

            Expression::Var(ref var) => {
                let (typed, ty) = self.infer_var(var, level, ctx, env, reporter)?;

                (ast::Expression::Var(typed, ty.clone()), ty)
            }

            _ => unimplemented!(),
        };

        Ok(ast::TypedExpression {
            expr: Box::new(typed),
            ty,
        })
    }

    fn infer_struct_lit(
        &self,
        lit: &Spanned<StructLit>,
        level: &mut Level,
        ctx: &mut Ctx,
        env: &mut Env,
        reporter: &mut Reporter,
    ) -> InferResult<(ast::Expression, Type)> {
        match lit.value {
            StructLit::Simple {
                ref ident,
                ref fields,
            } => {
                let record = if let Some(ty) = env.look_type(ident.value).cloned() {
                    ty
                } else {
                    let msg = format!("Undefined struct `{}` ", env.name(ident.value));
                    reporter.error(msg, ident.span);
                    return Err(());
                };

                match record {
                    Entry::Ty(Type::Poly(ref tvars, ref ty)) => match **ty {
                        Type::Struct(_, ref def_fields, ref unique) => {
                            let mut mappings = HashMap::new();

                            for (tvar, field) in tvars.iter().zip(fields) {
                                let ty =
                                    self.trans_expr(&field.value.expr, level, ctx, env, reporter)?;
                                mappings.insert(*tvar, ty);
                            }

                            let mut instance_fields = Vec::new();
                            let mut instance_exprs = Vec::new();
                            let mut found = false;

                            for (def_ty, lit_expr) in def_fields.iter().zip(fields) {
                                if def_ty.name == lit_expr.value.ident.value {
                                    found = true;

                                    let ty = self.infer_expr(
                                        &lit_expr.value.expr,
                                        level,
                                        ctx,
                                        env,
                                        reporter,
                                    )?;

                                    self.unify(
                                        &self.subst(&def_ty.ty, &mut mappings),
                                        &self.subst(&ty.ty, &mut mappings),
                                        reporter,
                                        lit_expr.span,
                                        env,
                                    )?;

                                    instance_fields.push(Field {
                                        name: lit_expr.value.ident.value,
                                        ty: ty.ty.clone(),
                                    });

                                    instance_exprs.push(ty)
                                } else {
                                    found = false;
                                    let msg = format!(
                                        "`{}` is not a member of `{}` ",
                                        env.name(lit_expr.value.ident.value),
                                        env.name(ident.value)
                                    );
                                    reporter.error(msg, lit_expr.value.ident.span);
                                }
                            }

                            if def_fields.len() > fields.len() {
                                let msg =
                                    format!("struct `{}` is missing fields", env.name(ident.value));
                                reporter.error(msg, lit.span);
                                return Err(());
                            } else if def_fields.len() < fields.len() {
                                let msg = format!(
                                    "struct `{}` has too many fields",
                                    env.name(ident.value)
                                );
                                reporter.error(msg, lit.span);
                                return Err(());
                            } else if !found {
                                return Err(());
                            }

                            Ok((
                                ast::Expression::StructLit(ident.value, instance_exprs),
                                Type::Struct(ident.value, instance_fields, *unique),
                            ))
                        }
                        _ => unreachable!(), // Polymorphics functions are stored in the var environment
                    },

                    _ => {
                        let msg = format!("`{}`is not a struct", env.name(ident.value));
                        reporter.error(msg, ident.span);
                        Err(())
                    }
                }
            }

            StructLit::Instantiation {
                ref ident,
                ref fields,
                ref tys,
            } => {
                let record = if let Some(ty) = env.look_type(ident.value).cloned() {
                    ty
                } else {
                    let msg = format!("Undefined struct `{}` ", env.name(ident.value));
                    reporter.error(msg, ident.span);
                    return Err(());
                };

                match record {
                    Entry::Ty(Type::Poly(ref tvars, ref ret)) => {
                        if tvars.len() > tys.value.len() || tvars.len() < tys.value.len() {
                            let msg = format!(
                                "Found `{}` type params expected `{}`",
                                tys.value.len(),
                                tvars.len()
                            );

                            reporter.error(msg, tys.span);

                            return Err(());
                        }

                        match **ret {
                            Type::Struct(_, ref type_fields, ref unique) => {
                                let mut mappings = HashMap::new();

                                for (tvar, ty) in tvars.iter().zip(&tys.value) {
                                    mappings.insert(*tvar, self.trans_ty(ty, env, reporter)?);
                                }

                                let mut instance_fields = Vec::new();
                                let mut instance_exprs = Vec::new();

                                let mut found = false;

                                for (ty, expr) in type_fields.iter().zip(fields) {
                                    if ty.name == expr.value.ident.value {
                                        found = true;
                                        let instance_ty = self.infer_expr(
                                            &expr.value.expr,
                                            level,
                                            ctx,
                                            env,
                                            reporter,
                                        )?;
                                        self.unify(
                                            &self.subst(&instance_ty.ty, &mut mappings),
                                            &self.subst(&ty.ty, &mut mappings),
                                            reporter,
                                            expr.span,
                                            env,
                                        )?;

                                        instance_fields.push(Field {
                                            name: expr.value.ident.value,
                                            ty: instance_ty.ty.clone(),
                                        });

                                        instance_exprs.push(instance_ty);
                                    } else {
                                        found = false;
                                        let msg = format!(
                                            "`{}` is not a member of `{}` ",
                                            env.name(expr.value.ident.value),
                                            env.name(ident.value)
                                        );
                                        reporter.error(msg, expr.value.ident.span);
                                    }
                                }

                                if type_fields.len() > fields.len() {
                                    let msg = format!(
                                        "struct `{}` is missing fields",
                                        env.name(ident.value)
                                    );
                                    reporter.error(msg, lit.span);
                                    return Err(());
                                } else if type_fields.len() < fields.len() {
                                    let msg = format!(
                                        "struct `{}` has too many fields",
                                        env.name(ident.value)
                                    );
                                    reporter.error(msg, lit.span);
                                    return Err(());
                                } else if !found {
                                    return Err(());
                                }

                                Ok((
                                    ast::Expression::StructLit(ident.value, instance_exprs),
                                    Type::Struct(ident.value, instance_fields, *unique),
                                ))
                            }

                            _ => unreachable!(), // Polymorphics functions are stored in the var environment
                        }
                    }
                    _ => {
                        let msg = format!(
                            "`{}` is not polymorphic and cannot be instantiated",
                            env.name(ident.value)
                        );

                        reporter.error(msg, ident.span);
                        Err(())
                    }
                }
            }
        }
    }

    fn infer_literal(&self, literal: &Literal) -> Type {
        match *literal {
            Literal::Char(_) => Type::App(TyCon::Int(Sign::Unsigned, Size::Bit8), vec![]),

            Literal::False(_) | Literal::True(_) => Type::App(TyCon::Bool, vec![]),

            Literal::Str(_) => Type::App(TyCon::String, vec![]),

            Literal::Nil => Type::App(TyCon::Void, vec![]), // Nil is given the type void as only statements return Nil

            Literal::Number(ref number) => match number.ty {
                Some((sign, size)) => Type::App(TyCon::Int(sign, size), vec![]),

                None => Type::Var(TypeVar::new()),
            },
        }
    }

    fn infer_var(
        &self,
        var: &Spanned<Var>,
        level: &mut Level,
        ctx: &mut Ctx,
        env: &mut Env,
        reporter: &mut Reporter,
    ) -> InferResult<(Symbol, Type)> {
        match var.value {
            Var::Simple(ref ident) => {
                if let Some(var) = env.look_var(ident.value).cloned() {
                    Ok((ident.value, var.get_ty()))
                } else {
                    let msg = format!("Undefined variable `{}` ", env.name(ident.value));
                    reporter.error(msg, var.span);
                    Err(())
                }
            }

            Var::Field {
                ref ident,
                ref value,
            } => {
                let record = if let Some(ident) = env.look_var(ident.value).cloned() {
                    ident
                } else {
                    let msg = format!("Undefined variable `{}` ", env.name(ident.value));
                    reporter.error(msg, var.span);
                    return Err(());
                };

                let record = record.get_ty();

                match record {
                    Type::Struct(ref ident, ref fields, _) => {
                        for field in fields {
                            if field.name == value.value {
                                return Ok((field.name, field.ty.clone()));
                            }
                        }

                        let msg = format!(
                            "struct `{}` doesn't have a field named `{}`",
                            env.name(*ident),
                            env.name(value.value)
                        );

                        reporter.error(msg, var.span);

                        Err(())
                    }

                    _ => {
                        let msg = format!(
                            "Type `{}` does not have a field named `{}` ",
                            record.print(env),
                            env.name(value.value)
                        );
                        reporter.error(msg, var.span);
                        Err(())
                    }
                }
            }

            Var::SubScript {
                ref expr,
                ref target,
            } => {
                let target_ty = if let Some(var) = env.look_var(target.value).cloned() {
                    var
                } else {
                    let msg = format!("Undefined variable `{}` ", env.name(target.value));
                    reporter.error(msg, var.span);
                    return Err(());
                };

                let target_ty = target_ty.get_ty();

                if !target_ty.is_int() {
                    let msg = format!(" Cannot index type `{}` ", target_ty.print(env));
                    reporter.error(msg, target.span);
                    return Err(());
                }

                let expr_ty = self.trans_expr(expr, level, ctx, env, reporter)?;

                if !expr_ty.is_int() {
                    let msg = format!("Index expr cannot be of type `{}`", expr_ty.print(env));
                    reporter.error(msg, var.span);
                    return Err(());
                }

                match target_ty {
                    Type::App(TyCon::String, _) => Ok((
                        target.value,
                        Type::App(TyCon::Int(Sign::Unsigned, Size::Bit8), vec![]),
                    )),
                    _ => {
                        let msg = format!(" Cannot index type `{}` ", target_ty.print(env));
                        reporter.error(msg, target.span);
                        Err(())
                    }
                }
            }
        }
    }

    fn infer_call(
        &self,
        call: &Spanned<Call>,
        level: &mut Level,
        ctx: &mut Ctx,
        env: &mut Env,
        reporter: &mut Reporter,
    ) -> InferResult<(ast::Expression, Type)> {
        match call.value {
            Call::Simple {
                ref callee,
                ref args,
            } => {
                let func = if let Some(func) = env.look_var(callee.value).cloned() {
                    func
                } else {
                    let msg = format!("Undefined function `{}`", env.name(callee.value));

                    reporter.error(msg, callee.span);

                    return Err(());
                };

                match func.get_ty() {
                    Type::Poly(ref tvars, ref ret) => match **ret {
                        Type::App(TyCon::Arrow, ref fn_types) => {
                            if fn_types.len() - 1 != args.len() {
                                let msg = format!(
                                    "Expected `{}` args found `{}` ",
                                    fn_types.len() - 1,
                                    args.len()
                                );
                                reporter.error(msg, call.span);
                                return Err(());
                            }

                            let mut mappings = HashMap::new();

                            let mut arg_tys = Vec::new();
                            let mut callee_exprs = vec![];

                            for (tvar, arg) in tvars.iter().zip(args) {
                                let ty = self.infer_expr(arg, level, ctx, env, reporter)?;
                                mappings.insert(*tvar, ty.ty.clone());

                                arg_tys.push((ty.ty.clone(), arg.span));
                                callee_exprs.push(ty);
                            }

                            for (ty, arg) in fn_types.iter().zip(arg_tys) {
                                self.unify(
                                    &self.subst(&arg.0, &mut mappings),
                                    &self.subst(ty, &mut mappings),
                                    reporter,
                                    arg.1,
                                    env,
                                )?;
                            }

                            Ok((
                                ast::Expression::Call(callee.value, callee_exprs),
                                self.subst(fn_types.last().unwrap(), &mut mappings),
                            ))
                        }

                        _ => unreachable!(), // Structs are not stored in the var environment so this path cannot be reached
                    },
                    ref e => {
                        println!("{:?}", e);
                        let msg = format!("`{}` is not callable", env.name(callee.value));

                        reporter.error(msg, callee.span);

                        Err(())
                    }
                }
            }

            Call::Instantiation {
                ref callee,
                ref tys,
                ref args,
            } => {
                let func = if let Some(func) = env.look_var(callee.value) {
                    func.clone()
                } else {
                    let msg = format!("Undefined function `{}`", env.name(callee.value));

                    reporter.error(msg, callee.span);

                    return Err(());
                };

                match func.get_ty() {
                    Type::Poly(ref tvars, ref ret) => {
                        if tvars.len() > tys.value.len() || tvars.len() < tys.value.len() {
                            let msg = format!(
                                "Found `{}` type params expected `{}`",
                                tys.value.len(),
                                tvars.len()
                            );

                            reporter.error(msg, tys.span);

                            return Err(());
                        }

                        // TODO check if type params matched defined number
                        // Error if not polymorphic function
                        let mut mappings = HashMap::new();
                        let mut callee_exprs = vec![];

                        for (tvar, ty) in tvars.iter().zip(&tys.value) {
                            mappings.insert(*tvar, self.trans_ty(ty, env, reporter)?);
                        }

                        match **ret {
                            Type::App(TyCon::Arrow, ref fn_types) => {
                                if fn_types.len() - 1 != args.len() {
                                    let msg = format!(
                                        "Expected `{}` args found `{}` ",
                                        fn_types.len() - 1,
                                        args.len()
                                    );
                                    reporter.error(msg, call.span);
                                    return Err(());
                                }
                                for (ty, arg) in fn_types.iter().zip(args) {
                                    let expr = self.infer_expr(arg, level, ctx, env, reporter)?;

                                    self.unify(
                                        &self.subst(&expr.ty, &mut mappings),
                                        &self.subst(ty, &mut mappings),
                                        reporter,
                                        arg.span,
                                        env,
                                    )?;

                                    callee_exprs.push(expr);
                                }

                                Ok((
                                    ast::Expression::Call(callee.value, callee_exprs),
                                    self.subst(fn_types.last().unwrap(), &mut mappings),
                                ))
                            }

                            _ => unreachable!(), // Structs are not stored in the var environment so this path cannot be reached
                        }
                    }

                    _ => {
                        let msg = format!("`{}` is not callable", env.name(callee.value));

                        reporter.error(msg, callee.span);

                        Err(())
                    }
                }
            }
        }
    }
}
