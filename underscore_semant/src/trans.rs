use super::{Infer, InferResult};
use ast;
use cast_check::*;
use codegen::gen::{CodeGen, Ctx};
use codegen::{temp,
              translate::{Level, Translator}};
use env::{Entry, Env, VarEntry};
use std::collections::HashMap;
use std::mem;
use syntax::ast::{Call, Expression, Function, Literal, Op, Sign, Size, Statement, Struct,
                  StructLit, Ty as astType, TyAlias, UnaryOp, Var};
use types::{Field, TyCon, Type, TypeVar, Unique};
use util::emitter::Reporter;
use util::pos::Spanned;

impl Infer {
    pub fn trans_ty(
        &self,
        ty: &Spanned<astType>,
        env: &mut Env,
        reporter: &mut Reporter,
    ) -> InferResult<Type> {
        match ty.value {
            astType::Bool => Ok(Type::App(TyCon::Bool, vec![])),
            astType::Str => Ok(Type::App(TyCon::String, vec![])),
            astType::Nil => Ok(Type::App(TyCon::Void, vec![])),
            astType::U8 => Ok(Type::App(TyCon::Int(Sign::Unsigned, Size::Bit8), vec![])),
            astType::I8 => Ok(Type::App(TyCon::Int(Sign::Signed, Size::Bit8), vec![])),
            astType::U32 => Ok(Type::App(TyCon::Int(Sign::Unsigned, Size::Bit32), vec![])),
            astType::I32 => Ok(Type::App(TyCon::Int(Sign::Signed, Size::Bit32), vec![])),
            astType::U64 => Ok(Type::App(TyCon::Int(Sign::Unsigned, Size::Bit64), vec![])),
            astType::I64 => Ok(Type::App(TyCon::Int(Sign::Signed, Size::Bit64), vec![])),
            astType::Simple(ref ident) => {
                if let Some(ty) = env.look_type(ident.value) {
                    match *ty {
                        Entry::Ty(ref ty) => match *ty {
                            Type::Poly(ref tvars, ref ret) => {
                                if !tvars.is_empty() {
                                    let msg = format!(
                                        "Type `{}` is polymorphic,Type arguments missing",
                                        env.name(ident.value)
                                    );

                                    reporter.error(msg, ident.span);
                                    return Err(());
                                }

                                Ok(*ret.clone())
                            }
                            _ => Ok(ty.clone()),
                        },
                        _ => panic!(""),
                    }
                } else {
                    let msg = format!("Undefined Type `{}`", env.name(ident.value));
                    reporter.error(msg, ident.span);
                    Err(())
                }
            }

            astType::Poly(ref ident, ref types) => {
                //Concrete generics i.e List<i32>. List<bool>
                let mut ty = if let Some(ty) = env.look_type(ident.value).cloned() {
                    ty
                } else {
                    let msg = format!("Undefined Type `{}`", env.name(ident.value));
                    reporter.error(msg, ident.span);
                    return Err(());
                };

                match ty {
                    Entry::Ty(Type::Poly(ref tvars, ref ty)) => match *ty.clone() {
                        Type::Struct(_, mut fields, unique) => {
                            if tvars.is_empty() {
                                let msg =
                                    format!("Type `{}` is not polymorphic", env.name(ident.value));
                                reporter.error(msg, ident.span);
                                return Err(());
                            }

                            let mut mappings = HashMap::new();

                            for (tvar, ty) in tvars.iter().zip(types) {
                                mappings.insert(*tvar, self.trans_ty(ty, env, reporter)?);
                            } // First create the mappings

                            for field in &mut fields {
                                let mut ty = self.subst(&field.ty, &mut mappings);

                                mem::swap(&mut field.ty, &mut ty);
                            }

                            Ok(Type::Struct(ident.value, fields, unique))
                        }
                        _ => unreachable!(), // Polymorphic functions are not stored as types they are stored as vars
                    },
                    _ => {
                        let msg = format!("Type `{}` is not polymorphic", env.name(ident.value));
                        reporter.error(msg, ident.span);
                        Err(())
                    }
                }
            }

            astType::Func(ref param_types, ref returns) => {
                let mut trans_types = Vec::new();

                for ty in param_types {
                    trans_types.push(self.trans_ty(ty, env, reporter)?)
                }

                let ret = if let Some(ref ret) = *returns {
                    self.trans_ty(ret, env, reporter)?
                } else {
                    Type::Nil
                };

                trans_types.push(ret); // Return type will always be last

                Ok(Type::App(TyCon::Arrow, trans_types))
            }
        }
    }

    pub fn trans_struct(
        &self,
        struct_def: &Spanned<Struct>,
        env: &mut Env,
        reporter: &mut Reporter,
    ) -> InferResult<()> {
        let mut poly_tvs = Vec::with_capacity(struct_def.value.name.value.type_params.len());

        for ident in &struct_def.value.name.value.type_params {
            let tv = TypeVar::new();
            env.add_type(ident.value, Entry::Ty(Type::Var(tv)));
            poly_tvs.push(tv)
        }

        let mut type_fields = Vec::with_capacity(struct_def.value.fields.value.len());

        let unique = Unique::new();

        env.add_type(
            struct_def.value.name.value.name.value,
            Entry::Ty(Type::Poly(
                poly_tvs.clone(),
                Box::new(Type::Struct(
                    struct_def.value.name.value.name.value,
                    vec![],
                    unique,
                )),
            )),
        ); // For recursive types we need to add the empty struct

        for field in &struct_def.value.fields.value {
            type_fields.push(Field {
                name: field.value.name.value,
                ty: self.trans_ty(&field.value.ty, env, reporter)?,
            });
        }

        env.add_type(
            struct_def.value.name.value.name.value,
            Entry::Ty(Type::Poly(
                poly_tvs.clone(),
                Box::new(Type::Struct(
                    struct_def.value.name.value.name.value,
                    type_fields,
                    unique,
                )),
            )),
        );

        Ok(())
    }

    pub fn trans_alias(
        &self,
        alias: &Spanned<TyAlias>,
        env: &mut Env,
        reporter: &mut Reporter,
    ) -> InferResult<()> {
        if alias.value.ident.value.type_params.is_empty() {
            let ty = self.trans_ty(&alias.value.ty, env, reporter)?;

            env.add_type(alias.value.ident.value.name.value, Entry::Ty(ty));
            return Ok(());
        }

        let mut poly_tvs = Vec::with_capacity(alias.value.ident.value.type_params.len());

        for ident in &alias.value.ident.value.type_params {
            let tv = TypeVar::new();
            env.add_type(ident.value, Entry::Ty(Type::Var(tv)));
            poly_tvs.push(tv);
        }

        let entry = Entry::Ty(Type::Poly(
            poly_tvs,
            Box::new(self.trans_ty(&alias.value.ty, env, reporter)?),
        ));

        env.add_type(alias.value.ident.value.name.value, entry);

        Ok(())
    }

    pub fn trans_function(
        &self,
        function: &Spanned<Function>,
        env: &mut Env,
        ctx: &mut Ctx,
        reporter: &mut Reporter,
        level: Level,
    ) -> InferResult<()> {
        let mut poly_tvs = Vec::with_capacity(function.value.name.value.type_params.len());

        for ident in &function.value.name.value.type_params {
            let tv = TypeVar::new();
            env.add_type(ident.value, Entry::Ty(Type::Var(tv)));
            poly_tvs.push(tv);
        }

        let mut param_tys = Vec::with_capacity(function.value.params.value.len());

        let returns = if let Some(ref return_ty) = function.value.returns {
            self.trans_ty(return_ty, env, reporter)?
        } else {
            Type::Nil
        };

        let mut formals = Vec::with_capacity(function.value.params.value.len() + 1);

        for param in &function.value.params.value {
            param_tys.push(self.trans_ty(&param.value.ty, env, reporter)?);
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
        let mut new_level = Translator::new_level(level, label, &mut formals);
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

        let body = self.trans_statement(&function.value.body, &mut new_level, ctx, env, reporter)?;

        self.unify(&returns, &body, reporter, function.value.body.span, env)?;

        env.end_scope();

        Ok(())
    }

    pub fn trans_statement(
        &self,
        statement: &Spanned<Statement>,
        level: &mut Level,
        ctx: &mut Ctx,
        env: &mut Env,
        reporter: &mut Reporter,
    ) -> InferResult<Type> {
        match statement.value {
            Statement::Block(ref statements) => {
                let mut result = Type::Nil;
                env.begin_scope();

                for statement in statements {
                    result = self.trans_statement(statement, level, ctx, env, reporter)?;
                }

                env.end_scope();

                Ok(result)
            }
            Statement::Break | Statement::Continue => Ok(Type::Nil),
            Statement::Expr(ref expr) => {
                self.trans_expr(expr, level, ctx, env, reporter)?;
                Ok(Type::Nil) // Expressions are given the type of Nil to signify that they return nothing
            }
            Statement::For {
                ref init,
                ref cond,
                ref incr,
                ref body,
            } => {
                if init.is_none() && cond.is_none() && incr.is_none() {
                    let body = self.trans_statement(body, level, ctx, env, reporter)?;

                    return Ok(body);
                }

                if let Some(ref init) = *init {
                    self.trans_statement(init, level, ctx, env, reporter)?;
                }

                if let Some(ref incr) = *incr {
                    let ty = self.trans_expr(incr, level, ctx, env, reporter)?;

                    if !ty.is_int() {
                        // Change
                        let msg = "Increment should be of type i8,u8,i32,u32,i64,u64";

                        reporter.error(msg, incr.span);
                        return Err(());
                    }
                }

                if let Some(ref cond) = *cond {
                    let ty = self.trans_expr(cond, level, ctx, env, reporter)?;

                    self.unify(
                        &Type::App(TyCon::Bool, vec![]),
                        &ty,
                        reporter,
                        cond.span,
                        env,
                    )?;
                }

                self.trans_statement(body, level, ctx, env, reporter)?;

                Ok(Type::Nil)
            }

            Statement::If {
                ref cond,
                ref then,
                ref otherwise,
            } => {
                self.unify(
                    &Type::App(TyCon::Bool, vec![]),
                    &self.trans_expr(cond, level, ctx, env, reporter)?,
                    reporter,
                    cond.span,
                    env,
                )?;

                let then_ty = self.trans_statement(then, level, ctx, env, reporter)?;

                if let Some(ref otherwise) = *otherwise {
                    self.unify(
                        &then_ty,
                        &self.trans_statement(otherwise, level, ctx, env, reporter)?,
                        reporter,
                        otherwise.span,
                        env,
                    )?;

                    Ok(then_ty)
                } else {
                    Ok(then_ty)
                }
            }

            Statement::Let {
                ref ident,
                ref ty,
                ref expr,
                ref escapes,
            } => {
                if let Some(ref expr) = *expr {
                    let expr_ty = self.trans_expr(expr, level, ctx, env, reporter)?;

                    if let Some(ref ty) = *ty {
                        let t = self.trans_ty(ty, env, reporter)?;

                        self.unify(&expr_ty, &t, reporter, ty.span, env)?;

                        env.add_var(
                            ident.value,
                            VarEntry::Var(
                                Some(Translator::alloc_local(level, *escapes)),
                                t.clone(),
                            ),
                        );

                        return Ok(t);
                    }

                    env.add_var(
                        ident.value,
                        VarEntry::Var(Some(Translator::alloc_local(level, *escapes)), expr_ty),
                    );

                    Ok(Type::Nil)
                } else {
                    if let Some(ref ty) = *ty {
                        let ty = self.trans_ty(ty, env, reporter)?;

                        env.add_var(
                            ident.value,
                            VarEntry::Var(Some(Translator::alloc_local(level, *escapes)), ty),
                        );
                        return Ok(Type::Nil);
                    }

                    env.add_var(
                        ident.value,
                        VarEntry::Var(Some(Translator::alloc_local(level, *escapes)), Type::Nil),
                    );

                    Ok(Type::Nil)
                }
            }

            Statement::Return(ref expr) => self.trans_expr(expr, level, ctx, env, reporter),
            Statement::While { ref cond, ref body } => {
                self.unify(
                    &Type::App(TyCon::Bool, vec![]),
                    &self.trans_expr(cond, level, ctx, env, reporter)?,
                    reporter,
                    cond.span,
                    env,
                )?;

                self.trans_statement(body, level, ctx, env, reporter)?;

                Ok(Type::Nil)
            }
        }
    }

    pub fn trans_expr(
        &self,
        expr: &Spanned<Expression>,
        level: &mut Level,
        ctx: &mut Ctx,
        env: &mut Env,
        reporter: &mut Reporter,
    ) -> InferResult<Type> {
        match expr.value {
            Expression::Assign {
                ref name,
                ref value,
            } => {
                let name = self.trans_var(name, level, ctx, env, reporter)?;
                let value_ty = self.trans_expr(value, level, ctx, env, reporter)?;

                self.unify(&name, &value_ty, reporter, expr.span, env)?;

                Ok(value_ty)
            }

            Expression::Binary {
                ref lhs,
                ref op,
                ref rhs,
            } => {
                let span = lhs.span.to(rhs.span);
                let lhs = self.trans_expr(lhs, level, ctx, env, reporter)?;
                let rhs = self.trans_expr(rhs, level, ctx, env, reporter)?;

                match op.value {
                    Op::NEq | Op::Equal => Ok(Type::App(TyCon::Bool, vec![])),
                    Op::LT | Op::LTE | Op::GT | Op::GTE | Op::And | Op::Or => {
                        self.unify(&lhs, &rhs, reporter, span, env)?;
                        Ok(Type::App(TyCon::Bool, vec![]))
                    }

                    Op::Plus | Op::Slash | Op::Star | Op::Minus => {
                        match self.unify(&lhs, &rhs, reporter, span, env) {
                            Ok(()) => (),
                            Err(_) => match self.unify(
                                &lhs,
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

                        Ok(lhs)
                    }
                }
            }

            Expression::Cast { ref from, ref to } => {
                let expr_ty = self.trans_expr(from, level, ctx, env, reporter)?;
                let ty = self.trans_ty(to, env, reporter)?;

                match cast_check(&expr_ty, &ty) {
                    Ok(()) => Ok(ty),
                    Err(_) => {
                        let msg = format!(
                            "Cannot cast `{}` to type `{}`",
                            expr_ty.print(env),
                            ty.print(env)
                        );
                        reporter.error(msg, expr.span);
                        Err(())
                    }
                }
            }
            Expression::Call(ref call) => self.trans_call(call, level, ctx, env, reporter),
            Expression::Closure(ref closure) => {
                let mut param_tys = Vec::with_capacity(closure.value.params.value.len());

                for param in &closure.value.params.value {
                    param_tys.push(self.trans_ty(&param.value.ty, env, reporter)?)
                }

                let label = temp::new_label(&mut env.escapes.clone());

                env.add_var(
                    closure.value.name.value.name.value,
                    VarEntry::Fun {
                        level: level.clone(),
                        ty: Type::App(TyCon::Arrow, param_tys.clone()),
                        label,
                    },
                );

                env.begin_scope();

                for (param, ident) in param_tys
                    .clone()
                    .into_iter()
                    .zip(&closure.value.params.value)
                {
                    env.add_var(ident.value.name.value, VarEntry::Var(None, param))
                }

                param_tys.push(self.trans_statement(
                    &closure.value.body,
                    level,
                    ctx,
                    env,
                    reporter,
                )?); // Add the return type of the body

                env.end_scope();

                Ok(Type::Poly(
                    Vec::with_capacity(0),
                    Box::new(Type::App(TyCon::Arrow, param_tys)),
                ))
            }
            Expression::Grouping { ref expr } => self.trans_expr(expr, level, ctx, env, reporter),
            Expression::Literal(ref literal) => {
                match *literal {
                    Literal::Char(_) => {
                        Ok(Type::App(TyCon::Int(Sign::Unsigned, Size::Bit8), vec![]))
                    }
                    Literal::False(_) | Literal::True(_) => Ok(Type::App(TyCon::Bool, vec![])),

                    Literal::Str(_) => Ok(Type::App(TyCon::String, vec![])),
                    Literal::Number(ref number) => match number.ty {
                        Some((sign, size)) => Ok(Type::App(TyCon::Int(sign, size), vec![])),
                        None => Ok(Type::Var(TypeVar::new())), // Change to use own supply
                    },
                    Literal::Nil => Ok(Type::App(TyCon::Void, vec![])), // Nil is given the type void as only statements return Nil
                }
            }
            Expression::StructLit(ref struct_lit) => {
                self.trans_struct_lit(struct_lit, level, ctx, env, reporter)
            }

            Expression::Unary { ref op, ref expr } => {
                let expr_ty = self.trans_expr(expr, level, ctx, env, reporter)?;

                match op.value {
                    UnaryOp::Bang => Ok(Type::App(TyCon::Bool, vec![])),
                    UnaryOp::Minus => {
                        if !expr_ty.is_int() {
                            let msg =
                                format!("Cannot use `-` operator on type `{}`", expr_ty.print(env));

                            reporter.error(msg, expr.span);
                            return Err(());
                        }

                        Ok(expr_ty)
                    }
                }
            }
            Expression::Var(ref var) => self.trans_var(var, level, ctx, env, reporter),
        }
    }

    fn trans_call(
        &self,
        call: &Spanned<Call>,
        level: &mut Level,
        ctx: &mut Ctx,
        env: &mut Env,
        reporter: &mut Reporter,
    ) -> InferResult<Type> {
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

                let mut callee_tys = vec![];

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

                            for (tvar, arg) in tvars.iter().zip(args) {
                                let ty = self.trans_expr(arg, level, ctx, env, reporter)?;
                                mappings.insert(*tvar, ty.clone());

                                arg_tys.push((ty, arg.span));
                            }

                            for (ty, arg) in fn_types.iter().zip(arg_tys) {
                                self.unify(
                                    &self.subst(&arg.0, &mut mappings),
                                    &self.subst(ty, &mut mappings),
                                    reporter,
                                    arg.1,
                                    env,
                                )?;

                                callee_tys.push(ty)
                            }

                            Ok(self.subst(fn_types.last().unwrap(), &mut mappings))
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
                                    self.unify(
                                        &self.subst(
                                            &self.trans_expr(arg, level, ctx, env, reporter)?,
                                            &mut mappings,
                                        ),
                                        &self.subst(ty, &mut mappings),
                                        reporter,
                                        arg.span,
                                        env,
                                    )?;
                                }

                                Ok(self.subst(fn_types.last().unwrap(), &mut mappings))
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

    fn trans_struct_lit(
        &self,
        lit: &Spanned<StructLit>,
        level: &mut Level,
        ctx: &mut Ctx,
        env: &mut Env,
        reporter: &mut Reporter,
    ) -> InferResult<Type> {
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
                            let mut found = false;

                            for (def_ty, lit_expr) in def_fields.iter().zip(fields) {
                                if def_ty.name == lit_expr.value.ident.value {
                                    found = true;

                                    let ty = self.trans_expr(
                                        &lit_expr.value.expr,
                                        level,
                                        ctx,
                                        env,
                                        reporter,
                                    )?;

                                    self.unify(
                                        &self.subst(&def_ty.ty, &mut mappings),
                                        &self.subst(&ty, &mut mappings),
                                        reporter,
                                        lit_expr.span,
                                        env,
                                    )?;

                                    instance_fields.push(Field {
                                        name: lit_expr.value.ident.value,
                                        ty,
                                    })
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

                            Ok(Type::Struct(ident.value, instance_fields, *unique))
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

                        let mut mappings = HashMap::new();

                        for (tvar, ty) in tvars.iter().zip(&tys.value) {
                            mappings.insert(*tvar, self.trans_ty(ty, env, reporter)?);
                        }

                        match **ret {
                            Type::Struct(_, ref type_fields, ref unique) => {
                                let mut instance_fields = Vec::new();

                                let mut found = false;

                                for (ty, expr) in type_fields.iter().zip(fields) {
                                    if ty.name == expr.value.ident.value {
                                        found = true;
                                        let instance_ty = self.trans_expr(
                                            &expr.value.expr,
                                            level,
                                            ctx,
                                            env,
                                            reporter,
                                        )?;
                                        self.unify(
                                            &self.subst(&instance_ty, &mut mappings),
                                            &self.subst(&ty.ty, &mut mappings),
                                            reporter,
                                            expr.span,
                                            env,
                                        )?;

                                        instance_fields.push(Field {
                                            name: expr.value.ident.value,
                                            ty: instance_ty,
                                        });
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

                                Ok(Type::Struct(ident.value, instance_fields, *unique))
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

    fn trans_var(
        &self,
        var: &Spanned<Var>,
        level: &mut Level,
        ctx: &mut Ctx,
        env: &mut Env,
        reporter: &mut Reporter,
    ) -> InferResult<Type> {
        match var.value {
            Var::Simple(ref ident) => {
                if let Some(var) = env.look_var(ident.value).cloned() {
                    Ok(var.get_ty())
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
                                return Ok(field.ty.clone());
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
                    Type::App(TyCon::String, _) => {
                        Ok(Type::App(TyCon::Int(Sign::Unsigned, Size::Bit8), vec![]))
                    }
                    _ => {
                        let msg = format!(" Cannot index type `{}` ", target_ty.print(env));
                        reporter.error(msg, target.span);
                        Err(())
                    }
                }
            }
        }
    }
}
