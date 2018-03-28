use constraints::{Infer, InferResult};
use std::collections::HashMap;
use constraints::{TyCon, Type, TypeVar,Unique,Field};
use env::{Entry, Env};
use syntax::ast::{Call, Expression, Function, Literal, Op, Program, Sign, Size, Statement,Ty as astType, TyAlias, UnaryOp, Var,StructLit,Struct};
use util::emitter::Reporter;
use util::pos::Spanned;

impl Infer {
    pub fn infer(
        &self,
        program: &Program,
        env: &mut Env,
        reporter: &mut Reporter,
    ) -> InferResult<()> {
        for alias in &program.type_alias {
            self.trans_alias(alias, env, reporter)?
        }

        for record in &program.structs {
            self.trans_struct(record, env,reporter)?
        }

        for function in &program.functions {
            self.trans_function(function, env, reporter)?
        }

        Ok(())
    }
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
                if let Some(ty) = env.look_type(ident.value).cloned() {
                    match ty {
                        Entry::Ty(ref ty) => Ok(ty.clone()),
                        _ => panic!(""),
                    }
                } else {
                    let msg = format!("Undefined Type '{}'", env.name(ident.value));
                    reporter.error(msg, ident.span);
                    Err(())
                }
            }

            astType::Poly(ref ident, ref types) => {
                //Concrete generics i.e List<i32>. List<bool>
                let mut ty = if let Some(ty) = env.look_type(ident.value).cloned() {
                    ty.clone()
                } else {
                    let msg = format!("Undefined Type '{}'", env.name(ident.value));
                    reporter.error(msg, ident.span);
                    return Err(());
                };

                let mut ty = match ty {
                    Entry::TyCon(ty) => ty,
                    Entry::Ty(Type::App(tycon,_)) => tycon,
                    _ => panic!("Dont think I should get here {:?}",),
                };

                let mut trans_types = Vec::new();

                for ty in types {
                    trans_types.push(self.trans_ty(ty, env, reporter)?)
                }

                Ok(Type::App(ty, trans_types))
            }

            astType::Func(ref param_types, ref returns) => {
                let mut trans_types = Vec::new();

                for ty in param_types {
                    trans_types.push(self.trans_ty(ty, env, reporter)?)
                }

                let ret = if let Some(ref ret) = *returns {
                    self.trans_ty(ret, env, reporter)?
                } else {
                    Type::App(TyCon::Void, vec![])
                };

                trans_types.push(ret); // Return type will always be last

                Ok(Type::App(TyCon::Arrow, trans_types))
            }
        }
    }

    pub fn trans_struct(&self,struct_def:&Spanned<Struct>,env:&mut Env,reporter:&mut Reporter) -> InferResult<()> {
        let mut poly_tvs = Vec::with_capacity(struct_def.value.name.value.type_params.len());

        for ident in &struct_def.value.name.value.type_params {
            let tv = TypeVar::new();
            env.add_type(ident.value, Entry::Ty(Type::Var(tv)));
            poly_tvs.push(tv)
        }

         let mut type_fields = Vec::with_capacity(struct_def.value.fields.value.len());


        for field in &struct_def.value.fields.value {
            type_fields.push(Field {
                name: field.value.name.value,
                ty: self.trans_ty(&field.value.ty, env,reporter)?,
                });
        }

        if  struct_def.value.name.value.type_params.is_empty() {
            env.add_type(struct_def.value.name.value.name.value, Entry::Ty(Type::App(TyCon::Unique(Box::new(TyCon::Struct(type_fields)),Unique::new()),vec![]
        )));
        }else {
             env.add_type(struct_def.value.name.value.name.value, Entry::TyCon(
            TyCon::Unique(Box::new(TyCon::Struct(type_fields)),Unique::new())
        ));

        }

       

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

        let entry = Entry::TyCon(TyCon::Fun(
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
        reporter: &mut Reporter,
    ) -> InferResult<()> {
        let mut poly_tvs = Vec::with_capacity(function.value.name.value.type_params.len());

        for ident in &function.value.name.value.type_params {
            let tv = TypeVar::new();
            env.add_type(ident.value, Entry::Ty(Type::Var(tv)));
            poly_tvs.push(tv);
        }

        let mut param_tys = Vec::new(); //change to use with_capictiy

        let returns = if let Some(ref return_ty) = function.value.returns {
            self.trans_ty(return_ty, env, reporter)?
        } else {
            Type::App(TyCon::Void, vec![])
        };

        for param in &function.value.params.value {
           
            param_tys.push(self.trans_ty(&param.value.ty, env, reporter)?);
           
        }

        param_tys.push(returns.clone());

        env.add_var(
            function.value.name.value.name.value,
            Type::Poly(
                poly_tvs,
                Box::new(Type::App(TyCon::Arrow, param_tys.clone())),
            ),
        );

        env.begin_scope();

        for (param, ident) in param_tys.into_iter().zip(&function.value.params.value) {
            env.add_var(ident.value.name.value, param)
        }

        let body = self.trans_statement(&function.value.body, env, reporter)?;

        println!("body {:?} vs returns {:?}", body, returns);

        self.unify(&returns, &body, reporter, function.value.body.span, env)?;

        env.end_scope();

        Ok(())
    }

    pub fn trans_statement(
        &self,
        statement: &Spanned<Statement>,
        env: &mut Env,
        reporter: &mut Reporter,
    ) -> InferResult<Type> {
        match statement.value {
            Statement::Block(ref statements) => {
                let mut result = Type::App(TyCon::Void, vec![]);

                for statement in statements {
                    result = self.trans_statement(statement, env, reporter)?
                }

                Ok(result)
            }
            Statement::Break | Statement::Continue => Ok(Type::Nil),
            Statement::Expr(ref expr) => self.trans_expr(expr, env, reporter),
            Statement::For {
                ref init,
                ref cond,
                ref incr,
                ref body,
            } => {
                if init.is_none() && cond.is_none() && incr.is_none() {
                    let body = self.trans_statement(body, env, reporter)?;

                    return Ok(body);
                }

                if let Some(ref init) = *init {
                    self.trans_statement(init, env, reporter)?;
                }

                if let Some(ref incr) = *incr {
                    let ty = self.trans_expr(incr, env, reporter)?;

                    if !ty.is_int() {
                        // Change
                        let msg = "Increment should be of type i8,u8,i32,u32,i64,u64";

                        reporter.error(msg, incr.span);
                        return Err(());
                    }
                }

                if let Some(ref cond) = *cond {
                    let ty = self.trans_expr(cond, env, reporter)?;

                    self.unify(
                        &Type::App(TyCon::Bool, vec![]),
                        &ty,
                        reporter,
                        cond.span,
                        env,
                    )?;
                }

                let body = self.trans_statement(body, env, reporter)?;

                Ok(body)
            }

            Statement::If {
                ref cond,
                ref then,
                ref otherwise,
            } => {
                self.unify(
                    &Type::App(TyCon::Bool, vec![]),
                    &self.trans_expr(cond, env, reporter)?,
                    reporter,
                    cond.span,
                    env,
                )?;

                let then_ty = self.trans_statement(then, env, reporter)?;

                if let Some(ref otherwise) = *otherwise {
                    self.unify(
                        &then_ty,
                        &self.trans_statement(otherwise, env, reporter)?,
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
            } => {
                if let Some(ref expr) = *expr {
                    let expr_ty = self.trans_expr(expr, env, reporter)?;

                    if let Some(ref ty) = *ty {
                        let t = self.trans_ty(ty, env, reporter)?;

                        self.unify(&expr_ty, &t, reporter, ty.span, env)?;

                        return Ok(t);
                    }

                    env.add_var(ident.value, expr_ty);

                    Ok(Type::App(TyCon::Void, vec![]))
                } else {
                    if let Some(ref ty) = *ty {
                        let ty = self.trans_ty(ty, env, reporter)?;

                        env.add_var(ident.value, ty);
                        return Ok(Type::App(TyCon::Void, vec![]));
                    }

                    Ok(Type::App(TyCon::Void, vec![]))
                }
            }

            Statement::Return(ref expr) => self.trans_expr(expr, env, reporter),
            Statement::While { ref cond, ref body } => {
                self.unify(
                    &Type::App(TyCon::Bool, vec![]),
                    &self.trans_expr(cond, env, reporter)?,
                    reporter,
                    cond.span,
                    env,
                )?;

                self.trans_statement(body, env, reporter)?;

                Ok(Type::App(TyCon::Void, vec![]))
            }
        }
    }

    fn trans_expr(
        &self,
        expr: &Spanned<Expression>,
        env: &mut Env,
        reporter: &mut Reporter,
    ) -> InferResult<Type> {
        match expr.value {
            Expression::Assign {
                ref name,
                ref value,
            } => {
                let name = self.trans_var(name, env, reporter)?;
                let value_ty = self.trans_expr(value, env, reporter)?;

                self.unify(&name, &value_ty, reporter, expr.span, env)?;

                Ok(value_ty)
            }

            Expression::Binary {
                ref lhs,
                ref op,
                ref rhs,
            } => {
                let lhs = self.trans_expr(lhs, env, reporter)?;
                let rhs = self.trans_expr(rhs, env, reporter)?;

                match op.value {
                    Op::NEq | Op::Equal => Ok(Type::App(TyCon::Bool, vec![])),
                    Op::LT | Op::LTE | Op::GT | Op::GTE |  Op::And | Op::Or => {
                        self.unify(&lhs, &rhs, reporter, expr.span, env)?;
                        Ok(Type::App(TyCon::Bool, vec![]))
                    }

                    Op::Plus | Op::Slash | Op::Star | Op::Minus => {
                        match self.unify(&lhs, &rhs, reporter, expr.span, env) {
                            Ok(()) => (),
                            Err(_) => {
                                self.unify(
                                    &lhs,
                                    &Type::App(TyCon::String, vec![]),
                                    reporter,
                                    expr.span,
                                    env,
                                )?;
                            }
                        }

                        Ok(lhs)
                    }

                   
                }
            }

            Expression::Cast { ref expr, ref to } => unimplemented!(),
            Expression::Call(ref call) => self.trans_call(call, env, reporter),
            Expression::Grouping { ref expr } => self.trans_expr(expr, env, reporter),
            Expression::Literal(ref literal) => match *literal {
                Literal::Char(_) => Ok(Type::App(TyCon::Int(Sign::Unsigned, Size::Bit8), vec![])),
                Literal::False(_) |Literal::True(_)  => Ok(Type::App(TyCon::Bool, vec![])),
               
                Literal::Str(_) => Ok(Type::App(TyCon::String, vec![])),
                Literal::Number(ref number) => match number.ty {
                    Some((sign, size)) => Ok(Type::App(TyCon::Int(sign, size), vec![])), // Change
                    None => Ok(Type::App(TyCon::Int(Sign::Signed, Size::Bit32), vec![])),
                },
                Literal::Nil => Ok(Type::App(TyCon::Void, vec![])),
            },
            Expression::StructLit(ref struct_lit) => { self.trans_struct_lit(struct_lit,env,reporter)},
    

            Expression::Unary { ref op, ref expr } => {
                let expr_ty = self.trans_expr(expr, env, reporter)?;

                match op.value {
                    UnaryOp::Bang => Ok(Type::App(TyCon::Bool, vec![])),
                    UnaryOp::Minus => {
                        if !expr_ty.is_int() {
                            let msg = "Expected one of type i8,u8,i32,u32,i64,u64";

                            reporter.error(msg, expr.span);
                            return Err(());
                        }

                        Ok(expr_ty)
                    }
                }
            }
            Expression::Var(ref var) => self.trans_var(var, env, reporter),
        }
    }

    fn trans_call(
        &self,
        call: &Spanned<Call>,
        env: &mut Env,
        reporter: &mut Reporter,
    ) -> InferResult<Type> {
        match call.value {
            Call::Simple {
                ref callee,
                ref args,
            } => {
                let func = if let Some(func) = env.look_var(callee.value) {
                    func.clone()
                } else {
                    let msg = format!("Undefined function {}", env.name(callee.value));

                    reporter.error(msg, callee.span);

                    return Err(());
                };

                match func {
                    Type::Poly(ref tvars, ref ret) => match **ret {
                        Type::App(TyCon::Arrow, ref fn_types) => {
                            let mut mappings = HashMap::new();

                            let mut arg_tys = Vec::new();

                            for (tvar, arg) in tvars.iter().zip(args) {
                                println!("({:?},{:?})", tvar, arg);
                                let ty = self.trans_expr(arg, env, reporter)?;
                                mappings.insert(*tvar, ty.clone());

                                arg_tys.push((ty, arg.span));
                            }

                            for (ty, arg) in fn_types.iter().zip(arg_tys) {
                                self.unify(
                                    &self.subst(&arg.0, &mut mappings, &mut env.metavars),
                                    &self.subst(ty, &mut mappings, &mut env.metavars),
                                    reporter,
                                    arg.1,
                                    env,
                                )?;
                            }

                            Ok(self.subst(
                                fn_types.last().unwrap(),
                                &mut mappings,
                                &mut env.metavars,
                            ))
                        }

                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
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
                    let msg = format!("Undefined function {}", env.name(callee.value));

                    reporter.error(msg, callee.span);

                    return Err(());
                };

                match func {
                    Type::Poly(ref tvars, ref ret) => {
                        // TODO check if type params matched defined number
                        // Error if not polymorphic function
                        let mut mappings = HashMap::new();

                        for (tvar, ty) in tvars.iter().zip(&tys.value) {
                            mappings.insert(*tvar, self.trans_ty(ty, env, reporter)?);
                        }

                        match **ret {
                            Type::App(TyCon::Arrow, ref fn_types) => {
                                for (ty, arg) in fn_types.iter().zip(args) {
                                    self.unify(
                                        &self.subst(
                                            &self.trans_expr(arg, env, reporter)?,
                                            &mut mappings,
                                            &mut env.metavars,
                                        ),
                                        &self.subst(ty, &mut mappings, &mut env.metavars),
                                        reporter,
                                        arg.span,
                                        env,
                                    )?;
                                }

                                Ok(self.subst(
                                    fn_types.last().unwrap(),
                                    &mut mappings,
                                    &mut env.metavars,
                                ))
                            }

                            _ => unreachable!(),
                        }
                    }

                    _ => unreachable!(),
                }
            }
        }
    }

    fn trans_struct_lit(&self,lit:&Spanned<StructLit>,env:&mut Env,reporter:&mut Reporter) -> InferResult<Type> {

        match lit.value {
            StructLit::Simple{ref ident,ref fields} => {
                let ty = if let Some(ty) = env.look_type(ident.value).cloned() {
                    ty
                } else {
                    let msg = format!("Undefined variable '{}' ", env.name(ident.value));
                   reporter.error(msg, ident.span);
                    return Err(());
                };

                let mut tycon = match ty {
                    Entry::TyCon(tycon) => tycon,
                    Entry::Ty(Type::App(tycon,_)) => tycon,
                    _ => panic!("Dont think I should get here {:?}",),
                };
               
                    if let TyCon::Unique(ref tycon,ref unique) = tycon {
                      
                        if let  TyCon::Struct(ref def_fields) = **tycon {
                              let mut lit_field_tys = Vec::new();
                              let mut found = false;

                                
                            for (def_ty,lit_expr) in def_fields.iter().zip(fields) {
                              

                                if def_ty.name == lit_expr.value.ident.value {
                                    found = true;
                                   

                                    let ty = self.trans_expr(&lit_expr.value.expr,env,reporter)?;

                                    self.unify(&def_ty.ty,&ty, reporter, lit_expr.span, env)?;

                                    lit_field_tys.push(Field{
                                        name:lit_expr.value.ident.value,
                                        ty
                                    })
                                }else {
                                    found = false;
                                     let msg =
                                    format!("`{}` is not a member of `{}` ", env.name(lit_expr.value.ident.value),env.name(ident.value));
                                    reporter.error(msg, lit_expr.value.ident.span);
        
                                }

                

                            }

                            if def_fields.len() > fields.len()  {
                                let msg =
                                    format!("Struct {} is missing fields", env.name(ident.value));
                               reporter.error(msg, lit.span);
                                return Err(());
                            } else if def_fields.len() < fields.len() {
                                let msg =
                                    format!("Struct {} has too many fields", env.name(ident.value));
                               reporter.error(msg, lit.span);
                                return Err(());
                            }else if !found {
                                return Err(());
                            }


                            
                          

                            env.add_type(ident.value, Entry::Ty(Type::App(TyCon::Unique(Box::new(TyCon::Struct(lit_field_tys.clone())),*unique),vec![])));

                            Ok(Type::App(TyCon::Unique(Box::new(TyCon::Struct(lit_field_tys.clone())),*unique),vec![]))
                    
                        }else {
                            let msg = format!("`{}`is not a struct",env.name(ident.value));
                               reporter.error(msg, ident.span);
                   Err(())
                        }
                        
                    }
                    else {
                        let msg = format!("`{}`is not a struct",env.name(ident.value));
                               reporter.error(msg, ident.span);
                   Err(())
                }
            },
            _ => unimplemented!()
        }
        
         
    }

    fn trans_var(
        &self,
        var: &Spanned<Var>,
        env: &mut Env,
        reporter: &mut Reporter,
    ) -> InferResult<Type> {
        match var.value {
            Var::Simple(ref ident) => {
                if let Some(var) = env.look_var(ident.value).cloned() {
                    Ok(var.clone())
                } else {
                    let msg = format!("Undefined variable '{}' ", env.name(ident.value));
                    reporter.error(msg, var.span);
                    Err(())
                }
            }

            Var::Field { .. } => unimplemented!(),

            Var::SubScript {
                ref expr,
                ref target,
            } => {
                let target_ty = if let Some(var) = env.look_type(target.value).cloned() {
                    var
                } else {
                    let msg = format!("Undefined variable '{}' ", env.name(target.value));
                    reporter.error(msg, var.span);
                    return Err(());
                };

                if !self.trans_expr(expr, env, reporter)?.is_int() {
                    reporter.error("Expected one of type i8,u8,i32,u32,i64,u64", var.span);
                    return Err(());
                }

                match target_ty {
                    Entry::Ty(Type::App(TyCon::String, _)) => {
                        Ok(Type::App(TyCon::Int(Sign::Unsigned, Size::Bit8), vec![]))
                    }
                    _ => {
                        let msg = format!("'{}' is not an indexable", env.name(target.value));
                        reporter.error(msg, target.span);
                        Err(())
                    }
                }
            }
        }
    }
}