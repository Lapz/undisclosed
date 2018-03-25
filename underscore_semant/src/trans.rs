use constraints::{TyCon, Type, TypeVar, Unique};
use syntax::ast::{Expression, Function, Sign, Size, Statement, Ty as astType, TyAlias};
use env::{Entry, Env};
use util::pos::Spanned;
use util::emitter::Reporter;
use constraints::{Infer, InferResult};

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
                    _ => panic!("Dont think I should get here"),
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

    pub fn type_alias(
        &self,
        alias: &Spanned<TyAlias>,
        env: &mut Env,
        reporter: &mut Reporter,
    ) -> InferResult<()> {
        if alias.value.ident.value.type_params.is_empty() {
            let ty = self.trans_ty(&alias.value.ty, env, reporter)?;
            println!("{:?}", ty);

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

        println!("{:?}", entry);

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
            // let ty =;
            param_tys.push(self.trans_ty(&param.value.ty, env, reporter)?);
            // env.add_var(param.value.name.value, ty)
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

        // self.unify(lhs, rhs, reporter, span)

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
                let mut result = Type::Nil;

                for statement in statements {
                    result = self.trans_statement(statement, env,reporter)?
                }

                Ok(result)
            }
            Statement::Break | Statement::Continue => Ok(Type::Nil),
            Statement::Expr(ref expr) => self.trans_expr(expr, env,reporter),
            Statement::For {
                ref init,
                ref cond,
                ref incr,
                ref body,
            } => {
                if init.is_none() && cond.is_none() && incr.is_none() {
                    let body = self.trans_statement(body, env,reporter)?;

                    return Ok(body);
                }

                if let Some(ref init) = *init {
                    self.trans_statement(init, env,reporter)?;
                }

                if let Some(ref incr) = *incr {
                    let ty = self.trans_expr(incr, env,reporter)?;

                    if !ty.is_int() { // Change
                        let msg = "Increment should be of type i8,u8,i32,u32,i64,u64";

                       reporter.error(msg, incr.span);
                        return Err(());
                    }
                }

                if let Some(ref cond) = *cond {
                    let ty = self.trans_expr(cond, env,reporter)?;



                    self.unify(&Type::App(TyCon::Bool, vec![]), &ty, reporter, cond.span)?;

                  
                }

                let body = self.trans_statement(body, env,reporter)?;

                Ok(body)
            }

            Statement::If {
                ref cond,
                ref then,
                ref otherwise,
            } => {

                self.unify(&Type::App(TyCon::Bool, vec![]), &self.trans_expr(cond, env,reporter)?, reporter, cond.span)?;

              

                let then_ty = self.trans_statement(then, env,reporter)?;

                if let Some(ref otherwise) = *otherwise {

                    self.unify(&then_ty, &self.trans_statement(otherwise, env,reporter)?, reporter, otherwise.span)?;

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
                    let expr_ty = self.trans_expr(expr, env,reporter)?;

                    if let Some(ref ty) = *ty {
                        let t = self.trans_ty(ty, env,reporter)?;

                      self.unify(&expr_ty, &t, reporter,ty.span)?;

                        let scheme = self.generalize(&t, env);

                        env.add_var(ident.value, scheme);

                        return Ok(t);
                    }

                    let scheme = self.generalize(&expr_ty, env);

                    env.add_var(ident.value, scheme);

                    Ok(Type::Nil)
                } else {
                    if let Some(ref ty) = *ty {
                        let ty = self.trans_ty(ty, env,reporter)?;

                        let scheme = self.generalize(&ty, env);

                        env.add_var(ident.value, scheme);
                        return Ok(ty);
                    }

                    Ok(Type::Nil)
                }
            }

            Statement::Return(ref expr) => self.trans_expr(expr, env,reporter),
            Statement::While { ref cond, ref body } => {
                self.unify(&Type::App(TyCon::Bool, vec![]), &self.trans_expr(cond, env,reporter)?, reporter, cond.span)?;
               
                self.trans_statement(body, env,reporter)?;

                Ok(Type::Nil)
            }
        }
    }


    fn trans_expr(&self,expr:&Spanned<Expression>,env:&mut Env, reporter:&mut Reporter) -> InferResult<Type> {

        unimplemented!()
    }
}

#[cfg(test)]
mod test {

    use syntax::ast::*;
    use env::Env;
    use util::emitter::Reporter;
    use util::symbol::*;
    use std::rc::Rc;
    use syntax::lexer::Lexer;
    use syntax::parser::Parser;
    use trans::type_alias;

    #[test]
    fn alias() {
        let mut reporter = Reporter::new();
        let input = "type transformer<T> = fn(T) -> T;";
        let tokens = Lexer::new(&input, reporter.clone()).lex();
        let strings = Rc::new(FactoryMap::new());

        let mut table = Table::new(Rc::clone(&strings));

        let mut parser = Parser::new(tokens, reporter.clone(), &mut table);

        let mut ast = parser.parse().expect("Failed to parse input");

        let mut env = Env::new(&strings);

        // println!("Befor {:#?}",env);

        type_alias(&ast.type_alias[0], &mut env, &mut reporter).unwrap();

        reporter.emit(input);
        // println!("After {:#?}",env);
    }

}
