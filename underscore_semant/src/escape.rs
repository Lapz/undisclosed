use super::InferResult;
use syntax::ast::{Call, Expression, Function, Program, Statement, StructLit, Var};
use util::{pos::Spanned,
           symbol::{Symbol, Symbols}};

pub struct FindEscape {
    depth: u32,
}

impl FindEscape {
    pub fn new() -> Self {
        FindEscape { depth: 0 }
    }
    pub fn find_escape(
        &mut self,
        program: &mut Program,
        env: &mut Symbols<(u32, bool)>,
    ) -> InferResult<()> {
        for function in &mut program.functions {
            self.escape_function(function, env)?;
        }
        Ok(())
    }

    fn escape_function(
        &mut self,
        function: &mut Spanned<Function>,
        env: &mut Symbols<(u32, bool)>,
    ) -> InferResult<()> {
        self.escape_statement(&mut function.value.body, env)
    }

    fn check_ident(&mut self, ident: Symbol, env: &mut Symbols<(u32, bool)>) -> InferResult<()> {
        if env.look(ident).is_none() {
            env.enter(ident, (self.depth, false));
            return Ok(());
        }

        let d = env.look(ident).unwrap().0;

        if d == self.depth {
            Ok(())
        } else {
            env.enter(ident, (self.depth, true));

            Ok(())
        }
    }

    fn escape_statement(
        &mut self,
        statement: &mut Spanned<Statement>,
        env: &mut Symbols<(u32, bool)>,
    ) -> InferResult<()> {
        match statement.value {
            Statement::Block(ref mut statements) => {
                env.begin_scope();
                self.depth += 1;
                for statement in statements {
                    self.escape_statement(statement, env)?;
                }

                env.end_scope();
                self.depth -= 1;
                Ok(())
            }

            Statement::Break | Statement::Continue => Ok(()),
            Statement::Expr(ref mut expr) | Statement::Return(ref mut expr) => {
                self.escape_expression(expr, env)
            }
            Statement::For { ref mut body, .. } => self.escape_statement(body, env),
            Statement::If {
                ref mut then,
                ref mut otherwise,
                ..
            } => {
                self.escape_statement(then, env)?;

                if let Some(ref mut otherwise) = *otherwise {
                    self.escape_statement(otherwise, env)?;
                }

                Ok(())
            }

            Statement::Let {
                ref mut escapes,
                ref ident,
                ..
            } => {
                if env.look(ident.value).is_none() {
                    env.enter(ident.value, (self.depth, false));
                    return Ok(());
                }

                let d = env.look(ident.value).unwrap().0;

                if d == self.depth {
                    Ok(())
                } else {
                    env.enter(ident.value, (self.depth, true));

                    *escapes = true;
                    Ok(())
                }
            }
            Statement::While { ref mut body, .. } => self.escape_statement(body, env),
        }
    }

    fn escape_expression(
        &mut self,
        expr: &mut Spanned<Expression>,
        env: &mut Symbols<(u32, bool)>,
    ) -> InferResult<()> {
        match expr.value {
            Expression::Assign {
                ref name,
                ref mut value,
            } => {
                self.escape_expression(value, env)?;
                self.check_var(name, env)?;

                Ok(())
            }

            Expression::Binary {
                ref mut lhs,
                ref mut rhs,
                ..
            } => {
                self.escape_expression(lhs, env)?;
                self.escape_expression(rhs, env)?;
                Ok(())
            }

            Expression::Cast { ref mut from, .. } => self.escape_expression(from, env),
            Expression::Call(ref mut call) => match call.value {
                Call::Simple {
                    ref callee,
                    ref mut args,
                } => {
                    self.check_ident(callee.value, env)?;

                    for arg in args {
                        self.escape_expression(arg, env)?;
                    }

                    Ok(())
                }
                Call::Instantiation {
                    ref callee,
                    ref mut args,
                    ..
                } => {
                    self.check_ident(callee.value, env)?;

                    for arg in args {
                        self.escape_expression(arg, env)?;
                    }

                    Ok(())
                }
            },
            Expression::Closure(ref mut closure) => self.escape_function(closure, env),
            Expression::Grouping { ref mut expr } => self.escape_expression(expr, env),
            Expression::Literal(_) => Ok(()),
            Expression::StructLit(ref mut struct_lit) => match struct_lit.value {
                StructLit::Simple { ref mut fields, .. } => {
                    for field in fields {
                        self.escape_expression(&mut field.value.expr, env)?;
                    }
                    Ok(())
                }

                StructLit::Instantiation { ref mut fields, .. } => {
                    for field in fields {
                        self.escape_expression(&mut field.value.expr, env)?;
                    }
                    Ok(())
                }
            },
            Expression::Unary { ref mut expr, .. } => self.escape_expression(expr, env),
            Expression::Var(ref var) => self.check_var(var, env),
        }
    }

    fn check_var(&mut self, var: &Spanned<Var>, env: &mut Symbols<(u32, bool)>) -> InferResult<()> {
        match var.value {
            Var::Simple(ref ident) => self.check_ident(ident.value, env),
            Var::Field { ref ident, .. } => self.check_ident(ident.value, env),

            Var::SubScript { ref target, .. } => self.check_ident(target.value, env),
        }
    }
}
