// use super::InferResult;
// use ctx::CompileCtx;
// use ir::Frame;
// use syntax::ast::{Call, Expression, Function, Program, Statement, StructLit, Var};
// use util::{pos::Spanned, symbol::Symbol};

// pub struct FindEscape {
//     depth: u32,
// }

// impl FindEscape {
//     pub fn new() -> Self {
//         FindEscape { depth: 0 }
//     }
//     pub fn find_escape(&mut self, program: &mut Program, ctx: &mut CompileCtx) -> InferResult<()> {
//         for function in &mut program.functions {
//             self.escape_function(function, ctx)?;
//         }
//         Ok(())
//     }

//     fn escape_function(
//         &mut self,
//         function: &mut Spanned<Function>,
//         ctx: &mut CompileCtx,
//     ) -> InferResult<()> {
//         self.escape_statement(&mut function.value.body, ctx)
//     }

//     fn check_ident(&mut self, ident: Symbol, ctx: &mut CompileCtx) -> InferResult<()> {
//         if ctx.look_escape(ident).is_none() {
//             ctx.add_escape(ident, (self.depth, false));
//             return Ok(());
//         }

//         let d = ctx.look_escape(ident).unwrap().0;

//         if d == self.depth {
//             Ok(())
//         } else {
//             ctx.add_escape(ident, (self.depth, true));

//             Ok(())
//         }
//     }

//     fn escape_statement(
//         &mut self,
//         statement: &mut Spanned<Statement>,
//         ctx: &mut CompileCtx,
//     ) -> InferResult<()> {
//         match statement.value {
//             Statement::Block(ref mut statements) => {
//                 ctx.begin_scope();
//                 self.depth += 1;
//                 for statement in statements {
//                     self.escape_statement(statement, ctx)?;
//                 }

//                 ctx.end_scope();
//                 self.depth -= 1;
//                 Ok(())
//             }

//             Statement::Break | Statement::Continue => Ok(()),
//             Statement::Expr(ref mut expr) | Statement::Return(ref mut expr) => {
//                 self.escape_expression(expr, ctx)
//             }
//             Statement::For { ref mut body, .. } => self.escape_statement(body, ctx),
//             Statement::If {
//                 ref mut then,
//                 ref mut otherwise,
//                 ..
//             } => {
//                 self.escape_statement(then, ctx)?;

//                 if let Some(ref mut otherwise) = *otherwise {
//                     self.escape_statement(otherwise, ctx)?;
//                 }

//                 Ok(())
//             }

//             Statement::Let {
//                 ref mut escapes,
//                 ref ident,
//                 ..
//             } => {
//                 if ctx.look_escape(ident.value).is_none() {
//                     ctx.add_escape(ident.value, (self.depth, false));
//                     return Ok(());
//                 }

//                 let d = ctx.look_escape(ident.value).unwrap().0;

//                 if d == self.depth {
//                     Ok(())
//                 } else {
//                     ctx.add_escape(ident.value, (self.depth, true));

//                     *escapes = true;
//                     Ok(())
//                 }
//             }
//             Statement::While { ref mut body, .. } => self.escape_statement(body, ctx),
//         }
//     }

//     fn escape_expression(
//         &mut self,
//         expr: &mut Spanned<Expression>,
//         ctx: &mut CompileCtx,
//     ) -> InferResult<()> {
//         match expr.value {
//             Expression::Array { ref mut items } => {
//                 for item in items {
//                     self.escape_expression(item, ctx)?;
//                 }

//                 Ok(())
//             }
//             Expression::Assign {
//                 ref name,
//                 ref mut value,
//             } => {
//                 self.escape_expression(value, ctx)?;
//                 self.check_var(name, ctx)?;

//                 Ok(())
//             }

//             Expression::Binary {
//                 ref mut lhs,
//                 ref mut rhs,
//                 ..
//             } => {
//                 self.escape_expression(lhs, ctx)?;
//                 self.escape_expression(rhs, ctx)?;
//                 Ok(())
//             }

//             Expression::Cast { ref mut from, .. } => self.escape_expression(from, ctx),
//             Expression::Call(ref mut call) => match call.value {
//                 Call::Simple {
//                     ref callee,
//                     ref mut args,
//                 } => {
//                     self.check_ident(callee.value, ctx)?;

//                     for arg in args {
//                         self.escape_expression(arg, ctx)?;
//                     }

//                     Ok(())
//                 }
//                 Call::Instantiation {
//                     ref callee,
//                     ref mut args,
//                     ..
//                 } => {
//                     self.check_ident(callee.value, ctx)?;

//                     for arg in args {
//                         self.escape_expression(arg, ctx)?;
//                     }

//                     Ok(())
//                 }
//             },
//             Expression::Closure(ref mut closure) => self.escape_function(closure, ctx),
//             Expression::Grouping { ref mut expr } => self.escape_expression(expr, ctx),
//             Expression::Literal(_) => Ok(()),
//             Expression::StructLit(ref mut struct_lit) => match struct_lit.value {
//                 StructLit::Simple { ref mut fields, .. } => {
//                     for field in fields {
//                         self.escape_expression(&mut field.value.expr, ctx)?;
//                     }
//                     Ok(())
//                 }

//                 StructLit::Instantiation { ref mut fields, .. } => {
//                     for field in fields {
//                         self.escape_expression(&mut field.value.expr, ctx)?;
//                     }
//                     Ok(())
//                 }
//             },
//             Expression::Unary { ref mut expr, .. } => self.escape_expression(expr, ctx),
//             Expression::Var(ref var) => self.check_var(var, ctx),
//         }
//     }

//     fn check_var(&mut self, var: &Spanned<Var>, ctx: &mut CompileCtx) -> InferResult<()> {
//         match var.value {
//             Var::Simple(ref ident) => self.check_ident(ident.value, ctx),
//             Var::Field { ref ident, .. } => self.check_ident(ident.value, ctx),

//             Var::SubScript { ref target, .. } => self.check_ident(target.value, ctx),
//         }
//     }
// }
