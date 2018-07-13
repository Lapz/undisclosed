use ast::typed as t;
use ctx::CompileCtx;
use ir::{ir, optimize::Optimizer, Label, Register, Scopes, Temp};
use std::collections::HashMap;
use syntax::ast::{Literal, Op, Sign, Size, UnaryOp};
use types::{TyCon, Type};

#[derive(Debug)]
pub(crate) struct Codegen {
    loop_label: Option<Label>,
    loop_break_label: Option<Label>,
    offset: i32,
    cmp_op: Option<ir::CmpOp>,
    cmp: bool,
}

impl Codegen {
    pub fn new() -> Self {
        Self {
            loop_label: None,
            loop_break_label: None,
            offset: 0,
            cmp: false,
            cmp_op: None,
        }
    }

    pub fn gen_program(&mut self, program: t::Program, ctx: &mut CompileCtx) -> ir::Program {
        let mut lowered = ir::Program {
            functions: Vec::new(),
        };

        for function in program.functions {
            let mut instructions = vec![];
            let mut locals = HashMap::new();
            let mut strings = HashMap::new();
            let mut params = HashMap::new();
            self.gen_function(
                &function,
                &mut instructions,
                &mut locals,
                &mut params,
                &mut strings,
                ctx,
            );

            Optimizer::strength_reduction(&mut instructions);
            Optimizer::unused_labels(&mut vec![], &mut instructions);

            lowered.functions.push(ir::Function {
                name: Label::named(function.name),
                body: instructions,
                linkage: function.linkage,
                locals,
                strings,
                params,
            });
        }

        lowered
    }

    fn gen_function(
        &mut self,
        func: &t::Function,
        instructions: &mut Vec<ir::Instruction>,
        locals: &mut HashMap<Temp, i32>,
        params: &mut HashMap<Temp, Register>,
        strings: &mut HashMap<Label, String>,
        ctx: &mut CompileCtx,
    ) {
        let mut scopes = Scopes::new();
        for (i, param) in func.params.iter().enumerate() {
            let temp = Temp::new();

            locals.insert(temp, self.offset -8);
            ctx.add_temp(param.name, temp);
        }

        // locals.insert(temp, self.offset - 8);

        self.gen_statement(&func.body, instructions, locals, strings, &mut scopes, ctx);
    }

    fn gen_statement(
        &mut self,
        statement: &t::Statement,
        instructions: &mut Vec<ir::Instruction>,
        locals: &mut HashMap<Temp, i32>,
        strings: &mut HashMap<Label, String>,
        scopes: &mut Scopes,
        ctx: &mut CompileCtx,
    ) {
        match *statement {
            t::Statement::Block(ref statements) => {
                ctx.begin_scope();

                scopes.begin_scope();

                for statement in statements {
                    self.gen_statement(statement, instructions, locals, strings, scopes, ctx)
                }
                instructions.push(ir::Instruction::Drop(scopes.locals() as isize));
                ctx.end_scope();
                scopes.end_scope();
            }

            t::Statement::Break => instructions.push(ir::Instruction::Jump(
                self.loop_break_label
                    .take()
                    .expect("Using continue out side loop"),
            )),

            t::Statement::Continue => instructions.push(ir::Instruction::Jump(
                self.loop_label
                    .take()
                    .expect("Using continue out side loop"),
            )),

            t::Statement::Let {
                ref ident,
                ref ty,
                ref expr,
                ..
            } => {
                let id_temp = Temp::new();
                ctx.add_temp(*ident, id_temp);
                self.offset -= 8;
                let offset = self.offset;
                locals.insert(id_temp, offset);
                scopes.enter(id_temp, offset);
                if let Some(ref expr) = *expr {
                    // let id_temp = Temp::new();
                    // ctx.add_temp(*ident, id_temp);

                    // match *ty {
                    //     Type::Array(_, ref len) => {
                    //         instructions.push(ir::Instruction::Copy(HP, ir::Value::Temp(id_temp)));

                    //         instructions.push(ir::Instruction::BinOp(
                    //             HP,
                    //             ir::BinOp::Plus,
                    //             ir:Instruction::Value(ir::Value::Const(4 * (*len as u64), Sign::Unsigned, Size::Bit64))
                    //             ir::Instruction::Value(Value::Temp(HP))
                    //         ));
                    //     }
                    //     _ => ,
                    // }

                    let instruction =
                        self.gen_expression(expr, id_temp, instructions, locals, strings, ctx);

                    instructions.push(instruction);

                    instructions.push(ir::Instruction::Deref(id_temp))

                    // unimplemented!()
                }
            }
            t::Statement::Expr(ref expr) => {
                let expr =
                    self.gen_expression(expr, Temp::new(), instructions, locals, strings, ctx);
                instructions.push(expr)
            }
            t::Statement::If {
                ref cond,
                ref then,
                ref otherwise,
            } => {
                let l1 = Label::new();

                if let Some(ref otherwise) = *otherwise {
                    let l2 = Label::new();
                    let l3 = Label::new();

                    self.cmp = true;

                    let cond =
                        self.gen_expression(cond, Temp::new(), instructions, locals, strings, ctx);

                    instructions.push(cond);

                    if self.cmp_op.is_none() {
                        instructions.push(ir::Instruction::Cmp);

                        instructions.push(ir::Instruction::JumpOp(ir::CmpOp::NE, l2))
                    } else {
                        instructions.push(ir::Instruction::JumpOp(self.cmp_op.take().unwrap(), l2));
                    }

                    self.cmp = false;

                    self.gen_statement(then, instructions, locals, strings, scopes, ctx);

                    instructions.push(ir::Instruction::Jump(l3));

                    instructions.push(ir::Instruction::Label(l2));

                    self.gen_statement(otherwise, instructions, locals, strings, scopes, ctx);

                    instructions.push(ir::Instruction::Label(l3));
                } else {
                    self.cmp = true;

                    let cond =
                        self.gen_expression(cond, Temp::new(), instructions, locals, strings, ctx);

                    self.cmp = false;

                    instructions.push(cond);

                    if self.cmp_op.is_none() {
                        instructions.push(ir::Instruction::Cmp);

                        instructions.push(ir::Instruction::JumpOp(ir::CmpOp::NE, l1))
                    } else {
                        instructions.push(ir::Instruction::JumpOp(self.cmp_op.take().unwrap(), l1));
                    }

                    self.gen_statement(then, instructions, locals, strings, scopes, ctx);

                    instructions.push(ir::Instruction::Label(l1));
                }
            }

            t::Statement::While(ref cond, ref body) => {
                let lbody = Label::new();
                let lcond = Label::new();
                let lout = Label::new();

                self.loop_break_label = Some(lout);
                self.loop_label = Some(lbody);

                instructions.push(ir::Instruction::Jump(lcond));

                instructions.push(ir::Instruction::Label(lbody));

                self.gen_statement(body, instructions, locals, strings, scopes, ctx);

                self.cmp = true;

                instructions.push(ir::Instruction::Label(lcond));

                let cond =
                    self.gen_expression(cond, Temp::new(), instructions, locals, strings, ctx);

                instructions.push(cond);

                if let Some(op) = self.cmp_op.take() {
                    instructions.push(ir::Instruction::JumpOp(op, lbody))
                }

                self.cmp = false;
            }

            t::Statement::Return(ref expr) => {
                let temp = Temp::new();
                let instruction =
                    self.gen_expression(expr, temp, instructions, locals, strings, ctx);
                instructions.push(ir::Instruction::Return(Box::new(instruction)))
            }
        }
    }

    fn gen_expression(
        &mut self,
        expr: &t::TypedExpression,
        temp: Temp,
        instructions: &mut Vec<ir::Instruction>,
        locals: &mut HashMap<Temp, i32>,
        strings: &mut HashMap<Label, String>,
        ctx: &mut CompileCtx,
    ) -> ir::Instruction {
        match *expr.expr {
            t::Expression::Array(ref items) => {
                let base = Temp::new();
                self.offset -= (items.len() * 4 + 8) as i32;

                locals.insert(base, self.offset);

                for item in items {
                    let temp = Temp::new();
                    self.offset += 4;
                    locals.insert(temp, self.offset);
                    let expr = self.gen_expression(item, temp, instructions, locals, strings, ctx);
                    instructions.push(expr);
                }

                self.offset += 4;
                locals.insert(temp, self.offset);

                self.gen_expression(
                    items.last().unwrap(),
                    temp,
                    instructions,
                    locals,
                    strings,
                    ctx,
                )
            }
            t::Expression::Assign(ref name, ref value) => {
                let temp = self.gen_var(name, instructions, locals, strings, ctx);

                // ;

                let expr = self.gen_expression(value, temp, instructions, locals, strings, ctx);

                instructions.push(expr);

                ir::Instruction::Move(Temp::new(), Register::RBP(*locals.get(&temp).unwrap()))
            }
            t::Expression::Binary(ref lhs, ref op, ref rhs) => {
                let bop = gen_bin_op(op);

                if self.cmp {
                    self.cmp_op = gen_cmp_op(op)
                }

                let lhs = self.gen_expression(lhs, Temp::new(), instructions, locals, strings, ctx);

                let rhs = self.gen_expression(rhs, Temp::new(), instructions, locals, strings, ctx);
                ir::Instruction::BinOp(temp, bop, Box::new(lhs), Box::new(rhs))
            }

            t::Expression::Call(ref name, ref exprs) => {
                for (i, expr) in exprs.iter().enumerate().rev() {
                    let gen_expr =
                        self.gen_expression(expr, temp, instructions, locals, strings, ctx);

                    instructions.push(gen_expr);
                    instructions.push(ir::Instruction::Push(Register::RAX));
                    instructions.push(ir::Instruction::Pop(get_register(i)))
                }

                ir::Instruction::Call(Label::named(*name))

                // ir::Instruction::Drop(exprs.len() as isize)
            }

            t::Expression::Cast(ref from, _) => {
                let temp = Temp::new();
                self.gen_expression(from, temp, instructions, locals, strings, ctx);

                match expr.ty {
                    Type::App(TyCon::Int(sign, size), _) => ir::Instruction::Cast(temp, sign, size),

                    _ => panic!("Can only cast to ints"),
                }
            }
            t::Expression::Grouping { ref expr } => {
                self.gen_expression(expr, temp, instructions, locals, strings, ctx)
            }
            t::Expression::Literal(ref literal) => {
                let value = match *literal {
                    Literal::Char(ref ch) => {
                        ir::Value::Const(*ch as u64, Sign::Unsigned, Size::Bit8)
                    }

                    Literal::True(ref b) | Literal::False(ref b) => {
                        ir::Value::Const(*b as u64, Sign::Unsigned, Size::Bit8)
                    }

                    Literal::Nil => ir::Value::Const(0, Sign::Unsigned, Size::Bit8),

                    Literal::Number(ref number) => match number.ty {
                        Some((sign, size)) => ir::Value::Const(number.value, sign, size),
                        None => match expr.ty {
                            Type::App(TyCon::Int(sign, size), _) => {
                                ir::Value::Const(number.value, sign, size)
                            }
                            Type::Var(_) => {
                                ir::Value::Const(number.value, Sign::Signed, Size::Bit32)
                            }
                            _ => unreachable!(),
                        },
                    },
                    Literal::Str(ref string) => {
                        let label = Label::new();

                        strings.insert(label, string.clone());
                        ir::Value::Name(label)
                    }
                };

                ir::Instruction::Store(temp, value)
            }

            t::Expression::Unary(ref op, ref expr) => {
                let op = gen_un_op(op);

                ir::Instruction::UnOp(
                    temp,
                    op,
                    Box::new(self.gen_expression(expr, temp, instructions, locals, strings, ctx)),
                )
            }

            t::Expression::Var(ref var) => {
                ir::Instruction::Load(self.gen_var(var, instructions, locals, strings, ctx))
            }

            _ => unimplemented!(),
        }
    }

    fn gen_var(
        &mut self,
        var: &t::Var,
        instructions: &mut Vec<ir::Instruction>,
        locals: &mut HashMap<Temp, i32>,
        strings: &mut HashMap<Label, String>,
        ctx: &mut CompileCtx,
    ) -> Temp {
        match *var {
            t::Var::Simple(ref sym, _) => *ctx.get_temp(*sym).unwrap(),

            t::Var::SubScript(ref sym, ref expr, _) => {
                let base = *ctx.get_temp(*sym).unwrap();
                // let base = locals.get(&base).unwrap();

                let addr = Temp::new();

                let expr = self.gen_expression(expr, addr, instructions, locals, strings, ctx);

                instructions.push(expr);

                // instructions.push(ir::Instruction::BinOp(
                //     addr,
                //     ir::BinOp::Mul,
                //     ir::Value::Const(4, Sign::Unsigned, Size::Bit64),
                //     ir::Value::Temp(addr),
                // ));

                // instructions.push(ir::Instruction::BinOp(
                //     addr,
                //     ir::BinOp::Plus,
                //     ir::Value::Temp(base),
                //     ir::Value::Temp(addr),
                // ));

                addr
            }

            _ => unimplemented!(),
        }
    }
}

fn gen_bin_op(op: &Op) -> ir::BinOp {
    match *op {
        Op::Plus => ir::BinOp::Plus,
        Op::Minus => ir::BinOp::Minus,
        Op::Star => ir::BinOp::Mul,
        Op::Slash => ir::BinOp::Div,
        Op::And => ir::BinOp::And,
        Op::Or => ir::BinOp::Or,
        Op::GT => ir::BinOp::GT,
        Op::GTE => ir::BinOp::GTE,
        Op::LT => ir::BinOp::LT,
        Op::LTE => ir::BinOp::LTE,
        Op::NEq => ir::BinOp::NE,
        Op::Equal => ir::BinOp::EQ,
    }
}

fn gen_cmp_op(op: &Op) -> Option<ir::CmpOp> {
    match *op {
        Op::LT => Some(ir::CmpOp::LT),
        Op::LTE => Some(ir::CmpOp::LTE),
        Op::GT => Some(ir::CmpOp::GT),
        Op::GTE => Some(ir::CmpOp::GTE),
        Op::NEq => Some(ir::CmpOp::NE),
        Op::Equal => Some(ir::CmpOp::EQ),
        _ => None,
    }
}

fn gen_un_op(op: &UnaryOp) -> ir::UnOp {
    match *op {
        UnaryOp::Minus => ir::UnOp::Minus,
        UnaryOp::Bang => ir::UnOp::Bang,
    }
}

fn get_register(i: usize) -> Register {
    match i {
        0 => Register::RDI,
        1 => Register::RSI,
        2 => Register::RDX,
        3 => Register::RCX,
        4 => Register::R8,
        5 => Register::R9,
        _ => panic!("To many params"),
    }
}
