use ast::typed as t;
use ctx::CompileCtx;
use ir::{ir, optimize::Optimizer, Label, Temp};
use std::collections::HashMap;
use syntax::ast::{Literal, Op, Sign, Size, UnaryOp};
use types::{TyCon, Type};

#[derive(Debug)]
pub struct Codegen {
    loop_label: Option<Label>,
    loop_break_label: Option<Label>,
    offset: i32,
    cmp_op: Option<ir::CmpOp>,
    cmp: bool,
}

const HP: Temp = Temp(0);

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
            self.gen_function(&function, &mut instructions, &mut locals, ctx);

            Optimizer::strength_reduction(&mut instructions);
            Optimizer::unused_labels(&mut vec![], &mut instructions);

            lowered.functions.push(ir::Function {
                name: Label::new(),
                body: instructions,
                linkage: function.linkage,
                locals,
            });
        }

        lowered
    }

    fn gen_function(
        &mut self,
        func: &t::Function,
        instructions: &mut Vec<ir::Instruction>,
        locals: &mut HashMap<Temp, i32>,
        ctx: &mut CompileCtx,
    ) {
        for param in &func.params {
            let temp = Temp::new();

            ctx.add_temp(param.name, temp);
        }

        self.gen_statement(&func.body, instructions, locals, ctx);
    }

    fn gen_statement(
        &mut self,
        statement: &t::Statement,
        instructions: &mut Vec<ir::Instruction>,
        locals: &mut HashMap<Temp, i32>,
        ctx: &mut CompileCtx,
    ) {
        match *statement {
            t::Statement::Block(ref statements) => {
                ctx.begin_scope();
                for statement in statements {
                    self.gen_statement(statement, instructions, locals, ctx)
                }
                ctx.end_scope();
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
                locals.insert(id_temp, self.offset - 4);

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

                    let instruction = self.gen_expression(expr, id_temp, instructions, ctx);

                    instructions.push(instruction)

                    // unimplemented!()
                }
            }
            t::Statement::Expr(ref expr) => {
                let expr = self.gen_expression(expr, Temp::new(), instructions, ctx);
                instructions.push(expr)
            }
            t::Statement::If {
                ref cond,
                ref then,
                ref otherwise,
            } => {
                let l1 = Label::new();
                let l2 = Label::new();

                if let Some(ref otherwise) = *otherwise {
                    let l3 = Label::new();

                    self.cmp = true;

                    let cond = self.gen_expression(cond, Temp::new(), instructions, ctx);

                    instructions.push(cond);

                    instructions.push(ir::Instruction::JumpOp(
                        self.cmp_op.take().expect("No cmpop found for jump cond"),
                        l2,
                    ));

                    self.gen_statement(then, instructions, locals, ctx);

                    instructions.push(ir::Instruction::Jump(l3));

                    instructions.push(ir::Instruction::Label(l2));

                    self.gen_statement(otherwise, instructions, locals, ctx);

                    instructions.push(ir::Instruction::Label(l3));
                    self.cmp = false;
                } else {
                    self.cmp = true;

                    self.gen_expression(cond, Temp::new(), instructions, ctx);

                    instructions.push(ir::Instruction::JumpOp(
                        self.cmp_op.take().expect("No cmpop found for jump cond"),
                        l2,
                    ));

                    self.gen_statement(then, instructions, locals, ctx);

                    instructions.push(ir::Instruction::Label(l2));
                    self.cmp = false;
                }
            }

            t::Statement::While(ref cond, ref body) => {
                let ltrue = Label::new();
                let lfalse = Label::new();

                self.loop_break_label = Some(ltrue);
                self.loop_label = Some(ltrue);

                self.cmp = true;

                instructions.push(ir::Instruction::Label(ltrue));

                let cond = self.gen_expression(cond, Temp::new(), instructions, ctx);

                instructions.push(cond);

                if let Some(op) = self.cmp_op.take() {
                    instructions.push(ir::Instruction::JumpOp(op, lfalse))
                }

                self.cmp = false;

                self.gen_statement(body, instructions, locals, ctx);

                instructions.push(ir::Instruction::Jump(ltrue));

                instructions.push(ir::Instruction::Label(lfalse));
            }

            t::Statement::Return(ref expr) => {
                let temp = Temp::new();
                let instruction = self.gen_expression(expr, temp, instructions, ctx);
                instructions.push(ir::Instruction::Return(Box::new(instruction)))
            }
        }
    }

    fn gen_expression(
        &mut self,
        expr: &t::TypedExpression,
        temp: Temp,
        instructions: &mut Vec<ir::Instruction>,
        ctx: &mut CompileCtx,
    ) -> ir::Instruction {
        match *expr.expr {
            t::Expression::Array(ref items) => {
                // let mut block = vec![];

                // for item in items {
                //     let temp = Temp::new();

                //     block.push(self.gen_expression(item, temp, instructions, ctx));
                // }

                // ir::Instruction::Block(Label::new(), block)

                unimplemented!()
            }
            t::Expression::Assign(ref name, ref value) => {
                let temp = self.gen_var(name, instructions, ctx);

                self.gen_expression(value, temp, instructions, ctx)
                //  let temp = self.symbols.look(symbol)
            }
            t::Expression::Binary(ref lhs, ref op, ref rhs) => {
                let lhs_temp = Temp::new();
                let lhs = self.gen_expression(lhs, lhs_temp, instructions, ctx);
                let rhs_temp = Temp::new();
                let rhs = self.gen_expression(rhs, rhs_temp, instructions, ctx);
                let bop = gen_bin_op(op);

                if self.cmp {
                    self.cmp_op = Some(gen_cmp_op(op))
                }

                ir::Instruction::BinOp(temp, bop, Box::new(lhs), Box::new(rhs))
            }

            t::Expression::Call(ref name, ref exprs) => {
                for expr in exprs {
                    let expr = self.gen_expression(expr, temp, instructions, ctx);
                    instructions.push(expr)
                }

                ir::Instruction::Call(Label::named(*name))
            }

            t::Expression::Cast(ref from, _) => {
                let temp = Temp::new();
                self.gen_expression(from, temp, instructions, ctx);

                match expr.ty {
                    Type::App(TyCon::Int(sign, size), _) => ir::Instruction::Cast(temp, sign, size),

                    _ => panic!("Can only cast to ints"),
                }
            }
            t::Expression::Grouping { ref expr } => {
                self.gen_expression(expr, temp, instructions, ctx)
            }
            t::Expression::Literal(ref literal) => {
                let value = match *literal {
                    Literal::Char(ref ch) => {
                        ir::Value::Const(*ch as u64, Sign::Unsigned, Size::Bit8)
                    }

                    Literal::True(ref b) | Literal::False(ref b) => {
                        ir::Value::Const(*b as u64, Sign::Unsigned, Size::Bit8)
                    }

                    Literal::Nil => ir::Value::Mem(vec![0x00000000]),

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
                        let mut bytes = vec![];
                        bytes.push(string.len() as u8);
                        bytes.extend(string.as_bytes());

                        ir::Value::Mem(bytes)
                    }
                };

                ir::Instruction::Store(temp, value)
            }

            t::Expression::Unary(ref op, ref expr) => {
                let op = gen_un_op(op);

                ir::Instruction::UnOp(
                    temp,
                    op,
                    Box::new(self.gen_expression(expr, temp, instructions, ctx)),
                )
            }

            t::Expression::Var(ref var) => {
                ir::Instruction::Load(self.gen_var(var, instructions, ctx))
            }

            _ => unimplemented!(),
        }
    }

    fn gen_var(
        &mut self,
        var: &t::Var,
        instructions: &mut Vec<ir::Instruction>,
        ctx: &mut CompileCtx,
    ) -> Temp {
        match *var {
            t::Var::Simple(ref sym, _) => *ctx.look_temp(*sym).unwrap(),

            t::Var::SubScript(ref sym, ref expr, _) => {
                let base = *ctx.look_temp(*sym).unwrap();

                let addr = Temp::new();

                self.gen_expression(expr, addr, instructions, ctx);

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

fn gen_cmp_op(op: &Op) -> ir::CmpOp {
    match *op {
        Op::LT => ir::CmpOp::LT,
        Op::LTE => ir::CmpOp::LTE,
        Op::GT => ir::CmpOp::GT,
        Op::GTE => ir::CmpOp::GTE,
        Op::NEq => ir::CmpOp::NE,
        Op::Equal => ir::CmpOp::EQ,
        _ => unreachable!(),
    }
}

fn gen_un_op(op: &UnaryOp) -> ir::UnOp {
    match *op {
        UnaryOp::Minus => ir::UnOp::Minus,
        UnaryOp::Bang => ir::UnOp::Bang,
    }
}
