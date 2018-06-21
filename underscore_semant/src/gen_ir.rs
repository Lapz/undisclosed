use ast::typed as t;
use ctx::CompileCtx;
use ir::{ir, optimize::Optimizer, Label, Temp};
use syntax::ast::{Literal, Op, Sign, Size, UnaryOp};
use types::{TyCon, Type};

#[derive(Debug)]
pub struct Codegen {
    loop_label: Option<Label>,
    loop_break_label: Option<Label>,
}

const HP: Temp = Temp(0);

impl Codegen {
    pub fn new() -> Self {
        Self {
            loop_label: None,
            loop_break_label: None,
        }
    }

    pub fn gen_program(&mut self, program: t::Program, ctx: &mut CompileCtx) -> ir::Program {
        let mut lowered = ir::Program {
            functions: Vec::new(),
        };

        for function in program.functions {
            let mut instructions = vec![];
            self.gen_function(&function, &mut instructions, ctx);

            Optimizer::strength_reduction(&mut instructions);
            Optimizer::unused_labels(&mut vec![], &mut instructions);

            lowered.functions.push(ir::Function {
                name: Label::new(),
                body: instructions,
                linkage: function.linkage,
            });
        }

        lowered
    }

    fn gen_function(
        &mut self,
        func: &t::Function,
        instructions: &mut Vec<ir::Instruction>,
        ctx: &mut CompileCtx,
    ) {
        for param in &func.params {
            ctx.add_temp(param.name, Temp::new());
        }

        self.gen_statement(&func.body, instructions, ctx);
    }

    fn gen_statement(
        &mut self,
        statement: &t::Statement,
        instructions: &mut Vec<ir::Instruction>,
        ctx: &mut CompileCtx,
    ) {
        match *statement {
            t::Statement::Block(ref statements) => {
                ctx.begin_scope();
                for statement in statements {
                    self.gen_statement(statement, instructions, ctx)
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

                if let Some(ref expr) = *expr {
                    let id_temp = Temp::new();
                    ctx.add_temp(*ident, id_temp);

                    match *ty {
                        Type::Array(_, ref len) => {
                            instructions.push(ir::Instruction::Copy(HP, ir::Value::Temp(id_temp)));

                            instructions.push(ir::Instruction::BinOp(
                                HP,
                                ir::BinOp::Plus,
                                ir::Value::Const(4 * (*len as u64), Sign::Unsigned, Size::Bit64),
                                ir::Value::Temp(HP),
                            ));
                        }
                        _ => self.gen_expression(expr, id_temp, instructions, ctx),
                    }
                }
            }
            t::Statement::Expr(ref expr) => {
                self.gen_expression(expr, Temp::new(), instructions, ctx)
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

                    self.gen_cond(cond, l1, l2, instructions, ctx);

                    instructions.push(ir::Instruction::Label(l1));

                    self.gen_statement(then, instructions, ctx);

                    instructions.push(ir::Instruction::Jump(l3));
                    instructions.push(ir::Instruction::Label(l2));
                    self.gen_statement(otherwise, instructions, ctx);

                    instructions.push(ir::Instruction::Label(l3));
                } else {
                    self.gen_cond(cond, l1, l2, instructions, ctx);

                    instructions.push(ir::Instruction::Label(l1));

                    self.gen_statement(then, instructions, ctx);

                    instructions.push(ir::Instruction::Label(l2));
                }
            }

            t::Statement::While(ref cond, ref body) => {
                let (start, end) = (Label::new(), Label::new());

                let lbody = Label::new();
                let ltrue = Label::new();
                let lfalse = Label::new();

                self.loop_break_label = Some(end);
                self.loop_label = Some(lbody);

                instructions.push(ir::Instruction::Label(start));

                instructions.push(ir::Instruction::Label(lbody));

                self.gen_cond(cond, ltrue, lfalse, instructions, ctx);

                instructions.push(ir::Instruction::Label(ltrue));

                self.gen_statement(body, instructions, ctx);

                instructions.push(ir::Instruction::Jump(lbody));

                instructions.push(ir::Instruction::Label(lfalse));

                instructions.push(ir::Instruction::Label(end));
            }

            t::Statement::Return(ref expr) => {
                let temp = Temp::new();

                self.gen_expression(expr, temp, instructions, ctx);

                instructions.push(ir::Instruction::Return(ir::Value::Temp(temp)))
            }
        }
    }

    fn gen_expression(
        &mut self,
        expr: &t::TypedExpression,
        temp: Temp,
        instructions: &mut Vec<ir::Instruction>,
        ctx: &mut CompileCtx,
    ) {
        match *expr.expr {
            t::Expression::Array(ref items) => {
                let mut block = vec![];

                for item in items {
                    let temp = Temp::new();

                    self.gen_expression(item, temp, instructions, ctx);
                    block.push(temp);
                }

                instructions.push(ir::Instruction::Block(temp, block))
            }
            t::Expression::Assign(ref name, ref value) => {
                let temp = self.gen_var(name, instructions, ctx);

                self.gen_expression(value, temp, instructions, ctx);

                instructions.push(ir::Instruction::Copy(temp, ir::Value::Temp(temp)))
                //  let temp = self.symbols.look(symbol)
            }
            t::Expression::Binary(ref lhs, ref op, ref rhs) => {
                let lhs_temp = Temp::new();

                let rhs_temp = Temp::new();

                match *op {
                    Op::Plus | Op::Minus | Op::Slash | Op::Star => {
                        self.gen_expression(lhs, lhs_temp, instructions, ctx);
                        self.gen_expression(rhs, rhs_temp, instructions, ctx);
                        let op = gen_bin_op(op);
                        instructions.push(ir::Instruction::BinOp(
                            temp,
                            op,
                            ir::Value::Temp(lhs_temp),
                            ir::Value::Temp(rhs_temp),
                        ));
                    }

                    _ => {
                        let ltrue = Label::new();
                        let lfalse = Label::new();

                        self.gen_cond(expr, ltrue, lfalse, instructions, ctx)
                    }
                }
            }

            t::Expression::Call(_, ref exprs) => {
                let mut params = vec![];

                for expr in exprs {
                    let temp = Temp::new();
                    self.gen_expression(expr, temp, instructions, ctx);
                    params.push(temp)
                }

                instructions.push(ir::Instruction::Call(temp, Label::new(), params))
            }

            t::Expression::Cast(ref from, _) => {
                let temp = Temp::new();
                self.gen_expression(from, temp, instructions, ctx);

                match expr.ty {
                    Type::App(TyCon::Int(sign, size), _) => {
                        instructions.push(ir::Instruction::Cast(temp, sign, size))
                    }

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

                instructions.push(ir::Instruction::Store(temp, value))
            }

            t::Expression::Unary(ref op, ref expr) => {
                let new_temp = Temp::new();
                self.gen_expression(expr, new_temp, instructions, ctx);
                let op = gen_un_op(op);

                instructions.push(ir::Instruction::UnOp(temp, op, ir::Value::Temp(new_temp)))
            }

            t::Expression::Var(ref var) => {
                let t = self.gen_var(var, instructions, ctx);
                instructions.push(ir::Instruction::Copy(temp, ir::Value::Temp(t)))
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

                instructions.push(ir::Instruction::BinOp(
                    addr,
                    ir::BinOp::Mul,
                    ir::Value::Const(4, Sign::Unsigned, Size::Bit64),
                    ir::Value::Temp(addr),
                ));

                instructions.push(ir::Instruction::BinOp(
                    addr,
                    ir::BinOp::Plus,
                    ir::Value::Temp(base),
                    ir::Value::Temp(addr),
                ));

                addr
            }

            _ => unimplemented!(),
        }
    }

    fn gen_cond(
        &mut self,
        cond: &t::TypedExpression,
        ltrue: Label,
        lfalse: Label,
        instructions: &mut Vec<ir::Instruction>,
        ctx: &mut CompileCtx,
    ) {
        match *cond.expr {
            t::Expression::Binary(ref lhs, ref op, ref rhs) => match *op {
                Op::NEq => self.gen_cond(cond, lfalse, ltrue, instructions, ctx),

                Op::And => {
                    let lnext = Label::new();

                    self.gen_cond(lhs, lnext, lfalse, instructions, ctx);

                    instructions.push(ir::Instruction::Jump(lnext));

                    self.gen_cond(rhs, ltrue, lfalse, instructions, ctx);

                    instructions.push(ir::Instruction::Label(lnext));
                }
                Op::Or => {
                    let lnext = Label::new();

                    self.gen_cond(lhs, ltrue, lnext, instructions, ctx);

                    instructions.push(ir::Instruction::Jump(lnext));

                    self.gen_cond(rhs, ltrue, lfalse, instructions, ctx);

                    instructions.push(ir::Instruction::Label(lnext));
                }

                Op::LT | Op::GT | Op::GTE | Op::LTE | Op::Equal => {
                    let lhs_temp = Temp::new();
                    let rhs_temp = Temp::new();
                    self.gen_expression(lhs, lhs_temp, instructions, ctx);
                    self.gen_expression(rhs, rhs_temp, instructions, ctx);
                    instructions.push(ir::Instruction::CJump(
                        lhs_temp,
                        gen_cmp_op(op),
                        rhs_temp,
                        ltrue,
                        lfalse,
                    ))
                }

                ref e => unreachable!("{:?}", e),
            },

            _ => {
                let true_temp = Temp::new();
                let expr_temp = Temp::new();

                self.gen_expression(cond, expr_temp, instructions, ctx);

                ir::Instruction::Store(
                    true_temp,
                    ir::Value::Const(true as u64, Sign::Unsigned, Size::Bit8),
                );

                instructions.push(ir::Instruction::CJump(
                    expr_temp,
                    ir::CmpOp::EQ,
                    true_temp,
                    ltrue,
                    lfalse,
                ))
            }
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
        _ => unreachable!(),
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
