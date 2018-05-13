use ast::typed as t;
use codegen::{
    ir::*, temp::{new_label, new_label_pair, new_named_label, Label, Temp},
};
use std::u64;
use syntax::ast::{Literal, Op, Sign, Size, UnaryOp};
use types::{TyCon, Type};
use util::symbol::{Symbols,Symbol};

#[derive(Debug)]
pub struct Codegen {
    pub instructions: Vec<Instruction>,
    loop_label: Option<Label>,
    loop_break_label: Option<Label>,
    symbols: Symbols<Temp>,
}


const HP: Temp = Temp(0);

impl Codegen {
    pub fn new(symbols: Symbols<Temp>) -> Self {
        Self {
            symbols,
            loop_label: None,
            loop_break_label: None,
            instructions: vec![],
        }
    }

    pub fn dump_to_file(&mut self, path: String) {
        use std::fs::File;
        use std::io::Write;

        let mut file = File::create(path).expect("Couldn't create file");

        for instruction in &self.instructions {
            file.write(instruction.fmt(&mut self.symbols).as_bytes())
                .expect("Couldn't write to the file");
        }

        file.write(format!("\n{:?}", self.instructions).as_bytes())
            .expect("Couldn't write to the file");
    }

    pub fn gen_program(&mut self, program: &t::Program) {
        for function in &program.functions {
            self.gen_statement(&function.body)
        }
    }

    fn gen_statement(&mut self, statement: &t::Statement) {
        match *statement {
            t::Statement::Block(ref statements) => {
                // let (start, end) = new_label_pair("start", "end", &mut self.symbols);
                // self.instructions.push(Instruction::Label(start));
                for statement in statements {
                    self.gen_statement(statement)
                }

                // self.instructions.push(Instruction::Label(end));
            }

            t::Statement::Break => self.instructions.push(Instruction::Jump(
                self.loop_break_label.expect("Using continue out side loop"),
            )),

            t::Statement::Continue => self.instructions.push(Instruction::Jump(
                self.loop_label.expect("Using continue out side loop"),
            )),
            t::Statement::Let {
                ref ident,
                ref ty,
                ref expr,
                ..
            } => {
                let id_temp = Temp::new();
                self.symbols.enter(*ident, id_temp);

                if let Some(ref expr) = *expr {
                    let id_temp = Temp::new();
                    self.symbols.enter(*ident, id_temp);

                    match *ty {
                        Type::Array(_, ref len) => {
                            self.instructions.push(Instruction::Copy(HP, id_temp));

                            let temp = Temp::new();

                            self.instructions.push(Instruction::Store(
                                temp,
                                Value::Const(4 * (*len as u64), Sign::Unsigned, Size::Bit64),
                            ));

                            self.instructions
                                .push(Instruction::BinOp(BinOp::Plus, HP, temp, HP));
                        }
                        _ => self.gen_expression(expr, id_temp),
                    }
                }
            }
            t::Statement::Expr(ref expr) => self.gen_expression(expr, Temp::new()),
            t::Statement::If {
                ref cond,
                ref then,
                ref otherwise,
            } => {
                let l1 = new_named_label("if_cond", &mut self.symbols);
                let l2 = new_named_label("if_then", &mut self.symbols);

                if let Some(ref otherwise) = *otherwise {
                    let l3 = new_named_label("if_else", &mut self.symbols);

                    self.gen_cond(cond, l1, l2);

                    self.instructions.push(Instruction::Label(l1));

                    self.gen_statement(then);

                    self.instructions.push(Instruction::Jump(l3));
                    self.instructions.push(Instruction::Label(l2));
                    self.gen_statement(otherwise);

                    self.instructions.push(Instruction::Label(l3));
                } else {
                    self.gen_cond(cond, l1, l2);

                    self.instructions.push(Instruction::Label(l1));

                    self.gen_statement(then);

                    self.instructions.push(Instruction::Label(l2));
                }
            }

            t::Statement::While(ref cond, ref body) => {
                let (start, end) = new_label_pair("while_start", "while_end", &mut self.symbols);

                let lbody = new_named_label("while_cond", &mut self.symbols);
                let ltrue = new_named_label("while_true", &mut self.symbols);
                let lfalse = new_named_label("while_false", &mut self.symbols);

                self.loop_break_label = Some(end);
                self.loop_label = Some(lbody);

                self.instructions.push(Instruction::Label(start));

                self.instructions.push(Instruction::Label(lbody));

                self.gen_cond(cond, ltrue, lfalse);

                self.instructions.push(Instruction::Label(ltrue));

                self.gen_statement(body);

                self.instructions.push(Instruction::Jump(lbody));

                self.instructions.push(Instruction::Label(lfalse));

                self.instructions.push(Instruction::Label(end));
            }

            t::Statement::Return(ref expr) => {
                let temp = Temp::new();

                self.gen_expression(expr, temp);

                self.instructions.push(Instruction::Return(temp))
            }
        }
    }

    fn gen_expression(&mut self, expr: &t::TypedExpression, temp: Temp) {
        match *expr.expr {
            t::Expression::Array(ref items) => {
                let mut block = vec![];

                for item in items {
                    let temp = Temp::new();

                    self.gen_expression(item, temp);
                    block.push(temp);
                }

                self.instructions.push(Instruction::Block(temp, block))
            }
            t::Expression::Assign(ref name, ref value) => {
                let temp = self.gen_var(name);

                self.gen_expression(value, temp);

                self.instructions.push(Instruction::Copy(temp, temp))
                //  let temp = self.symbols.look(symbol)
            }
            t::Expression::Binary(ref lhs, ref op, ref rhs) => {
                let lhs_temp = Temp::new();

                let rhs_temp = Temp::new();

                match *op {
                    Op::Plus | Op::Minus | Op::Slash | Op::Star => {
                        self.gen_expression(lhs, lhs_temp);
                        self.gen_expression(rhs, rhs_temp);
                        let op = gen_bin_op(op);
                        self.instructions
                            .push(Instruction::BinOp(op, lhs_temp, rhs_temp, temp));
                    }

                    _ => {
                        let ltrue = new_named_label("ltrue", &mut self.symbols);
                        let lfalse = new_named_label("lfalse", &mut self.symbols);

                        self.gen_cond(expr, ltrue, lfalse)
                    }
                }
            }

            t::Expression::Call(ref name, ref exprs) => {
                let label = new_label(*name, &mut self.symbols);

                let mut params = vec![];

                for expr in exprs {
                    let temp = Temp::new();
                    self.gen_expression(expr, temp);
                    params.push(temp)
                }

                self.instructions
                    .push(Instruction::Call(temp, label, params))
            }

            t::Expression::Cast(ref from, _) => {
                let temp = Temp::new();
                self.gen_expression(from, temp);

                match expr.ty {
                    Type::App(TyCon::Int(sign, size), _) => {
                        self.instructions.push(Instruction::Cast(temp, sign, size))
                    }

                    _ => panic!("Can only cast to ints"),
                }
            }
            t::Expression::Grouping { ref expr } => self.gen_expression(expr, temp),
            t::Expression::Literal(ref literal) => {
                let value = match *literal {
                    Literal::Char(ref ch) => Value::Const(*ch as u64, Sign::Unsigned, Size::Bit8),

                    Literal::True(ref b) | Literal::False(ref b) => {
                        Value::Const(*b as u64, Sign::Unsigned, Size::Bit8)
                    }

                    Literal::Nil => Value::Mem(vec![0x00000000]),

                    Literal::Number(ref number) => match number.ty {
                        Some((sign, size)) => Value::Const(number.value, sign, size),
                        None => match expr.ty {
                            Type::App(TyCon::Int(sign, size), _) => {
                                Value::Const(number.value, sign, size)
                            }
                            Type::Var(_) => Value::Const(number.value, Sign::Signed, Size::Bit32),
                            _ => unreachable!(),
                        },
                    },
                    Literal::Str(ref string) => {
                        let mut bytes = vec![];
                        bytes.push(string.len() as u8);
                        bytes.extend(string.as_bytes());

                        Value::Mem(bytes)
                    }
                };

                self.instructions.push(Instruction::Store(temp, value))
            }

            t::Expression::Unary(ref op, ref expr) => {
                let new_temp = Temp::new();
                self.gen_expression(expr, new_temp);
                let op = gen_un_op(op);

                self.instructions
                    .push(Instruction::UnOp(op, temp, new_temp))
            }

            t::Expression::Var(ref var) => {
                let t = self.gen_var(var);
                self.instructions.push(Instruction::Copy(temp, t))
            }

            _ => unimplemented!(),
        }
    }

    fn gen_var(&mut self, var: &t::Var) -> Temp {
        match *var {
            t::Var::Simple(ref sym, _) => *self.symbols.look(*sym).unwrap(),

            t::Var::SubScript(ref sym, ref expr, _) => {
                let base = *self.symbols.look(*sym).unwrap();

                let addr = Temp::new();

                self.gen_expression(expr, addr);

                let temp = Temp::new();

                self.instructions.push(Instruction::Store(
                    temp,
                    Value::Const(4, Sign::Unsigned, Size::Bit64),
                ));

                self.instructions
                    .push(Instruction::BinOp(BinOp::Mul, addr, temp, addr));

                self.instructions
                    .push(Instruction::BinOp(BinOp::Plus, addr, base, addr));

                addr
            }

            _ => unimplemented!(),
        }
    }

    fn gen_cond(&mut self, cond: &t::TypedExpression, ltrue: Label, lfalse: Label) {
        match *cond.expr {
            t::Expression::Binary(ref lhs, ref op, ref rhs) => match *op {
                Op::NEq => self.gen_cond(cond, lfalse, ltrue),

                Op::And => {
                    let lnext = new_named_label("next", &mut self.symbols);

                    self.gen_cond(lhs, lnext, lfalse);

                    self.instructions.push(Instruction::Jump(lnext));

                    self.gen_cond(rhs, ltrue, lfalse);

                    self.instructions.push(Instruction::Label(lnext));
                }
                Op::Or => {
                    let lnext = new_named_label("next", &mut self.symbols);

                    self.gen_cond(lhs, ltrue, lnext);

                    self.instructions.push(Instruction::Jump(lnext));

                    self.gen_cond(rhs, ltrue, lfalse);

                    self.instructions.push(Instruction::Label(lnext));
                }

                Op::LT | Op::GT | Op::GTE | Op::LTE => {
                    let lhs_temp = Temp::new();
                    let rhs_temp = Temp::new();
                    self.gen_expression(lhs, lhs_temp);
                    self.gen_expression(rhs, rhs_temp);
                    self.instructions.push(Instruction::CJump(
                        gen_cmp_op(op),
                        lhs_temp,
                        rhs_temp,
                        ltrue,
                        lfalse,
                    ))
                }

                ref e => unreachable!("{:?}", e),
            },

            _ => self.gen_expression(cond, Temp::new()),
        }
    }
}

fn gen_bin_op(op: &Op) -> BinOp {
    match *op {
        Op::Plus => BinOp::Plus,
        Op::Minus => BinOp::Minus,
        Op::Star => BinOp::Mul,
        Op::Slash => BinOp::Div,
        Op::And => BinOp::And,
        Op::Or => BinOp::Or,
        _ => unreachable!(),
    }
}

fn gen_cmp_op(op: &Op) -> CmpOp {
    match *op {
        Op::LT => CmpOp::LT,
        Op::LTE => CmpOp::LTE,
        Op::GT => CmpOp::GT,
        Op::GTE => CmpOp::GTE,
        Op::NEq => CmpOp::NE,
        Op::Equal => CmpOp::EQ,
        _ => unreachable!(),
    }
}
fn gen_un_op(op: &UnaryOp) -> UnOp {
    match *op {
        UnaryOp::Minus => UnOp::Minus,
        UnaryOp::Bang => UnOp::Bang,
    }
}
