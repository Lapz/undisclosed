use codegen::{temp::Temp,ir::*};
use std::mem;
use ast;
use util::symbol::{Symbols};
use syntax::ast::{Literal,Sign,Size,Op,UnaryOp};
use types::{TyCon,Type};
use std::u64;
#[derive(Debug)]
pub struct Codegen {
    instructions: Vec<Instruction>,
    symbols: Symbols<Temp>,
}


impl Codegen {

    pub fn new(symbols:Symbols<Temp>)  -> Self {
        Self {
            symbols,
            instructions:vec![]
        }
    }

   pub fn dump_to_file(&self, path: String) {
        use std::fs::File;
        use std::io::Write;

        let mut file = File::create(path).expect("Couldn't create file");

        for instruction in &self.instructions {
            file.write(format!("{}", instruction).as_bytes())
                .expect("Couldn't write to the file");
        }

        file.write(format!("\n{:?}", self.instructions).as_bytes())
            .expect("Couldn't write to the file");
    }

    pub fn gen_program(&mut self,program: &ast::Program, ) {
        for function in &program.functions {
            self.gen_statement(&function.body)
        }
    }

    fn gen_statement(&mut self,statement: &ast::Statement) {
        match statement {
            ast::Statement::Block(ref statements) => {
                for statement in statements {
                    self.gen_statement(statement)
                }
            }
            ast::Statement::Expr(ref expr) => {
                self.gen_expression(expr,Temp::new())
            },
            _ => unimplemented!()
        }
    }

     fn gen_expression(&mut self,expr: &ast::TypedExpression, temp: Temp) {
         match *expr.expr {
             ast::Expression::Binary(ref lhs,ref op,ref rhs) => {
                  let lhs_temp = Temp::new();

                let rhs_temp = Temp::new();

                self.gen_expression(lhs, lhs_temp);
                self.gen_expression(rhs, rhs_temp);

                let op = gen_bin_op(op);

                self.instructions.push(Instruction::BinOp(op,lhs_temp,rhs_temp,temp));
             }
             ast::Expression::Literal(ref literal) => {
                 let value = match *literal {
                    Literal::Char(ref ch) => {
                        Value::Const(*ch as u64,Sign::Unsigned,Size::Bit8)
                    }

                    Literal::True(ref b) | Literal::False(ref b) => {
                        Value::Const(*b as u64,Sign::Unsigned,Size::Bit8)
                    }

                    Literal::Nil => Value::Mem(vec![]),

                    Literal::Number(ref number) => match number.ty {
                        Some((sign, size)) => Value::Const(number.value, sign,size),
                        None => {
                            match expr.ty {
                                Type::App(TyCon::Int(sign,size),_) => {
                                    Value::Const(number.value, sign,size)
                                },
                                Type::Var(_) => Value::Const(number.value,Sign::Signed,Size::Bit32),
                                _ => unreachable!()
                            }
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

             },

             ast::Expression::Unary (ref op, ref expr ) => {
                let new_temp = Temp::new();
                self.gen_expression(expr, new_temp);
                let op = gen_un_op(op);

                self.instructions.push(Instruction::UnOp(op, temp, new_temp))
            }

             _ => unimplemented!()
         }
     }
      
}


 fn gen_bin_op(op: &Op) -> BinOp {
        match *op {
            Op::Plus => BinOp::Plus,
            Op::Minus => BinOp::Minus,
            Op::Star => BinOp::Minus,
            Op::Slash => BinOp::Div,
            Op::And => BinOp::And,
            Op::Or => BinOp::Or,
            _ => unreachable!(),
        }
    }

fn gen_un_op(op: &UnaryOp) -> UnOp {
        match *op {
            UnaryOp::Minus => UnOp::Minus,
            UnaryOp::Bang => UnOp::Bang,
        }
    }