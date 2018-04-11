use ir::*;
use util::pos::Spanned;
use temp::Temp;
use util::symbol::{Symbol, Symbols};
use syntax::ast::{Program,Statement,Function,StructLit,Call,Var,Expression,Literal,Number,Op,UnaryOp};

#[derive(Debug)]
pub struct CodeGen<'a> {
    instructions:Vec<Instruction>,
    symbols:&'a mut Symbols<Temp>,
}

enum CodeGenResult {
    Val(Value),
    Instruction(Instruction),
    None
}




impl <'a> CodeGen<'a> {
    pub fn new(symbols:&'a mut Symbols<Temp>) -> Self {
        Self {
            instructions:vec![],
            symbols
        }
    }

    pub fn dump_to_file(&self,path:String) {
        use std::io::{Write};
        use std::fs::File;

    let mut file = File::create(path).expect("Couldn't create file");

    for instruction in &self.instructions {
    file.write(format!("{}", instruction).as_bytes())
                    .expect("Couldn't write to the file");
    }
               
    }


    pub fn gen_program(&mut self, prog:&Program) {
        for function in &prog.functions {
            self.gen_function(function)
        }
    }

    pub fn gen_function(&mut self,func:&Spanned<Function>) {
        self.gen_statement(&func.value.body)
    }

    fn gen_statement(&mut self, statement:&Spanned<Statement>) {

        match statement.value {
            Statement::Block(ref statements) => {
                
                for statement in statements {
                    self.gen_statement(statement)
                }

            }
            Statement::Let{ref ident,ref ty,ref expr,ref escapes} => {

                if let Some(ref expr) = *expr {
                    let id_temp = Temp::new();

                    self.symbols.enter(ident.value, id_temp);
                    self.gen_expression(expr,id_temp);

                    
                }
                

            },
            Statement::Expr(ref expr) => {
            self.gen_expression(expr,Temp::new());
             
            }
            ref e => unimplemented!("{:?}",e)
        }

    }

    fn gen_expression(&mut self,expr:&Spanned<Expression>,temp:Temp) {
        match expr.value {
            Expression::Assign {
                ref value,
                ref name,
            } => {

            let temp = self.symbols.look(self.gen_var(name)).unwrap().clone();
            self.gen_expression(value,temp);

           
           
            },
            Expression::Binary{ref lhs,ref op,ref rhs} => {

                let lhs_temp = Temp::new();

                let rhs_temp = Temp::new();


                self.gen_expression(lhs,lhs_temp);

                self.gen_expression(rhs,rhs_temp);

                let op = self.gen_bin_op(op);

                self.instructions.push(Instruction::BinOp(op,lhs_temp ,rhs_temp,temp))
            }
            Expression::Literal(ref literal) => {
                let value = match *literal {
                Literal::Char(ref ch) => {
                   Value::Mem(vec![*ch as u8])
                    
                },

                Literal::True(ref b) | Literal::False(ref b) => {
                    Value::Mem(vec![*b as u8])
                },

                Literal::Nil => {
                     Value::Mem(vec![])
                },

                Literal::Number(ref number) =>Value::Const(number.value)
                ,
                Literal::Str(ref string) => {
                     Value::Mem(string.as_bytes().into())
                }

            };

            self.instructions.push(Instruction::Store(temp,value))
            },

            Expression::Unary{ref op,ref expr } => {

                let new_temp = Temp::new();
                self.gen_expression(expr, new_temp);
                let op = self.gen_un_op(op);

                self.instructions.push(Instruction::UnOp(op,temp,new_temp))
            }
            Expression::Var(ref var) => {
                let symbol = self.gen_var(var);

                self.instructions.push(Instruction::Copy(temp,self.symbols.look(symbol).unwrap().clone()))
            }

            ref e_ => unimplemented!("{:?}",e_)
        }
    }

    fn gen_bin_op(&self,op:&Spanned<Op>) -> BinOp {
        match op.value {
            Op::Plus => BinOp::Plus,
            Op::Minus => BinOp::Minus,
            Op::Star => BinOp::Minus,
            Op::Slash => BinOp::Div,
            Op::And => BinOp::And,
            Op::Or => BinOp::Or,
            _ => unreachable!()
        }
    }

    fn gen_un_op(&self,op:&Spanned<UnaryOp>) -> UnOp {
        match op.value {
            UnaryOp::Minus => UnOp::Minus,
            UnaryOp::Bang => UnOp::Bang,
        }
    }

    fn gen_var(&self,var:&Spanned<Var>) ->  Symbol {
        match var.value {
            Var::Simple(ref symbol) => symbol.value,

            _ => unimplemented!()
        }
    }
}