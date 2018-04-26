use ir::*;
use std::mem;
use syntax::ast::{Call, Expression, Function, Literal, Op, Program, Size, Statement, StructLit,
                  Ty, UnaryOp, Var};
use temp::Temp;
use util::pos::Spanned;
use util::symbol::{Symbol, Symbols};

#[derive(Debug)]
pub struct Ctx {
    instructions: Vec<Instruction>,
    symbols: Symbols<Temp>,
    pub emit_ir: bool,
}

impl Ctx {
    pub fn new(symbols: Symbols<Temp>) -> Self {
        Self {
            instructions: vec![],
            symbols,
            emit_ir: true,
        }
    }

    pub fn dump_to_file(&self, path: String) {
        use std::fs::File;
        use std::io::Write;

        let mut file = File::create(path).expect("Couldn't create file");

        for instruction in &self.instructions {
            // file.write(ins.as_bytes())
            // .expect("Couldn't write to the file");
        }

        file.write(format!("\n{:?}", self.instructions).as_bytes())
            .expect("Couldn't write to the file");
    }
}

pub struct CodeGen {}

impl CodeGen {
    pub fn gen_program(program: &Program, ctx: &mut Ctx) {
        for function in &program.functions {
            Self::gen_statement(&function.value.body, ctx)
        }
    }
    fn gen_statement(statement: &Spanned<Statement>, ctx: &mut Ctx) {
        match statement.value {
            Statement::Block(ref statements) => for statement in statements {
                Self::gen_statement(statement, ctx)
            },
            Statement::Let {
                ref ident,
                ref ty,
                ref expr,
                ref escapes,
            } => {
                if let Some(ref expr) = *expr {
                    let id_temp = Temp::new();

                    ctx.symbols.enter(ident.value, id_temp);
                    Self::gen_expression(expr, ctx, id_temp);
                }
            }
            Statement::Expr(ref expr) => {
                Self::gen_expression(expr, ctx, Temp::new());
            }
            ref e => unimplemented!("{:?}", e),
        }
    }

    fn gen_expression<'a>(expr: &Spanned<Expression>, ctx: &mut Ctx, temp: Temp) {
        match expr.value {
            Expression::Assign {
                ref value,
                ref name,
            } => {
                let temp = ctx.symbols.look(Self::gen_var(name)).unwrap().clone();
                Self::gen_expression(value, ctx, temp);
            }
            Expression::Binary {
                ref lhs,
                ref op,
                ref rhs,
            } => {
                let lhs_temp = Temp::new();

                let rhs_temp = Temp::new();

                Self::gen_expression(lhs, ctx, lhs_temp);

                Self::gen_expression(rhs, ctx, rhs_temp);

                let op = Self::gen_bin_op(op);

                ctx.instructions
                    .push(Instruction::BinOp(op, lhs_temp, rhs_temp, temp))
            }
            Expression::Cast { ref from, ref to } => {
                let temp = Temp::new();

                Self::gen_expression(from, ctx, temp);

                // ctx.instructions
                // .push(Instruction::Cast(temp, Self::get_size(to)))
            }
            Expression::Literal(ref literal) => {
                let value = match *literal {
                    Literal::Char(ref ch) => {
                        Value::Mem(vec![mem::size_of::<char>() as u8, *ch as u8])
                    }

                    Literal::True(ref b) | Literal::False(ref b) => {
                        Value::Mem(vec![mem::size_of::<bool>() as u8, *b as u8])
                    }

                    Literal::Nil => Value::Mem(vec![]),

                    Literal::Number(ref number) => unimplemented!(),
                    Literal::Str(ref string) => {
                        let mut bytes = vec![];
                        bytes.push(string.len() as u8);
                        bytes.extend(string.as_bytes());

                        Value::Mem(bytes)
                    }
                };

                ctx.instructions.push(Instruction::Store(temp, value))
            }

            Expression::Unary { ref op, ref expr } => {
                let new_temp = Temp::new();
                Self::gen_expression(expr, ctx, new_temp);
                let op = Self::gen_un_op(op);

                ctx.instructions.push(Instruction::UnOp(op, temp, new_temp))
            }
            Expression::Var(ref var) => {
                let symbol = Self::gen_var(var);

                ctx.instructions.push(Instruction::Copy(
                    temp,
                    ctx.symbols.look(symbol).unwrap().clone(),
                ))
            }

            ref e_ => unimplemented!("{:?}", e_),
        }
    }

    fn gen_bin_op(op: &Spanned<Op>) -> BinOp {
        match op.value {
            Op::Plus => BinOp::Plus,
            Op::Minus => BinOp::Minus,
            Op::Star => BinOp::Minus,
            Op::Slash => BinOp::Div,
            Op::And => BinOp::And,
            Op::Or => BinOp::Or,
            _ => unreachable!(),
        }
    }

    fn gen_un_op(op: &Spanned<UnaryOp>) -> UnOp {
        match op.value {
            UnaryOp::Minus => UnOp::Minus,
            UnaryOp::Bang => UnOp::Bang,
        }
    }

    fn gen_var(var: &Spanned<Var>) -> Symbol {
        match var.value {
            Var::Simple(ref symbol) => symbol.value,

            _ => unimplemented!(),
        }
    }

    fn get_size(ty: &Spanned<Ty>) -> usize {
        match ty.value {
            Ty::I8 => 1,
            Ty::I32 => 4,
            Ty::I64 => 8,
            Ty::U8 => 1,
            Ty::U32 => 4,
            Ty::U64 => 8,
            Ty::Bool => 1,
            _ => unreachable!(), // cast check
        }
    }
}
