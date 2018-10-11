use ast::typed as t;
use ir::tac::{Instruction, Label, Temp, Value,Function,Program,BinaryOp};
use std::collections::HashMap;
use syntax::ast::{Linkage, Literal, Sign, Size,Op};
use types::{TyCon, Type};
use util::symbol::{Symbol,SymbolMap};
// struct Builder<'a> {
//     parameters: Vec<cfg::Reg>,
//     registers: HashMap<cfg::Reg, cfg::Type>,
//     blocks: HashMap<cfg::BlockId, cfg::Block>,
//     current_loop: Option<LoopDescr>,
//     ctx: &'a mut CompileCtx,
//     next_block_id: u32,
//     next_reg: u32,
//     var_registers: HashMap<Symbol, cfg::Reg>,
//     register_vars: HashMap<cfg::Reg, Symbol>,
//     current_block: Option<(cfg::BlockId, Vec<Spanned<cfg::Instruction>>)>,
//     var_mutability: HashMap<Symbol, Mut>,
// }

#[derive(Debug)]
struct Builder<'a> {
    symbols: &'a SymbolMap<()>,
    temps: Vec<()>,
    parameters: HashMap<Symbol, Label>,
    instructions: Option<Vec<Instruction>>,
}

impl<'a> Builder<'a> {
    pub fn new(symbols: &'a SymbolMap<()>) -> Self {
        Builder {
            instructions: Some(vec![]),
            symbols,
            temps: Vec::new(),
            parameters: HashMap::new(),
        }
    }

    pub fn instructions(&mut self) -> Vec<Instruction> {
        self.instructions.take().unwrap()
    }

    pub fn params(&mut self) -> Vec<Label> {
        self.parameters.iter().map(|(_,label)| label.clone()).collect()
    }
    pub fn emit_instruction(&mut self, inst: Instruction) {
        self.instructions.as_mut().unwrap().push(inst);
    }

    pub fn emit_store(&mut self, dest: Value, source: Value) {
        self.instructions
            .as_mut()
            .unwrap()
            .push(Instruction::Store(dest, source));
    }

    pub fn add_param(&mut self, symbol: Symbol) {
        self.parameters
            .insert(symbol, Label::named(self.symbols.name(symbol)));
    }

    pub fn build_statement(&mut self, s: t::Statement) {
        use t::Statement;
        match s {
            Statement::Block(statements) => {
                for statement in statements {
                    self.emit_instruction(Instruction::StatementStart);
                    self.build_statement(statement)
                }
            }

            Statement::Expr(expr) => {
                self.build_expr(expr);
            }

            _ => unimplemented!(),
        }
    }

    fn build_expr(&mut self, expr: t::TypedExpression) -> Value {
        use t::Expression;

        let ty = expr.ty;
        let expr = *expr.expr;
    
        match expr {
            Expression::Literal(literal) => {
                let tmp = Temp::new();
                match literal {
                    Literal::Char(ch) => {
                        
                        self.emit_store(
                            Value::Temp(tmp),
                            Value::Const(ch as u64, Sign::Unsigned, Size::Bit8),
                        );
                    }
                    Literal::False(_) => {
                    
                        self.emit_store(
                            Value::Temp(tmp),
                            Value::Const(0, Sign::Unsigned, Size::Bit8),
                        );
                    }
                    Literal::Nil => {
                        self.emit_store(Value::Temp(tmp),Value::Const(0x0000, Sign::Unsigned, Size::Bit8));
                    }
                    Literal::Number(number) => {
                        let val = match ty {
                            Type::App(TyCon::Int(sign, size), _) => {
                                Value::Const(number.value, sign, size)
                            }
                            Type::Var(_) => Value::Const(number.value, Sign::Signed, Size::Bit32),

                            _ => unreachable!(),
                        };

                        self.emit_store(Value::Temp(tmp), val)
                    },

                    Literal::Str(string) => {
                        let mut bytes = Vec::with_capacity(string.len() +1);

                        bytes.extend(string.as_bytes());
                        bytes.push(b'\0');

                        self.emit_store(Value::Temp(tmp), Value::Mem(bytes));
                    }

                    Literal::True(_) => {
                    
                        self.emit_store(
                            Value::Temp(tmp),
                            Value::Const(1, Sign::Unsigned, Size::Bit8),
                        );
                    }
                };

               Value::Temp(tmp)
            },

            Expression::Binary(lhs,op,rhs) => {

                let lhs = self.build_expr(lhs);
                let rhs = self.build_expr(rhs);
                let op = gen_bin_op(op);
                let result = Temp::new();

                self.emit_instruction(Instruction::Binary(result,lhs,op,rhs));
                Value::Temp(result)
            }

            _ => unimplemented!(),
        }

        
    }
}

fn build_function(function: t::Function, symbols: &SymbolMap<()>)  -> Function {
    let mut builder = Builder::new(symbols);

    for param in function.params {
        builder.add_param(param.name);
    }

    if function.linkage == Linkage::External {

    } else {
        builder.build_statement(function.body);
    };

    Function {
        name:function.name,
        params:builder.params(),
        body:builder.instructions(),
        linkage:function.linkage
    }
}

pub fn build_program(symbols: &SymbolMap<()>,old_program:t::Program)  -> Program {
    let mut new_program = Program {
        functions:vec![]
    };

    for function in old_program.functions {
        new_program.functions.push(build_function(function,symbols));
    }

    new_program
}


fn gen_bin_op(op: Op) -> BinaryOp {
    match op {
        Op::Plus => BinaryOp::Plus,
        Op::Minus => BinaryOp::Minus,
        Op::Star => BinaryOp::Mul,
        Op::Slash => BinaryOp::Div,
        Op::And => BinaryOp::And,
        Op::Or => BinaryOp::Or,
        _ => unreachable!(),
    }
}