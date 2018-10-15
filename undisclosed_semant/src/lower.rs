use ast::typed as t;
use ir::tac::{BinaryOp, Function, Instruction, Label, Program, Register, UnaryOp, Value};
use std::collections::HashMap;
use syntax::ast::{Linkage, Literal, Op, Sign, Size, UnaryOp as UnOp};
use types::{TyCon, Type};
use util::symbol::{Symbol, SymbolMap};

#[derive(Debug, Clone)]
pub struct LoopDescription {
    start:(BlockID,Label),
    end: (BlockID,Label),
}

#[derive(Debug)]
struct Builder<'a> {
    symbols: &'a SymbolMap<()>,
    locals: HashMap<Symbol, Register>,
    parameters: HashMap<Symbol, Register>,
    instructions: Option<Vec<Instruction>>,
    current_loop: Option<LoopDescription>,
    blocks: HashMap<BlockID, Block>,
    current_block: Option<(BlockID, Vec<Instruction>)>,
}

impl<'a> Builder<'a> {
    pub fn new(symbols: &'a SymbolMap<()>) -> Self {
        Builder {
            instructions: Some(vec![]),
            symbols,
            locals: HashMap::new(),
            parameters: HashMap::new(),
            current_loop: None,
            current_block: None,
            blocks: HashMap::new(),
        }
    }

    pub fn instructions(&mut self) -> Vec<Instruction> {
        self.instructions.take().unwrap()
    }
    
    pub fn blocks(self) -> HashMap<BlockID,Block> {
        self.blocks
    }

    pub fn new_block(&mut self) -> BlockID {
        BlockID::new()
    }

    pub fn start_block(&mut self,id:BlockID) {

        if self.current_block.is_some() {
            panic!("Block is unfinished");
        }
        
        self.current_block = Some((id,Vec::new()));
    }

    pub fn end_block(&mut self,end:BlockEnd) {
        
        let (id,inst) = self.current_block.take().unwrap();

        self.blocks.insert(id, Block {
            instructions:inst,
            end
        });
    }

    pub fn params(&mut self) -> Vec<Register> {
        self.parameters
            .iter()
            .map(|(_, label)| label.clone())
            .collect()
    }
    pub fn emit_instruction(&mut self, inst: Instruction) {
        self.current_block
            .as_mut()
            .unwrap()
            .1
            .push(inst);
    }

    pub fn emit_store(&mut self, dest: Value, source: Value) {
        self.current_block
            .as_mut()
            .unwrap()
            .1
            .push(Instruction::Store(dest, source));
    }

    pub fn add_param(&mut self, symbol: Symbol) {
        self.parameters.insert(symbol, Register::new());
    }
    pub fn add_local(&mut self, symbol: Symbol) -> Register {
        let reg = Register::new();
        self.locals.insert(symbol, reg);

        reg
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

            Statement::Break => {
                let description = self
                    .current_loop
                    .take()
                    .expect("Using break outside a loop");

                let new = self.new_block();

                self.start_block(new);

                self.emit_instruction(Instruction::Jump(description.end.1.clone()));

                self.end_block(BlockEnd::Jump(description.end.0));

                self.current_loop = Some(description);
            
            }

            Statement::Continue => {
                let description = self
                    .current_loop
                    .take()
                    .expect("Using break outside a loop");
                let label = description.start.clone();

                let new = self.new_block();

                self.start_block(new);

                self.emit_instruction(Instruction::Jump(description.start.1.clone()));

                self.end_block(BlockEnd::Jump(description.start.0));

                self.current_loop = Some(description);
            }

            Statement::Expr(expr) => {
                
                self.build_expr(expr);
            }

            Statement::If {
                cond,
                then,
                otherwise: Some(otherwise),
            } => {
                let then_label = Label::new();
                let otherwise_label = Label::new();

                let result = self.build_expr(cond);

                self.emit_instruction(Instruction::JumpIf(result, otherwise_label.clone()));

                self.emit_instruction(Instruction::Label(then_label));

                self.build_statement(*then);

                self.emit_instruction(Instruction::Label(otherwise_label));

                self.build_statement(*otherwise)
            }

            Statement::If {
                cond,
                then,
                otherwise: None,
            } => {
                let then_label = Label::new();
                let otherwise_label = Label::new();

                let result = self.build_expr(cond);

                self.emit_instruction(Instruction::JumpIf(result, otherwise_label.clone()));

                self.emit_instruction(Instruction::Label(then_label));

                self.build_statement(*then);

                self.emit_instruction(Instruction::Label(otherwise_label));
            }

            Statement::Let { ident, ty, expr } => {
                let label = self.add_local(ident);

                if let Some(expr) = expr {
                    let expr = self.build_expr(expr);
                    self.emit_store(Value::Name(ident), expr);
                }
            }

            Statement::Return(expr) => {
                let result = self.build_expr(expr);
                let new = self.new_block();

                // Store in into return register

                self.emit_instruction(Instruction::Return(result));
                self.end_block(BlockEnd::Return(result));

                self.start_block(new);
            }

            Statement::While(cond, body) => {
                let cond = self.build_expr(cond);

                let test = Label::new(); // The labels of each block
                let done = Label::new(); // The labels of each block
                
                let start = self.new_block();
                let cond_id = self.new_block();
                let end = self.new_block();

                self.current_loop = Some(LoopDescription {
                    start: (start,test.clone()),
                    end: (end,done.clone()),
                });

                self.end_block(BlockEnd::Jump(start));

                self.start_block(start);
                
                self.emit_instruction(Instruction::Label(test.clone()));

                self.build_statement(*body);

                self.end_block(BlockEnd::Jump(cond_id));

                self.start_block(cond_id);

                self.emit_instruction(Instruction::JumpIf(cond, test.clone()));

                self.emit_instruction(Instruction::Label(done));

            }
        }
    }

    fn build_expr(&mut self, expr: t::TypedExpression) -> Value {
        use t::Expression;

        let ty = expr.ty;
        let expr = *expr.expr;

        match expr {
            Expression::Array(items) => {
                let tmp = Register::new();
                let size = if items.is_empty() {
                    0
                } else {
                    match &items[0].ty {
                        Type::App(TyCon::Int(sign, size), _) => match size {
                            Size::Bit8 => 1,
                            Size::Bit32 => 4,
                            Size::Bit64 => 8,
                        },
                        Type::App(TyCon::String, _) => 1,
                        Type::Var(_) => 4,
                        ref ty => unimplemented!("Unknown ty {:?}", ty),
                    }
                }; // Calculate the size of the elements

                self.emit_instruction(Instruction::Array(Value::Register(tmp), size * items.len()));

                // self.emit_instruction(Instruction::Call(
                //     Value::Register(tmp),
                //     Label::named("malloc".to_string()),
                //     vec![Value::Const(
                //         size * (items.len() as u64),
                //         Sign::Unsigned,
                //         Size::Bit32,
                //     )],
                // )); // Al

                for (i, item) in items.into_iter().enumerate() {
                    let result = self.build_expr(item); // get the expr
                    let offset = Register::new();

                    self.emit_instruction(Instruction::Binary(
                        offset,
                        Value::Register(tmp),
                        BinaryOp::Plus,
                        Value::Const(i as u64, Sign::Unsigned, Size::Bit32),
                    )); // calculate the offset of the variable

                    // Store the expr in the location which holds the array index
                    self.emit_store(Value::Register(offset), result);

                    //TODO calculate each item offset and write the value to the particular place
                }

                Value::Register(tmp)
            }

            Expression::Assign(var, expr) => {
                let expr = self.build_expr(expr);
                let var = self.build_var(var).expect("Undefined Variable");

                self.emit_store(var.clone(), expr);

                var
            }

            Expression::Binary(lhs, op, rhs) => match op {
                Op::And => self.build_and(lhs, rhs),
                Op::Or => self.build_or(lhs, rhs),
                _ => {
                    let lhs = self.build_expr(lhs);
                    let rhs = self.build_expr(rhs);

                    let op = gen_bin_op(op);
                    let result = Register::new();

                    self.emit_instruction(Instruction::Binary(result, lhs, op, rhs));
                    Value::Register(result)
                }
            },

            Expression::Call(callee, exprs) => {
                let result = Register::new();
                let ident = Value::Name(callee);
                let mut temps = Vec::with_capacity(exprs.len()); // Temps where the expressions are stored

                for expr in exprs {
                    temps.push(self.build_expr(expr))
                }

                self.emit_instruction(Instruction::Call(Value::Register(result), ident, temps));

                Value::Register(result)
            }

            Expression::Cast(expr, ty) => {
                let result = self.build_expr(expr);

                let (sign, size) = match ty {
                    Type::App(TyCon::Int(sign, size), _) => (sign, size),
                    Type::Var(_) => (Sign::Signed, Size::Bit32),

                    Type::App(TyCon::Bool, _) => (Sign::Unsigned, Size::Bit8),

                    Type::App(TyCon::Char, _) => (Sign::Unsigned, Size::Bit8),

                    _ => unreachable!(), // Check cast_check for which types are possible
                };

                self.emit_instruction(Instruction::Cast(result.clone(), sign, size));

                result
            }

            Expression::Grouping { expr } => self.build_expr(expr),

            Expression::Literal(literal) => {
                let tmp = Register::new();
                match literal {
                    Literal::Char(ch) => {
                        self.emit_store(
                            Value::Register(tmp),
                            Value::Const(ch as u64, Sign::Unsigned, Size::Bit8),
                        );
                    }
                    Literal::False(_) => {
                        self.emit_store(
                            Value::Register(tmp),
                            Value::Const(0, Sign::Unsigned, Size::Bit8),
                        );
                    }
                    Literal::Nil => {
                        self.emit_store(
                            Value::Register(tmp),
                            Value::Const(0x0000, Sign::Unsigned, Size::Bit8),
                        );
                    }
                    Literal::Number(number) => {
                        let val = match ty {
                            Type::App(TyCon::Int(sign, size), _) => {
                                Value::Const(number.value, sign, size)
                            }
                            Type::Var(_) => Value::Const(number.value, Sign::Signed, Size::Bit32),

                            _ => unreachable!(),
                        };

                        self.emit_store(Value::Register(tmp), val)
                    }

                    Literal::Str(string) => {
                        let mut bytes = Vec::with_capacity(string.len() + 1);

                        bytes.extend(string.as_bytes());
                        bytes.push(b'\0');

                        self.emit_store(Value::Register(tmp), Value::Mem(bytes));
                    }

                    Literal::True(_) => {
                        self.emit_store(
                            Value::Register(tmp),
                            Value::Const(1, Sign::Unsigned, Size::Bit8),
                        );
                    }
                };

                Value::Register(tmp)
            }

            Expression::Unary(op, expr) => {
                let dest = Register::new();

                let rhs = self.build_expr(expr);

                let op = gen_un_op(op);

                self.emit_instruction(Instruction::Unary(Value::Register(dest), rhs, op));

                Value::Register(dest)
            }

            Expression::Var(var) => self.build_var(var).expect("Undefined Var"),

            ref e => unimplemented!("{:?}", e),
        }
    }

    fn build_and(&mut self, l: t::TypedExpression, r: t::TypedExpression) -> Value {
        let f_label = Label::new();

        let result = Register::new();

        let lhs = self.build_expr(l);

        self.emit_store(Value::Register(result), lhs.clone());

        self.emit_instruction(Instruction::JumpNot(lhs, f_label));

        let rhs = self.build_expr(r);

        self.emit_store(Value::Register(result), rhs);

        self.emit_instruction(Instruction::Label(f_label));

        Value::Register(result)
    }

    fn build_or(&mut self, l: t::TypedExpression, r: t::TypedExpression) -> Value {
        let t_label = Label::new();

        let result = Register::new();

        let lhs = self.build_expr(l);

        self.emit_store(Value::Register(result), lhs.clone());

        self.emit_instruction(Instruction::JumpIf(lhs, t_label));

        let rhs = self.build_expr(r);

        self.emit_store(Value::Register(result), rhs);

        self.emit_instruction(Instruction::Label(t_label));

        Value::Register(result)
    }

    fn build_var(&mut self, v: t::Var) -> Option<Value> {
        use t::Var;
        match v {
            Var::Simple(sym, _) => {
                if let Some(label) = self.locals.get(&sym) {
                    Some(Value::Name(sym))
                } else if let Some(label) = self.parameters.get(&sym) {
                    Some(Value::Name(sym))
                } else {
                    None
                }
            }

            Var::SubScript(sym, expr, ty) => {
                let label = if let Some(label) = self.locals.get(&sym) {
                    Value::Name(sym)
                } else if let Some(label) = self.parameters.get(&sym) {
                    Value::Name(sym)
                } else {
                    return None;
                }; // get the variable

                let size = match ty {
                    Type::App(TyCon::Int(sign, size), _) => match size {
                        Size::Bit8 => 1,
                        Size::Bit32 => 4,
                        Size::Bit64 => 8,
                    },
                    Type::App(TyCon::String, _) => 1,
                    Type::Var(_) => 4,
                    Type::Nil => 0,
                    ref ty => unimplemented!("Unknown ty {:?}", ty),
                }; // Work out the size

                let result = self.build_expr(expr); // Build the index expr

                let offset = Register::new();

                self.emit_instruction(Instruction::Binary(offset, label, BinaryOp::Plus, result)); // Store in temp location the indexed value

                Some(Value::Register(offset))
            }

            _ => unimplemented!(),
        }
    }
}

fn build_function(function: t::Function, symbols: &SymbolMap<()>) -> Function {
    let mut builder = Builder::new(symbols);

    for param in function.params {
        builder.add_param(param.name);
    }

    if function.linkage == Linkage::Normal {
        let start = builder.new_block();

        builder.start_block(start);
        builder.build_statement(function.body);
        // builder.end_block(BlockEnd::End);

        
    }

    

    Function {
        name: function.name,
        params: builder.params(),
        body: builder.instructions(),
        blocks: builder.blocks(),
        linkage: function.linkage,
    }
}

pub fn build_program(symbols: &SymbolMap<()>, old_program: t::Program) -> Program {
    let mut new_program = Program { functions: vec![] };

    for function in old_program.functions {
        new_program
            .functions
            .push(build_function(function, symbols));
    }

    new_program
}

fn gen_un_op(op: UnOp) -> UnaryOp {
    match op {
        UnOp::Minus => UnaryOp::Minus,
        UnOp::Bang => UnaryOp::Bang,
    }
}

fn gen_bin_op(op: Op) -> BinaryOp {
    match op {
        Op::Plus => BinaryOp::Plus,
        Op::Minus => BinaryOp::Minus,
        Op::Star => BinaryOp::Mul,
        Op::Slash => BinaryOp::Div,
        Op::LT => BinaryOp::Lt,
        Op::GT => BinaryOp::Gt,
        Op::LTE => BinaryOp::Lte,
        Op::GTE => BinaryOp::Gte,
        Op::Equal => BinaryOp::Equal,
        Op::NEq => BinaryOp::NotEqual,
        // Op::And => BinaryOp::And,
        // Op::Or => BinaryOp::Or,
        _ => unreachable!(),
    }
}
