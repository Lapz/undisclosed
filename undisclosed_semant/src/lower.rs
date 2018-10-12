use ast::typed as t;
use ir::tac::{BinaryOp, Function, Instruction, Label, Program, Temp, UnaryOp, Value};
use std::collections::HashMap;
use syntax::ast::{Linkage, Literal, Op, Sign, Size, UnaryOp as UnOp};
use types::{TyCon, Type};
use util::symbol::{Symbol, SymbolMap};
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
    locals: HashMap<Symbol, Label>,
    parameters: HashMap<Symbol, Label>,
    instructions: Option<Vec<Instruction>>,
}

impl<'a> Builder<'a> {
    pub fn new(symbols: &'a SymbolMap<()>) -> Self {
        Builder {
            instructions: Some(vec![]),
            symbols,
            locals: HashMap::new(),
            parameters: HashMap::new(),
        }
    }

    pub fn instructions(&mut self) -> Vec<Instruction> {
        self.instructions.take().unwrap()
    }

    pub fn params(&mut self) -> Vec<Label> {
        self.parameters
            .iter()
            .map(|(_, label)| label.clone())
            .collect()
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
    pub fn add_local(&mut self, symbol: Symbol) {
        self.locals
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

            Statement::Let { ident, ty, expr } => {
                let label = Label::named(self.symbols.name(ident));
                self.add_local(ident);

                if let Some(expr) = expr {
                    let expr = self.build_expr(expr);

                    self.emit_store(Value::Name(label), expr);
                }
            }

            _ => unimplemented!(),
        }
    }

    fn build_expr(&mut self, expr: t::TypedExpression) -> Value {
        use t::Expression;

        let ty = expr.ty;
        let expr = *expr.expr;

        match expr {
            Expression::Array(items) => {
                let tmp = Temp::new();
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

                self.emit_instruction(Instruction::Array(Value::Temp(tmp), size * items.len()));

                // self.emit_instruction(Instruction::Call(
                //     Value::Temp(tmp),
                //     Label::named("malloc".to_string()),
                //     vec![Value::Const(
                //         size * (items.len() as u64),
                //         Sign::Unsigned,
                //         Size::Bit32,
                //     )],
                // )); // Al

                for (i, item) in items.into_iter().enumerate() {
                    let result = self.build_expr(item); // get the expr
                    let offset = Temp::new();

                    self.emit_instruction(Instruction::Binary(
                        offset,
                        Value::Temp(tmp),
                        BinaryOp::Plus,
                        Value::Const(i as u64, Sign::Unsigned, Size::Bit32),
                    )); // calculate the offset of the variable

                    // Store the expr in the location which holds the array index
                    self.emit_store(Value::Temp(offset), result);

                    //TODO calculate each item offset and write the value to the particular place
                }

                Value::Temp(tmp)
            }

            Expression::Assign(var, expr) => {
                let expr = self.build_expr(expr);
                let var = self.build_var(var).expect("Undefined Variable");

                self.emit_store(var.clone(), expr);

                var
            }

            Expression::Binary(lhs, op, rhs) => {
                let lhs = self.build_expr(lhs);
                let rhs = self.build_expr(rhs);
                let op = gen_bin_op(op);
                let result = Temp::new();

                self.emit_instruction(Instruction::Binary(result, lhs, op, rhs));
                Value::Temp(result)
            }

            Expression::Call(callee, exprs) => {
                let result = Temp::new();
                let label = Label::named(self.symbols.name(callee));
                let mut temps = Vec::with_capacity(exprs.len()); // Temps where the expressions are stored

                for expr in exprs {
                    temps.push(self.build_expr(expr))
                }

                self.emit_instruction(Instruction::Call(Value::Temp(result), label, temps));

                Value::Temp(result)
            }

            Expression::Grouping { expr } => self.build_expr(expr),

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
                        self.emit_store(
                            Value::Temp(tmp),
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

                        self.emit_store(Value::Temp(tmp), val)
                    }

                    Literal::Str(string) => {
                        let mut bytes = Vec::with_capacity(string.len() + 1);

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
            }

            Expression::Unary(op, expr) => {
                let dest = Temp::new();

                let rhs = self.build_expr(expr);

                let op = gen_un_op(op);

                self.emit_instruction(Instruction::Unary(Value::Temp(dest), rhs, op));

                Value::Temp(dest)
            }

            Expression::Var(var) => self.build_var(var).expect("Undefined Var"),

            ref e => unimplemented!("{:?}", e),
        }
    }

    fn build_var(&mut self, v: t::Var) -> Option<Value> {
        use t::Var;
        match v {
            Var::Simple(sym, _) => {
                if let Some(label) = self.locals.get(&sym) {
                    Some(Value::Name(label.clone()))
                } else if let Some(label) = self.parameters.get(&sym) {
                    Some(Value::Name(label.clone()))
                } else {
                    None
                }
            }

            Var::SubScript(sym, expr, ty) => {
                let label = if let Some(label) = self.locals.get(&sym) {
                    Value::Name(label.clone())
                } else if let Some(label) = self.parameters.get(&sym) {
                    Value::Name(label.clone())
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

                let offset = Temp::new();

                self.emit_instruction(Instruction::Binary(
                    offset,
                    label,
                    BinaryOp::Plus,
                    result,
                )); // Store in temp location the indexed value

                Some(Value::Temp(offset))
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
        builder.build_statement(function.body);
    }

    Function {
        name: function.name,
        params: builder.params(),
        body: builder.instructions(),
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
        Op::And => BinaryOp::And,
        Op::Or => BinaryOp::Or,
        _ => unreachable!(),
    }
}
