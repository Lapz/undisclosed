use ir;
use ir::ir::Value;
use std::collections::HashMap;
use std::fs::File;
use std::io::Write;

pub struct Compiler {
    file: File,
    strings: Vec<Vec<u8>>,
}

impl Compiler {
    pub fn new() -> Compiler {
        let mut file = File::create("out.s").expect("\tCouldn't create the file");

        file.write_all(b".text \n\t\t.global _main\n_main:\n")
            .unwrap();

        Compiler {
            file,
            strings: vec![],
        }
    }

    pub fn write(&mut self, code: &str) {
        self.file.write(code.as_bytes()).unwrap();
    }

    pub fn compile(&mut self, program: ir::ir::Program) {
        for function in program.functions.iter() {
            self.compile_function(function);
        }
    }

    pub fn compile_function(&mut self, function: &ir::ir::Function) {
        self.write(&format!("\n{}:\n", function.name));
        self.emit_func_prologue(function.locals.len());

        let locals = &function.locals;

        for instruction in function.body.iter() {
            println!("{:?}", instruction);
            self.compile_instruction(instruction, locals);
        }

        self.emit_func_epilogue();
    }

    pub fn emit_func_prologue(&mut self, nparams: usize) {
        if nparams == 0 {
            self.write("\tpushq %rbp\n\tmovq %rsp, %rbp  #pro\n")
        } else {
            self.write("\tpushq %rbp\n\tmovq %rsp, %rbp\n\tmovl %edi, -20(%rbp) #pro\n")
        }
    }

    pub fn emit_func_epilogue(&mut self) {
        self.write("\tmovq %rbp, %rsp #epi\n\tpopq %rbp  \n\tret")
    }

    pub fn compile_instruction(
        &mut self,
        instruction: &ir::ir::Instruction,
        locals: &HashMap<ir::Temp, i32>,
    ) {
        use ir::ir::Instruction;

        match *instruction {
            Instruction::Label(ref label) => self.write(&format!(".{}:\n", label)),
            Instruction::Store(ref temp, ref value) => {
                if let Some(ref offset) = locals.get(temp) {
                    self.compile_value(value, locals);
                    write!(&mut self.file, "\tmovq %rax, {}(%rbp)\n", offset).unwrap();
                } else {
                    self.compile_value(value, locals);
                }
            }
            Instruction::BinOp(_, ref op, ref v1, ref v2) => {
                use ir::ir::BinOp;
                match *op {
                    BinOp::Plus => {
                        self.compile_instruction(v1, locals);
                        self.write("\tpushq %rax\n");
                        self.compile_instruction(v2, locals);
                        self.write("\tpopq %rdx\n");
                        self.write("\taddq %rdx,%rax\n")
                    }

                    BinOp::Minus => {
                        self.compile_instruction(v1, locals);
                        self.write("\tpushq %rax\n");
                        self.compile_instruction(v2, locals);
                        self.write("\tpopq %rdx\n");
                        self.write("\tsubq %rdx,%rax\n")
                    }
                    BinOp::Mul => {
                        self.compile_instruction(v1, locals);
                        self.write("\tpushq %rax\n");
                        self.compile_instruction(v2, locals);
                        self.write("\tpopq %rdx\n");
                        self.write("\timulq %rdx,%rax\n")
                    }
                    BinOp::Div => {
                        self.compile_instruction(v1, locals);
                        self.write("\tpushq %rax\n");
                        self.compile_instruction(v2, locals);
                        self.write("\tpopq %rdx\n");
                        self.write("\tmovq %rdx, %rbx\n");
                        self.write("\tcqto\n");
                        self.write("\tidivq %rbx\n");
                    }

                    BinOp::And => {
                        self.compile_instruction(v1, locals);
                        self.write("\tpushq %rax\n");
                        self.compile_instruction(v2, locals);
                        self.write("\tpopq %rdx\n");
                        self.write("\tcmpq $0, %rax \n");
                        self.write("\tmovq $0, %rax #zero out EAX without changing ZF \n ");
                        self.write("\tsetne %cl\n");
                        self.write("\tcmpq $0, %rdx \n");
                        self.write("\tmovq $0, %rdx #zero out EAX without changing ZF \n ");
                        self.write("\tsetne %al\n");
                        self.write("\tandb %cl,%al\n")
                    }

                    BinOp::Or => {
                        self.compile_instruction(v1, locals);
                        self.write("\tpushq %rax\n");
                        self.compile_instruction(v2, locals);
                        self.write("\tpopq %rdx\n");
                        self.write("\torq %rdx, %rax #compute e1 | e2, set ZF \n ");
                        self.write("\tmovq $0, %rax #zero out EAX without changing ZF \n ");
                        self.write("\tsetne %al #set AL register (the lower byte of EAX) to 1 iff e1 | e2 != 0 \n ");
                    }

                    BinOp::EQ => {
                        self.compile_instruction(v1, locals);
                        self.write("\tpushq %rax\n");
                        self.compile_instruction(v2, locals);
                        self.write("\tpopq %rdx\n");
                        self.write("\tcmpq %rdx, %rax #compute e1 == e2, set ZF \n ");
                        self.write("\tmovq $0, %rax #zero out EAX without changing ZF \n ");
                        self.write("\tsete %al #set AL register (the lower byte of EAX) to 1 iff e1 | e2 != 0 \n ");
                    }
                    BinOp::NE => {
                        self.compile_instruction(v1, locals);
                        self.write("\tpushq %rax\n");
                        self.compile_instruction(v2, locals);
                        self.write("\tpopq %rdx\n");
                        self.write("\tcmpq %rdx, %rax #compute e1 != e2, set ZF \n ");
                        self.write("\tmovq $0, %rax #zero out EAX without changing ZF \n ");
                        self.write("\tsetne %al #set AL register (the lower byte of EAX) to 1 iff e1 | e2 != 0 \n ");
                    }
                    BinOp::LT => {
                        self.compile_instruction(v1, locals);
                        self.write("\tpushq %rax\n");
                        self.compile_instruction(v2, locals);
                        self.write("\tpopq %rdx\n");
                        self.write("\tcmpq %rdx, %rax #compute e1 < e2, set ZF \n ");
                        self.write("\tmovq $0, %rax #zero out EAX without changing ZF \n ");
                        self.write("\tsetl %al #set AL register (the lower byte of EAX) to 1 iff e1 | e2 != 0 \n ");
                    }
                    BinOp::LTE => {
                        self.compile_instruction(v1, locals);
                        self.write("\tpushq %rax\n");
                        self.compile_instruction(v2, locals);
                        self.write("\tpopq %rdx\n");
                        self.write("\tcmpq %rdx, %rax #compute e1 <= e2, set ZF \n ");
                        self.write("\tmovq $0, %rax #zero out EAX without changing ZF \n ");
                        self.write("\tsetle %al #set AL register (the lower byte of EAX) to 1 iff e1 | e2 != 0 \n ");
                    }
                    BinOp::GT => {
                        self.compile_instruction(v1, locals);
                        self.write("\tpushq %rax\n");
                        self.compile_instruction(v2, locals);
                        self.write("\tpopq %rdx\n");
                        self.write("\tcmpq %rdx, %rax #compute e1 > e2, set ZF \n ");
                        self.write("\tmovq $0, %rax #zero out EAX without changing ZF \n ");
                        self.write("\tsetg %al #set AL register (the lower byte of EAX) to 1 iff e1 | e2 != 0 \n ");
                    }
                    BinOp::GTE => {
                        self.compile_instruction(v1, locals);
                        self.write("\tpushq %rax\n");
                        self.compile_instruction(v2, locals);
                        self.write("\tpopq %rdx\n");
                        self.write("\tcmpq %rdx, %rax #compute e1 >= e2, set ZF \n ");
                        self.write("\tmovq $0, %rax #zero out EAX without changing ZF \n ");
                        self.write("\tsetge %al #set AL register (the lower byte of EAX) to 1 iff e1 | e2 != 0 \n ");
                    }
                }
            }
            Instruction::UnOp(_, ref op, ref value) => {
                use ir::ir::UnOp;
                match *op {
                    UnOp::Bang => {
                        self.compile_instruction(value, locals);
                        self.write("\tcmpq $0, %rax\n");
                        self.write("\tmovq $0, %rax\n");
                        self.write("\tsete   %al\n");
                    }
                    UnOp::Minus => {
                        self.compile_instruction(value, locals);
                        self.write("\tneg %rax\n");
                    }
                }
            }
            Instruction::Return(ref value) => self.compile_instruction(value, locals),
            Instruction::Jump(ref label) => write!(&mut self.file, "\tjmp .{} \n", label).unwrap(),
            Instruction::JumpOp(ref op, ref label) => {
                use ir::ir::CmpOp;
                match *op {
                    CmpOp::LT => write!(&mut self.file, "\tjge .{}\n", label).unwrap(),
                    CmpOp::LTE => write!(&mut self.file, "\tjg .{}\n", label).unwrap(),
                    CmpOp::GT => write!(&mut self.file, "\tjle .{}\n", label).unwrap(),
                    CmpOp::GTE => write!(&mut self.file, "\tjl .{}\n", label).unwrap(),
                    CmpOp::NE => write!(&mut self.file, "\tje  .{}\n", label).unwrap(),
                    CmpOp::EQ => write!(&mut self.file, "\tjne .{}\n", label).unwrap(),
                }
            }
            Instruction::Load(ref temp) => {
                if let Some(ref offset) = locals.get(temp) {
                    write!(&mut self.file, "\tmovq {}(%rbp),%rax\n", offset).unwrap();
                } else {
                    panic!("Undefined temporary {}", temp);
                }
            }

            Instruction::Call(ref name) => write!(&mut self.file, "\tcallq {}\n", name).unwrap(),

            ref e => unimplemented!("{:?}", e),
        }
    }

    pub fn compile_value(&mut self, value: &Value, locals: &HashMap<ir::Temp, i32>) {
        match *value {
            Value::Const(ref i, _, _) => write!(&mut self.file, "\tmovq ${}, %rax\n", i).unwrap(),
            Value::Temp(ref temp) => {
                if let Some(ref offset) = locals.get(temp) {
                    write!(&mut self.file, "\tmovq {}(%rbp),%rax\n", offset).unwrap();
                }
            }
            _ => unimplemented!(),
        }
    }
}
