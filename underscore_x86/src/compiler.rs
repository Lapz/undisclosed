use ir;
use ir::ir::Value;
use ir::Label;
use std::collections::HashMap;
use std::fs::File;
use std::io::Write;
use std::rc::Rc;
use util::symbol::{Hasher, Symbol, SymbolMap};

pub struct Compiler {
    file: File,
    labels: SymbolMap<()>,
}

impl Compiler {
    pub fn new(strings: &Rc<Hasher<Symbol>>) -> Compiler {
        let mut file = File::create("out.s").expect("\tCouldn't create the file");

        file.write_all(b".text \n\t.global _main").unwrap();

        Compiler {
            file,
            labels: SymbolMap::new(strings.clone()),
        }
    }

    pub fn write(&mut self, code: &str) {
        self.file.write(code.as_bytes()).unwrap();
    }

    pub fn compile(&mut self, program: ir::ir::Program) {
        let mut extern_funcs = vec![];
        for function in program.functions.iter() {
            if function.linkage == ir::ir::Linkage::External {
                extern_funcs.push(function.name);
            }
        }

        extern_funcs.iter().for_each(|e| {
            self.write("\n\t.extern ");
            e.fmt(&mut self.file, &mut self.labels).unwrap();
            self.write("\n");
        });

        for function in program.functions.iter() {
            if function.linkage == ir::ir::Linkage::External {
                continue;
            }
            self.compile_function(function);
        }
    }

    pub fn compile_function(&mut self, function: &ir::ir::Function) {
        self.write("\n");
        function.name.fmt(&mut self.file, &mut self.labels).unwrap();
        self.write(":\n");
        self.emit_func_prologue(function.params.len(), function.locals.len());

        let locals = &function.locals;
        let params = &function.params;

        for instruction in function.body.iter() {
            self.compile_instruction(instruction, locals, params);
        }

        self.emit_func_epilogue();
        self.emit_strings(&function.strings);
        write!(
            &mut self.file,
            "/*\nlocals:{:#?} ,\nparams:{:#?}\n*/",
            locals, params
        ).unwrap();
    }

    pub fn emit_strings(&mut self, strings: &HashMap<Label, String>) {
        for (label, string) in strings {
            write!(&mut self.file, ".{}:\n\t.asciz {:?}\n", label, string).unwrap();
        }
    }

    pub fn emit_func_prologue(&mut self, nparams: usize, nlocals: usize) {
        self.write("\tpushq %rbp\n\tmovq %rsp,%rbp\n");
        write!(
            &mut self.file,
            "\tsubq ${}, %rsp #pro\n",
            (nparams + nlocals) * 16
        ).unwrap();
    }

    pub fn emit_func_epilogue(&mut self) {
        self.write("\tmovq %rbp, %rsp #epi\n\tpopq %rbp  \n\tret\n")
    }

    pub fn compile_instruction(
        &mut self,
        instruction: &ir::ir::Instruction,
        locals: &HashMap<ir::Temp, i32>,
        params: &HashMap<ir::Temp, ir::Register>,
    ) {
        use ir::ir::Instruction;

        match *instruction {
            Instruction::Add => self.write("\t addq %rdx,%rax"),
            Instruction::Label(ref label) => self.write(&format!(".{}:\n", label)),
            Instruction::Store(ref temp, ref value) => {
                if let Some(ref offset) = locals.get(temp) {
                    self.compile_value(value, locals);
                } else {
                    self.compile_value(value, locals);
                }
            }
            Instruction::BinOp(_, ref op, ref v1, ref v2) => {
                use ir::ir::BinOp;
                match *op {
                    BinOp::Plus => {
                        self.compile_instruction(v1, locals, params);
                        self.write("\tpushq %rax\n");
                        self.compile_instruction(v2, locals, params);
                        self.write("\tpopq %rdx\n");
                        self.write("\taddq %rdx,%rax\n")
                    }

                    BinOp::Minus => {
                        self.compile_instruction(v1, locals, params);
                        self.write("\tpushq %rax\n");
                        self.compile_instruction(v2, locals, params);
                        self.write("\tpopq %rdx\n");
                        self.write("\tsubq %rdx,%rax\n")
                    }
                    BinOp::Mul => {
                        self.compile_instruction(v1, locals, params);
                        self.write("\tpushq %rax\n");
                        self.compile_instruction(v2, locals, params);
                        self.write("\tpopq %rdx\n");
                        self.write("\timulq %rdx,%rax\n")
                    }
                    BinOp::Div => {
                        self.compile_instruction(v1, locals, params);
                        self.write("\tpushq %rax\n");
                        self.compile_instruction(v2, locals, params);
                        self.write("\tpopq %rdx\n");
                        self.write("\tmovq %rdx, %rbx\n");
                        self.write("\tcqto\n");
                        self.write("\tidivq %rbx\n");
                    }

                    BinOp::And => {
                        self.compile_instruction(v1, locals, params);
                        self.write("\tpushq %rax\n");
                        self.compile_instruction(v2, locals, params);
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
                        self.compile_instruction(v1, locals, params);
                        self.write("\tpushq %rax\n");
                        self.write("\tcmpq $0, %rax \n");
                        let label = Label::new();
                        write!(&mut self.file, "\tje .{}\n", label).unwrap();
                        self.compile_instruction(v2, locals, params);
                        write!(&mut self.file, ".{}:\n", label).unwrap();
                    }

                    BinOp::EQ => {
                        self.compile_instruction(v1, locals, params);
                        self.write("\tpushq %rax\n");
                        self.compile_instruction(v2, locals, params);
                        self.write("\tpopq %rdx\n");
                        self.write("\tcmpq %rax,%rdx #compute e1 == e2, set ZF \n ");
                        // self.write("\tmovq $0, %rax #zero out EAX without changing ZF \n ");
                        // self.write("\tsete %al #set AL register (the lower byte of EAX) to 1 iff e1 | e2 != 0 \n ");
                    }
                    BinOp::NE => {
                        self.compile_instruction(v1, locals, params);
                        self.write("\tpushq %rax\n");
                        self.compile_instruction(v2, locals, params);
                        self.write("\tpopq %rdx\n");
                        self.write("\tcmpq %rax,%rdx #compute e1 != e2, set ZF \n ");
                        // self.write("\tmovq $0, %rax #zero out EAX without changing ZF \n ");
                        // self.write("\tsetne %al #set AL register (the lower byte of EAX) to 1 iff e1 | e2 != 0 \n ");
                    }
                    BinOp::LT => {
                        self.compile_instruction(v1, locals, params);
                        self.write("\tpushq %rax\n");
                        self.compile_instruction(v2, locals, params);
                        self.write("\tpopq %rdx\n");
                        self.write("\tcmpq %rax,%rdx #compute e1 < e2, set ZF \n ");
                        // self.write("\tmovq $0, %rax #zero out EAX without changing ZF \n ");
                        // self.write("\tsetl %al #set AL register (the lower byte of EAX) to 1 iff e1 | e2 != 0 \n ");
                    }
                    BinOp::LTE => {
                        self.compile_instruction(v1, locals, params);
                        self.write("\tpushq %rax\n");
                        self.compile_instruction(v2, locals, params);
                        self.write("\tpopq %rdx\n");
                        self.write("\tcmpq %rax,%rdx #compute e1 <= e2, set ZF \n ");
                        // self.write("\tmovq $0, %rax #zero out EAX without changing ZF \n ");
                        // self.write("\tsetle %al #set AL register (the lower byte of EAX) to 1 iff e1 | e2 != 0 \n ");
                    }
                    BinOp::GT => {
                        self.compile_instruction(v1, locals, params);
                        self.write("\tpushq %rax\n");
                        self.compile_instruction(v2, locals, params);
                        self.write("\tpopq %rdx\n");
                        self.write("\tcmpq %rax,%rdx #compute e1 > e2, set ZF \n ");
                        // self.write("\tmovq $0, %rax #zero out EAX without changing ZF \n ");
                        // self.write("\tsetg %al #set AL register (the lower byte of EAX) to 1 iff e1 | e2 != 0 \n ");
                    }
                    BinOp::GTE => {
                        self.compile_instruction(v1, locals, params);
                        self.write("\tpushq %rax\n");
                        self.compile_instruction(v2, locals, params);
                        self.write("\tpopq %rdx\n");
                        self.write("\tcmpq %rax,%rdx #compute e1 >= e2, set ZF \n ");
                        // self.write("\tmovq $0, %rax #zero out EAX without changing ZF \n ");
                        // self.write("\tsetge %al #set AL register (the lower byte of EAX) to 1 iff e1 | e2 != 0 \n ");
                    }
                }
            }
            Instruction::UnOp(_, ref op, ref value) => {
                use ir::ir::UnOp;
                match *op {
                    UnOp::Bang => {
                        self.compile_instruction(value, locals, params);
                        self.write("\tcmpq $0, %rax\n");
                        self.write("\tmovq $0, %rax\n");
                        self.write("\tsete   %al\n");
                    }
                    UnOp::Minus => {
                        self.compile_instruction(value, locals, params);
                        self.write("\tneg %rax\n");
                    }
                }
            }
            Instruction::Return(ref value,ref label) => {
                self.compile_instruction(value, locals, params);
                write!(&mut self.file, "\tjmp .{} \n", label).unwrap()
            }
            Instruction::Jump(ref label) => write!(&mut self.file, "\tjmp .{} \n", label).unwrap(),
            Instruction::JumpOp(ref op, ref label) => {
                use ir::ir::CmpOp;
                match *op {
                    CmpOp::LT => write!(&mut self.file, "\tje .{}\n", label).unwrap(),
                    CmpOp::LTE => write!(&mut self.file, "\tjge .{}\n", label).unwrap(),
                    CmpOp::GT => write!(&mut self.file, "\tjl .{}\n", label).unwrap(),
                    CmpOp::GTE => write!(&mut self.file, "\tjle .{}\n", label).unwrap(),
                    CmpOp::NE => write!(&mut self.file, "\tje  .{}\n", label).unwrap(),
                    CmpOp::EQ => write!(&mut self.file, "\tjne .{}\n", label).unwrap(),
                    _ => unreachable!(),
                }
            }
            Instruction::Load(ref temp) => {
                if let Some(ref offset) = locals.get(temp) {
                    write!(&mut self.file, "\tmovq {}(%rbp),%rax\n", offset).unwrap();
                } else if let Some(ref reg) = params.get(temp) {
                    write!(&mut self.file, "\tmovq {},%rax\n", reg).unwrap();
                } else {
                    panic!("Undefined temporary {}", temp);
                }
            }

            Instruction::Call(ref name) => {
                self.write("\tcallq ");

                name.fmt(&mut self.file, &mut self.labels).unwrap();
                self.write("\n");
            }
            Instruction::Drop(ref size) => {
                if *size != 0 {
                    write!(&mut self.file, "\taddq ${},%rsp\n", 8 * size).unwrap()
                }
            }
            Instruction::Move(_, ref reg) => {
                write!(&mut self.file, "\tmovq %rax,{}\n", reg).unwrap()
            }
            Instruction::Deref(ref temp) => {
                if let Some(ref offset) = locals.get(temp) {
                    write!(&mut self.file, "\tmovq %rax,{}(%rbp)\n", offset).unwrap();
                }
            }

            Instruction::Cmp => write!(&mut self.file, "\tcmpq $1,%rax\n").unwrap(),

            Instruction::Push(ref reg) => {
                write!(&mut self.file, "\tpushq {}\n", reg).unwrap();
            }

            Instruction::Pop(ref reg) => {
                write!(&mut self.file, "\tpopq {}\n", reg).unwrap();
            }

            ref e => unimplemented!("{:?}", e),
        }
    }

    pub fn compile_value(&mut self, value: &Value, locals: &HashMap<ir::Temp, i32>) {
        match *value {
            Value::Const(ref i, _, _) => {
                write!(&mut self.file, "\tmovq ${}, %rax #{} \n", *i, *i).unwrap()
            }
            Value::Temp(ref temp) => {
                if let Some(ref offset) = locals.get(temp) {
                    write!(&mut self.file, "\tmovq {}(%rbp),%rax\n", offset).unwrap();
                }
            }
            Value::Name(ref label) => {
                write!(&mut self.file, "\tleaq .{}(%rip),%rax\n", label).unwrap();
            }
        }
    }
}
