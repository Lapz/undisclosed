use ir;
use std::collections::HashMap;
use std::fs::File;
use std::io::Write;
use ir::ir::Value;

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
        self.write(&format!("\t{}:\n", function.name));
        // self.emit_func_prologue();

        let locals = &function.locals;

        for instruction in function.body.iter() {
            self.compile_instruction(instruction,locals);
        }
        // self.write("\tpopq %rbp\n");

        self.write("\tret");
    }

    pub fn emit_func_prologue(&mut self) {
        self.write("\tpushq   %rbp\n\tmovq %rsp, %rbp\n")
    }

    pub fn compile_instruction(&mut self, instruction: &ir::ir::Instruction,locals:&HashMap<ir::Temp,i32>) {
        use ir::ir::{Instruction, Value};

        println!("{:?}",locals);

        match *instruction {
            Instruction::Label(ref label) => self.write(&format!("\t{}:\n", label)),
            Instruction::Store(ref temp, ref value) => {
                // println!("{:?}");
                if let Some(ref offset) =  locals.get(temp) {
                    self.compile_value(value,locals);
                    write!(&mut self.file,"\tmovq %rax, {}(%rbp)\n",offset).unwrap();
                }else {
                    self.compile_value(value,locals);
                }
            },

            Instruction::BinOp(_, ref op, ref v1, ref v2) => {
                use ir::ir::BinOp;
                match *op {
                    BinOp::Plus => {
                        self.compile_instruction(v1,locals);
                        self.write("\tpushq %rax\n");
                        self.compile_instruction(v2,locals);
                        self.write("\tpopq %rdx\n");
                        self.write("\taddq %rdx,%rax\n")
                    }

                    BinOp::Minus => {
                        self.compile_instruction(v1,locals);
                        self.write("\tpushq %rax\n");
                        self.compile_instruction(v2,locals);
                        self.write("\tpopq %rdx\n");
                        self.write("\tsubq %rdx,%rax\n")
                    },
                    BinOp::Mul => {
                        self.compile_instruction(v1,locals);
                        self.write("\tpushq %rax\n");
                        self.compile_instruction(v2,locals);
                        self.write("\tpopq %rdx\n");
                        self.write("\timulq %rdx,%rax\n")
                    },
                    BinOp::Div => {
                         self.compile_instruction(v1,locals);
                        self.write("\tpushq %rax\n");
                        self.compile_instruction(v2,locals);
                        self.write("\tpopq %rdx\n");
                        self.write("\tmovq %rdx, %rbx\n");
                        self.write("\tcqto\n");
                        self.write("\tidivq %rbx\n");
                        
                    },

                    _ => unimplemented!(),
                }
            }

            Instruction::Value(ref value) => self.compile_value(value, locals),
            _ => unimplemented!(),
        }
    }

    pub fn compile_value(&mut self, value:&Value,locals:&HashMap<ir::Temp,i32>) {
        match *value {
           Value::Const(ref i,_,_) => write!(&mut self.file,"\tmovq ${}, %rax\n",i).unwrap(),
           Value::Temp(ref temp) => {
            //    if let Some(ref offset) =  locals.get(temp) {
                   
            //         write!(&mut self.file,"\tmovq %rax, {}(%rbp)\n",offset).unwrap();
            //     }
           }
            _ => unimplemented!()
        }
    }


    // pub fn compile_function()
}

// fn compile_function(function: &ir::Function, ctx: &mut Ctx) {
//     for instruction in &function.body {
//         compile_instruction(instruction, ctx)
//     }
// }

// fn compile_instruction(instruction: &ir::Instruction, ctx: &mut Ctx) {
//     //  match *instruction {
//     //      Instruction::
//     //  }
// }
// // fn
