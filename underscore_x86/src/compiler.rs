use ir;
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
        self.write(&format!("\t{}:\n", function.name));
        // self.emit_func_prologue();

        for instruction in function.body.iter() {
            self.compile_instruction(instruction);
        }


        self.write("ret");
    }

    pub fn emit_func_prologue(&mut self) {
        self.write("\tpushq   %rbp\n\tmovq %rsp, %rbp\n")
    }

    pub fn compile_instruction(&mut self, instruction: &ir::ir::Instruction) {
        // use ir::ir::{Instruction, Value};

        // match *instruction {
        //     Instruction::Label(ref label) => self.write(&format!("\t{}:\n", label)),
        //     Instruction::Store(ref temp, ref value) => match *value {
        //         Value::Const(ref i, _, _) => self.write(&format!("\tmovl ${},%eax\n", i)),

        //         _ => unimplemented!(),
        //     },

        //     Instruction::BinOp(_, ref op, ref v1, ref v2) => {
        //         use ir::ir::BinOp;
        //         match *op {
        //             BinOp::Plus => {
        //                 match (v1, v2) {
        //                     (&Value::Const(ref i1, _, _), &Value::Const(i2, _, _)) => {
        //                         write!(self.file, "\t addl {},{}\n", i1, i2).unwrap();
        //                     }
        //                     (&Value::Temp(ref t), &Value::Const(ref i, _, _))
        //                     | (&Value::Const(ref i, _, _), &Value::Temp(ref t)) => {
        //                         let offset = self.offsets.get(t).cloned().unwrap();

        //                         write!(self.file, "\t addl {},{}\n", offset, i).unwrap();
        //                     },

        //                     (&)

        //                     _ => unimplemented!("{:?},{:?}",v1,v2),
        //                 }
        //             }

        //             BinOp::Minus => self.write("\tsubl %eax,%eax\n"),
        //             BinOp::Mul => self.write("\timull %eax, %eax\n"),
        //             BinOp::Div => self.write("\ti %eax, %eax\n"),

        //             _ => unimplemented!(),
        //         }
        //     }

        //     // Instruction::V
        //     _ => unimplemented!(),
        // }
    }

    pub fn compile_value(&mut self, v1: &ir::ir::Value) {
        
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
