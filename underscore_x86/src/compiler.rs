use ir::ir;
use std::fs::File;

struct Ctx {
    si: i64,
    file: File,
}

impl Ctx {
    pub fn new() -> Ctx {
        Ctx {
            si: -4,
            file: File::create("./prog.asm").expect("Cou;dn't create file"),
        }
    }
}

fn compile_function(function: &ir::Function, ctx: &mut Ctx) {
    for instruction in &function.body {
        compile_instruction(instruction, ctx)
    }
}

fn compile_instruction(instruction: &ir::Instruction, ctx: &mut Ctx) {
    //  match *instruction {
    //      Instruction::
    //  }
}
// fn
