use op::{OpCode, TryFrom};
use vm::VM;

macro_rules! pop {
    ([$stack:expr, $top:expr] => $type:ty) => {{
        use std::default;
        use std::mem;

        let mut b: [u8; mem::size_of::<$type>()] = default::Default::default();
        b.copy_from_slice($stack);
        unsafe { mem::transmute::<_, $type>(b) }
    }};
}

#[cfg(feature = "debug")]
pub fn simple_instruction(name: &str, offset: usize) -> usize {
    println!("{}", name);
    offset + 2
}

impl<'a> VM<'a> {
    #[cfg(feature = "debug")]
    pub fn dissassemble(&mut self, name: &str) {
        println!("== {} ==", name);

        let mut i = 0;
        while i < self.code.len() {
            i = self.dissassemble_instruction(i);
        }
    }

    #[cfg(feature = "debug")]
    pub fn dissassemble_instruction(&self, offset: usize) -> usize {
        print!("{:04} ", offset);

        // if offset > 0 && self.lines[offset] == self.lines[offset - 1] {
        //     print!("   | ");
        // } else {
        //     print!("{:04} ", self.lines[offset]);
        // }

        let instruction = self.code[offset];

        match OpCode::try_from(instruction) {
            Ok(OpCode::Return) => simple_instruction("OP_RETURN", offset),
            Ok(OpCode::Neg) => simple_instruction("OP_NEG", offset),
            Ok(OpCode::Add) => simple_instruction("OP_ADD", offset),
            Ok(OpCode::Subtract) => simple_instruction("OP_SUBTRACT", offset),
            Ok(OpCode::Multiply) => simple_instruction("OP_MULTIPLY", offset),
            Ok(OpCode::Divide) => simple_instruction("OP_DIVIDE", offset),

            Ok(OpCode::Constant) => self.constant_instruction("OP_CONSTANT", offset as usize),
            _ => {
                println!("Unknown opcode {}", instruction);
                offset + 1
            }
        }
    }

    #[cfg(feature = "debug")]
    /// Pulls out the `OpCode::Constant` and prints that out
    /// Matches on the instruction and uses that pointer offset for
    /// were the value is stored
    pub fn constant_instruction(&self, name: &str, mut offset: usize) -> usize {
        offset += 1;

        let size = self.code[offset] as usize;

        match size {
            1 => {
                println!("{:16} {:04}", name, self.code[offset + 1]);
                offset + 1
            }
            4 => {
                let mut slice = [0; 4];

                slice.copy_from_slice(&self.code[offset + 1..offset + 5]);

                println!("{:16} {:04}", name, unsafe {
                    use std::mem;
                    mem::transmute::<[u8; 4], i32>(slice)
                });
                offset as usize + 5
            }

            8 => {
                println!("{:16} {:04}", name, pop!([self.code,offset] => i64));
                offset as usize + 3
            }

            ref e => unreachable!("{:?}", e),
        }
    }
}
