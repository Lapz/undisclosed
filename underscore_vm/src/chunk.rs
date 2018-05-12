/// A sequence of bytecode instrutcions, constant pool and
/// the line from which an instruction orginated from

use op::{OpCode,TryFrom};
use std::ops::{Index,Range};






type Line = usize;
#[derive(Debug, Clone, Default)]
pub struct Chunk {
    code: Vec<u8>,
    pub constants: Vec<u8>,
    lines: Vec<Line>,
}

#[cfg(feature = "debug")]
pub fn simple_instruction(name: &str, offset: usize) -> usize {
    println!("{}", name);
    offset + 2
}


macro_rules! to_num {
    ([$stack:expr, $top:expr] => $type:ty) => {{
        use std::default;
        use std::mem;

        let mut b: [u8; mem::size_of::<$type>()] = default::Default::default();
     
        b.copy_from_slice($stack);
        unsafe { mem::transmute::<_, $type>(b) }
    }};
} 


impl Chunk {
    pub fn new() -> Self {
        Self { ..Self::default() }
    }

    /// Write a single byte to this chunk
    pub fn write<T:Into<u8>>(&mut self, byte: T,line:Line) {
        self.lines.push(line);
        self.code.push(byte.into())
    }

    /// Write multiple bytes to this chunk
    pub fn write_values(&mut self, bytes: &[u8],line:Line) {
        self.lines.push(line);
        self.constants.extend(bytes)
    }

    /// Adds a constant to the constant pool.
    /// Constant must be in bytes.
    /// Returns the offset at which the constant is stored
    pub fn add_constant(&mut self, bytes: &[u8],line:Line) -> usize {
        let len = self.constants.len();
        self.write_values(bytes,line);
        len
    }

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

        if offset > 0 && self.lines[offset] == self.lines[offset - 1] {
            print!("   | ");
        } else {
            print!("{:04} ", self.lines[offset]);
        }

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
    pub fn constant_instruction(&self, name: &str, offset: usize) -> usize {

        // opcode size start
        
    
        let size = self.code[offset+1] as usize;
        
        let index = self.code[offset+2] as usize;

        match size {
            1 => {
                println!("{:16} {:04}", name, to_num!([&self.constants,offset] => i8));
            }
            4 => {
                let mut slice = [0; 4];

                


                slice.copy_from_slice(&self.constants[index..index+size]);

                

                println!("{:16} {:04}", name,to_num!([&self.constants[index..index+size],index] => i32));
              
            }

            8 => {
                println!("{:16} {:04}", name, to_num!([&self.constants,offset] => i64));
                
            }

            ref e => unreachable!("{:?}", e),
        }

        offset as usize + 3

       
    }
}


impl Index<usize> for Chunk {
    type Output = u8;

    fn index(&self, index: usize) -> &Self::Output {
        &self.code[index]
    }
}

impl Index<Range<usize>> for Chunk {
    type Output = [u8];

    fn index(&self, index: Range<usize>) -> &Self::Output {
        self.code.index(index)
    }
}

#[cfg(test)]
#[cfg(feature = "debug")]
mod test {

    use op::OpCode;
    use chunk::Chunk;

    #[test]
    fn it_work() {
        let mut chunk = Chunk::new();
        let constant = chunk.add_constant(&[12, 0, 0, 0],1);

        chunk.write(OpCode::Constant,1);
        chunk.write(4,1);
        chunk.write(constant as u8,1);
       

        chunk.write(OpCode::Return,2);
println!("{:?}",chunk );
        chunk.dissassemble("test");
    }
}
