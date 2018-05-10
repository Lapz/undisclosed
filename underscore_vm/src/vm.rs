use op::{OpCode, TryFrom};

pub struct VM<'a> {
    pub code: &'a mut [u8],
    stack: [u8; 256],
    stack_top: usize,
    ip: usize,
}

macro_rules! pop {
    ([$stack:expr, $top:expr] => $type:ty) => {{
        use std::default;
        use std::mem;

        $top -= mem::size_of::<$type>();

        let mut b: [u8; mem::size_of::<$type>()] = default::Default::default();
        b.copy_from_slice($stack);
        unsafe { mem::transmute::<_, $type>(b) }
    }};
}

macro_rules! debug {
    ($($p:tt)*) => (if cfg!(feature = "debug") { println!($($p)*) } else { })
}

macro_rules! push {
    ($bytes:expr => $stack:expr,[$from:expr, $to:expr]) => {{
        use std::default;
        use std::mem;

        let mut b = &mut$stack[$from..$to];

        b.copy_from_slice($bytes);

        $from += $to;
    }};
}

type VMResult = Result<(), VMError>;

#[derive(Debug)]
pub enum VMError {
    CompilerError,
    RuntimeError,
}

impl<'a> VM<'a> {
    fn reset_stack(&mut self) {
        self.stack_top = 0;
    }

    pub fn new(code: &'a mut [u8]) -> Self {
        VM { ip: 0,stack_top:1,stack:[0;256],code }
    }

    pub fn run(&mut self) -> VMResult {
        loop {
            debug!("{:?}", self.dissassemble("run"));
            
            if cfg!(feature = "debug") {
               
                print!("[");


                for (i,byte) in self.stack.iter().enumerate() {
                    if i + 1 == self.stack.len() {
                        print!("{}",byte);
                    } else {
                        print!("{},",byte);
                    }
                }

               print!("]");
            }

            match OpCode::try_from(self.code[self.ip]) {
                Ok(OpCode::Return) => return Ok(()),
                Ok(OpCode::Constant) => {
                    let size = self.code[self.ip + 1] as usize;
                    self.ip += 1;

                    match size {
                        1 => {
                            push!(&self.code[self.stack_top..self.stack_top+1] => self.stack,[self.stack_top,size])
                        }
                        4 => {
                            push!(&self.code[self.stack_top..self.stack_top+1] => self.stack,[self.stack_top,size])
                        }

                        8 => {
                            push!(&self.code[self.stack_top..self.stack_top+1] => self.stack,[self.stack_top,size])
                        }

                        _ => unreachable!(),
                    }

                    // break;
                }
                Ok(OpCode::ConstantLong) => {
                    break;
                }
                Err(_) => return Err(VMError::RuntimeError),
            }
        }
        Ok(())
    }
}
