use op::{OpCode, TryFrom};

pub struct VM<'a> {
    pub code: &'a mut [u8],
    stack: [u8; 256],
    stack_top: usize,
    values:[u8; 256],
    values_top: usize,
    ip: usize,
}

macro_rules! pop {
    ([$stack:expr, $top:expr] => $type:ty) => {{
        use std::default;
        use std::mem;

        $top -= mem::size_of::<$type>();

        let mut b: [u8; mem::size_of::<$type>()] = default::Default::default();
       
        b.copy_from_slice(&$stack[$top..$top+mem::size_of::<$type>()]);
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

       
        let mut b = &mut$stack[$from..($from+$to)];

        
        b.copy_from_slice($bytes);

        $from += $to;
    }};
}

macro_rules! to_bytes {
    ($expr:expr => $type:ty) => {{
        use std::mem;
        unsafe {
            mem::transmute::<_,[u8;mem::size_of::<$type>()]>($expr)
        }
    }}
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
        VM { ip: 0,stack_top:1,stack:[0;256],values:[0;256],values_top:0,code }
    }

    pub fn run(&mut self) -> VMResult {
        debug!("{:?}", self.dissassemble("run"));
        loop {
            
            
            if cfg!(feature = "stack") {
               
                println!("[");


                for (i,byte) in self.values.iter().enumerate() {
                    if i + 1 == self.values.len() {
                        print!("{}",byte);
                    } else {
                        print!("{},",byte);
                    }
                }

               println!("]");
            }

            

            match OpCode::try_from(self.code[self.ip]) {
                Ok(OpCode::Return) => {
                     self.ip += 1;

                    let size = self.code[self.ip] as usize;

                    match size {
                        1 => {

                             println!("{}",pop!([&self.values,self.values_top] => i8))
                            
                        }

                        4 => {
                             println!("{}",pop!([&self.values,self.values_top] => i32));
                            
            
                        }

                        8 => {
                            println!("{}",pop!([&self.values,self.values_top] => i64));
                             
                        },
                        _ => unreachable!()
                    };

                   
                    
                    return Ok(())
                },
                Ok(OpCode::Constant) => {
                    
                    self.ip += 1;

                    let size = self.code[self.ip] as usize;
                   
                    
                    self.ip +=1;
                  
                
                    match size {
                        1 => {
                            push!(&self.code[self.ip..self.ip+size] => self.stack,[self.stack_top,size]);
                        }

                        4 => {
                            push!(&self.code[self.ip..self.ip+size] => self.values,[self.values_top,size]);
                            
            
                        }

                        8 => {
                            push!(&self.code[self.ip..self.ip+size] => self.stack,[self.stack_top,size]);
                             
                        }

                        _ => unreachable!(),
                    }

                    self.ip+=size; 

                    // break;
                },

                Ok(OpCode::Neg) => {
                    self.ip += 1;

                    let size = self.code[self.ip] as usize;
                     self.ip += 1;
                    

           
                    match size {
                        1 => {
                            let a = pop!([&self.values,self.values_top] => i8) ;
                            push!( &to_bytes!(-a => i8)     => self.values,[self.values_top,size]);
                           
                            
                        }

                        4 => {
                            let a = pop!([&self.values,self.values_top] => i32) ;
                             push!( &to_bytes!(-a => i32)     => self.values,[self.values_top,size]);
                            
            
                        }

                        8 => {
                            let a = pop!([&self.values,self.values_top] => i64) ;
                            push!( &to_bytes!(-a => i64)     => self.values,[self.values_top,size]);
                        },
                        _ => unreachable!()
                    };
                }
               

                
                Err(_) => {
                   println!("{:?}",self.code[self.ip]);
                    return Err(VMError::RuntimeError)
                },
            }

        }
        Ok(())
    }
}
