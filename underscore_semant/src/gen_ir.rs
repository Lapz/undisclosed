use ast::typed as t;
use ir::{
    ir,
    Temp,
    Label,
};
use syntax::ast::{Literal, Op, Sign, Size, UnaryOp};
use types::{TyCon, Type};
use util::symbol::Symbols;

#[derive(Debug)]
pub struct Codegen {
    pub instructions: Vec<ir::Instruction>,
    loop_label: Option<Label>,
    loop_break_label: Option<Label>,
    symbols: Symbols<Temp>,
}

impl Codegen {
     pub fn new(symbols: Symbols<Temp>) -> Self {
        Self {
            symbols,
            loop_label: None,
            loop_break_label: None,
            instructions: vec![],
        }
    }

     pub fn dump_to_file(&mut self, path: String) {
        use std::fs::File;
        use std::io::Write;
      

        let mut file = File::create(path).expect("Couldn't create file");
        
        for instruction in &self.instructions {

            write!(file,"{}",instruction).expect("Couldn't write to the file");

            // file.write(format!("{}",instruction)).expect("Couldn't write to the file");
        
           
        }

         write!(file,"\n{:?}",self.instructions).expect("Couldn't write to the file");

       
     }
}