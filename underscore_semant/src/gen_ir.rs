use ast::typed as t;
use ir::{ir, Label, Temp};
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
            write!(file, "{}", instruction).expect("Couldn't write to the file");
        }

        write!(file, "\n{:?}", self.instructions).expect("Couldn't write to the file");
    }

    pub fn gen_program(&mut self, program: t::Program) -> ir::Program {
        let mut lowered = ir::Program {
            functions: Vec::new(),
        };

        for function in program.functions {
            let mut instructions = vec![];
            self.gen_function(&function, &mut instructions);

            // Optimizer::strength_reduction(&mut instructions);
            // Optimizer::unused_labels(&mut vec![], &mut instructions);

            lowered.functions.push(ir::Function {
                name: Label::new(),
                body: instructions,
                linkage: function.linkage,
            });
        }

        lowered
    }

    fn gen_function(&mut self, func: &t::Function, instructions: &mut Vec<ir::Instruction>) {
        for param in &func.params {
            self.symbols.enter(param.name, Temp::new());
        }

        self.gen_statement(&func.body, instructions);
    }

    fn gen_statement(&mut self, statement: &t::Statement, instructions: &mut Vec<ir::Instruction>) {
        unimplemented!()
    }
}
