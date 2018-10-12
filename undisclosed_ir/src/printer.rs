use std::io::{self, Write};
use tac::{Function, Instruction, Program, Temp};
use util::symbol::SymbolMap;

pub struct Printer<'a> {
    pub buffer: Vec<u8>,
    ident_level: usize,
    symbols: &'a SymbolMap<()>,
}

impl<'a> Printer<'a> {
    pub fn new(symbols: &'a SymbolMap<()>) -> Self {
        Self {
            buffer: Vec::new(),
            ident_level: 0,
            symbols,
        }
    }

    pub fn print_program(mut self, p: &Program) -> io::Result<Vec<u8>> {
        for function in p.functions.iter() {
            self.print_function(function)?;
        }

        Ok(self.buffer)
    }

    pub fn print_function(&mut self, f: &Function) -> io::Result<()> {
        write!(&mut self.buffer, "function ")?;
        write!(&mut self.buffer, "{}", &self.symbols.name(f.name))?;

        write!(&mut self.buffer, "(")?;

        for (i, param) in f.params.iter().enumerate() {
            if (i + 1) == f.params.len() {
                write!(&mut self.buffer, "{}", param)?;
            } else {
                write!(&mut self.buffer, "{},", param)?;
            }
        }

        write!(&mut self.buffer, ")")?;

        write!(&mut self.buffer, "{{")?;

        for inst in f.body.iter() {
            write!(&mut self.buffer, "\t")?;

            self.print_instructions(inst)?;

            write!(&mut self.buffer, "\n")?;
        }

        write!(&mut self.buffer, "}}")?;

        Ok(())
    }

    pub fn print_instructions(&mut self, i: &Instruction) -> io::Result<()> {
        match *i {
            Instruction::Array(ref l, ref s) => write!(&mut self.buffer, "{} <- [{}]", l, s),
            Instruction::Label(ref l) => write!(&mut self.buffer, "{}", l),
            Instruction::StatementStart => Ok(()),
            Instruction::Jump(ref l) => write!(&mut self.buffer, "jmp @{}", l),
            Instruction::Binary(ref res, ref lhs, ref op, ref rhs) => {
                write!(&mut self.buffer, "{} <- {} {} {}", res, lhs, op, rhs)
            }
            Instruction::Store(ref dest, ref source) => {
                write!(&mut self.buffer, "{} <- {}", dest, source)
            }
            Instruction::Cast(ref dest, ref sign, ref size) => {
                write!(&mut self.buffer, "{} as {}{}", dest, sign, size)
            }
            Instruction::Unary(ref dest, ref source, ref op) => {
                write!(&mut self.buffer, "{} <- {}{}", dest, op, source)
            }
            Instruction::Return(ref label) => write!(&mut self.buffer, "return @{}", label),
            Instruction::Call(ref dest, ref callee, ref args) => {
                write!(&mut self.buffer, "{} <- call {} ", dest, callee)?;

                for arg in args {
                    write!(&mut self.buffer, "{}", arg)?;
                }

                Ok(())
            }
            Instruction::CJump(_, _, _, _, _) => unimplemented!(),
        }
    }
}
