use ir::tac::Function;
use ir::tac::Linkage;
use ir::tac::Program;
use std::io::{self, Write};
pub struct Compiler {}

impl Compiler {
    pub fn new() -> Self {
        Self {}
    }
}
pub fn compile<T: Write>(p: Program, out: &mut T) -> io::Result<()> {
    for f in p.functions {
        compile_function(f, out)?;
    }

    Ok(())
}

pub fn compile_function<T: Write>(f: Function, out: &mut T) -> io::Result<()> {
    if f.linkage == Linkage::External {
        write!(out, "\n\t.extern {}", f.name)?;
        return Ok(());
    }

    write!(out, "{}:\n", f.name)?;

    write!(out, "pushq %rbp\n")?;

    write!(out, "movq %rsp\n")?;
    write!(out, "movq %rsp,%rbp\n")?;
    write!(out, "subq $16,%rsp")?;

    Ok(())
}
