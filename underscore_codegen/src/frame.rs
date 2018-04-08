use temp::*;

pub trait Frame {
    /// Allocate a new frame
    fn new(name: Label, formals: &[bool]) -> Self;

    // fn name(&self) -> Label;

    fn formals(&self,) -> Vec<Access>;

    fn alloc_local(&mut self, escape: bool) -> Access;
}

static mut FRAME_COUNT: u32 = 0;

#[derive(Debug, Clone, Copy)]
pub enum Access {
    /// Data that will be held in a register
    Reg(Temp), 
    /// A memory location at offset X from the frame pointer
    Frame(u32), 
}
