use temp::{Label};
use ir::Instruction;

pub trait Frame {
    type Access;

    type FP;

    const WORD_SIZE:u32;

    fn new(name: Label, formals: &[bool]) -> Self;

    fn name(&self) -> Label;

    fn formals(&self) -> Vec<Self::Access>;

    fn alloc_local(&self, escapes: bool) -> Self::Access;

    fn exp(&self,access:&Self::Access,ir:&mut Instruction) -> Instruction;
}
