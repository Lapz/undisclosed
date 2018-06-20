use temp::{Label,Temp};

pub trait Frame {
    type Access;

    

    const WORD_SIZE: u32;

    fn new(name: Label, formals: &[bool]) -> Self;

    fn name(&self) -> Label;

    fn formals(&self) -> &[Self::Access];

    fn alloc_local(&self, escapes: bool) -> Self::Access;

    fn fp(&self) -> Temp;
}
