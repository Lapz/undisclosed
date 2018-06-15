use temp::Label;

pub trait Frame: Clone {
    type Access: Clone;

    type FP;

    const WORD_SIZE: u32;

    fn new(name: Label, formals: &[bool]) -> Self;

    fn name(&self) -> &Label;

    fn formals(&self) -> &[Self::Access];

    fn alloc_local(&self, escapes: bool) -> Self::Access;
}
