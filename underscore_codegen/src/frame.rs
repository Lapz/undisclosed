use temp::Label;

pub trait Frame {
    type Access;

    fn new(name: Label, formals: &[bool]) -> Self;

    fn name(&self) -> Label;

    fn formals(&self) -> Vec<Self::Access>;

    fn alloc_local(&self, escapes: bool) -> Self::Access;
}
