use temp::Label;

trait Frame {
    type access;

    fn new(name:Label,formals:&[bool]) -> Self;

    fn name(&self) -> Label;

    fn formals(&self) -> Vec<Self::access>;

    fn alloc_local(&self,escapes:bool) -> Self::access;

}