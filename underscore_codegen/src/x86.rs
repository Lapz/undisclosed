use frame::Frame;
use temp::{Label, Temp};

#[derive(Debug,Clone)]
pub struct x86 {
    name: Label,
    formals: Vec<Access>,
}

impl Frame for x86 {
    type Access = Access;

    fn name(&self) -> Label {
        self.name
    }

    fn new(name: Label, formals: &[bool]) -> Self {
        let mut frame = x86 {
            name,
            formals: vec![],
        };

        let mut access_formals = Vec::new();

        for formal in formals {
            access_formals.push(frame.alloc_local(*formal))
        }

        frame.formals = access_formals;
        frame
    }

    fn formals(&self) -> Vec<Access> {
        self.formals.clone()
    }

    fn alloc_local(&self, escapes: bool) -> Access {
        if escapes {
            Access::Frame(0)
        } else {
            Access::Reg(Temp::new())
        }
    }
}

#[derive(Clone, Debug)]
pub enum Access {
    /// Data that will be held in a register
    Reg(Temp),
    /// A memory location at offset X from the frame pointer
    Frame(u32),
}
