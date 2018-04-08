use frame::{Access, Frame};
use temp::{Label,Temp};

struct X86 {
    name: Label,
    formals: Vec<Access>,
}

impl Frame for X86 {
    fn new(name: Label, formals: &[bool]) -> Self {
       let mut frame =  X86 {
            name,
            formals:vec![]
        };
       
        // let mut access_formals = Vec::new();

        // for formal in formals {
        //     access_formals.push(frame.alloc_local(*formal))
        // }

        // frame.formals = access_formals;

        // frame
        unimplemented!()
    }

    // fn name(&self) -> Label;

    fn formals(&self) -> Vec<Access> {
        self.formals.clone()
    }

    fn alloc_local(&mut self, escape: bool) -> Access {
        unimplemented!()
        // if escape {
        //     unimplemented!()
        // } else {
        //     self.local_count += 1;

        //     let access = Access::Frame(self.frame_offset);

        //     self.frame_offset -= 8;

        //     access
        // }
    }
}


impl X86 {
    fn to_formal(escape:bool) -> Access {
        if escape {
            Access::Frame(0)
        } else {
            Access::Reg(Temp::new())
        }
    }
}