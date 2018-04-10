use frame::Frame;
use x86::{x86,Access};
use temp::Label;
use std::marker::PhantomData;

#[derive(Clone,Debug)]
pub enum Level {
    Top,
    Level {
        parent:Box<Level>,
        frame:x86
    }
}

type TranslateAccess= (Level,Access);


pub static OUTER_LEVEL: u32 = 0;

pub struct Translator {

}

impl  Translator  {
    pub fn new_level(parent:Level,name:Label,formals:Vec<bool>)  -> Level {
        Level::Level{
            parent:Box::new(parent),
            frame:Frame::new(name,&formals)
        }
    }

    pub fn formals(level:Level) -> Vec<TranslateAccess> {
        unimplemented!()
    }

    pub fn alloc_local(level:Level,escape:bool,frame:x86) -> TranslateAccess {

     match level {
         Level::Top => unreachable!(),
         Level::Level {
             ref frame,
             ref parent
         } => {
             (level.clone(),frame.alloc_local(escape))
         }
     }
       
    }
}