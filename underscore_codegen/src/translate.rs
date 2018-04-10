use frame::Frame;
use std::cell::RefCell;
use std::rc::Rc;
use temp::Label;
use x86::{x86, Access};

#[derive(Clone, Debug)]
pub enum Level {
    Top,
    Level {
        parent: Rc<RefCell<Level>>,
        frame: x86,
    },
}

pub type TranslateAccess = (Level, Option<Access>);

pub static OUTER_LEVEL: u32 = 0;

pub struct Translator {}

impl Translator {
    pub fn new_level(parent: Level, name: Label, formals: &mut Vec<bool>) -> Level {
        formals.insert(0, true);
        Level::Level {
            parent: Rc::new(RefCell::new(parent)),
            frame: Frame::new(name, formals),
        }
    }

    pub fn formals(level: Level) -> Vec<TranslateAccess> {
        match level {
            Level::Top => vec![],
            Level::Level { ref frame, .. } => unimplemented!(),
        }
    }

    pub fn alloc_local(level: &mut Level, escape: bool) -> TranslateAccess {
        match *level {
            Level::Top => (Level::Top, None),
            Level::Level { ref frame, .. } => (level.clone(), Some(frame.alloc_local(escape))),
        }
    }
}
