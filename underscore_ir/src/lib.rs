extern crate underscore_syntax as syntax;
extern crate underscore_util as util;

mod frame;
pub mod ir;
pub mod optimize;
mod temp;
mod translate;

pub use frame::Frame;
pub use ir::{BinOp, CmpOp, Expr, Function, Ir, Program, Stm, UnOp};
pub use temp::{Label, Temp};

#[derive(Clone)]
pub struct Level<T: Clone + Frame> {
    pub parent: Option<Box<Level<T>>>,
    pub frame: T,
}

impl<T: Clone + Frame> Level<T> {
    pub fn top() -> Self {
        Self {
            parent: None,
            frame: T::new(Label::new(), &vec![]),
        }
    }

    pub fn new(parent: Level<T>, name: Label, formals: &mut Vec<bool>) -> Level<T> {
        formals.insert(0, true);

        Self {
            parent: Some(Box::new(parent)),
            frame: T::new(name, formals),
        }
    }
}
