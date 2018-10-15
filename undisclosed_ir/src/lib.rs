extern crate undisclosed_syntax as syntax;
extern crate undisclosed_util as util;

pub mod asm;
pub mod ir;
pub mod optimize;
pub mod printer;
mod register;
mod scopes;
pub mod tac;
mod temp;
mod translate;

pub use temp::{Label, Temp};
// pub use cfg::construct_cfg;
pub use register::Register;
pub use scopes::Scopes;
