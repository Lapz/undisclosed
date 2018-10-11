extern crate undisclosed_syntax as syntax;
extern crate undisclosed_util as util;

mod cfg;
pub mod ir;
pub mod tac;
pub mod optimize;
pub mod printer;
mod register;
mod scopes;
mod temp;
mod translate;


pub use temp::{Label, Temp};
// pub use cfg::construct_cfg;
pub use register::Register;
pub use scopes::Scopes;
