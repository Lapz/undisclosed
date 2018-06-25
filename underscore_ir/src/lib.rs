extern crate underscore_syntax as syntax;
extern crate underscore_util as util;

mod cfg;
pub mod ir;
pub mod optimize;
mod register;
mod temp;
mod translate;
mod scopes;

pub use temp::{Label, Temp};
// pub use cfg::construct_cfg;
pub use scopes::Scopes;


