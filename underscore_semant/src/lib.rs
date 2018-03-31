extern crate underscore_syntax as syntax;
extern crate underscore_util as util;

mod cast_check;
mod semant;
mod env;
mod trans;

pub use semant::Infer;
pub use env::Env as TypeEnv;
