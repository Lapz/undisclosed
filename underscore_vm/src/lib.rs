#![feature(trace_macros)]
#![feature(log_syntax)]

#[macro_use]
mod chunk;

mod op;
#[macro_use]
mod vm;

pub use chunk::Chunk;
pub use vm::VM;
