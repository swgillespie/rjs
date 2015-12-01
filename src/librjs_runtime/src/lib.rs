//#![deny(warnings)]

extern crate librjs_syntax;
extern crate bit_set;
extern crate bit_vec;
#[macro_use]
extern crate log;

pub mod heap;
pub mod values;
pub mod exec;
pub mod compiler;
