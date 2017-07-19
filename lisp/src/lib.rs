extern crate cell_gc;
#[macro_use]
extern crate cell_gc_derive;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate nom;

pub mod builtins;
mod compile;
pub mod parser;
pub mod repl;
pub mod value;
pub mod vm;
