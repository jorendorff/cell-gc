extern crate cell_gc;
#[macro_use]
extern crate cell_gc_derive;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate nom;
#[macro_use]
extern crate error_chain;
extern crate rustyline;

pub mod errors {
    error_chain!{}
}

pub mod builtins;
mod compile;
pub mod parse;
pub mod repl;
pub mod value;
pub mod vm;
