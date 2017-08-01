extern crate cell_gc;
#[macro_use]
extern crate cell_gc_derive;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate nom;
#[macro_use]
extern crate error_chain;

#[allow(unused_doc_comment)] // silence new warning in Nightly
pub mod errors {
    error_chain!{}
}

pub mod value;

pub mod parse;
mod compile;
mod env;
pub mod vm;

mod builtins;
pub mod toplevel;
