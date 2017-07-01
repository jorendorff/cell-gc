extern crate lisp;

use lisp::repl::repl;
use std::process;

fn main() {
    if let Err(e) = repl() {
        println!("Error: {}", e);
        process::exit(1);
    }
}
