extern crate lisp;

use lisp::toplevel;
use std::process;

fn main() {
    if let Err(e) = toplevel::repl() {
        println!("Error: {}", e);
        process::exit(1);
    }
}
