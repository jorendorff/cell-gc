extern crate cell_gc;
extern crate lisp;

use lisp::toplevel;
use std::env;
use std::fs;
use std::io::{self, Read};
use std::process;

fn main() {
    if let Err(e) = try_main() {
        println!("Error: {}", e);
        process::exit(1);
    }
}

fn try_main() -> io::Result<()> {
    if env::args().count() == 1 {
        return toplevel::repl();
    }

    cell_gc::with_heap(|hs| {
        let env = toplevel::default_env(hs);

        let mut source = String::new();
        for file in env::args().skip(1) {
            let mut file = fs::File::open(file)?;
            file.read_to_string(&mut source)?;

            if let Err(e) = toplevel::eval_str(hs, &env, &source) {
                return Err(io::Error::new(io::ErrorKind::Other, e.to_string()));
            }
            source.clear();
        }

        Ok(())
    })
}
