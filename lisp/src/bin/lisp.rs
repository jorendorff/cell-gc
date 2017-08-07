extern crate cell_gc;
extern crate lisp;

use lisp::toplevel;
use lisp::errors::*;
use std::env;
use std::fs;
use std::io::Read;
use std::process;

fn main() {
    if let Err(e) = try_main() {
        println!("Error: {}", e);
        process::exit(1);
    }
}

fn try_main() -> Result<()> {
    if env::args().count() == 1 {
        return toplevel::repl();
    }

    cell_gc::with_heap(|hs| {
        let env = toplevel::default_env(hs);

        let mut source = String::new();
        for filename in env::args().skip(1) {
            let mut file = fs::File::open(&filename)
                .chain_err(|| format!("error opening file: {}", filename))?;
            file.read_to_string(&mut source)
                .chain_err(|| format!("error reading file: {}", filename))?;

            toplevel::eval_str(hs, &env, &source)?;
            source.clear();
        }

        Ok(())
    })
}
