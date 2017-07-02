extern crate asexp;
extern crate cell_gc;
#[macro_use]
extern crate cell_gc_derive;

use std::io::{self, Write};
use std::process;
use std::sync::Arc;

pub mod parser;
pub mod vm;

fn main() {
    if let Err(e) = repl() {
        println!("Error: {}", e);
        process::exit(1);
    }
}

fn repl() -> io::Result<()> {
    cell_gc::with_heap(|hs| {
        let mut env = vm::Nil;
        env.push_env(
            hs,
            Arc::new("+".to_string()),
            vm::Builtin(cell_gc::GcLeaf::new(vm::BuiltinFnPtr(vm::add))),
        );

        loop {
            {
                let stdout = io::stdout();
                let mut stdout = stdout.lock();
                write!(&mut stdout, "lisp> ")?;
                stdout.flush()?;
            }

            // Read
            let mut source = String::new();
            io::stdin().read_line(&mut source)?;
            let expr = parser::parse(hs, &source)
                .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;

            // Eval
            let result = vm::eval(hs, expr, &env);

            // Print
            println!("{:?}", result);

            // Loop...
        }

        // Unreachable...
    })
}
