use cell_gc;
use parse;
use std::io;
use value::Value;
use vm;

use rustyline;
use rustyline::error::ReadlineError;

pub fn repl() -> io::Result<()> {
    let mut rl = rustyline::Editor::<()>::new();

    cell_gc::with_heap(|hs| {
        let env = vm::Environment::default_env(hs);

        loop {
            // Read
            match rl.readline("rllisp> ") {
                Ok(source) => {
                    rl.add_history_entry(&source);
                    let exprs = parse::parse(hs, &source)
                        .map_err(|e| io::Error::new(io::ErrorKind::Other, e.description()))?;


                    // Eval
                    let mut result = Value::Unspecified;
                    for expr in exprs {
                        let val = vm::eval(hs, expr, env.clone())
                            .map_err(|e| io::Error::new(io::ErrorKind::Other, e.description()))?;
                        result = val;
                    }

                    // Print
                    if !result.is_unspecified() {
                        println!("{}", result);
                    }
                },
                Err(ReadlineError::Eof) => return Ok(()),
                Err(ReadlineError::Interrupted) => {
                    println!("Interrupted.");
                },
                Err(other) => return Err(io::Error::new(io::ErrorKind::Other, other)),
            }

            // Loop...
        }
    })
}
