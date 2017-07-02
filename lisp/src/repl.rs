use cell_gc;
use parser::parse;
use print::print;
use std::io::{self, Write};
use vm;

pub fn repl() -> io::Result<()> {
    cell_gc::with_heap(|hs| {
        let mut env = vm::Environment::default_env(hs);

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
            let expr = parse(hs, &source)
                .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;

            // Eval
            let (val, new_env) = vm::eval(hs, expr, env)
                .map_err(|e| io::Error::new(io::ErrorKind::Other, e))?;
            env = new_env;

            // Print
            print(val);
            println!();

            // Loop...
        }

        // Unreachable...
    })
}
