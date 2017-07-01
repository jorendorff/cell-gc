use cell_gc;
use parser::parse;
use std::io::{self, Write};
use vm;

pub fn repl() -> io::Result<()> {
    cell_gc::with_heap(|hs| {
        let env = vm::Value::default_env(hs);

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
            let result = vm::eval(hs, expr, &env);

            // Print
            println!("{:?}", result);

            // Loop...
        }

        // Unreachable...
    })
}
