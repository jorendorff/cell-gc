use cell_gc;
use parse;
use std::io::{self, Write};
use value::Value;
use vm;

pub fn repl() -> io::Result<()> {
    cell_gc::with_heap(|hs| {
        let env = vm::Environment::default_env(hs);

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
            if source.is_empty() {
                break;
            }
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

            // Loop...
        }

        Ok(())
    })
}
