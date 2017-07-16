use cell_gc;
use parser::parse;
use print::print;
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
            let exprs = parse(hs, &source)
                .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;

            // Eval
            let mut result = Value::Nil;
            for expr in exprs {
                let val = vm::eval(hs, expr, env.clone())
                    .map_err(|e| io::Error::new(io::ErrorKind::Other, e))?;
                result = val;
            }

            // Print
            print(result);
            println!();

            // Loop...
        }

        // Unreachable...
    })
}
