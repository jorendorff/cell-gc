use cell_gc;
use parse;
use prompt;
use std::io;
use value::Value;
use vm;

pub fn repl() -> io::Result<()> {
    let mut prompt = prompt::Prompt::new();

    cell_gc::with_heap(|hs| {
        let env = vm::Environment::default_env(hs);

        loop {
            // Read
            let source = match prompt.read_expr("lisp> ")? {
                None => return Ok(()),
                Some(source) => source
            };

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
    })
}
