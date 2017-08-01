//! High-level pieces.

use builtins;
use cell_gc::{self, GcHeapSession};
use compile;
use env::{Environment, EnvironmentRef};
use errors::Result;
use parse;
use std::io::{self, Write};
use value::{InternedString, Value};
use vm::{self, Trampoline};


pub fn default_env<'h>(hs: &mut GcHeapSession<'h>) -> EnvironmentRef<'h> {
    let env = Environment::empty(hs);

    builtins::define_builtins(hs, &env);

    const PRELUDE: &'static str = include_str!("prelude.scm");
    let _ = eval_str(hs, &env, PRELUDE).expect("unexpected error running the prelude");

    const EXPANDER_CODE: &'static str = concat!(
        include_str!("psyntax-support.scm"),
        include_str!("psyntax.pp"),
        "\nsc-expand\n"
    );
    let xenv = env.new_nested_environment(hs);
    xenv.push(
        InternedString::get("psyntax-environment"),
        Value::Environment(xenv.clone()),
    );
    xenv.debug_assert_consistent();

    let expander = eval_str(hs, &xenv, EXPANDER_CODE)
        .expect("unexpected error initializing the expander");
    env.set_expander(expander);

    env
}

pub fn eval_to_tail_call<'h>(
    hs: &mut GcHeapSession<'h>,
    env: &EnvironmentRef<'h>,
    expr: Value<'h>,
) -> Result<Trampoline<'h>> {
    let expr = env.expand(hs, expr)?;
    let expr = compile::compile_toplevel(hs, &env.senv(), expr)?;
    vm::eval_compiled_to_tail_call(hs, env, expr)
}

pub fn eval<'h>(
    hs: &mut GcHeapSession<'h>,
    env: &EnvironmentRef<'h>,
    expr: Value<'h>,
) -> Result<Value<'h>> {
    eval_to_tail_call(hs, env, expr)?.eval(hs)
}

pub fn eval_str<'h>(
    hs: &mut GcHeapSession<'h>,
    env: &EnvironmentRef<'h>,
    code: &str,
) -> Result<Value<'h>> {
    let forms = parse::parse(hs, code)?;

    let mut result = Value::Nil;
    for form in forms {
        result = eval(hs, env, form)?;
    }
    Ok(result)
}

pub fn repl() -> io::Result<()> {
    cell_gc::with_heap(|hs| {
        let env = default_env(hs);

        loop {
            print!("lisp> ");
            io::stdout().flush()?;

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
                let val = eval(hs, &env, expr)
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

#[test]
fn add_in_lambda() {
    use cell_gc::GcHeap;

    let mut heap = GcHeap::new();
    heap.enter(|hs| {
        let env = default_env(hs);
        let program = "((lambda (x y z) (+ x (+ y z))) 3 4 5)";
        let result = eval_str(hs, &env, program).expect("Should eval OK");
        assert_eq!(result, Value::Int(12));
    });
}
