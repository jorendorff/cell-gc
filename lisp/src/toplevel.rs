//! High-level pieces.

use builtins;
use cell_gc::{self, GcHeapSession, GcLeaf};
use compile;
use env::{Environment, EnvironmentRef};
use errors::*;
use parse;
use std::io::{self, Write};
use value::{BuiltinFnPtr, InternedString, Value};
use vm;

/// Create an environment that supports only core Scheme syntax.
///
/// It supports `(letrec)`, for example, but not `(let)`, `(and)`, or `(or)`.
pub fn core_env<'h>(hs: &mut GcHeapSession<'h>) -> EnvironmentRef<'h> {
    let env = Environment::empty(hs);

    builtins::define_builtins(hs, &env);

    const PRELUDE: &'static str = include_str!("prelude.scm");
    let _ = eval_str(hs, &env, PRELUDE).expect("unexpected error running the prelude");
    env
}

/// Create an environment that supports core syntax plus a handful of goodies
/// like `(let)`, `(and)`, and `(or)`. These are supported using simple-expander.scm
/// instead of psyntax.pp.
pub fn expanded_env<'h>(hs: &mut GcHeapSession<'h>) -> EnvironmentRef<'h> {
    let env = core_env(hs);

    const EXPANDER_CODE: &'static str = include_str!("simple-expander.scm");
    let xenv = env.new_nested_environment(hs);
    let expander = eval_str(hs, &xenv, EXPANDER_CODE)
        .expect("unexpected error initializing the simple expander");
    env.set_expander(expander);

    env
}

pub fn default_env<'h>(hs: &mut GcHeapSession<'h>) -> EnvironmentRef<'h> {
    let env = core_env(hs);

    const EXPANDER_CODE: &'static str = concat!(
        include_str!("psyntax-support.scm"),
        include_str!("psyntax.pp"),
        "\nsc-expand-toplevel\n"
    );
    let xenv = env.new_nested_environment(hs);
    xenv.push(
        InternedString::get("original-eval"),
        Value::Builtin(GcLeaf::new(BuiltinFnPtr(builtins::get_eval()))),
    );
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

pub fn eval<'h>(
    hs: &mut GcHeapSession<'h>,
    env: &EnvironmentRef<'h>,
    expr: Value<'h>,
) -> Result<Value<'h>> {
    let expr = env.expand(hs, expr)?;
    let expr = compile::compile_toplevel(hs, &env.senv(), expr)?;
    vm::eval_compiled(hs, env, expr)
}

/// Evaluate a toplevel script, possibly including many definitions and
/// expressions.
pub fn eval_toplevel_forms<'h>(
    hs: &mut GcHeapSession<'h>,
    env: &EnvironmentRef<'h>,
    forms: Vec<Value<'h>>,
) -> Result<Value<'h>> {
    // To help compilation bind as many identifiers statically as possible, we
    // detect infallible definitions here and create the defined binding before
    // compiling any code.
    //
    // As long as env is a global environment, this is a pure optimization
    // (with one caveat, see next paragraph). The only reason we cannot simply
    // predefine *all* bindings is that a runtime error or compilation error
    // may occur.
    //
    // Caveat: We assume that macro expansion never produces code that will
    // fail to compile. If it does, we can err by expanding code that we should
    // have errored out before reaching; this is a problem because expanding
    // code can have the side effect of defining macros (or I guess modules).

    let senv = env.senv();
    let mut result = Value::Unspecified;
    let mut iter = forms.into_iter().peekable();
    while iter.peek().is_some() {
        let mut batch = vec![];
        for form in &mut iter {
            let form = env.expand(hs, form)?;
            let infallible = compile::toplevel_form_is_infallible(hs, &senv, &form);
            batch.push(form.clone());
            if infallible {
                if let Some(name) = compile::toplevel_form_defined_name(hs, &form) {
                    env.define(name, Value::Unspecified);
                }
            } else {
                break;
            }
        }
        let code = compile::compile_toplevel_forms(hs, &env.senv(), batch)?;
        result = vm::eval_compiled(hs, &env, code)?;
    }
    Ok(result)
}

pub fn eval_str<'h>(
    hs: &mut GcHeapSession<'h>,
    env: &EnvironmentRef<'h>,
    code: &str,
) -> Result<Value<'h>> {
    let forms = parse::parse(hs, code)?;
    eval_toplevel_forms(hs, env, forms)
}

pub fn repl() -> Result<()> {
    cell_gc::with_heap(|hs| {
        let env = default_env(hs);

        loop {
            print!("lisp> ");
            io::stdout().flush()
                .chain_err(|| "error writing to stdout")?;

            // Read
            let mut source = String::new();
            io::stdin().read_line(&mut source)
                .chain_err(|| "error reading stdin")?;
            if source.is_empty() {
                break;
            }
            let forms = parse::parse(hs, &source)
                .chain_err(|| "parse error")?;

            // Eval
            let result = eval_toplevel_forms(hs, &env, forms)?;

            // Print
            if !result.is_unspecified() {
                println!("{}", result);
            }

            // Loop...
        }

        Ok(())
    })
}

#[cfg(test)]
use value::RetType;

#[cfg(test)]
#[allow(dead_code)]
enum Env {
    Core,
    Expanded,
    Full
}

#[cfg(test)]
fn test_eval<R: for<'h> RetType<'h>>(env_type: Env, program: &str, expected: R) {
    use cell_gc::GcHeap;
    use vm::Trampoline;

    let mut heap = GcHeap::new();
    heap.enter(|hs| {
        let env = match env_type {
            Env::Core => core_env(hs),
            Env::Expanded => expanded_env(hs),
            Env::Full => default_env(hs),
        };
        let v = match expected.pack(hs).expect("expected return value should pack OK") {
            Trampoline::Value(v) => v,
            _ => panic!("packing expected return value produced unexpected tail call"),
        };
        let result = eval_str(hs, &env, program).expect("Should eval OK");
        assert_eq!(result, v);
    });
}

#[test]
fn test_2p2e4() {
    test_eval(Env::Core, r"(+ 2 2)", 4);
}

#[test]
fn test_simple_lambda() {
    test_eval(Env::Core, r"((lambda (x y z) (+ x (+ y z))) 3 4 5)", 12);
}

#[test]
fn test_list() {
    test_eval(Env::Core, r#"(equal? "abcd" (apply string (list #\a #\b #\c #\d)))"#, true);
}

#[test]
fn test_simple_lambda_with_psyntax() {
    test_eval(Env::Full, "((lambda (x y z) (+ x (+ y z))) 3 4 5)", 12);
}
