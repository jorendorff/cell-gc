//! If you use enough force, you can actually use this GC to implement a toy VM.

use builtins;
use cell_gc::{GcHeapSession, GcLeaf};
use cell_gc::collections::VecRef;
use parser;
use value::{BuiltinFnPtr, InternedString, Pair, Value};
use value::Value::*;

/// A potentially partially evaluated value.
pub enum Trampoline<'h> {
    /// A completely evaluated value.
    Value(Value<'h>),
    /// The continuation of a partial evaluation in tail position. The stack
    /// should be unwound before resumption of its evaluation.
    TailCall {
        func: Value<'h>,
        args: Vec<Value<'h>>,
    },
}

impl<'h> Trampoline<'h> {
    /// Complete the evaluation of this value. Avoids recursion to implement
    /// proper tail calls and keep from blowing the stack.
    pub fn eval(mut self, hs: &mut GcHeapSession<'h>) -> Result<Value<'h>, String> {
        while let Trampoline::TailCall { func, args } = self {
            self = apply(hs, func, args)?;
        }
        match self {
            Trampoline::Value(v) => Ok(v),
            Trampoline::TailCall { .. } => unreachable!(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, IntoHeap)]
pub struct Environment<'h> {
    parent: Option<EnvironmentRef<'h>>,
    names: VecRef<'h, GcLeaf<InternedString>>,
    values: VecRef<'h, Value<'h>>,
}

impl<'h> Environment<'h> {
    pub fn default_env(hs: &mut GcHeapSession<'h>) -> EnvironmentRef<'h> {
        let env = Environment {
            parent: None,
            names: hs.alloc(vec![]),
            values: hs.alloc(vec![]),
        };

        macro_rules! builtin {
            ( $lisp_ident:expr , $func:expr ) => {
                env.names.push(GcLeaf::new(InternedString::get($lisp_ident)));
                env.values.push(Builtin(GcLeaf::new(BuiltinFnPtr($func))));
            }
        }

        builtin!("+", builtins::add);
        builtin!("-", builtins::sub);
        builtin!("*", builtins::mul);
        builtin!("=", builtins::numeric_eq);
        builtin!("<", builtins::numeric_lt);
        builtin!(">", builtins::numeric_gt);
        builtin!("<=", builtins::numeric_le);
        builtin!(">=", builtins::numeric_ge);
        builtin!("apply", builtins::apply);
        builtin!("assert", builtins::assert);
        builtin!("car", builtins::car);
        builtin!("cdr", builtins::cdr);
        builtin!("cons", builtins::cons);
        builtin!("eqv?", builtins::eqv_question);
        builtin!("eq?", builtins::eq_question);
        builtin!("print", builtins::print);
        builtin!("boolean?", builtins::boolean_question);
        builtin!("null?", builtins::null_question);
        builtin!("pair?", builtins::pair_question);
        builtin!("procedure?", builtins::procedure_question);
        builtin!("vector?", builtins::vector_question);
        builtin!("vector", builtins::vector);
        builtin!("vector-length", builtins::vector_length);
        builtin!("vector-ref", builtins::vector_ref);

        const PRELUDE: &'static str = include_str!("prelude.sch");
        let prelude = match parser::parse(hs, PRELUDE) {
            Ok(forms) => forms,
            Err(err) => panic!("unexpected error parsing prelude: {}", err),
        };

        let env = hs.alloc(env);
        for expr in prelude {
            if let Err(err) = eval(hs, expr, env.clone()) {
                panic!("unexpected error evaluating prelude: {}", err);
            }
        }
        env
    }
}

impl<'h> EnvironmentRef<'h> {
    pub fn push(&self, key: InternedString, value: Value<'h>) {
        self.names().push(GcLeaf::new(key));
        self.values().push(value);
    }

    pub fn lookup(&self, name: InternedString) -> Result<(EnvironmentRef<'h>, usize), String> {
        let mut next = Some(self.clone());
        while let Some(env) = next {
            let names = env.names();
            for i in (0..names.len()).rev() {
                if name == *names.get(i) {
                    return Ok((env, i));
                }
            }
            next = env.parent();
        }
        Err(format!("undefined symbol: {:?}", name.as_str()))
    }

    pub fn get(&self, name: InternedString) -> Result<Value<'h>, String> {
        let (env, i) = self.lookup(name)?;
        Ok(env.values().get(i))
    }

    pub fn set(&self, name: InternedString, value: Value<'h>) -> Result<(), String> {
        let (env, i) = self.lookup(name)?;
        env.values().set(i, value);
        Ok(())
    }
}

#[macro_export]
macro_rules! lisp {
    { ( ) , $_hs:expr } => {
        Nil
    };
    { ( $h:tt $($t:tt)* ) , $hs:expr } => {
        {
            let h = lisp!($h, $hs);
            let t = lisp!(($($t)*), $hs);
            Cons($hs.alloc(Pair { car: h, cdr: t }))
        }
    };
    { $s:tt , $_hs:expr } => {
        {
            let s = stringify!($s);  // lame, but nothing else matches after an `ident` match fails
            if s.starts_with(|c: char| c.is_digit(10)) {
                Int(s.parse().expect("invalid numeric literal in `lisp!`"))
            } else {
                Symbol($crate::cell_gc::GcLeaf::new($crate::value::InternedString::get(s)))
            }
        }
    };
}

fn parse_pair<'h>(v: Value<'h>, msg: &'static str) -> Result<(Value<'h>, Value<'h>), String> {
    match v {
        Cons(r) => Ok((r.car(), r.cdr())),
        _ => Err(msg.to_string()),
    }
}

fn map_eval<'h>(
    hs: &mut GcHeapSession<'h>,
    exprs: Value<'h>,
    env: EnvironmentRef<'h>,
) -> Result<Vec<Value<'h>>, String> {
    exprs
        .into_iter()
        .map(|expr_res| eval(hs, expr_res?, env.clone()))
        .collect()
}

pub fn eval_block_body<'h>(
    hs: &mut GcHeapSession<'h>,
    exprs: Value<'h>,
    env: EnvironmentRef<'h>,
) -> Result<Trampoline<'h>, String> {
    let mut it = exprs.into_iter().peekable();
    while let Some(expr_res) = it.next() {
        let expr = expr_res?;
        if it.peek().is_some() {
            let _ = eval(hs, expr, env.clone());
        } else {
            return eval_to_tail_call(hs, expr, env);
        }
    }
    return Ok(Trampoline::Value(Nil));
}

pub fn apply<'h>(
    hs: &mut GcHeapSession<'h>,
    fval: Value<'h>,
    mut args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    match fval {
        Builtin(f) => (f.0)(hs, args),
        Lambda(pair) => {
            let parent = Some(match pair.cdr() {
                Environment(pe) => pe,
                _ => panic!("internal error: bad lambda"),
            });
            let (mut params, body) = parse_pair(pair.car(), "syntax error in lambda")?;
            let names = hs.alloc(Vec::<GcLeaf<InternedString>>::new());
            let values = hs.alloc(Vec::<Value<'h>>::new());
            let env = hs.alloc(Environment {
                parent,
                names,
                values,
            });

            let mut i = 0;
            while let Cons(pair) = params {
                if i > args.len() {
                    return Err("apply: not enough arguments".to_string());
                }
                if let Symbol(s) = pair.car() {
                    env.push(s.unwrap(), args[i].clone());
                } else {
                    return Err("syntax error in lambda arguments".to_string());
                }
                params = pair.cdr();
                i += 1;
            }
            if let Symbol(rest_name) = params {
                let mut rest_list = Nil;
                for v in args.drain(i..).rev() {
                    rest_list = Cons(hs.alloc(Pair {
                        car: v,
                        cdr: rest_list,
                    }));
                }
                env.push(rest_name.unwrap(), rest_list);
            } else if i < args.len() {
                return Err("apply: too many arguments".to_string());
            }
            let result = eval_block_body(hs, body, env)?;
            Ok(result)
        }
        _ => Err("apply: not a function".to_string()),
    }
}

/// Evaluate `expr` until we reach a tail call, at which point it is packaged up
/// as a `Trampoline::TailCall` and returned so we can unwind the stack before
/// continuing evaluation.
fn eval_to_tail_call<'h>(
    hs: &mut GcHeapSession<'h>,
    expr: Value<'h>,
    env: EnvironmentRef<'h>,
) -> Result<Trampoline<'h>, String> {
    match expr {
        Symbol(s) => env.get(s.unwrap()).map(Trampoline::Value),
        Cons(p) => {
            let f = p.car();
            if let Symbol(ref s) = f {
                if s.as_str() == "lambda" {
                    return Ok(Trampoline::Value(Lambda(hs.alloc(Pair {
                        car: p.cdr(),
                        cdr: Value::Environment(env.clone()),
                    }))));
                } else if s.as_str() == "quote" {
                    let (datum, rest) = parse_pair(p.cdr(), "(quote) with no arguments")?;
                    if !rest.is_nil() {
                        return Err("too many arguments to (quote)".to_string());
                    }
                    return Ok(Trampoline::Value(datum));
                } else if s.as_str() == "if" {
                    let (cond, rest) = parse_pair(p.cdr(), "(if) with no arguments")?;
                    let (t_expr, rest) = parse_pair(rest, "missing arguments after (if COND)")?;
                    let (f_expr, rest) =
                        parse_pair(rest, "missing 'else' argument after (if COND X)")?;
                    if !rest.is_nil() {
                        return Err("too many arguments in (if) expression".to_string());
                    }
                    let cond_result = eval(hs, cond, env.clone())?;
                    let selected_expr = if cond_result.to_bool() {
                        t_expr
                    } else {
                        f_expr
                    };
                    return eval_to_tail_call(hs, selected_expr, env);
                } else if s.as_str() == "begin" {
                    return eval_block_body(hs, p.cdr(), env.clone());
                } else if s.as_str() == "define" {
                    let (name, rest) = parse_pair(p.cdr(), "(define) with no name")?;
                    match name {
                        Symbol(s) => {
                            let (expr, rest) = parse_pair(rest, "(define) with no value")?;
                            match rest {
                                Nil => {}
                                _ => {
                                    return Err(
                                        "too many items in (define) special form".to_string(),
                                    )
                                }
                            };

                            let val = eval(hs, expr, env.clone())?;
                            env.push(s.unwrap(), val);
                            return Ok(Trampoline::Value(Nil));
                        }
                        Cons(pair) => {
                            let name = match pair.car() {
                                Symbol(s) => s,
                                _ => return Err("(define) with no name".to_string()),
                            };
                            let code = Cons(hs.alloc(Pair {
                                car: pair.cdr(), // formals
                                cdr: rest,
                            }));
                            let f = Lambda(hs.alloc(Pair {
                                car: code,
                                cdr: Value::Environment(env.clone()),
                            }));
                            env.push(name.unwrap(), f);
                            return Ok(Trampoline::Value(Nil));
                        }
                        _ => {
                            return Err("(define) with a non-symbol name".to_string());
                        }
                    }
                } else if s.as_str() == "set!" {
                    let (first, rest) = parse_pair(p.cdr(), "(set!) with no name")?;
                    let name = first.as_symbol("(set!) first argument must be a name")?;
                    let (expr, rest) = parse_pair(rest, "(set!) with no value")?;
                    if !rest.is_nil() {
                        return Err("(set!): too many arguments".to_string());
                    }
                    let val = eval(hs, expr, env.clone())?;
                    env.set(name, val)?;
                    return Ok(Trampoline::Value(Nil));
                }
            }
            let func = eval(hs, f, env.clone())?;
            let args = map_eval(hs, p.cdr(), env)?;
            Ok(Trampoline::TailCall { func, args })
        }
        Builtin(_) => Err(format!("builtin function found in source code")),
        Vector(_) => Err(format!("vectors are not expressions")),
        Nil => Err(format!("expected expression, got ()")),
        _ => Ok(Trampoline::Value(expr)),  // numbers are self-evaluating
    }
}

/// Evaluate the give expression in the given environment, and return the
/// resulting value.
pub fn eval<'h>(
    hs: &mut GcHeapSession<'h>,
    expr: Value<'h>,
    env: EnvironmentRef<'h>,
) -> Result<Value<'h>, String> {
    let tail = eval_to_tail_call(hs, expr, env)?;
    tail.eval(hs)
}

#[cfg(test)]
include!("tests.rs");
