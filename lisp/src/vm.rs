//! If you use enough force, you can actually use this GC to implement a toy VM.

use builtins::{self, BuiltinFnPtr};
use cell_gc::{GcLeaf, HeapSession};
use std::sync::Arc;

#[derive(Debug, IntoHeap)]
pub struct Pair<'h> {
    pub car: Value<'h>,
    pub cdr: Value<'h>,
}

#[derive(Clone, Debug, PartialEq, IntoHeap)]
pub enum Value<'h> {
    Nil,
    Bool(bool),
    Int(i32),
    Symbol(Arc<String>),
    Cons(PairRef<'h>),
    Lambda(PairRef<'h>),
    Builtin(GcLeaf<BuiltinFnPtr>),
}

pub use self::Value::*;

impl<'h> Value<'h> {
    pub fn default_env(hs: &mut HeapSession<'h>) -> Value<'h> {
        let mut env = Nil;

        macro_rules! builtin {
            ( $lisp_ident:expr , $func:expr ) => {
                env.push_env(hs,
                             Arc::new($lisp_ident.to_string()),
                             Builtin(GcLeaf::new(BuiltinFnPtr($func))));
            }
        }

        builtin!("+", builtins::add);
        builtin!("-", builtins::sub);
        builtin!("*", builtins::mul);
        builtin!("assert", builtins::assert);
        builtin!("eq?", builtins::eq_question);
        builtin!("print", builtins::print);

        env
    }
}

impl<'h> Value<'h> {
    pub fn push_env(&mut self, hs: &mut HeapSession<'h>, key: Arc<String>, value: Value<'h>) {
        let pair = Cons(hs.alloc(Pair {
            car: Symbol(key),
            cdr: value,
        }));
        *self = Cons(hs.alloc(Pair {
            car: pair,
            cdr: self.clone(),
        }));
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
                Symbol(Arc::new(s.to_string()))
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

fn lookup<'h>(mut env: Value<'h>, name: Arc<String>) -> Result<Value<'h>, String> {
    let v = Symbol(name.clone());
    while let Cons(p) = env {
        let (key, value) = parse_pair(p.car(), "internal error: bad environment structure")?;
        if key == v {
            return Ok(value);
        }
        env = p.cdr();
    }
    Err(format!("undefined symbol: {:?}", *name))
}

fn map_eval<'h>(
    hs: &mut HeapSession<'h>,
    mut exprs: Value<'h>,
    env: &Value<'h>,
) -> Result<Vec<Value<'h>>, String> {
    let mut v = vec![];
    while let Cons(pair) = exprs {
        v.push(eval(hs, pair.car(), env)?);
        exprs = pair.cdr();
    }
    Ok(v)
}

fn apply<'h>(
    hs: &mut HeapSession<'h>,
    fval: Value<'h>,
    args: Vec<Value<'h>>,
) -> Result<Value<'h>, String> {
    match fval {
        Builtin(f) => (f.0)(args),
        Lambda(pair) => {
            let mut env = pair.cdr();
            let (mut params, rest) = parse_pair(pair.car(), "syntax error in lambda")?;
            let (body, rest) = parse_pair(rest, "syntax error in lambda")?;
            if rest != Nil {
                return Err("syntax error in lambda".to_string());
            }

            let mut i = 0;
            while let Cons(pair) = params {
                if i > args.len() {
                    return Err("apply: not enough arguments".to_string());
                }
                if let Symbol(s) = pair.car() {
                    let pair = Cons(hs.alloc(Pair {
                        car: Symbol(s),
                        cdr: args[i].clone(),
                    }));
                    env = Cons(hs.alloc(Pair {
                        car: pair,
                        cdr: env,
                    }));
                } else {
                    return Err("syntax error in lambda arguments".to_string());
                }
                params = pair.cdr();
                i += 1;
            }
            if i < args.len() {
                return Err("apply: too many arguments".to_string());
            }
            eval(hs, body, &env)
        }
        _ => Err("apply: not a function".to_string()),
    }
}

pub fn eval<'h>(
    hs: &mut HeapSession<'h>,
    expr: Value<'h>,
    env: &Value<'h>,
) -> Result<Value<'h>, String> {
    match expr {
        Symbol(s) => lookup(env.clone(), s),
        Cons(p) => {
            let f = p.car();
            if let Symbol(ref s) = f {
                if &**s == "lambda" {
                    return Ok(Lambda(hs.alloc(Pair {
                        car: p.cdr(),
                        cdr: env.clone(),
                    })));
                } else if &**s == "if" {
                    let (cond, rest) = parse_pair(p.cdr(), "(if) with no arguments")?;
                    let (t_expr, rest) = parse_pair(rest, "missing arguments after (if COND)")?;
                    let (f_expr, rest) =
                        parse_pair(rest, "missing 'else' argument after (if COND X)")?;
                    match rest {
                        Nil => {}
                        _ => return Err("too many arguments in (if) expression".to_string()),
                    };
                    let cond_result = eval(hs, cond, env)?;
                    let selected_expr = if cond_result == Nil || cond_result == Bool(false) {
                        f_expr
                    } else {
                        t_expr
                    };
                    return eval(hs, selected_expr, env);
                }
            }
            let fval = eval(hs, f, env)?;
            let args = map_eval(hs, p.cdr(), env)?;
            apply(hs, fval, args)
        }
        Builtin(_) => Err(format!("builtin function found in source code")),
        _ => Ok(expr),  // nil and numbers are self-evaluating
    }
}

#[cfg(test)]
include!("tests.rs");
