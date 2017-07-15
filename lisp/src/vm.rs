//! If you use enough force, you can actually use this GC to implement a toy VM.

use builtins::{self, BuiltinFnPtr};
use cell_gc::{GcLeaf, GcHeapSession};
use cell_gc::collections::VecRef;
use parser;
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
    Lambda(PairRef<'h>),
    Builtin(GcLeaf<BuiltinFnPtr>),
    Cons(PairRef<'h>),
    Vector(VecRef<'h, Value<'h>>),
    Environment(EnvironmentRef<'h>),
}

pub use self::Value::*;

impl<'h> Value<'h> {
    fn null(&self) -> bool {
        match *self {
            Nil => true,
            _ => false
        }
    }

    pub fn is_boolean(&self) -> bool {
        match *self {
            Bool(_) => true,
            _ => false,
        }
    }

    /// True unless this value is `#f`. Conditional expressions (`if`, `cond`,
    /// etc.) should use this to check whether a value is a "true value".
    pub fn to_bool(&self) -> bool {
        match *self {
            Bool(false) => false,
            _ => true
        }
    }

    pub fn as_index(self, error_msg: &str) -> Result<usize, String> {
        match self {
            Int(i) =>
                if i >= 0 {
                    Ok(i as usize)
                } else {
                    Err(format!("{}: negative vector index", error_msg))
                },
            _ => Err(format!("{}: vector index required", error_msg))
        }
    }

    pub fn is_pair(&self) -> bool {
        match *self {
            Cons(_) => true,
            _ => false
        }
    }

    pub fn is_vector(&self) -> bool {
        match *self {
            Vector(_) => true,
            _ => false
        }
    }

    pub fn as_vector(self, error_msg: &str) -> Result<VecRef<'h, Value<'h>>, String> {
        match self {
            Vector(v) => Ok(v),
            _ => Err(format!("{}: vector expected", error_msg))
        }
    }

    fn as_symbol(self, error_msg: &str) -> Result<Arc<String>, String> {
        match self {
            Symbol(s) => Ok(s),
            _ => Err(error_msg.to_string())
        }
    }
}

#[derive(Clone, Debug, PartialEq, IntoHeap)]
pub struct Environment<'h> {
    parent: Option<EnvironmentRef<'h>>,
    names: VecRef<'h, Arc<String>>,
    values: VecRef<'h, Value<'h>>,
}

impl<'h> Environment<'h> {
    pub fn default_env(hs: &mut GcHeapSession<'h>) -> EnvironmentRef<'h> {
        let env = Environment {
            parent: None,
            names: hs.alloc(vec![]),
            values: hs.alloc(vec![])
        };

        macro_rules! builtin {
            ( $lisp_ident:expr , $func:expr ) => {
                env.names.push(Arc::new($lisp_ident.to_string()));
                env.values.push(Builtin(GcLeaf::new(BuiltinFnPtr($func))));
            }
        }

        builtin!("+", builtins::add);
        builtin!("-", builtins::sub);
        builtin!("*", builtins::mul);
        builtin!("assert", builtins::assert);
        builtin!("car", builtins::car);
        builtin!("cdr", builtins::cdr);
        builtin!("cons", builtins::cons);
        builtin!("eqv?", builtins::eqv_question);
        builtin!("eq?", builtins::eq_question);
        builtin!("print", builtins::print);
        builtin!("boolean?", builtins::boolean_question);
        builtin!("pair?", builtins::pair_question);
        builtin!("vector?", builtins::vector_question);
        builtin!("vector", builtins::vector);
        builtin!("vector-length", builtins::vector_length);
        builtin!("vector-ref", builtins::vector_ref);

        const PRELUDE: &'static str = include_str!("prelude.sch");
        let prelude = match parser::parse(hs, PRELUDE) {
            Ok(forms) => forms,
            Err(err) => panic!("unexpected error parsing prelude: {}", err)
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
    pub fn push(&self, key: Arc<String>, value: Value<'h>) {
        self.names().push(key);
        self.values().push(value);
    }

    pub fn lookup(&self, name: Arc<String>) -> Result<(EnvironmentRef<'h>, usize), String> {
        let mut next = Some(self.clone());
        while let Some(env) = next {
            let names = env.names();
            for i in (0..names.len()).rev() {
                if name == names.get(i) {
                    return Ok((env, i));
                }
            }
            next = env.parent();
        }
        Err(format!("undefined symbol: {:?}", *name))
    }

    pub fn get(&self, name: Arc<String>) -> Result<Value<'h>, String> {
        let (env, i) = self.lookup(name)?;
        Ok(env.values().get(i))
    }

    pub fn set(&self, name: Arc<String>, value: Value<'h>) -> Result<(), String> {
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

fn eval_each<'h, F>(
    hs: &mut GcHeapSession<'h>,
    mut exprs: Value<'h>,
    env: EnvironmentRef<'h>,
    mut f: F,
) -> Result<(), String>
    where F: FnMut(&mut GcHeapSession<'h>, Value<'h>) -> Result<(), String>
{
    while let Cons(pair) = exprs {
        let val = eval(hs, pair.car(), env.clone())?;
        f(hs, val)?;
        exprs = pair.cdr();
    }
    match exprs {
        Nil => Ok(()),
        _ => Err("improper list of expressions".to_string())
    }
}

fn map_eval<'h>(
    hs: &mut GcHeapSession<'h>,
    exprs: Value<'h>,
    env: EnvironmentRef<'h>,
) -> Result<Vec<Value<'h>>, String> {
    let mut v = vec![];
    eval_each(hs, exprs, env, |_hs, val| { v.push(val); Ok(()) })?;
    Ok(v)
}

pub fn eval_block_body<'h>(
    hs: &mut GcHeapSession<'h>,
    exprs: Value<'h>,
    env: EnvironmentRef<'h>,
) -> Result<Value<'h>, String> {
    let mut v = Nil;
    let _ = eval_each(hs, exprs, env, |_hs, val| { v = val; Ok(()) })?;
    Ok(v)
}

fn apply<'h>(
    hs: &mut GcHeapSession<'h>,
    fval: Value<'h>,
    args: Vec<Value<'h>>,
) -> Result<Value<'h>, String> {
    match fval {
        Builtin(f) => (f.0)(hs, args),
        Lambda(pair) => {
            let parent = Some(
                match pair.cdr() {
                    Environment(pe) => pe,
                    _ => panic!("internal error: bad lambda")
                }
            );
            let (mut params, body) = parse_pair(pair.car(), "syntax error in lambda")?;
            let names = hs.alloc(Vec::<Arc<String>>::new());
            let values = hs.alloc(Vec::<Value<'h>>::new());
            let env = hs.alloc(Environment { parent, names, values });

            let mut i = 0;
            while let Cons(pair) = params {
                if i > args.len() {
                    return Err("apply: not enough arguments".to_string());
                }
                if let Symbol(s) = pair.car() {
                    env.push(s, args[i].clone());
                } else {
                    return Err("syntax error in lambda arguments".to_string());
                }
                params = pair.cdr();
                i += 1;
            }
            if i < args.len() {
                return Err("apply: too many arguments".to_string());
            }
            let result = eval_block_body(hs, body, env)?;
            Ok(result)
        }
        _ => Err("apply: not a function".to_string()),
    }
}

/// Evaluate the give expression in the given environment, and return the
/// resulting value and new environment.
pub fn eval<'h>(
    hs: &mut GcHeapSession<'h>,
    expr: Value<'h>,
    env: EnvironmentRef<'h>,
) -> Result<Value<'h>, String> {
    match expr {
        Symbol(s) => env.get(s),
        Cons(p) => {
            let f = p.car();
            if let Symbol(ref s) = f {
                if &**s == "lambda" {
                    return Ok(
                        Lambda(hs.alloc(Pair {
                            car: p.cdr(),
                            cdr: Value::Environment(env.clone()),
                        }))
                    );
                } else if &**s == "quote" {
                    let (datum, rest) = parse_pair(p.cdr(), "(quote) with no arguments")?;
                    if !rest.null() {
                        return Err("too many arguments to (quote)".to_string());
                    }
                    return Ok(datum);
                } else if &**s == "if" {
                    let (cond, rest) = parse_pair(p.cdr(), "(if) with no arguments")?;
                    let (t_expr, rest) = parse_pair(rest, "missing arguments after (if COND)")?;
                    let (f_expr, rest) =
                        parse_pair(rest, "missing 'else' argument after (if COND X)")?;
                    if !rest.null() {
                        return Err("too many arguments in (if) expression".to_string());
                    }
                    let cond_result = eval(hs, cond, env.clone())?;
                    let selected_expr = if cond_result.to_bool() { t_expr } else { f_expr };
                    return eval(hs, selected_expr, env);
                } else if &**s == "begin" {
                    let result = eval_block_body(hs, p.cdr(), env.clone())?;
                    return Ok(result);
                } else if &**s == "define" {
                    let (name, rest) = parse_pair(p.cdr(), "(define) with no name")?;
                    match name {
                        Symbol(s) => {
                            let (expr, rest) = parse_pair(rest, "(define) with no value")?;
                            match rest {
                                Nil => {}
                                _ => return Err("too many items in (define) special form".to_string()),
                            };

                            let val = eval(hs, expr, env.clone())?;
                            env.push(s, val);
                            return Ok(Nil);
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
                            env.push(name, f);
                            return Ok(Nil);
                        }
                        _ => {
                            return Err("(define) with a non-symbol name".to_string());
                        }
                    }
                } else if &**s == "set!" {
                    let (first, rest) = parse_pair(p.cdr(), "(set!) with no name")?;
                    let name = first.as_symbol("(set!) first argument must be a name")?;
                    let (expr, rest) = parse_pair(rest, "(set!) with no value")?;
                    if !rest.null() {
                        return Err("(set!): too many arguments".to_string());
                    }
                    let val = eval(hs, expr, env.clone())?;
                    env.set(name, val)?;
                    return Ok(Nil);
                }
            }
            let fval = eval(hs, f, env.clone())?;
            let args = map_eval(hs, p.cdr(), env)?;
            apply(hs, fval, args)
        }
        Builtin(_) => Err(format!("builtin function found in source code")),
        Vector(_) => Err(format!("vectors are not expressions")),
        _ => Ok(expr),  // nil and numbers are self-evaluating
    }
}

#[cfg(test)]
include!("tests.rs");
