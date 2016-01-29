//! If you use enough force, you can actually use this GC to implement a toy VM.

#[macro_use] extern crate cellgc;
use cellgc::{Heap, with_heap};
use std::rc::Rc;

gc_ref_type! {
    struct Pair / PairRef / PairStorage / PairRefStorage <'a> {
        car / set_car: Value<'a>,
        cdr / set_cdr: Value<'a>
    }
}

gc_inline_enum! {
    enum Value / ValueStorage <'a> {
        Nil,
        Int(i32),
        Symbol(Rc<String>),
        Cons(PairRef<'a>),
        Lambda(PairRef<'a>),
        Builtin(Box<BuiltinFnPtr>)
    }
}

use Value::*;

type BuiltinFnTrait = for<'b> fn (Vec<Value<'b>>) -> Result<Value<'b>, String>;

#[derive(Clone)]
struct BuiltinFnPtr(&'static BuiltinFnTrait);

impl PartialEq for BuiltinFnPtr {
    fn eq(&self, other: &BuiltinFnPtr) -> bool {
        *self.0 as usize == *other.0 as usize
    }
}

impl std::fmt::Debug for BuiltinFnPtr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "BuiltinFn({:p})", *self.0 as usize as *const ());
        Ok(())
    }
}

impl<'a> Value<'a> {
    fn push_env(&mut self, heap: &mut Heap<'a>, key: Rc<String>, value: Value<'a>) {
        let pair = Cons(heap.alloc(Pair {
            car: Symbol(key),
            cdr: value
        }));
        *self = Cons(heap.alloc(Pair {
            car: pair,
            cdr: self.clone()
        }));
    }
}

macro_rules! lisp {
    { ( ) , $_heap:expr } => {
        Nil
    };
    { ( $h:tt $($t:tt)* ) , $heap:expr } => {
        {
            let h = lisp!($h, $heap);
            let t = lisp!(($($t)*), $heap);
            Cons($heap.alloc(Pair { car: h, cdr: t }))
        }
    };
    { $s:tt , $_heap:expr } => {
        {
            let s = stringify!($s);  // lame, but nothing else matches after an `ident` match fails
            if s.starts_with(|c: char| c.is_digit(10)) {
                Int(s.parse().expect("invalid numeric literal in `lisp!`"))
            } else {
                Symbol(Rc::new(s.to_string()))
            }
        }
    };
}

fn parse_pair<'a>(v: Value<'a>, msg: &'static str) -> Result<(Value<'a>, Value<'a>), String> {
    match v {
        Cons(r) => Ok((r.car(), r.cdr())),
        _ => Err(msg.to_string())
    }
}

fn lookup<'a>(mut env: Value<'a>, name: Rc<String>) -> Result<Value<'a>, String> {
    let v = Symbol(name.clone());
    while let Cons(p) = env {
        let (key, value) = try!(parse_pair(p.car(), "internal error: bad environment structure"));
        if key == v {
            return Ok(value);
        }
        env = p.cdr();
    }
    Err(format!("undefined symbol: {:?}", *name))
}

fn map_eval<'a>(heap: &mut Heap<'a>, mut exprs: Value<'a>, env: &Value<'a>)
    -> Result<Vec<Value<'a>>, String>
{
    let mut v = vec![];
    while let Cons(pair) = exprs {
        v.push(try!(eval(heap, pair.car(), env)));
        exprs = pair.cdr();
    }
    Ok(v)
}

fn apply<'a>(heap: &mut Heap<'a>, fval: Value<'a>, args: Vec<Value<'a>>) -> Result<Value<'a>, String> {
    match fval {
        Builtin(f) => (*f.0)(args),
        Lambda(pair) => {
            let mut env = pair.cdr();
            let (mut params, rest) = try!(parse_pair(pair.car(), "syntax error in lambda"));
            let (body, rest) = try!(parse_pair(rest, "syntax error in lambda"));
            if rest != Nil {
                return Err("syntax error in lambda".to_string());
            }

            let mut i = 0;
            while let Cons(pair) = params {
                if i > args.len() {
                    return Err("apply: not enough arguments".to_string());
                }
                if let Symbol(s) = pair.car() {
                    let pair = Cons(heap.alloc(Pair {
                        car: Symbol(s),
                        cdr: args[i].clone()
                    }));
                    env = Cons(heap.alloc(Pair {
                        car: pair,
                        cdr: env
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
            eval(heap, body, &env)
        }
        _ => Err("apply: not a function".to_string())
    }
}

fn eval<'a>(heap: &mut Heap<'a>, expr: Value<'a>, env: &Value<'a>) -> Result<Value<'a>, String> {
    match expr {
        Symbol(s) => lookup(env.clone(), s),
        Cons(p) => {
            let f = p.car();
            if let Symbol(ref s) = f {
                if &**s == "lambda" {
                    return Ok(Lambda(heap.alloc(Pair {
                        car: p.cdr(),
                        cdr: env.clone()
                    })));
                } else if &**s == "if" {
                    let (cond, rest) = try!(parse_pair(p.cdr(), "(if) with no arguments"));
                    let (t_expr, rest) = try!(parse_pair(rest, "missing arguments after (if COND)"));
                    let (f_expr, rest) = try!(parse_pair(rest, "missing 'else' argument after (if COND X)"));
                    match rest {
                        Nil => {}
                        _ => return Err("too many arguments in (if) expression".to_string())
                    };
                    let cond_result = try!(eval(heap, cond, env));
                    let selected_expr = if cond_result == Nil { f_expr } else { t_expr };
                    return eval(heap, selected_expr, env);
                }
            }
            let fval = try!(eval(heap, f, env));
            let args = try!(map_eval(heap, p.cdr(), env));
            apply(heap, fval, args)
        }
        Builtin(_) => Err(format!("builtin function found in source code")),
        _ => Ok(expr)  // nil and numbers are self-evaluating
    }
}

fn add<'a>(args: Vec<Value<'a>>) -> Result<Value<'a>, String> {
    let mut total = 0;
    for v in args {
        if let Int(n) = v {
            total += n;
        } else {
            return Err("add: non-numeric argument".to_string());
        }
    }
    Ok(Int(total))
}

const add_ptr: &'static BuiltinFnTrait = &(add as BuiltinFnTrait);

fn main() {
    with_heap(|heap| {
        let mut env = Nil;
        env.push_env(heap, Rc::new("+".to_string()), Builtin(Box::new(BuiltinFnPtr(add_ptr))));
        let program = lisp!(
            ((lambda (x y z) (+ x (+ y z))) 3 4 5)
            , heap);
        let result = eval(heap, program, &env);
        assert_eq!(result, Ok(Int(12)));
    });
}
