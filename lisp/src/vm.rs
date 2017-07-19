//! If you use enough force, you can actually use this GC to implement a toy VM.

use builtins;
use cell_gc::{GcHeapSession, GcLeaf};
use cell_gc::collections::VecRef;
use compile::{self, Expr};
use parse;
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
        builtin!("gensym", builtins::gensym);
        builtin!("gensym?", builtins::gensym_question);
        builtin!("print", builtins::print);
        builtin!("boolean?", builtins::boolean_question);
        builtin!("null?", builtins::null_question);
        builtin!("pair?", builtins::pair_question);
        builtin!("procedure?", builtins::procedure_question);
        builtin!("string->symbol", builtins::string_to_symbol);
        builtin!("string?", builtins::string_question);
        builtin!("string=?", builtins::string_eq_question);
        builtin!("symbol->string", builtins::symbol_to_string);
        builtin!("symbol?", builtins::symbol_question);
        builtin!("vector?", builtins::vector_question);
        builtin!("vector", builtins::vector);
        builtin!("vector-length", builtins::vector_length);
        builtin!("vector-ref", builtins::vector_ref);

        const PRELUDE: &'static str = include_str!("prelude.sch");
        let prelude = match parse::parse(hs, PRELUDE) {
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

pub fn apply<'h>(
    hs: &mut GcHeapSession<'h>,
    fval: Value<'h>,
    mut args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    match fval {
        Builtin(f) => (f.0)(hs, args),
        Lambda(pair) => {
            let code = match pair.car() {
                Code(code) => code,
                _ => panic!("internal error: bad lambda"),
            };
            let parent = Some(match pair.cdr() {
                Environment(pe) => pe,
                _ => panic!("internal error: bad lambda"),
            });
            let names = hs.alloc(Vec::<GcLeaf<InternedString>>::new());
            let values = hs.alloc(Vec::<Value<'h>>::new());
            let env = hs.alloc(Environment {
                parent,
                names,
                values,
            });

            let params = code.params();
            let n = params.len();
            if args.len() < n {
                return Err("apply: not enough arguments".to_string());
            }
            for i in 0..n {
                env.push(params.get(i).unwrap().clone().unwrap(), args[i].clone());
            }
            if let Some(rest_name) = code.rest() {
                let mut rest_list = Nil;
                for v in args.drain(n..).rev() {
                    rest_list = Cons(hs.alloc(Pair {
                        car: v,
                        cdr: rest_list,
                    }));
                }
                env.push(rest_name.unwrap(), rest_list);
            } else if n < args.len() {
                return Err("apply: too many arguments".to_string());
            }

            eval_to_tail_call(hs, code.body(), env)
        }
        _ => Err("apply: not a function".to_string()),
    }
}

/// Evaluate `expr` until we reach a tail call, at which point it is packaged up
/// as a `Trampoline::TailCall` and returned so we can unwind the stack before
/// continuing evaluation.
fn eval_to_tail_call<'h>(
    hs: &mut GcHeapSession<'h>,
    expr: Expr<'h>,
    env: EnvironmentRef<'h>,
) -> Result<Trampoline<'h>, String> {
    match expr {
        Expr::Con(k) =>
            Ok(Trampoline::Value(k)),
        Expr::Var(s) =>
            Ok(Trampoline::Value(env.get(s.unwrap())?)),
        Expr::Fun(code) =>
            Ok(Trampoline::Value(Lambda(hs.alloc(Pair {
                car: Value::Code(code),
                cdr: Value::Environment(env.clone()),
            })))),
        Expr::App(subexprs) => {
            let func = eval_compiled(hs, subexprs.get(0), env.clone())?;
            let args: Vec<Value<'h>> =
                (1..subexprs.len())
                .map(|i| eval_compiled(hs, subexprs.get(i), env.clone()))
                .collect::<Result<Vec<Value<'h>>, String>>()?;
            Ok(Trampoline::TailCall { func, args })
        }
        Expr::Seq(exprs) => {
            let len = exprs.len();
            if len == 0 {
                Ok(Trampoline::Value(Nil))
            } else {
                for i in 0..(len - 1) {
                    eval_compiled(hs, exprs.get(i), env.clone())?;
                }
                eval_to_tail_call(hs, exprs.get(len - 1), env)
            }
        }
        Expr::If(if_parts) => {
            let cond_value = eval_compiled(hs, if_parts.cond(), env.clone())?;
            let selected_expr =
                if cond_value.to_bool() {
                    if_parts.t_expr()
                } else {
                    if_parts.f_expr()
                };
            eval_to_tail_call(hs, selected_expr, env)
        }
        Expr::Def(def) => {
            let val = eval_compiled(hs, def.value(), env.clone())?;
            env.push(def.name().unwrap(), val);
            Ok(Trampoline::Value(Nil))
        }
        Expr::Set(def) => {
            let val = eval_compiled(hs, def.value(), env.clone())?;
            env.set(def.name().unwrap(), val)?;
            Ok(Trampoline::Value(Nil))
        }
    }
}

pub fn eval_compiled<'h>(
    hs: &mut GcHeapSession<'h>,
    expr: Expr<'h>,
    env: EnvironmentRef<'h>,
) -> Result<Value<'h>, String> {
    let tail = eval_to_tail_call(hs, expr, env)?;
    tail.eval(hs)
}

pub fn eval<'h>(
    hs: &mut GcHeapSession<'h>,
    expr: Value<'h>,
    env: EnvironmentRef<'h>,
) -> Result<Value<'h>, String> {
    let expr = compile::compile(hs, expr)?;
    eval_compiled(hs, expr, env)
}

#[cfg(test)]
include!("tests.rs");
