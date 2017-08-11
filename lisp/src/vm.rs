//! Interpreter for compiled code.

use cell_gc::GcHeapSession;
use cell_gc::collections::VecRef;
use compile::{op, CodeRef};
use env::{Environment, EnvironmentRef};
use errors::Result;
use value::{Lambda, Pair, Value};

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
    pub fn eval(mut self, hs: &mut GcHeapSession<'h>) -> Result<Value<'h>> {
        loop {
            match self {
                Trampoline::Value(v) => {
                    return Ok(v);
                }
                Trampoline::TailCall { func, args } => {
                    self = apply(hs, func, args)?;
                }
            }
        }
    }
}

pub fn apply<'h>(
    hs: &mut GcHeapSession<'h>,
    fval: Value<'h>,
    mut args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>> {
    match fval {
        Value::Builtin(f) => (f.0)(hs, args),
        Value::Lambda(lambda) => {
            let code = lambda.code();
            let parent = lambda.env();
            let senv = code.environments().get(0);
            let names = senv.names();
            let n_names = names.len();
            let has_rest = code.rest();

            let n_required_params = n_names - has_rest as usize;
            if args.len() < n_required_params {
                return Err("apply: not enough arguments".into());
            }
            if has_rest {
                let mut rest_list = Value::Nil;
                for v in args.drain(n_required_params..).rev() {
                    rest_list = Value::Cons(hs.alloc(Pair {
                        car: v,
                        cdr: rest_list,
                    }));
                }
                args.push(rest_list);
            } else if args.len() > n_required_params {
                return Err("apply: too many arguments".into());
            }

            let values = hs.alloc(args);
            let env = Environment::new(hs, Some(parent), senv, values);
            eval_compiled_to_tail_call(hs, &env, code)
        }
        _ => Err("apply: not a function".into()),
    }
}

/// Evaluate `expr` until we reach a tail call, at which point it is packaged up
/// as a `Trampoline::TailCall` and returned so we can unwind the stack before
/// continuing evaluation.
pub fn eval_compiled_to_tail_call<'h>(
    hs: &mut GcHeapSession<'h>,
    env: &EnvironmentRef<'h>,
    code: CodeRef<'h>,
) -> Result<Trampoline<'h>> {
    let constants = code.constants();
    let environments = code.environments();
    let insns = code.insns();
    let mut env = env.clone();
    let mut pc = 0;
    let mut operands = Vec::with_capacity(code.operands_max());

    loop {
        let op_code = insns.get(pc);
        pc += 1;
        match op_code {
            op::RETURN => {
                assert_eq!(operands.len(), 1);
                return Ok(Trampoline::Value(operands.pop().unwrap()));
            }

            op::POP => {
                operands.pop().unwrap();
            }

            op::CONSTANT => {
                let i = insns.get(pc) as usize;
                pc += 1;
                operands.push(constants.get(i));
            }

            op::GET_DYNAMIC => {
                let i = insns.get(pc) as usize;
                pc += 1;
                let symbol = match constants.get(i) {
                    Value::Symbol(id) => id,
                    _ => panic!("internal error: bad GetDynamic insn"),
                };
                operands.push(env.dynamic_get(&symbol)?);
            }

            op::GET_STATIC => {
                let up_count = insns.get(pc) as usize;
                pc += 1;
                let i = insns.get(pc) as usize;
                pc += 1;
                operands.push(env.get(up_count, i));
            }

            op::LAMBDA => {
                let i = insns.get(pc) as usize;
                pc += 1;
                let fn_code = match constants.get(i) {
                    Value::Code(code) => code,
                    _ => panic!("internal error: bad Lambda insn"),
                };
                let fn_value = Value::Lambda(hs.alloc(Lambda {
                    code: fn_code,
                    env: env.clone(),
                }));
                operands.push(fn_value);
            }

            op::CALL => {
                let argc = insns.get(pc) as usize;
                pc += 1;
                let top = operands.len();
                let args = operands.split_off(top - argc);
                let fval = operands.pop().unwrap();
                operands.push(apply(hs, fval, args)?.eval(hs)?);
            }

            op::TAIL_CALL => {
                let argc = insns.get(pc) as usize;
                // No `pc += 1;` here because pc is a dead value.
                assert_eq!(operands.len(), argc + 1);
                let fval = operands.remove(0);
                return Ok(Trampoline::TailCall {
                    func: fval,
                    args: operands
                });
            }

            op::JUMP_IF_FALSE => {
                let offset = match operands.pop().unwrap() {
                    Value::Bool(false) => insns.get(pc) as usize,
                    _ => 1
                };
                pc += offset;
            }

            op::JUMP => {
                pc += insns.get(pc) as usize;
            }

            op::DEFINE => {
                let i = insns.get(pc) as usize;
                pc += 1;
                let symbol = match constants.get(i) {
                    Value::Symbol(id) => id,
                    _ => panic!("internal error: bad Define insn"),
                };
                let value = operands.pop().unwrap();
                env.define(symbol.unwrap(), value);
            }

            op::SET_STATIC => {
                let up_count = insns.get(pc) as usize;
                pc += 1;
                let i = insns.get(pc) as usize;
                pc += 1;
                env.set(up_count, i, operands.pop().unwrap());
            }

            op::SET_DYNAMIC => {
                let i = insns.get(pc) as usize;
                pc += 1;
                let symbol = match constants.get(i) {
                    Value::Symbol(id) => id.unwrap(),
                    _ => panic!("internal error: bad Set insn"),
                };
                let value = operands.pop().unwrap();
                env.dynamic_set(&symbol, value)?;
            }

            op::PUSH_ENV => {
                let i = insns.get(pc) as usize;
                pc += 1;
                let senv = environments.get(i);
                let values: VecRef<'h, Value<'h>> = hs.alloc(
                    (0..senv.names().len())
                        .map(|_| Value::Unspecified)
                        .collect::<Vec<Value<'h>>>(),
                );
                env = Environment::new(hs, Some(env), senv, values);
            }

            op::POP_ENV => {
                env = env.parent().unwrap();
            }

            _ => panic!("internal error: invalid opcode {}", op_code),
        }
    }
}

pub fn eval_compiled<'h>(
    hs: &mut GcHeapSession<'h>,
    env: &EnvironmentRef<'h>,
    code: CodeRef<'h>,
) -> Result<Value<'h>> {
    assert_eq!(code.environments().get(0), env.senv(),
               "code can only run in the environment for which it was compiled");
    eval_compiled_to_tail_call(hs, env, code)?.eval(hs)
}

#[cfg(test)]
include!("tests.rs");
