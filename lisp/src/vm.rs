//! Interpreter for compiled code.

use cell_gc::GcHeapSession;
use cell_gc::collections::VecRef;
use compile::{op, CodeRef};
use env::{Environment, EnvironmentRef, StaticEnvironmentRef};
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

fn new_call_env<'h>(
    hs: &mut GcHeapSession<'h>,
    senv: StaticEnvironmentRef<'h>,
    has_rest: bool,
    parent: EnvironmentRef<'h>,
    mut args: Vec<Value<'h>>,
) -> Result<EnvironmentRef<'h>> {
    let names = senv.names();
    let n_names = names.len();
    let n_required_params = n_names - has_rest as usize;

    let n_actual = args.len();
    if n_actual < n_required_params {
        return Err("not enough arguments".into());
    }
    if has_rest {
        let mut rest_list = Value::Nil;
        for arg in args.drain(n_required_params..).rev() {
            rest_list = Value::Cons(hs.alloc(Pair {
                car: arg,
                cdr: rest_list,
            }));
        }
        args.push(rest_list);
    } else if n_actual > n_required_params {
        return Err("too many arguments".into());
    }

    let args = hs.alloc(args);  // move values into heap
    Ok(Environment::new(hs, Some(parent), senv, args))
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
                return Err("not enough arguments".into());
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
                return Err("too many arguments".into());
            }

            let values = hs.alloc(args);
            let env = Environment::new(hs, Some(parent), senv, values);
            eval_compiled_to_tail_call(hs, &env, code)
        }
        _ => Err("not a procedure".into()),
    }
}

/// Evaluate `expr` until we reach a tail call, at which point it is packaged up
/// as a `Trampoline::TailCall` and returned so we can unwind the stack before
/// continuing evaluation.
pub fn eval_compiled_to_tail_call<'h>(
    hs: &mut GcHeapSession<'h>,
    env: &EnvironmentRef<'h>,
    mut code: CodeRef<'h>,
) -> Result<Trampoline<'h>> {
    // The stack is a sequence of concatenated stack frames. Each frame is laid out like this:
    //   #(code pc env operand0 ...)

    let mut stack = Vec::<Value<'h>>::with_capacity(1024);
    let mut constants = code.constants();
    let mut environments = code.environments();
    let mut insns = code.insns();
    let mut env = env.clone();
    let mut pc = 0;

    macro_rules! return_value {
        ($value:ident) => {
            if stack.is_empty() {
                return Ok(Trampoline::Value($value));
            }

            // Read the caller's stack frame back into local variables.
            env = stack.pop().unwrap().as_environment("internal error").unwrap();
            pc = stack.pop().unwrap().as_int("internal error").unwrap() as usize;
            code = stack.pop().unwrap().as_code("internal error").unwrap();
            constants = code.constants();
            environments = code.environments();
            insns = code.insns();

            //println!("resuming at pc={}", pc);
            //code.dump();

            // Push the return value to its operand stack.
            stack.push($value);
        }
    }

    loop {
        let op_code = insns.get(pc);
        pc += 1;
        //println!("{} op {}", pc - 1, op_code);
        match op_code {
            op::RETURN => {
                // The return value is the last thing left on the current
                // frame's operand stack.
                let value = stack.pop().unwrap();
                return_value!(value);
            }

            op::POP => {
                stack.pop().unwrap();
            }

            op::CONSTANT => {
                let i = insns.get(pc) as usize;
                pc += 1;
                stack.push(constants.get(i));
            }

            op::GET_DYNAMIC => {
                let i = insns.get(pc) as usize;
                pc += 1;
                let symbol = match constants.get(i) {
                    Value::Symbol(id) => id,
                    _ => panic!("internal error: bad GetDynamic insn"),
                };
                stack.push(env.dynamic_get(&symbol)?);
            }

            op::GET_STATIC => {
                let up_count = insns.get(pc) as usize;
                pc += 1;
                let i = insns.get(pc) as usize;
                pc += 1;
                stack.push(env.get(up_count, i));
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
                stack.push(fn_value);
            }

            op::CALL | op::TAIL_CALL => {
                let argc = insns.get(pc) as usize;
                pc += 1;

                let top = stack.len();
                let mut args = stack.split_off(top - argc);
                let mut fval = stack.pop().unwrap();

                loop {
                    match fval {
                        Value::Lambda(lambda) => {
                            if op_code == op::CALL {
                                stack.push(Value::Code(code));
                                assert!(pc <= i32::max_value() as usize);
                                stack.push(Value::Int(pc as i32));
                                stack.push(Value::Environment(env));
                            } else {
                                // Tail call. Our caller's frame is already on top
                                // of stack. Assert that it's correctly laid out.
                                let top = stack.len();
                                if top != 0 {
                                    assert!(stack[top - 3].is_code());
                                    assert!(stack[top - 2].is_int());
                                    assert!(stack[top - 1].is_environment());
                                }
                            }

                            code = lambda.code();
                            constants = code.constants();
                            environments = code.environments();
                            insns = code.insns();
                            pc = 0;
                            env = new_call_env(hs, code.environments().get(0), code.rest(), lambda.env(), args)?;
                            break;
                        }
                        Value::Builtin(f) => {
                            match (f.0)(hs, args)? {
                                Trampoline::Value(v) => {
                                    if op_code == op::TAIL_CALL {
                                        return_value!(v);
                                    } else {
                                        stack.push(v);
                                    }
                                    break;
                                }
                                Trampoline::TailCall { func: new_fval, args: new_args } => {
                                    fval = new_fval;
                                    args = new_args;
                                }
                            }
                        }
                        _ => return Err("not a procedure".into())
                    }
                }
            }

            op::JUMP_IF_FALSE => {
                let offset = match stack.pop().unwrap() {
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
                let value = stack.pop().unwrap();
                env.define(symbol.unwrap(), value);
            }

            op::SET_STATIC => {
                let up_count = insns.get(pc) as usize;
                pc += 1;
                let i = insns.get(pc) as usize;
                pc += 1;
                env.set(up_count, i, stack.pop().unwrap());
            }

            op::SET_DYNAMIC => {
                let i = insns.get(pc) as usize;
                pc += 1;
                let symbol = match constants.get(i) {
                    Value::Symbol(id) => id.unwrap(),
                    _ => panic!("internal error: bad Set insn"),
                };
                let value = stack.pop().unwrap();
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
