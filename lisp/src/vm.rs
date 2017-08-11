//! Interpreter for compiled code.

use cell_gc::GcHeapSession;
use cell_gc::collections::VecRef;
use compile::{op, CodeRef};
use env::{Environment, EnvironmentRef, StaticEnvironmentRef};
use errors::Result;
use value::{Lambda, Pair, Value};

#[derive(IntoHeap)]
pub struct Frame<'h> {
    pub parent: Option<FrameRef<'h>>,
    pub code: CodeRef<'h>,
    pub pc: usize,
    pub env: EnvironmentRef<'h>,
    pub operands: Value<'h>,
}


fn get_single_argument<'h>(arg_list: Value<'h>) -> Result<Value<'h>> {
    match arg_list {
        Value::Cons(pair) => {
            if pair.cdr().is_nil() {
                Ok(pair.car())
            } else {
                Err("continuation: too many arguments".into())
            }
        }
        _ => Err("continuation: argument required".into())
    }
}

pub fn partial_apply<'h>(
    hs: &mut GcHeapSession<'h>,
    fval: Value<'h>,
    arg_list: Value<'h>,
    ctn: Option<FrameRef<'h>>
) -> Result<(Value<'h>, Option<FrameRef<'h>>)> {
    match fval {
        Value::Builtin(f) => (f.0)(hs, arg_list, ctn),
        Value::Lambda(lambda) => {
            let code = lambda.code();
            let senv = code.environments().get(0);
            let env = new_call_env(hs, lambda.env(), senv, code.rest(), arg_list)?;
            let frame = hs.alloc(Frame {
                parent: ctn,
                code,
                pc: 0,
                env,
                operands: Value::Nil,
            });
            Ok((Value::Unspecified, Some(frame)))
        }
        Value::Continuation(stack) => {
            let value = get_single_argument(arg_list)?;
            Ok((value, stack))
        }
        _ => Err("apply: not a function".into()),
    }
}

fn new_call_env<'h>(
    hs: &mut GcHeapSession<'h>,
    parent: EnvironmentRef<'h>,
    senv: StaticEnvironmentRef<'h>,
    has_rest: bool,
    mut arg_list: Value<'h>
) -> Result<EnvironmentRef<'h>> {
    let names = senv.names();
    let n_names = names.len();
    let n_required_params = n_names - has_rest as usize;
    let argv = hs.alloc(Vec::<Value<'h>>::new());
    argv.reserve(n_names);

    for _ in 0..n_required_params {
        let (arg, tail) = arg_list.as_pair("not enough arguments")?;
        argv.push(arg);
        arg_list = tail;
    }

    if has_rest {
        argv.push(arg_list);
    } else if !arg_list.is_nil() {
        return Err("too many arguments".into());
    }

    Ok(Environment::new(hs, Some(parent), senv, argv))
}

pub fn interpret<'h>(
    hs: &mut GcHeapSession<'h>,
    mut stack: Option<FrameRef<'h>>,
    mut acc: Value<'h>,
) -> Result<Value<'h>> {
    let mut code: CodeRef<'h>;
    let mut insns: VecRef<'h, u32>;
    let mut pc: usize;
    let mut env: EnvironmentRef<'h>;
    let mut operands: Value<'h>;

    macro_rules! return_acc {
        () => {
            match stack {
                None => {
                    return Ok(acc);
                }
                Some(frame) => {
                    stack = frame.parent();
                    code = frame.code();
                    insns = code.insns();
                    pc = frame.pc();
                    env = frame.env();
                    operands = frame.operands();
                }
            }
        }
    }

    return_acc!();

    loop {
        let op_code = insns.get(pc);
        pc += 1;
        match op_code {
            op::CONSTANT => {
                let offset = insns.get(pc) as usize;
                pc += 1;
                acc = code.constants().get(offset);
            }

            op::GET_STATIC => {
                let up_count = insns.get(pc) as usize;
                pc += 1;
                let i = insns.get(pc) as usize;
                pc += 1;
                acc = env.get(up_count, i);
            }

            op::GET_DYNAMIC => {
                let i = insns.get(pc) as usize;
                pc += 1;
                let symbol = match code.constants().get(i) {
                    Value::Symbol(id) => id,
                    _ => panic!("internal error: bad GetDynamic insn"),
                };
                acc = env.dynamic_get(&symbol)?;
            }

            op::SET_STATIC => {
                let up_count = insns.get(pc) as usize;
                pc += 1;
                let i = insns.get(pc) as usize;
                pc += 1;
                env.set(up_count, i, acc.steal());
            }

            op::SET_DYNAMIC => {
                let i = insns.get(pc) as usize;
                pc += 1;
                let symbol = match code.constants().get(i) {
                    Value::Symbol(id) => id.unwrap(),
                    _ => panic!("internal error: bad Set insn"),
                };
                env.dynamic_set(&symbol, acc.steal())?;
            }

            op::LAMBDA => {
                let i = insns.get(pc) as usize;
                pc += 1;
                let fn_code = match code.constants().get(i) {
                    Value::Code(code) => code,
                    _ => panic!("internal error: bad Lambda insn"),
                };
                acc = Value::Lambda(hs.alloc(Lambda {
                    code: fn_code,
                    env: env.clone(),
                }));
            }

            op::FRAME => {
                // Push a stack frame, for an upcoming non-tail call.
                // After this, the stack reflects that call's continuation.
                let target_pc = pc + insns.get(pc) as usize;
                pc += 1;
                stack = Some(hs.alloc(Frame {
                    parent: stack,
                    code: code.clone(),
                    pc: target_pc,
                    env: env.clone(),
                    operands,
                }));
                operands = Value::Nil;
            }

            op::ARGUMENT => {
                operands = Value::Cons(hs.alloc(Pair {
                    car: acc.steal(),
                    cdr: operands
                }));
            }

            op::APPLY => {
                match acc.steal() {
                    Value::Builtin(f) => {
                        let pair = f.0(hs, operands, stack)?;
                        acc = pair.0;
                        stack = pair.1;
                        return_acc!();
                    }
                    Value::Lambda(lambda) => {
                        code = lambda.code();
                        insns = code.insns();
                        env = new_call_env(hs, lambda.env(), code.environments().get(0),
                                           code.rest(), operands)?;
                        operands = Value::Nil;
                        pc = 0;
                    }
                    Value::Continuation(ctn_stack) => {
                        acc = get_single_argument(operands)?;
                        stack = ctn_stack;
                        return_acc!();
                    }
                    _ => {
                        return Err("not a procedure".into());
                    }
                }
            }

            op::RETURN => {
                return_acc!();
            }

            op::PUSH_ENV => {
                let i = insns.get(pc) as usize;
                pc += 1;
                let senv = code.environments().get(i);
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

            op::JUMP => {
                pc += insns.get(pc) as usize;
            }

            op::JUMP_IF_FALSE => {
                if acc.to_bool() {
                    pc += 1;
                } else {
                    pc += insns.get(pc) as usize;
                }
            }

            op::DEFINE => {
                let i = insns.get(pc) as usize;
                pc += 1;
                let symbol = match code.constants().get(i) {
                    Value::Symbol(id) => id,
                    _ => panic!("internal error: bad Define insn"),
                };
                env.define(symbol.unwrap(), acc.steal());
            }

            _ => {
                panic!("internal error: invalid opcode {}", op_code);
            }
        }
    }
}

pub fn apply<'h>(
    hs: &mut GcHeapSession<'h>,
    fval: Value<'h>,
    arg_list: Value<'h>,
) -> Result<Value<'h>> {
    let (v, ctn) = partial_apply(hs, fval, arg_list, None)?;
    interpret(hs, ctn, v)
}

pub fn eval_compiled<'h>(
    hs: &mut GcHeapSession<'h>,
    env: EnvironmentRef<'h>,
    code: CodeRef<'h>,
) -> Result<Value<'h>> {
    let stack = Some(hs.alloc(Frame {
        parent: None,
        code,
        pc: 0,
        env,
        operands: Value::Nil
    }));
    interpret(hs, stack, Value::Unspecified)
}

