use cell_gc::GcHeapSession;
use print::print as print_value;
use std::fmt;
use vm::{self, Pair, Trampoline, Value};
use vm::Value::*;

pub struct BuiltinFnPtr(
    pub for<'b> fn(&mut GcHeapSession<'b>, Vec<Value<'b>>)
        -> Result<Trampoline<'b>, String>
);

// This can't be #[derive]d because function pointers aren't Clone.
// But they are Copy. A very weird thing about Rust.
impl Clone for BuiltinFnPtr {
    fn clone(&self) -> BuiltinFnPtr {
        BuiltinFnPtr(self.0)
    }
}

impl PartialEq for BuiltinFnPtr {
    fn eq(&self, other: &BuiltinFnPtr) -> bool {
        self.0 as usize == other.0 as usize
    }
}

impl fmt::Debug for BuiltinFnPtr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "BuiltinFn({:p})", self.0 as usize as *mut ())?;
        Ok(())
    }
}

// Builtin function definitions ////////////////////////////////////////////////

// 6.1 Booleans
fn simple_predicate<'h, F>(
    fn_name: &str,
    mut args: Vec<Value<'h>>,
    f: F,
) -> Result<Trampoline<'h>, String>
where
    F: FnOnce(Value<'h>) -> bool,
{
    if args.len() != 1 {
        return Err(format!("{}: exactly 1 argument required", fn_name));
    }
    Ok(Trampoline::Value(Bool(f(args.pop().unwrap()))))
}

pub fn boolean_question<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    simple_predicate("boolean?", args, |v| v.is_boolean())
}

// 6.2 Equivalence predicates
pub fn eq_question<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    let first = args.get(0);
    Ok(Trampoline::Value(
        Bool(args.iter().all(|arg| Some(arg) == first)),
    ))
}

pub fn eqv_question<'h>(
    _hs: &mut GcHeapSession<'h>,
    mut args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    if args.len() != 2 {
        return Err("eqv?: exactly 2 arguments required".into());
    }
    let b = args.pop().unwrap();
    let a = args.pop().unwrap();
    Ok(Trampoline::Value(Bool(a == b)))
}

// 6.3 Pairs and lists
pub fn pair_question<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    simple_predicate("pair?", args, |v| v.is_pair())
}

pub fn cons<'h>(
    hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    if args.len() != 2 {
        Err("cons: require exactly 2 arguments".into())
    } else {
        let pair = hs.alloc(Pair {
            car: args[0].clone(),
            cdr: args[1].clone(),
        });
        Ok(Trampoline::Value(Cons(pair)))
    }
}

pub fn car<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    if args.len() != 1 {
        Err("car: require exactly 1 argument".into())
    } else {
        if let Cons(ref p) = args[0] {
            Ok(Trampoline::Value(p.car()))
        } else {
            Err("car: non-pair argument passed to car".into())
        }
    }
}

pub fn cdr<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    if args.len() != 1 {
        Err("car: require exactly 1 argument".into())
    } else {
        if let Cons(ref p) = args[0] {
            Ok(Trampoline::Value(p.cdr()))
        } else {
            Err("cdr: non-pair argument passed to cdr".into())
        }
    }
}

pub fn null_question<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    simple_predicate("null?", args, |v| v.is_nil())
}

// 6.5 Numbers
pub fn numeric_eq<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    let mut it = args.into_iter();
    if let Some(arg0) = it.next() {
        let z0 = arg0.as_int("=")?;
        for arg in it {
            let z = arg.as_int("=")?;
            if !(z0 == z) {
                return Ok(Trampoline::Value(Bool(false)));
            }
        }
    }
    Ok(Trampoline::Value(Bool(true)))
}

pub fn numeric_compare<'h, F>(args: Vec<Value<'h>>, cmp: F) -> Result<Trampoline<'h>, String>
where
    F: Fn(i32, i32) -> bool,
{
    let mut it = args.into_iter();
    if let Some(arg0) = it.next() {
        let mut prev = arg0.as_int("=")?;
        for arg in it {
            let x = arg.as_int("=")?;
            if !cmp(prev, x) {
                return Ok(Trampoline::Value(Bool(false)));
            }
            prev = x;
        }
    }
    Ok(Trampoline::Value(Bool(true)))
}

pub fn numeric_lt<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    numeric_compare(args, |a, b| a < b)
}

pub fn numeric_gt<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    numeric_compare(args, |a, b| a > b)
}

pub fn numeric_le<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    numeric_compare(args, |a, b| a <= b)
}

pub fn numeric_ge<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    numeric_compare(args, |a, b| a >= b)
}

pub fn add<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    let mut total = 0;
    for v in args {
        if let Int(n) = v {
            total += n;
        } else {
            return Err("add: non-numeric argument".to_string());
        }
    }
    Ok(Trampoline::Value(Int(total)))
}

pub fn mul<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    let mut total = 1;
    for v in args {
        if let Int(n) = v {
            total *= n;
        } else {
            return Err("mul: non-numeric argument".to_string());
        }
    }
    Ok(Trampoline::Value(Int(total)))
}

pub fn sub<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    if args.len() == 0 {
        Err("sub: need at least one argument".into())
    } else if args.len() == 1 {
        if let Int(n) = args[0] {
            Ok(Trampoline::Value(Int(-n)))
        } else {
            Err("sub: non-numeric argument".into())
        }
    } else {
        let mut total = if let Int(n) = args[0] {
            n
        } else {
            return Err("sub: non-numeric argument".into());
        };
        for v in &args[1..] {
            if let Int(n) = *v {
                total -= n;
            } else {
                return Err("add: non-numeric argument".to_string());
            }
        }
        Ok(Trampoline::Value(Int(total)))
    }
}

// 6.8 Vectors
pub fn vector_question<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    simple_predicate("vector?", args, |v| v.is_vector())
}

pub fn vector<'h>(
    hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    Ok(Trampoline::Value(Vector(hs.alloc(args))))
}

pub fn vector_length<'h>(
    _hs: &mut GcHeapSession<'h>,
    mut args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    if args.len() != 1 {
        return Err("vector-length: exactly 1 argument required".into());
    }
    let n = args.pop().unwrap().as_vector("vector-length")?.len();
    if n as i32 as usize != n {
        return Err("vector-length: integer overflow".into());
    }
    Ok(Trampoline::Value(Value::Int(n as i32)))
}

pub fn vector_ref<'h>(
    _hs: &mut GcHeapSession<'h>,
    mut args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    if args.len() != 2 {
        return Err("vector-ref: exactly 2 arguments required".into());
    }
    let index = args.pop().unwrap().as_index("vector-ref")?;
    let v = args.pop().unwrap().as_vector("vector-ref")?;
    if index >= v.len() {
        return Err(format!(
            "vector-ref: index out of bounds (got {}, length {})",
            index,
            v.len()
        ));
    }
    Ok(Trampoline::Value(v.get(index)))
}

// 6.9 Control features
pub fn procedure_question<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    simple_predicate("procedure?", args, |v| v.is_procedure())
}

pub fn apply<'h>(
    hs: &mut GcHeapSession<'h>,
    mut args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    if args.len() < 2 {
        return Err("apply: at least 2 arguments required".into());
    }
    let mut trailing = args.pop().unwrap();
    let mut arg_vec = args.split_off(1);
    while let Cons(pair) = trailing {
        arg_vec.push(pair.car());
        trailing = pair.cdr();
    }
    if !trailing.is_nil() {
        return Err("apply: last argument must be a list".into());
    }
    assert_eq!(args.len(), 1);
    let fval = args.pop().unwrap();

    vm::apply(hs, fval, arg_vec)
}

// Extensions
pub fn print<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    for v in args {
        print_value(v);
        println!();
    }
    Ok(Trampoline::Value(Nil))
}

pub fn assert<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    if args.len() < 1 || args.len() > 2 {
        return Err(format!(
            "assert: wrong number of args, expected 1 or 2, found {}",
            args.len()
        ));
    }

    let v = &args[0];

    if let Bool(true) = *v {
        Ok(Trampoline::Value(Nil))
    } else if let Bool(false) = *v {
        if let Some(msg) = args.get(1) {
            Err(format!("assert: assertion failed: {:?}", msg))
        } else {
            Err("assert: assertion failed".into())
        }
    } else {
        Err("assert: non-boolean argument".into())
    }
}
