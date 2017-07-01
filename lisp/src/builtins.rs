use std::fmt;
use vm::Value;
use vm::Value::*;

pub struct BuiltinFnPtr(pub for<'b> fn(Vec<Value<'b>>) -> Result<Value<'b>, String>);

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

pub fn add<'h>(args: Vec<Value<'h>>) -> Result<Value<'h>, String> {
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

pub fn assert<'h>(args: Vec<Value<'h>>) -> Result<Value<'h>, String> {
    if args.len() < 1 || args.len() > 2 {
        return Err(format!(
            "assert: wrong number of args, expected 1 or 2, found {}",
            args.len()
        ));
    }

    let v = &args[0];

    if let Bool(true) = *v {
        Ok(Nil)
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

pub fn eq_question<'h>(args: Vec<Value<'h>>) -> Result<Value<'h>, String> {
    let first = args.get(0);
    Ok(Bool(args.iter().all(|arg| Some(arg) == first)))
}

pub fn mul<'h>(args: Vec<Value<'h>>) -> Result<Value<'h>, String> {
    let mut total = 1;
    for v in args {
        if let Int(n) = v {
            total *= n;
        } else {
            return Err("add: non-numeric argument".to_string());
        }
    }
    Ok(Int(total))
}
