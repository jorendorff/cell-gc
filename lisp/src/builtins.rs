//! Essential procedures.

use cell_gc::{GcHeapSession, GcLeaf};
use cell_gc::collections::VecRef;
use compile::{self, Code};
use env::{self, EnvironmentRef, StaticEnvironment};
use errors::*;
use std::io::{self, Write};
use std::sync::Arc;
use std::vec;
use value::{self, BuiltinFn, BuiltinFnPtr, InternedString, Lambda, Pair, Value};
use value::{ArgType, Rest, RetType};
use value::Value::*;
use vm::Trampoline;


// The builtins! macro /////////////////////////////////////////////////////////

fn check_done<'h>(proc_name: &str, iter: &mut vec::IntoIter<Value<'h>>) -> Result<()> {
    match iter.next() {
        Some(_) => Err(format!("{}: too many arguments", proc_name).into()),
        None => Ok(())
    }
}

macro_rules! builtins {
    {
        $(
            fn $name:ident $namestr:tt <$h:tt>($hs:ident $(, $arg:ident : $argty:ty )* )
            -> $retty:ty
            $body:block
        )*
    }
    =>
    {
        $(
            fn $name<$h>(
                $hs: &mut GcHeapSession<$h>,
                args: Vec<Value<$h>>,
            ) -> Result<Trampoline<$h>> {
                let mut args_iter = args.into_iter();
                $( let $arg = <$argty as ArgType<$h>>::unpack_argument($namestr, &mut args_iter)?; )*
                check_done($namestr, &mut args_iter)?;
                <$retty as RetType<$h>>::pack($body, $hs)
            }
        )*
    }
}


// Builtin function definitions ////////////////////////////////////////////////

// 6.1 Booleans
builtins! {
    fn boolean_question "boolean?" <'h>(_hs, v: Value<'h>) -> bool {
        v.is_boolean()
    }
}

// 6.2 Equivalence predicates
builtins! {
    fn eq_question "eq?" <'h>(_hs, args: Rest<'h>) -> bool {
        let mut args = args;
        match args.next() {
            None => true,
            Some(v) => args.all(|arg| arg == v),
        }
    }

    fn eqv_question "eqv?" <'h>(_hs, a: Value<'h>, b: Value<'h>) -> bool {
        a == b
    }
}

// 6.3 Pairs and lists
builtins! {
    fn pair_question "pair?" <'h>(_hs, v: Value<'h>) -> bool {
        v.is_pair()
    }

    fn cons "cons" <'h>(hs, car: Value<'h>, cdr: Value<'h>) -> value::PairRef<'h> {
        hs.alloc(Pair { car, cdr })
    }

    fn car "car" <'h>(_hs, pair: value::PairRef<'h>) -> Value<'h> {
        pair.car()
    }

    fn cdr "cdr" <'h>(_hs, pair: value::PairRef<'h>) -> Value<'h> {
        pair.cdr()
    }

    fn null_question "null?" <'h>(_hs, v: Value<'h>) -> bool {
        v.is_nil()
    }

    fn set_car "set-car!" <'h>(_hs, pair: value::PairRef<'h>, value: Value<'h>) -> () {
        pair.set_car(value);
    }

    fn set_cdr "set-cdr!" <'h>(_hs, pair: value::PairRef<'h>, value: Value<'h>) -> () {
        pair.set_cdr(value);
    }
}

// 6.4 Symbols
builtins! {
    fn symbol_question "symbol?" <'h>(_hs, v: Value<'h>) -> bool {
        v.is_symbol()
    }

    fn symbol_to_string "symbol->string" <'h>(_hs, v: Value<'h>) -> String {
        let in_str = v.as_symbol("symbol->string")?;
        in_str.as_string().clone()
    }

    fn string_to_symbol "string->symbol" <'h>(_hs, v: Value<'h>) -> Value<'h> {
        let arc_str = v.as_string("string->symbol")?;
        Value::Symbol(GcLeaf::new(InternedString::intern_arc(arc_str)))
    }
}

// 6.5 Numbers
builtins! {
    fn number_question "number?" <'h>(_hs, v: Value<'h>) -> bool {
        v.is_number()
    }
}

fn numeric_compare<'h, F>(
    name: &'static str,
    args: Vec<Value<'h>>,
    cmp: F,
) -> Result<Trampoline<'h>>
where
    F: Fn(i32, i32) -> bool,
{
    let mut it = args.into_iter();
    if let Some(arg0) = it.next() {
        let mut prev = arg0.as_int(name)?;
        for arg in it {
            let x = arg.as_int(name)?;
            if !cmp(prev, x) {
                return Ok(Trampoline::Value(Bool(false)));
            }
            prev = x;
        }
    }
    Ok(Trampoline::Value(Bool(true)))
}

fn numeric_eq<'h>(_hs: &mut GcHeapSession<'h>, args: Vec<Value<'h>>) -> Result<Trampoline<'h>> {
    numeric_compare("=", args, |a, b| a == b)
}

fn numeric_lt<'h>(_hs: &mut GcHeapSession<'h>, args: Vec<Value<'h>>) -> Result<Trampoline<'h>> {
    numeric_compare("<", args, |a, b| a < b)
}

fn numeric_gt<'h>(_hs: &mut GcHeapSession<'h>, args: Vec<Value<'h>>) -> Result<Trampoline<'h>> {
    numeric_compare(">", args, |a, b| a > b)
}

fn numeric_le<'h>(_hs: &mut GcHeapSession<'h>, args: Vec<Value<'h>>) -> Result<Trampoline<'h>> {
    numeric_compare("<=", args, |a, b| a <= b)
}

fn numeric_ge<'h>(_hs: &mut GcHeapSession<'h>, args: Vec<Value<'h>>) -> Result<Trampoline<'h>> {
    numeric_compare(">=", args, |a, b| a >= b)
}

builtins! {
    fn add "+" <'h>(_hs, args: Rest<'h>) -> Result<i32> {
        let mut total = 0;
        for v in args {
            if let Int(n) = v {
                total += n;
            } else {
                return Err("+: number required".into());
            }
        }
        Ok(total)
    }

    fn mul "*" <'h>(_hs, args: Rest<'h>) -> Result<i32> {
        let mut total = 1;
        for v in args {
            if let Int(n) = v {
                total *= n;
            } else {
                return Err("*: number required".into());
            }
        }
        Ok(total)
    }

    fn sub "-" <'h>(_hs, minuend: i32, args: Rest<'h>) -> Result<i32> {
        let mut total = minuend;
        let mut any = false;
        for v in args {
            any = true;
            if let Int(subtrahend) = v {
                total -= subtrahend;
            } else {
                return Err("-: number required".into());
            }
        }
        if !any {
            Ok(-total)
        } else {
            Ok(total)
        }
    }

    fn number_to_string "number->string" <'h>(_hs, n: i32) -> String {
        format!("{}", n)
    }
}


// 6.6 Characters
builtins! {
    fn char_question "char?" <'h>(_hs, v: Value<'h>) -> bool {
        v.is_char()
    }
}

fn char_compare<'h, F>(name: &'static str, args: Vec<Value<'h>>, cmp: F) -> Result<Trampoline<'h>>
where
    F: Fn(char, char) -> bool,
{
    let mut it = args.into_iter();
    if let Some(arg0) = it.next() {
        let mut prev = arg0.as_char(name)?;
        for arg in it {
            let x = arg.as_char(name)?;
            if !cmp(prev, x) {
                return Ok(Trampoline::Value(Bool(false)));
            }
            prev = x;
        }
    }
    Ok(Trampoline::Value(Bool(true)))
}

fn char_eq<'h>(_hs: &mut GcHeapSession<'h>, args: Vec<Value<'h>>) -> Result<Trampoline<'h>> {
    char_compare("char=?", args, |a, b| a == b)
}

fn char_lt<'h>(_hs: &mut GcHeapSession<'h>, args: Vec<Value<'h>>) -> Result<Trampoline<'h>> {
    char_compare("char<?", args, |a, b| a < b)
}

fn char_gt<'h>(_hs: &mut GcHeapSession<'h>, args: Vec<Value<'h>>) -> Result<Trampoline<'h>> {
    char_compare("char>?", args, |a, b| a > b)
}

fn char_le<'h>(_hs: &mut GcHeapSession<'h>, args: Vec<Value<'h>>) -> Result<Trampoline<'h>> {
    char_compare("char<=?", args, |a, b| a <= b)
}

// CHAAAAARGE!
fn char_ge<'h>(_hs: &mut GcHeapSession<'h>, args: Vec<Value<'h>>) -> Result<Trampoline<'h>> {
    char_compare("char>=?", args, |a, b| a >= b)
}

builtins! {
    fn char_alphabetic_question "char-alphabetic?" <'h>(_hs, c: char) -> bool {
        c.is_alphabetic()
    }

    fn char_numeric_question "char-numeric?" <'h>(_hs, c: char) -> bool {
        c.is_numeric()
    }

    fn char_whitespace_question "char-whitespace?" <'h>(_hs, c: char) -> bool {
        c.is_whitespace()
    }

    fn char_upper_case_question "char-upper-case?" <'h>(_hs, c: char) -> bool {
        c.is_uppercase()
    }

    fn char_lower_case_question "char-lower-case?" <'h>(_hs, c: char) -> bool {
        c.is_lowercase()
    }

    fn char_upcase "char-upcase" <'h>(_hs, c: char) -> char {
        // I think the only character that doesn't upcase to a single character is
        // U+00DF LATIN SMALL LETTER SHARP S ('ß'). Guile returns it unchanged.
        // Fine with me.
        let mut up = c.to_uppercase();
        match (up.next(), up.next()) {
            (Some(d), None) => d,
            _ => c,
        }
    }

    fn char_downcase "char-downcase" <'h>(_hs, c: char) -> char {
        // I think the only character that doesn't downcase to a single character
        // is U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE ('İ'). Guile converts it
        // to 'i'.  Fine with me.
        let mut up = c.to_lowercase();
        up.next().unwrap_or(c)
    }
}

// 6.7 Strings
builtins! {
    fn string_question "string?" <'h>(_hs, v: Value<'h>) -> bool {
        v.is_string()
    }

    fn string "string" <'h>(_hs, args: Rest<'h>) -> String {
        let mut s = String::new();
        for arg in args {
            s.push(arg.as_char("string")?);
        }
        s
    }

    fn string_length "string-length" <'h>(_hs, s: Arc<String>) -> usize {
        s.chars().count() // bleurgh! O(n)
    }

    fn string_ref "string-ref" <'h>(_hs, s: Arc<String>, i: usize) -> Result<char> {
        match s.as_str().chars().nth(i) {  // bleurgh! O(n)
            Some(c) => Ok(c),
            None => Err("string-ref: index out of range".into()),
        }
    }

    fn string_eq_question "string=?" <'h>(_hs, a: Arc<String>, b: Arc<String>) -> bool {
        a.as_str() == b.as_str()
    }

    fn string_append "string-append" <'h>(_hs, args: Rest<'h>) -> String {
        args
            .map(|v| v.as_string("string-append"))
            .collect::<Result<Vec<Arc<String>>>>()?
            .iter()
            .map(|arc_str| arc_str as &str)
            .collect::<Vec<&str>>()
            .concat()
    }

    fn string_to_list "string->list" <'h>(hs, s: Arc<String>) -> Value<'h> {
        let mut list = Value::Nil;
        for c in s.chars().rev() {
            list = Value::Cons(hs.alloc(Pair {
                car: Value::Char(c),
                cdr: list,
            }));
        }
        list
    }

    fn list_to_string "list->string" <'h>(_hs, list: Value<'h>) -> Result<String> {
        list.into_iter()
            .map(|v| v?.as_char("list->string"))
            .collect()
    }
}


// 6.8 Vectors
builtins! {
    fn vector_question "vector?" <'h>(_hs, v: Value<'h>) -> bool {
        v.is_vector()
    }

    fn make_vector "make-vector" <'h>(hs, n: usize, v: Option<Value<'h>>) -> Vec<Value<'h>> {
        let value = v.unwrap_or(Value::Unspecified);
        vec![value; n]
    }
}

fn vector<'h>(hs: &mut GcHeapSession<'h>, args: Vec<Value<'h>>) -> Result<Trampoline<'h>> {
    Ok(Trampoline::Value(Vector(hs.alloc(args))))
}

builtins! {
    fn vector_length "vector-length" <'h>(_hs, v: VecRef<'h, Value<'h>>) -> usize {
        v.len()
    }

    fn vector_ref "vector-ref" <'h>(
        _hs, vec: VecRef<'h, Value<'h>>, index: usize
    ) -> Result<Value<'h>> {
        if index >= vec.len() {
            return Err(
                format!(
                    "vector-ref: index out of bounds (got {}, length {})",
                    index,
                    vec.len()
                ).into(),
            );
        }
        Ok(vec.get(index))
    }

    fn vector_set "vector-set!" <'h>(
        _hs, vec: VecRef<'h, Value<'h>>, index: usize, value: Value<'h>
    ) -> () {
        if index >= vec.len() {
            return Err(
                format!(
                    "vector-set!: index out of bounds (got {}, length {})",
                    index,
                    vec.len()
                ).into(),
            );
        }
        vec.set(index, value);
    }

    fn list_to_vector "list->vector" <'h>(hs, list: Value<'h>) -> Result<Vec<Value<'h>>> {
        list.into_iter().collect()
    }
}

// 6.9 Control features
builtins! {
    fn procedure_question "procedure?" <'h>(_hs, v: Value<'h>) -> bool {
        v.is_procedure()
    }
}

fn apply<'h>(_hs: &mut GcHeapSession<'h>, mut args: Vec<Value<'h>>) -> Result<Trampoline<'h>> {
    if args.len() < 2 {
        return Err("apply: at least 2 arguments required".into());
    }
    let trailing = args.pop().unwrap();
    let mut arg_vec = args.split_off(1);
    for arg in trailing {
        arg_vec.push(arg?);
    }
    assert_eq!(args.len(), 1);
    let func = args.pop().unwrap();

    Ok(Trampoline::TailCall {
        func,
        args: arg_vec,
    })
}

// 6.10 Input and output
builtins! {
    fn write "write" <'h>(_hs, obj: Value<'h>) -> Result<()> {
        let stdout = io::stdout();
        let mut guard = stdout.lock();
        write!(guard, "{}", obj)
            .chain_err(|| "error writing to stdout")
    }

    fn display "display" <'h>(_hs, obj: Value<'h>) -> Result<()> {
        let stdout = io::stdout();
        let mut guard = stdout.lock();
        write!(guard, "{}", value::DisplayValue(obj))
            .chain_err(|| "error writing to stdout")
    }

    fn newline "newline" <'h>(_hs) -> Result<()> {
        write!(io::stdout(), "\n")
            .chain_err(|| "error writing to stdout")
    }

    fn write_char "write-char" <'h>(_hs, c: char) -> Result<()> {
        write!(io::stdout(), "{}", c)
            .chain_err(|| "error writing to stdout")
    }
}

// Extensions
builtins! {
    fn assert "assert" <'h>(_hs, ok: bool, msg: Option<Value<'h>>) -> Result<()> {
        if ok {
            Ok(())
        } else if let Some(msg) = msg {
            Err(format!("assert: assertion failed: {:?}", msg).into())
        } else {
            Err("assert: assertion failed".into())
        }
    }

    fn gensym "gensym" <'h>(_hs) -> Value<'h> {
        let sym = InternedString::gensym();
        assert!(sym.is_gensym());
        Value::Symbol(GcLeaf::new(sym))
    }

    fn gensym_question "gensym?" <'h>(_hs, v: Value<'h>) -> bool {
        match v {
            Symbol(s) => s.is_gensym(),
            _ => false,
        }
    }

    fn eval "eval" <'h>(hs, expr: Value<'h>, env: EnvironmentRef<'h>) -> Result<Trampoline<'h>> {
        let expr = env.expand(hs, expr)?;
        let code = compile::compile_toplevel(hs, &env.senv(), expr)?;
        Ok(Trampoline::TailEval { code, env })
    }

    fn dis "dis" <'h>(_hs, expr: Value<'h>) -> Result<()> {
        if let Lambda(lambda) = expr {
            lambda.code().dump();
            Ok(())
        } else {
            Err("not a lambda".into())
        }
    }

}

// Builtin function list ///////////////////////////////////////////////////////

pub static BUILTINS: &[(&'static str, BuiltinFn)] = &[
    ("+", add),
    ("-", sub),
    ("*", mul),
    ("<", numeric_lt),
    ("<=", numeric_le),
    ("=", numeric_eq),
    (">", numeric_gt),
    (">=", numeric_ge),
    ("apply", apply),
    ("assert", assert),
    ("boolean?", boolean_question),
    ("car", car),
    ("cdr", cdr),
    ("char?", char_question),
    ("char-alphabetic?", char_alphabetic_question),
    ("char-downcase", char_downcase),
    ("char-lower-case?", char_lower_case_question),
    ("char-numeric?", char_numeric_question),
    ("char-upcase", char_upcase),
    ("char-upper-case?", char_upper_case_question),
    ("char-whitespace?", char_whitespace_question),
    ("char<?", char_lt),
    ("char<=?", char_le),
    ("char=?", char_eq),
    ("char>?", char_gt),
    ("char>=?", char_ge),
    ("cons", cons),
    ("dis", dis),
    ("display", display),
    ("eq?", eq_question),
    ("eqv?", eqv_question),
    ("eval", eval),
    ("gensym", gensym),
    ("gensym?", gensym_question),
    ("list->string", list_to_string),
    ("list->vector", list_to_vector),
    ("make-vector", make_vector),
    ("newline", newline),
    ("null?", null_question),
    ("number?", number_question),
    ("number->string", number_to_string),
    ("pair?", pair_question),
    ("procedure?", procedure_question),
    ("set-car!", set_car),
    ("set-cdr!", set_cdr),
    ("string", string),
    ("string?", string_question),
    ("string->list", string_to_list),
    ("string->symbol", string_to_symbol),
    ("string-append", string_append),
    ("string-length", string_length),
    ("string-ref", string_ref),
    ("string=?", string_eq_question),
    ("symbol?", symbol_question),
    ("symbol->string", symbol_to_string),
    ("vector", vector),
    ("vector?", vector_question),
    ("vector-length", vector_length),
    ("vector-ref", vector_ref),
    ("vector-set!", vector_set),
    ("write", write),
    ("write-char", write_char),
];

pub fn get_eval() -> BuiltinFn {
    eval
}

pub fn define_builtins<'h>(hs: &mut GcHeapSession<'h>, env: &EnvironmentRef<'h>) {
    for &(name, f) in BUILTINS {
        env.push(
            InternedString::get(name),
            Builtin(GcLeaf::new(BuiltinFnPtr(f))),
        );
    }

    // Call/cc is implemented as a "lambda" because builtins do not get full
    // access to all the information that makes up the current continuation.
    // We use a special opcode.
    let call_cc = {
        use compile::op;

        let insns = hs.alloc(vec![
            op::SAVE,              // save this lambda's continuation
            op::PUSH_ENV, 1,       // push "locals" environment
            op::SET_STATIC, 0, 0,  // store the saved stack in this local
            op::GET_STATIC, 1, 0,  // get procedure to invoke
            op::LAMBDA, 0,         // create the continuation procedure
            op::TAIL_CALL, 1,      // call procedure, passing current continuation
        ]);
        let names = hs.alloc(vec![GcLeaf::new(InternedString::get("proc"))]);
        let params = hs.alloc(StaticEnvironment { parent: Some(env.senv()), names });
        let names = hs.alloc(vec![GcLeaf::new(InternedString::get("saved-stack"))]);
        let locals = hs.alloc(StaticEnvironment { parent: Some(params.clone()), names });
        let environments = hs.alloc(vec![params, locals.clone()]);
        let ctn_code = {
            let insns = hs.alloc(vec![
                op::RESTORE
            ]);
            let environments = {
                let names = hs.alloc(vec![GcLeaf::new(InternedString::get("return-value"))]);
                let params = hs.alloc(StaticEnvironment { parent: Some(locals), names });
                hs.alloc(vec![params])
            };
            let constants = hs.alloc(Vec::<Value>::new());
            hs.alloc(Code { insns, environments, constants, rest: false, operands_max: 0 })
        };
        let constants = hs.alloc(vec![Value::Code(ctn_code)]);
        let code = hs.alloc(Code { insns, environments, constants, rest: false, operands_max: 2 });
        Value::Lambda(hs.alloc(Lambda { code, env: env.clone() }))
    };
    env.push(InternedString::get("call-with-current-continuation"), call_cc.clone());
    env.push(InternedString::get("call/cc"), call_cc);

    // One last extension, implemented as a "lambda" because builtins don't
    // have an environment pointer.
    let interaction_env_proc = env::constant_proc(hs, Value::Environment(env.clone()));
    env.push(
        InternedString::get("interaction-environment"),
        interaction_env_proc,
    );
}
