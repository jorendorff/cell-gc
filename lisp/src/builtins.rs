//! Essential procedures.

use cell_gc::{GcHeapSession, GcLeaf};
use cell_gc::collections::VecRef;
use compile;
use env::{self, EnvironmentRef};
use errors::*;
use std::io::{self, Write};
use std::sync::Arc;
use value::{self, BuiltinFn, BuiltinFnPtr, InternedString, Pair, Value};
use value::{ArgType, Rest, RetType};
use value::Value::*;
use vm::{self, Frame, FrameRef};


// The builtins! macro /////////////////////////////////////////////////////////

fn check_done<'h>(proc_name: &str, iter: &mut Value<'h>) -> Result<()> {
    match *iter {
        Value::Nil => Ok(()),
        _ => Err(format!("{}: too many arguments", proc_name).into()),
    }
}

macro_rules! builtins {
    {
        $(
            fn $name:ident $namestr:tt <$h:tt>($hs:ident $(, $arg:ident : $argty:ty )* )
            -> $retty:ty
            $body:block
        )*
    } => {
        $(
            fn $name<$h>(
                $hs: &mut GcHeapSession<$h>,
                mut args: Value<$h>,
                ctn: Option<FrameRef<$h>>,
            ) -> Result<(Value<$h>, Option<FrameRef<$h>>)> {
                $( let $arg = <$argty as ArgType<$h>>::unpack_argument($namestr, &mut args)?; )*
                check_done($namestr, &mut args)?;
                let return_value = <$retty as RetType<$h>>::pack($body, $hs)?;
                Ok((return_value, ctn))
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
    fn eq_question "eq?" <'h>(_hs, args: Rest<'h>) -> Result<bool> {
        let mut args = args;
        let mut all_equal = true;
        if let Some(item) = args.next() {
            let first = item?;
            for item in args {
                if first != item? {
                    all_equal = false;
                    break;
                }
            }
        }
        Ok(all_equal)
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
    mut args: Value<'h>,
    cmp: F,
    ctn: Option<FrameRef<'h>>
) -> Result<(Value<'h>, Option<FrameRef<'h>>)>
where
    F: Fn(i32, i32) -> bool,
{
    if let Some(arg0) = args.next() {
        let mut prev = arg0?.as_int(name)?;
        for arg in args {
            let x = arg?.as_int(name)?;
            if !cmp(prev, x) {
                return Ok((Bool(false), ctn));
            }
            prev = x;
        }
    }
    Ok((Bool(true), ctn))
}

macro_rules! numeric_compare {
    ($fnname:ident, $name:expr, $op:tt) => {
        fn $fnname<'h>(
            _hs: &mut GcHeapSession<'h>,
            args: Value<'h>,
            ctn: Option<FrameRef<'h>>
        ) -> Result<(Value<'h>, Option<FrameRef<'h>>)> {
            numeric_compare($name, args, |a, b| a $op b, ctn)
        }
    }
}

numeric_compare!(numeric_eq, "=", ==);
numeric_compare!(numeric_lt, "<", <);
numeric_compare!(numeric_gt, ">", >);
numeric_compare!(numeric_le, "<=", <=);
numeric_compare!(numeric_ge, ">=", >=);

builtins! {
    fn add "+" <'h>(_hs, args: Rest<'h>) -> Result<i32> {
        let mut total = 0;
        for v in args {
            if let Int(n) = v? {
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
            if let Int(n) = v? {
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
            if let Int(subtrahend) = v? {
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

fn char_compare<'h, F>(
    name: &'static str,
    mut arg_list: Value<'h>,
    cmp: F,
    ctn: Option<FrameRef<'h>>,
) -> Result<(Value<'h>, Option<FrameRef<'h>>)>
where
    F: Fn(char, char) -> bool,
{
    if let Some(arg0) = arg_list.next() {
        let mut prev = arg0?.as_char(name)?;
        for arg in arg_list {
            let x = arg?.as_char(name)?;
            if !cmp(prev, x) {
                return Ok((Bool(false), ctn));
            }
            prev = x;
        }
    }
    Ok((Bool(true), ctn))
}

macro_rules! char_compare_builtin {
    ($fnname:ident, $name:expr, $op:tt) => {
        fn $fnname<'h>(
            _hs: &mut GcHeapSession<'h>,
            arg_list: Value<'h>,
            ctn: Option<FrameRef<'h>>
        ) -> Result<(Value<'h>, Option<FrameRef<'h>>)> {
            char_compare($name, arg_list, |a, b| a $op b, ctn)
        }
    }
}

char_compare_builtin!(char_eq, "char=?", ==);
char_compare_builtin!(char_lt, "char<?", <);
char_compare_builtin!(char_gt, "char>?", >);
char_compare_builtin!(char_le, "char<=?", <=);
char_compare_builtin!(char_ge, "char>=?", >=); // CHAAAAARGE!

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
            s.push(arg?.as_char("string")?);
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
            .map(|v| -> Result<Arc<String>> { v?.as_string("string-append") })
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

    fn vector "vector" <'h>(hs, args: Rest<'h>) -> Result<Vec<Value<'h>>> {
        args.collect()
    }

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

fn apply<'h>(
    hs: &mut GcHeapSession<'h>,
    args: Value<'h>,
    ctn: Option<FrameRef<'h>>
) -> Result<(Value<'h>, Option<FrameRef<'h>>)> {
    // Parse arguments, which are super annoying for this one
    let (func, args) = args.as_pair("apply")?;
    let (first, mut args) = args.as_pair("apply")?;
    let actual;
    if args.is_nil() {
        actual = first;
    } else {
        let mut last_link = hs.alloc(Pair { car: first, cdr: Value::Nil });
        actual = Value::Cons(last_link.clone());
        while let Some(element) = args.next() {
            if args.is_nil() { // last element
                last_link.set_cdr(element?);
            } else {
                let new_link = hs.alloc(Pair {car: element?, cdr: Value::Nil });
                last_link.set_cdr(Value::Cons(new_link.clone()));
                last_link = new_link;
            }
        }
    }

    vm::partial_apply(hs, func, actual, ctn)
}

fn call_cc<'h>(
    hs: &mut GcHeapSession<'h>,
    args: Value<'h>,
    ctn: Option<FrameRef<'h>>
) -> Result<(Value<'h>, Option<FrameRef<'h>>)> {
    let (func, rest) = args.as_pair("call/cc")?;
    if !rest.is_nil() {
        return Err("call/cc: too many arguments".into());
    }

    let ctn_func = Value::Continuation(ctn.clone());
    let arg_list = Value::Cons(hs.alloc(Pair { car: ctn_func, cdr: Value::Nil }));
    vm::partial_apply(hs, func, arg_list, ctn)
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

fn eval<'h>(
    hs: &mut GcHeapSession<'h>,
    operands: Value<'h>,
    stack: Option<FrameRef<'h>>,
) -> Result<(Value<'h>, Option<FrameRef<'h>>)> {
    // Parse arguments.
    let (expr, rest) = operands.as_pair("eval")?;
    let (env_value, rest) = rest.as_pair("eval")?;
    let env = env_value.as_environment("eval")?;
    if !rest.is_nil() {
        return Err("eval: too many arguments".into());
    }

    // Compile the expression.
    let expr = env.expand(hs, expr)?;
    let code = compile::compile_toplevel(hs, &env.senv(), expr)?;

    // Create and return the appropriate command continuation.
    let frame = hs.alloc(Frame {
        parent: stack,
        code,
        pc: 0,
        env,
        operands: Value::Nil,
    });
    Ok((Value::Unspecified, Some(frame)))
}

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
    ("call/cc", call_cc),
    ("call-with-current-continuation", call_cc),
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

    // One last extension, implemented as a "lambda" because builtins don't
    // have an environment pointer.
    let interaction_env_proc = env::constant_proc(hs, Value::Environment(env.clone()));
    env.push(
        InternedString::get("interaction-environment"),
        interaction_env_proc,
    );
}
