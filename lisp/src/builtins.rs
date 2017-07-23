use cell_gc::{GcHeapSession, GcLeaf};
use compile;
use value::{BuiltinFn, BuiltinFnPtr, Pair, Value, InternedString};
use value::Value::*;
use vm::{self, EnvironmentRef, Trampoline};

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

fn boolean_question<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    simple_predicate("boolean?", args, |v| v.is_boolean())
}

// 6.2 Equivalence predicates
fn eq_question<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    let first = args.get(0);
    Ok(Trampoline::Value(
        Bool(args.iter().all(|arg| Some(arg) == first)),
    ))
}

fn eqv_question<'h>(
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
fn pair_question<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    simple_predicate("pair?", args, |v| v.is_pair())
}

fn cons<'h>(
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

fn car<'h>(
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

fn cdr<'h>(
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

fn null_question<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    simple_predicate("null?", args, |v| v.is_nil())
}

fn set_car<'h>(
    _hs: &mut GcHeapSession<'h>,
    mut args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    if args.len() != 2 {
        return Err("set-car!: exactly 2 arguments required".into());
    }
    let obj = args.pop().unwrap();
    match args.pop().unwrap() {
        Cons(pair) => pair.set_car(obj),
        _ => return Err("set-car!: pair required".into()),
    }
    Ok(Trampoline::Value(Nil))
}

fn set_cdr<'h>(
    _hs: &mut GcHeapSession<'h>,
    mut args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    if args.len() != 2 {
        return Err("set-cdr!: exactly 2 arguments required".into());
    }
    let obj = args.pop().unwrap();
    match args.pop().unwrap() {
        Cons(pair) => pair.set_cdr(obj),
        _ => return Err("set-cdr!: pair required".into()),
    }
    Ok(Trampoline::Value(Nil))
}

// 6.4 Symbols
fn symbol_question<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    simple_predicate("symbol?", args, |v| v.is_symbol())
}

fn symbol_to_string<'h>(
    _hs: &mut GcHeapSession<'h>,
    mut args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    if args.len() != 1 {
        return Err("symbol->string: 1 argument required".into());
    }
    let sym = args.pop().unwrap().as_symbol("symbol->string")?;
    Ok(Trampoline::Value(ImmString(GcLeaf::new(sym.really_intern()))))
}

fn string_to_symbol<'h>(
    _hs: &mut GcHeapSession<'h>,
    mut args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    if args.len() != 1 {
        return Err("string->symbol: 1 argument required".into());
    }
    let str = args.pop().unwrap().as_string("string->symbol")?;
    Ok(Trampoline::Value(Symbol(GcLeaf::new(str.really_intern()))))
}

// 6.5 Numbers
fn number_question<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    simple_predicate("number?", args, |v| v.is_number())
}

fn numeric_compare<'h, F>(name: &'static str, args: Vec<Value<'h>>, cmp: F) -> Result<Trampoline<'h>, String>
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

fn numeric_eq<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    numeric_compare("=", args, |a, b| a == b)
}

fn numeric_lt<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    numeric_compare("<", args, |a, b| a < b)
}

fn numeric_gt<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    numeric_compare(">", args, |a, b| a > b)
}

fn numeric_le<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    numeric_compare("<=", args, |a, b| a <= b)
}

fn numeric_ge<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    numeric_compare(">=", args, |a, b| a >= b)
}

fn add<'h>(
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

fn mul<'h>(
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

fn sub<'h>(
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

fn number_to_string<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    if args.len() != 1 {
        return Err("number->string: 1 argument required".to_string());
    }
    let n = args[0].clone().as_int("number->string")?;
    let s = InternedString::get(format!("{}", n));  // heurgh!
    Ok(Trampoline::Value(Value::ImmString(GcLeaf::new(s))))
}


// 6.6 Characters
fn char_question<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    simple_predicate("char?", args, |v| v.is_char())
}

fn char_compare<'h, F>(name: &'static str, args: Vec<Value<'h>>, cmp: F) -> Result<Trampoline<'h>, String>
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

fn char_eq<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    char_compare("char=?", args, |a, b| a == b)
}

fn char_lt<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    char_compare("char<?", args, |a, b| a < b)
}

fn char_gt<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    char_compare("char>?", args, |a, b| a > b)
}

fn char_le<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    char_compare("char<=?", args, |a, b| a <= b)
}

fn char_ge<'h>( // CHAAAAARGE!
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    char_compare("char>=?", args, |a, b| a >= b)
}

fn char_predicate<'h, F>(
    name: &'static str,
    args: Vec<Value<'h>>,
    f: F
) -> Result<Trampoline<'h>, String>
    where
    F: Fn(char) -> bool,
{
    if args.len() != 1 {
        return Err(format!("{}: 1 argument required", name));
    }
    let c = args[0].clone().as_char("character required")?;
    Ok(Trampoline::Value(Value::Bool(f(c))))
}

fn char_alphabetic_question<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    char_predicate("char-alphabetic?", args, |c| c.is_alphabetic())
}

fn char_numeric_question<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    char_predicate("char-numeric?", args, |c| c.is_numeric())
}

fn char_whitespace_question<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    char_predicate("char-whitespace?", args, |c| c.is_whitespace())
}

fn char_upper_case_question<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    char_predicate("char-upper-case?", args, |c| c.is_uppercase())
}

fn char_lower_case_question<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    char_predicate("char-lower-case?", args, |c| c.is_lowercase())
}

fn char_upcase<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    if args.len() != 1 {
        return Err("char-upcase: 1 argument required".into());
    }
    let c = args[0].clone().as_char("char-upcase: character required")?;

    // I think the only character that doesn't upcase to a single character is
    // U+00DF LATIN SMALL LETTER SHARP S ('ß'). Guile returns it unchanged.
    // Fine with me.
    let mut up = c.to_uppercase();
    let result = match (up.next(), up.next()) {
        (Some(d), None) => d,
        _ => c
    };

    Ok(Trampoline::Value(Value::Char(result)))
}

fn char_downcase<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    if args.len() != 1 {
        return Err("char-downcase: 1 argument required".into());
    }
    let c = args[0].clone().as_char("char-downcase: character required")?;

    // I think the only character that doesn't downcase to a single character
    // is U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE ('İ'). Guile converts it
    // to 'i'.  Fine with me.
    let mut up = c.to_lowercase();
    let result = up.next().unwrap_or(c);

    Ok(Trampoline::Value(Value::Char(result)))
}


// 6.7 Strings
fn string_question<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    simple_predicate("string?", args, |v| v.is_string())
}

fn string<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    let mut s = String::new();
    for arg in args {
        s.push(arg.as_char("string")?);
    }
    Ok(Trampoline::Value(Value::ImmString(GcLeaf::new(InternedString::get(s)))))
}

fn string_length<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    if args.len() != 1 {
        return Err("string-length: 1 argument required".to_string());
    }
    let s = args[0].clone().as_string("string-length")?;
    let n = s.as_str().chars().count();  // bleurgh! O(n)
    if n > i32::max_value() as usize {
        return Err("string-length: integer overflow".into());
    }
    Ok(Trampoline::Value(Value::Int(n as i32)))
}

fn string_ref<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    if args.len() != 2 {
        return Err("string-ref: 2 arguments required".into())
    }
    let s = args[0].clone().as_string("string-ref")?;
    let i = args[1].clone().as_index("string-ref")?;
    match s.as_str().chars().nth(i) {  // bleurgh! O(n)
        Some(c) => Ok(Trampoline::Value(Value::Char(c))),
        None => Err("string-ref: index out of range".into())
    }
}

fn string_eq_question<'h>(
    _hs: &mut GcHeapSession<'h>,
    mut args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    if args.len() != 2 {
        return Err("string=?: exactly 2 arguments required".into());
    }
    let b = args.pop().unwrap().as_string("string=?")?;
    let a = args.pop().unwrap().as_string("string=?")?;
    Ok(Trampoline::Value(Bool(a.as_str() == b.as_str())))
}

fn string_append<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    let s =
        args.into_iter()
        .map(|v| v.as_string("string-append"))
        .collect::<Result<Vec<InternedString>, String>>()?
        .concat();

    let in_str = InternedString::get(s);
    Ok(Trampoline::Value(Value::ImmString(GcLeaf::new(in_str))))
}

fn list_to_string<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    if args.len() != 1 {
        return Err("list->string: 1 argument required".into());
    }
    let s: String =
        args.into_iter()
        .map(|v| v.as_char("list->string: list of characters required"))
        .collect::<Result<String, String>>()?;
    let in_str = InternedString::get(s);
    Ok(Trampoline::Value(Value::ImmString(GcLeaf::new(in_str))))
}


// 6.8 Vectors
fn vector_question<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    simple_predicate("vector?", args, |v| v.is_vector())
}

fn make_vector<'h>(
    hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    let value =
        if args.len() == 1 {
            Value::Nil
        } else if args.len() == 2 {
            args[1].clone()
        } else {
            return Err("make-vector: 1 or 2 arguments required".into());
        };
    let n = args[0].clone().as_index("make-vector")?;
    Ok(Trampoline::Value(Vector(hs.alloc(vec![value; n]))))
}

fn vector<'h>(
    hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    Ok(Trampoline::Value(Vector(hs.alloc(args))))
}

fn vector_length<'h>(
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

fn vector_ref<'h>(
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

fn vector_set<'h>(
    _hs: &mut GcHeapSession<'h>,
    mut args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    if args.len() != 3 {
        return Err("vector-set!: exactly 3 arguments required".into());
    }
    let value = args.pop().unwrap();
    let index = args.pop().unwrap().as_index("vector-set!")?;
    let v = args.pop().unwrap().as_vector("vector-set!")?;
    if index >= v.len() {
        return Err(format!(
            "vector-set!: index out of bounds (got {}, length {})",
            index,
            v.len()
        ));
    }
    v.set(index, value);
    Ok(Trampoline::Value(Value::Nil))
}

fn list_to_vector<'h>(
    hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    if args.len() != 1 {
        return Err("list->vector: exactly 1 argument required".into());
    }

    let v: Result<Vec<Value<'h>>, String> = args[0].clone()
        .into_iter()
        .collect();

    Ok(Trampoline::Value(Value::Vector(hs.alloc(v?))))
}

// 6.9 Control features
fn procedure_question<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    simple_predicate("procedure?", args, |v| v.is_procedure())
}

fn apply<'h>(
    _hs: &mut GcHeapSession<'h>,
    mut args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
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

    Ok(Trampoline::TailCall { func, args: arg_vec })
}

// Extensions
fn print<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    let strings: Vec<String> =
        args.into_iter().map(|v| format!("{}", v)).collect();
    println!("{}", strings.join(" "));
    Ok(Trampoline::Value(Nil))
}

fn assert<'h>(
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

fn gensym<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    if !args.is_empty() {
        return Err("gensym: 0 arguments required".into());
    }
    let sym = InternedString::gensym();
    assert!(sym.is_gensym());
    Ok(Trampoline::Value(Symbol(GcLeaf::new(sym))))
}

fn gensym_question<'h>(
    _hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    simple_predicate("gensym?", args, |v| {
        match v {
            Symbol(s) => s.is_gensym(),
            _ => false
        }
    })
}

fn eval<'h>(
    hs: &mut GcHeapSession<'h>,
    args: Vec<Value<'h>>,
) -> Result<Trampoline<'h>, String> {
    if args.len() != 2 {
        return Err("eval: 2 arguments required".into());
    }
    let expr = args[0].clone();
    let expr_compiled = compile::compile_toplevel(hs, expr)?;
    let env = args[1].clone().as_environment("eval: environment required")?;

    vm::eval_to_tail_call(hs, expr_compiled, env)
}

// Builtin function list ///////////////////////////////////////////////////////

pub static BUILTINS: &[(&'static str, BuiltinFn)] = &[
    ("+", add),
    ("-", sub),
    ("*", mul),
    ("=", numeric_eq),
    ("<", numeric_lt),
    (">", numeric_gt),
    ("<=", numeric_le),
    (">=", numeric_ge),
    ("apply", apply),
    ("assert", assert),
    ("car", car),
    ("cdr", cdr),
    ("char?", char_question),
    ("char=?", char_eq),
    ("char<?", char_lt),
    ("char>?", char_gt),
    ("char<=?", char_le),
    ("char>=?", char_ge),
    ("char-alphabetic?", char_alphabetic_question),
    ("char-numeric?", char_numeric_question),
    ("char-whitespace?", char_whitespace_question),
    ("char-upper-case?", char_upper_case_question),
    ("char-lower-case?", char_lower_case_question),
    ("char-upcase", char_upcase),
    ("char-downcase", char_downcase),
    ("cons", cons),
    ("eqv?", eqv_question),
    ("eq?", eq_question),
    ("eval", eval),
    ("gensym", gensym),
    ("gensym?", gensym_question),
    ("print", print),
    ("boolean?", boolean_question),
    ("list->string", list_to_string),
    ("list->vector", list_to_vector),
    ("make-vector", make_vector),
    ("null?", null_question),
    ("number?", number_question),
    ("number->string", number_to_string),
    ("pair?", pair_question),
    ("procedure?", procedure_question),
    ("set-car!", set_car),
    ("set-cdr!", set_cdr),
    ("string", string),
    ("string->symbol", string_to_symbol),
    ("string?", string_question),
    ("string=?", string_eq_question),
    ("string-append", string_append),
    ("string-length", string_length),
    ("string-ref", string_ref),
    ("symbol->string", symbol_to_string),
    ("symbol?", symbol_question),
    ("vector?", vector_question),
    ("vector", vector),
    ("vector-length", vector_length),
    ("vector-ref", vector_ref),
    ("vector-set!", vector_set),
];

pub fn define_builtins<'h>(hs: &mut GcHeapSession<'h>, env: EnvironmentRef<'h>) {
    for &(name, f) in BUILTINS {
        env.push(
            InternedString::get(name),
            Builtin(GcLeaf::new(BuiltinFnPtr(f))),
        );
    }

    // One last extension, implemented as a "lambda" because builtins don't
    // have an environment pointer. It is not actually possible to write this
    // lambda in scheme, though.
    use compile::{Expr, Code, CodeRef};

    let params = hs.alloc(vec![]);
    let code: CodeRef<'h> =
        hs.alloc(Code {
            params,
            rest: false,
            body: Expr::ToplevelEnv,
        });
    let interaction_environment_fn =
        Lambda(hs.alloc(Pair {
            car: Value::Code(code),
            cdr: Value::Environment(env.clone()),
        }));
    env.push(
        InternedString::get("interaction-environment"),
        interaction_environment_fn);
}
