//! Essential procedures.

use cell_gc::{GcHeapSession, GcLeaf};
use cell_gc::collections::VecRef;
use compile::{self, Code};
use env::{self, EnvironmentRef, StaticEnvironment};
use errors::*;
use ports::{self, PortRef};
use std::fmt;
use std::fs::File;
use std::io::{self, BufReader, BufWriter, Write};
use std::path::Path;
use std::sync::Arc;
use std::vec;
use value::{self, BuiltinFn, BuiltinFnPtr, ConstBytevector, InternedString, Lambda, Pair, Value};
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

    fn quotient "quotient" <'h>(_hs, n1: i32, n2: i32) -> Result<i32> {
        if n2 == 0 {
            Err("quotient: divide by zero".into())
        } else if n1 == i32::min_value() && n2 == -1 {
            Err("quotient: integer overflow".into())
        } else {
            Ok(n1 / n2)
        }
    }

    fn remainder "remainder" <'h>(_hs, n1: i32, n2: i32) -> Result<i32> {
        if n2 == 0 {
            Err("remainder: divide by zero".into())
        } else {
            Ok(n1 % n2)
        }
    }

    fn modulo "modulo" <'h>(_hs, n1: i32, n2: i32) -> Result<i32> {
        if n2 == 0 {
            Err("modulo: divide by zero".into())
        } else {
            let rem = n1 % n2;
            if n1.signum() != n2.signum() && rem != 0 {
                Ok(rem + n2)
            } else {
                Ok(rem)
            }
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

// R7RS 6.9 Bytevectors

fn optional_range(
    proc_name: &str,
    start: Option<usize>,
    end: Option<usize>,
    len: usize,
) -> Result<(usize, usize)> {
    let start = match start {
        Some(j) if j > len => return Err(
            format!("{}: start index {} out of range {}", proc_name, j, len).into()
        ),
        Some(j) => j,
        None => 0
    };
    let end = match end {
        Some(k) if k < start => return Err(
            format!("{}: start index {} > end index {}", proc_name, start, k).into()
        ),
        Some(k) => k,
        None => len
    };
    Ok((start, end))
}

builtins! {
    fn bytevector_question "bytevector?" <'h>(_hs, v: Value<'h>) -> bool {
        v.is_bytevector()
    }

    fn make_bytevector "make-bytevector" <'h>(_hs, len: usize, byte: Option<u8>) -> Vec<u8> {
        vec![byte.unwrap_or(0u8); len]
    }

    fn bytevector "bytevector" <'h>(_hs, rest: Rest<'h>) -> Result<Vec<u8>> {
        let mut vec = Vec::with_capacity(rest.len());
        for v in rest {
            vec.push(v.as_byte("bytevector")?);
        }
        Ok(vec)
    }

    fn bytevector_length "bytevector-length" <'h>(_hs, bytevector: ConstBytevector<'h>) -> usize {
        bytevector.0.len()
    }

    fn bytevector_u8_ref "bytevector-u8-ref" <'h>(_hs, bytevector: ConstBytevector<'h>, k: usize) -> Result<u8> {
        if k >= bytevector.0.len() {
            return Err(
                format!("bytevector-u8-ref: index {} out of range {}", k, bytevector.0.len()).into()
            );
        }
        Ok(bytevector.0.get(k))
    }

    fn bytevector_u8_set "bytevector-u8-set!" <'h>(
        _hs, bytevector: VecRef<'h, u8>, k: usize, byte: u8
    ) -> Result<()> {
        if k >= bytevector.len() {
            return Err(
                format!("bytevector-u8-set!: index {} out of range {}", k, bytevector.len()).into()
            );
        }
        bytevector.set(k, byte);
        Ok(())
    }

    fn bytevector_copy "bytevector-copy" <'h>(
        _hs,
        bytevector: ConstBytevector<'h>,
        start: Option<usize>,
        end: Option<usize>
    ) -> Result<Vec<u8>> {
        let len = bytevector.0.len();
        let (start, end) = optional_range("bytevector-copy", start, end, len)?;
        let mut bytes = Vec::with_capacity(end - start);  // bad: slow copy
        for i in start..end {
            bytes.push(bytevector.0.get(i));
        }
        Ok(bytes)
    }

    fn bytevector_copy_mut "bytevector-copy!" <'h>(
        _hs,
        to: VecRef<'h, u8>,
        at: usize,
        from: ConstBytevector<'h>,
        start: Option<usize>,
        end: Option<usize>
    ) -> Result<()> {
        if at > to.len() {
            return Err(
                format!("bytevector-copy!: second argument {} greater than destination bytevector length {}",
                        at, to.len()).into()
            );
        }
        let len = from.0.len();
        let (start, end) = optional_range("bytevector-copy!", start, end, len)?;
        if to.len() - at < end - start {
            return Err(
                format!("bytevector-copy!: can't copy range from {} to {} ({} bytes) \
                         into bytevector of length {} starting at {} ({} bytes)",
                        start, end, end - start,
                        to.len(), at, to.len() - at).into()
            );
        }

        let mut j = at;
        for i in start .. end {
            to.set(j, from.0.get(i));
            j += 1;
        }
        Ok(())
    }

    fn bytevector_append "bytevector-append" <'h>(_hs, args: Rest<'h>) -> Result<Vec<u8>> {
        let mut bytes = vec![];
        for v in args {
            bytes.extend(v.as_bytevector("bytevector-append")?.0.into_iter());
        }
        Ok(bytes)
    }

    fn utf8_to_string "utf8->string" <'h>(
        _hs,
        bytevector: ConstBytevector<'h>,
        start: Option<usize>,
        end: Option<usize>
    ) -> Result<String> {
        let len = bytevector.0.len();
        let (start, end) = optional_range("utf8->string", start, end, len)?;
        let mut bytes = Vec::with_capacity(end - start);
        for i in start..end {
            bytes.push(bytevector.0.get(i));
        }
        String::from_utf8(bytes).chain_err(|| "utf8->string: invalid UTF-8")
    }

    // TODO: support character indices (lame)
    fn string_to_utf8 "string->utf8" <'h>(_hs, string: Arc<String>) -> Vec<u8> {
        let slice: &str = &string;
        slice.into()
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

// R7RS 6.13 Input and output

builtins! {
    fn input_port_question "input-port?" <'h>(_hs, v: Value<'h>) -> bool {
        v.is_input_port()
    }

    fn output_port_question "output-port?" <'h>(_hs, v: Value<'h>) -> bool {
        v.is_output_port()
    }

    fn textual_port_question "textual-port?" <'h>(_hs, v: Value<'h>) -> bool {
        v.is_textual_port()
    }

    fn binary_port_question "binary-port?" <'h>(_hs, v: Value<'h>) -> bool {
        v.is_binary_port()
    }

    fn port_question "port?" <'h>(_hs, v: Value<'h>) -> bool {
        v.is_port()
    }

    fn input_port_open_question "input-port-open?" <'h>(_hs, port: PortRef<'h>) -> Result<bool> {
        let arc = port.port_arc();
        let guard = arc.lock().expect("port is poisoned");
        guard.is_input_open()
    }

    fn output_port_open_question "output-port-open?" <'h>(_hs, port: PortRef<'h>) -> Result<bool> {
        let arc = port.port_arc();
        let guard = arc.lock().expect("port is poisoned");
        guard.is_output_open()
    }

    fn open_input_file "open-input-file" <'h>(hs, filename: Arc<String>) -> Result<Value<'h>> {
        let path = Path::new(&filename as &str);
        let file = File::open(path).chain_err(|| format!("open-input-file: error opening {:?}", path))?;
        Ok(ports::buf_read_into_textual_input_port(hs, BufReader::new(file)))
    }

    fn open_binary_input_file "open-binary-input-file" <'h>(hs, filename: Arc<String>) -> Result<Value<'h>> {
        let path = Path::new(&filename as &str);
        let file = File::open(path).chain_err(|| format!("open-input-file: error opening {:?}", path))?;
        Ok(ports::buf_read_into_binary_input_port(hs, BufReader::new(file)))
    }

    fn open_output_file "open-output-file" <'h>(hs, filename: Arc<String>) -> Result<Value<'h>> {
        let path = Path::new(&filename as &str);
        let file = File::create(path).chain_err(|| format!("open-output-file: error opening {:?}", path))?;
        Ok(ports::writer_into_textual_output_port(hs, BufWriter::new(file)))
    }

    fn open_binary_output_file "open-binary-output-file" <'h>(hs, filename: Arc<String>) -> Result<Value<'h>> {
        let path = Path::new(&filename as &str);
        let file = File::create(path).chain_err(|| format!("open-binary-output-file: error opening {:?}", path))?;
        Ok(ports::writer_into_binary_output_port(hs, BufWriter::new(file)))
    }

    fn close_port "close-port" <'h>(_hs, port: PortRef<'h>) -> Result<()> {
        let arc = port.port_arc();
        let mut guard = arc.lock().expect("port is poisoned");
        guard.close()
    }

    fn close_input_port "close-input-port" <'h>(_hs, port: PortRef<'h>) -> Result<()> {
        let arc = port.port_arc();
        let mut guard = arc.lock().expect("port is poisoned");
        guard.close_input()
    }

    fn close_output_port "close-output-port" <'h>(_hs, port: PortRef<'h>) -> Result<()> {
        let arc = port.port_arc();
        let mut guard = arc.lock().expect("port is poisoned");
        guard.close_output()
    }

    fn open_input_string "open-input-string" <'h>(hs, s: Arc<String>) -> Value<'h> {
        let v: Vec<u8> = s.as_bytes().to_owned();
        ports::buf_read_into_textual_input_port(hs, io::Cursor::new(v))
    }

    fn open_output_string "open-output-string" <'h>(hs) -> Value<'h> {
        ports::new_output_string_port(hs)
    }

    fn get_output_string "get-output-string" <'h>(hs, port: PortRef<'h>) -> Result<String> {
        let arc = port.port_arc();
        let mut guard = arc.lock().expect("port is poisoned");
        guard.get_output_string()
    }

    fn read "read" <'h>(hs, port: Option<PortRef<'h>>) -> Result<Value<'h>> {
        use ports::TextualInputPort;

        match port {
            None => ports::StdinPort::new().read(hs),
            Some(port) => {
                let arc = port.port_arc();
                let mut guard = arc.lock().expect("port is poisoned");
                guard.as_open_textual_input()?.read(hs)
            }
        }
    }

    fn read_char "read-char" <'h>(_hs, port: Option<PortRef<'h>>) -> Result<Value<'h>> {
        use ports::TextualInputPort;

        let result = match port {
            None => ports::StdinPort::new().read_char(),
            Some(port) => {
                let arc = port.port_arc();
                let mut guard = arc.lock().expect("port is poisoned");
                guard.as_open_textual_input()?.read_char()
            }
        };
        Ok(match result? {
            None => Value::EofObject,
            Some(c) => Value::Char(c),
        })
    }

    fn peek_char "peek-char" <'h>(_hs, port: Option<PortRef<'h>>) -> Result<Value<'h>> {
        use ports::TextualInputPort;

        let result = match port {
            None => ports::StdinPort::new().peek_char(),
            Some(port) => {
                let arc = port.port_arc();
                let mut guard = arc.lock().expect("port is poisoned");
                guard.as_open_textual_input()?.peek_char()
            }
        };
        Ok(match result? {
            None => Value::EofObject,
            Some(c) => Value::Char(c)
        })
    }

    fn read_line "read-line" <'h>(_hs, port: Option<PortRef<'h>>) -> Result<String> {
        use ports::TextualInputPort;

        match port {
            None => ports::StdinPort::new().read_line(),
            Some(port) => {
                let arc = port.port_arc();
                let mut guard = arc.lock().expect("port is poisoned");
                guard.as_open_textual_input()?.read_line()
            }
        }
    }

    fn eof_object_question "eof-object?" <'h>(_hs, v: Value<'h>) -> bool {
        v.is_eof_object()
    }

    fn eof_object "eof-object" <'h>(_hs) -> Value<'h> {
        Value::EofObject
    }

    fn char_ready_question "char-ready?" <'h>(_hs, port: Option<PortRef<'h>>) -> Result<bool> {
        use ports::TextualInputPort;

        match port {
            None => ports::StdinPort::new().is_char_ready(),
            Some(port) => {
                let arc = port.port_arc();
                let mut guard = arc.lock().expect("port is poisoned");
                guard.as_open_textual_input()?.is_char_ready()
            }
        }
    }

    fn read_string "read-string" <'h>(_hs, k: usize, port: Option<PortRef<'h>>) -> Result<String> {
        use ports::TextualInputPort;

        match port {
            None => ports::StdinPort::new().read_string(k),
            Some(port) => {
                let arc = port.port_arc();
                let mut guard = arc.lock().expect("port is poisoned");
                guard.as_open_textual_input()?.read_string(k)
            }
        }
    }

    fn read_u8 "read-u8" <'h>(_hs, port: Option<PortRef<'h>>) -> Result<Value<'h>> {
        use ports::BinaryInputPort;

        let result = match port {
            None => ports::StdinPort::new().read_u8(),
            Some(port) => {
                let arc = port.port_arc();
                let mut guard = arc.lock().expect("port is poisoned");
                guard.as_open_binary_input()?.read_u8()
            }
        };
        match result? {
            None => Ok(Value::EofObject),
            Some(u) => Ok(Value::Int(u as i32)),
        }
    }

    fn peek_u8 "peek-u8" <'h>(_hs, port: Option<PortRef<'h>>) -> Result<Value<'h>> {
        use ports::BinaryInputPort;

        let result = match port {
            None => ports::StdinPort::new().peek_u8(),
            Some(port) => {
                let arc = port.port_arc();
                let mut guard = arc.lock().expect("port is poisoned");
                guard.as_open_binary_input()?.peek_u8()
            }
        };
        match result? {
            None => Ok(Value::EofObject),
            Some(u) => Ok(Value::Int(u as i32)),
        }
    }

    fn read_bytevector "read-bytevector" <'h>(
        hs,
        k: usize,
        port: Option<PortRef<'h>>
    ) -> Result<Value<'h>> {
        use ports::BinaryInputPort;

        let mut buf = vec![0; k];
        let bytes_read = match port {
            None => ports::StdinPort::new().read_into(&mut buf),
            Some(port) => {
                let arc = port.port_arc();
                let mut guard = arc.lock().expect("port is poisoned");
                guard.as_open_binary_input()?.read_into(&mut buf)
            }
        };

        match bytes_read? {
            None => Ok(Value::EofObject),
            Some(u) => {
                buf.truncate(u);
                Ok(Value::MutBytevector(hs.alloc(buf)))
            }
        }
    }

    fn read_bytevector_mut "read-bytevector!" <'h>(
        hs,
        bytevector: VecRef<'h, u8>,
        port: Option<PortRef<'h>>,
        start: Option<usize>,
        end: Option<usize>
    ) -> Result<Value<'h>> {
        use ports::BinaryInputPort;

        let (start, end) = optional_range("read-bytevector!", start, end, bytevector.len())?;
        let mut buf = vec![0; end - start]; // bad, forced to copy
        let bytes_read = match port {
            None => ports::StdinPort::new().read_into(&mut buf),
            Some(port) => {
                let arc = port.port_arc();
                let mut guard = arc.lock().expect("port is poisoned");
                guard.as_open_binary_input()?.read_into(&mut buf)
            }
        };

        match bytes_read? {
            None => Ok(Value::EofObject),
            Some(u) => {
                for i in 0..u {
                    bytevector.set(start + i, buf[i]);
                }
                // So far, there's no way to create a bytevector with a size
                // larger than i32::max.
                assert!(u > i32::max_value() as usize);
                Ok(Value::Int(u as i32))
            }
        }
    }
}

fn write_to_port<'h, T: fmt::Display>(proc_name: &str, value: T, out: Option<PortRef<'h>>) -> Result<()> {
    match out {
        None => {
            let stdout = io::stdout();
            let mut guard = stdout.lock();
            write!(guard, "{}", value)
                .chain_err(|| format!("{}: error writing to stdout", proc_name))
        }
        Some(port) => {
            let arc = port.port_arc();
            let mut guard = arc.lock().expect("port is poisoned");
            let w = guard.as_open_textual_output()?;
            write!(w, "{}", value)
                .chain_err(|| format!("{}: error writing to port", proc_name))
        }
    }
}

builtins! {
    fn write "write" <'h>(_hs, obj: Value<'h>, out: Option<PortRef<'h>>) -> Result<()> {
        write_to_port("write", obj, out)
    }

    fn display "display" <'h>(_hs, obj: Value<'h>, out: Option<PortRef<'h>>) -> Result<()> {
        write_to_port("display", value::DisplayValue(obj), out)
    }

    fn write_char "write-char" <'h>(_hs, c: char, out: Option<PortRef<'h>>) -> Result<()> {
        write_to_port("write-char", c, out)
    }

    fn flush_output_port "flush-output-port" <'h>(_hs, out: Option<PortRef<'h>>) -> Result<()> {
        match out {
            None => io::stdout().flush().chain_err(|| "flush-output-port: "),
            Some(port) => {
                let arc = port.port_arc();
                let mut guard = arc.lock().expect("port is poisoned");
                let w = if guard.is_textual_output() {
                    guard.as_open_textual_output()
                } else {
                    guard.as_open_binary_output()
                }?;
                w.flush().chain_err(|| "flush-output-port: ")
            }
        }
    }
}


// Extensions
builtins! {
    fn assert "assert" <'h>(_hs, ok: bool, msg: Option<Value<'h>>) -> Result<()> {
        if ok {
            Ok(())
        } else if let Some(msg) = msg {
            Err(format!("assert: assertion failed: {}", msg).into())
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

    // Parse the given string. Return value is a keyword-payload pair,
    // (cons 'ok <list>) if data is a list of parsed values;
    // (cons 'error <message>) where message is a string;
    // (cons 'incomplete '()) if data doesn't parse, but is a prefix of something that would.
    fn parse "parse" <'h>(hs, data: Arc<String>) -> Value<'h> {
        use parse;
        let (keyword, payload) =
            match parse::parse(hs, &data) {
                Ok(values) => {
                    let mut out = Value::Nil;
                    for v in values.into_iter().rev() {
                        out = Value::Cons(hs.alloc(Pair {
                            car: v,
                            cdr: out
                        }));
                    }
                    ("ok", out)
                }
                Err(ref err) if err.description() == "parse error: unexpected end of input" =>
                    ("incomplete", Value::Nil),
                Err(err) =>
                    ("error", Value::ImmString(GcLeaf::new(InternedString::get(err.description())))),
            };
        Value::Cons(hs.alloc(Pair {
            car: Value::Symbol(GcLeaf::new(InternedString::get(keyword))),
            cdr: payload,
        }))
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
    ("binary-port?", binary_port_question),
    ("boolean?", boolean_question),
    ("bytevector", bytevector),
    ("bytevector?", bytevector_question),
    ("bytevector-append", bytevector_append),
    ("bytevector-copy", bytevector_copy),
    ("bytevector-copy!", bytevector_copy_mut),
    ("bytevector-length", bytevector_length),
    ("bytevector-u8-ref", bytevector_u8_ref),
    ("bytevector-u8-set!", bytevector_u8_set),
    ("car", car),
    ("cdr", cdr),
    ("char?", char_question),
    ("char-alphabetic?", char_alphabetic_question),
    ("char-downcase", char_downcase),
    ("char-lower-case?", char_lower_case_question),
    ("char-numeric?", char_numeric_question),
    ("char-ready?", char_ready_question),
    ("char-upcase", char_upcase),
    ("char-upper-case?", char_upper_case_question),
    ("char-whitespace?", char_whitespace_question),
    ("char<?", char_lt),
    ("char<=?", char_le),
    ("char=?", char_eq),
    ("char>?", char_gt),
    ("char>=?", char_ge),
    ("close-input-port", close_input_port),
    ("close-output-port", close_output_port),
    ("close-port", close_port),
    ("cons", cons),
    ("dis", dis),
    ("display", display),
    ("eof-object", eof_object),
    ("eof-object?", eof_object_question),
    ("eq?", eq_question),
    ("eqv?", eqv_question),
    ("eval", eval),
    ("flush-output-port", flush_output_port),
    ("gensym", gensym),
    ("gensym?", gensym_question),
    ("get-output-string", get_output_string),
    ("input-port?", input_port_question),
    ("input-port-open?", input_port_open_question),
    ("list->string", list_to_string),
    ("list->vector", list_to_vector),
    ("make-bytevector", make_bytevector),
    ("make-vector", make_vector),
    ("modulo", modulo),
    ("null?", null_question),
    ("number?", number_question),
    ("number->string", number_to_string),
    ("open-binary-input-file", open_binary_input_file),
    ("open-binary-output-file", open_binary_output_file),
    ("open-input-file", open_input_file),
    ("open-input-string", open_input_string),
    ("open-output-file", open_output_file),
    ("open-output-string", open_output_string),
    ("output-port?", output_port_question),
    ("output-port-open?", output_port_open_question),
    ("pair?", pair_question),
    ("parse", parse),
    ("peek-char", peek_char),
    ("peek-u8", peek_u8),
    ("port?", port_question),
    ("procedure?", procedure_question),
    ("quotient", quotient),
    ("read", read),
    ("read-bytevector", read_bytevector),
    ("read-bytevector!", read_bytevector_mut),
    ("read-char", read_char),
    ("read-line", read_line),
    ("read-string", read_string),
    ("read-u8", read_u8),
    ("remainder", remainder),
    ("set-car!", set_car),
    ("set-cdr!", set_cdr),
    ("string", string),
    ("string?", string_question),
    ("string->list", string_to_list),
    ("string->symbol", string_to_symbol),
    ("string->utf8", string_to_utf8),
    ("string-append", string_append),
    ("string-length", string_length),
    ("string-ref", string_ref),
    ("string=?", string_eq_question),
    ("symbol?", symbol_question),
    ("symbol->string", symbol_to_string),
    ("textual-port?", textual_port_question),
    ("utf8->string", utf8_to_string),
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

fn make_call_cc_lambda<'h>(hs: &mut GcHeapSession<'h>, env: &EnvironmentRef<'h>) -> Value<'h> {
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
}

fn make_default_exception_handler<'h>(
    hs: &mut GcHeapSession<'h>,
    env: &EnvironmentRef<'h>
) -> Value<'h> {
    use compile::op;

    let insns = hs.alloc(vec![
        op::GET_STATIC, 0, 0,  // get the first argument
        op::TERMINATE,         // exit the interpreter
    ]);
    let names = hs.alloc(vec![GcLeaf::new(InternedString::get("obj"))]);
    let params = hs.alloc(StaticEnvironment { parent: Some(env.senv()), names });
    let environments = hs.alloc(vec![params]);
    let constants = hs.alloc(vec![]);
    let code = hs.alloc(Code { insns, environments, constants, rest: false, operands_max: 1 });
    Value::Lambda(hs.alloc(Lambda { code, env: env.clone() }))
}

pub fn define_builtins<'h>(hs: &mut GcHeapSession<'h>, env: &EnvironmentRef<'h>) {
    for &(name, f) in BUILTINS {
        env.push(
            InternedString::get(name),
            Builtin(GcLeaf::new(BuiltinFnPtr(f))),
        );
    }

    // The three standard streams are implemented as constant-procs for now.
    let stdin = ports::stdin(hs);
    env.push(InternedString::get("current-input-port"), env::constant_proc(hs, stdin));
    let stdout = ports::stdout(hs);
    env.push(InternedString::get("current-output-port"), env::constant_proc(hs, stdout));
    let stderr = ports::stderr(hs);
    env.push(InternedString::get("current-error-port"), env::constant_proc(hs, stderr));

    // Call/cc is implemented as a "lambda" because builtins do not get full
    // access to all the information that makes up the current continuation.
    // We use a special opcode.
    let call_cc = make_call_cc_lambda(hs, env);
    env.push(InternedString::get("call-with-current-continuation"), call_cc.clone());
    env.push(InternedString::get("call/cc"), call_cc);

    // The default exception handler uses a special opcode to exit the interpreter
    // (without triggering a second exception).
    let default_exception_handler = make_default_exception_handler(hs, env);
    env.push(InternedString::get("default-exception-handler"), default_exception_handler);

    // One last extension, implemented as a "lambda" because builtins don't
    // have an environment pointer.
    let interaction_env_proc = env::constant_proc(hs, Value::Environment(env.clone()));
    env.push(
        InternedString::get("interaction-environment"),
        interaction_env_proc,
    );
}
