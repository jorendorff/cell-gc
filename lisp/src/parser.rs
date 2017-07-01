//! Parsing of s-expressions.

use asexp::{Atom, Sexp};
use cell_gc::HeapSession;
use std::rc::Rc;
use vm::{Pair, Value};

/// Top level entry point to s-expression parsing. Takes a source string and
/// returns a `Value` which can contain `Pair`s allocated in the GC heap.
pub fn parse<'h>(hs: &mut HeapSession<'h>, source: &str) -> Result<Value<'h>, &'static str> {
    let sexp = Sexp::parse(source).map_err(|_| "s-exp parsing failed")?;
    sexp_to_value(hs, sexp)
}

/// Given an `asexp::Sexp` value and a GC heap, convert it into a `Value` which
/// can point to `Pair`s inside the GC heap.
pub fn sexp_to_value<'h>(hs: &mut HeapSession<'h>, sexp: Sexp) -> Result<Value<'h>, &'static str> {
    match sexp {
        Sexp::Atom(atom) => {
            match atom {
                Atom::Str(s) => Ok(Value::Symbol(Rc::new(s))),
                Atom::SInt(s) => Ok(Value::Int(s as _)),
                Atom::UInt(u) => Ok(Value::Int(u as _)),
                Atom::Float(_) => Err("floats aren't supported"),
            }
        }

        // ( ... )
        Sexp::Tuple(children) |
        // [ ... ]
        Sexp::Array(children) => {
            children.into_iter()
                .rev()
                .map(|c| sexp_to_value(hs, c))
                .collect::<Vec<_>>()
                .into_iter()
                .fold(Ok(Value::Nil), |cdr, car| {
                    let cdr = cdr?;
                    let car = car?;
                    let pair = hs.alloc(Pair { car, cdr });
                    Ok(Value::Cons(pair))
                })
        }

        // { key val ... } which we flatten into conses as well
        Sexp::Map(pairs) => {
            pairs.into_iter()
                .flat_map(|pair| vec![pair.0, pair.1])
                .rev()
                .map(|c| sexp_to_value(hs, c))
                .collect::<Vec<_>>()
                .into_iter()
                .fold(Ok(Value::Nil), |cdr, car| {
                    let cdr = cdr?;
                    let car = car?;
                    let pair = hs.alloc(Pair { car, cdr });
                    Ok(Value::Cons(pair))
                })
        }
    }
}
