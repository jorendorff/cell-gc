//! Printing s-expressions.

use vm::{PairRef, Value};
use vm::Value::*;

// Note that this will need to add a set of already-printed pairs if we add
// `set-car!` and/or `set-cdr!` and introduce the possibility of cycles.

pub fn print<'h>(value: Value<'h>) {
    match value {
        Nil => print!("nil"),
        Bool(true) => print!("#t"),
        Bool(false) => print!("#f"),
        Int(n) => print!("{}", n),
        Symbol(s) => print!("{}", s),
        Lambda(_) => print!("#lambda"),
        Builtin(_) => print!("#builtin"),
        Cons(p) => {
            print!("(");
            print_pair(p);
            print!(")");
        }
        Vector(v) => {
            print!("#(");
            for i in 0..v.len() {
                if i != 0 {
                    print!(" ");
                }
                print(v.get(i));
            }
            print!(")");
        }
    }
}

fn print_pair<'h>(pair: PairRef<'h>) {
    print(pair.car());
    match pair.cdr() {
        Nil => return,
        Cons(p) => {
            print!(" ");
            print_pair(p);
        }
        otherwise => {
            print!(" . ");
            print(otherwise);
        }
    }
}
