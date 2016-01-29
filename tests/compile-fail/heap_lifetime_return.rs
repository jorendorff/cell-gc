//! `with_heap` callers can't smuggle the heap out of the lifetime of the
//! closure callback.

// error-pattern: cannot infer an appropriate lifetime

#[macro_use] extern crate cellgc;
mod pairs_aux;
use cellgc::*;
use pairs_aux::*;

fn main() {
    with_heap(|heap1| {
        let mut heap2 = with_heap(|h| *h);
        let obj1 = alloc_null_pair(heap1);
        let obj2 = alloc_pair(&mut heap2, Value::Null, Value::Pair(obj1));
    });
}
