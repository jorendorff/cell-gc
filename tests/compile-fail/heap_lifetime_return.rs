//! `with_heap` callers can't smuggle the heap session out of the lifetime of
//! the closure callback.

// error-pattern: cannot infer an appropriate lifetime

#[macro_use] extern crate cell_gc;
#[macro_use] extern crate cell_gc_derive;
mod pairs_aux;
use cell_gc::*;
use pairs_aux::*;

fn main() {
    with_heap(|hs1| {
        let mut hs2 = with_heap(|hs| *hs);
        let obj1 = alloc_null_pair(hs1);
        let obj2 = alloc_pair(&mut hs2, Value::Null, Value::Pair(obj1));
    });
}
