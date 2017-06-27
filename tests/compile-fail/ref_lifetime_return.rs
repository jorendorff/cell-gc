//! A GCRef must not outlive the HeapSession it points into, so it can't be returned
//! from the `with_heap` callback.

#[macro_use] extern crate cell_gc;
#[macro_use] extern crate cell_gc_derive;
mod pairs_aux;
use cell_gc::*;
use pairs_aux::*;

fn main() {
    let obj = with_heap(|hs| {
        alloc_null_pair(hs)
        //~^ ERROR cannot infer an appropriate lifetime
    });

    let val = Value::Pair(obj.clone());
    obj.set_head(val);  // would occur after the heap is gone
}
