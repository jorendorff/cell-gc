//! A GCRef must not outlive the Heap it points into, so it can't be returned
//! from the `with_heap` callback.

#[macro_use] extern crate cell_gc_derive;
#[macro_use] extern crate cell_gc;
mod pairs_aux;
use cell_gc::*;
use pairs_aux::*;

fn main() {
    let obj = with_heap(|heap| {
        alloc_null_pair(heap)
        //~^ ERROR cannot infer an appropriate lifetime
    });

    let val = Value::Pair(obj.clone());
    obj.set_head(val);  // would occur after the heap is gone
}
