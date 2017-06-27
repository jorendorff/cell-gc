//! Same as `cross-heap-edge-alloc.rs` but crossing the other direction.

#[macro_use] extern crate cell_gc;
#[macro_use] extern crate cell_gc_derive;
mod pairs_aux;
use cell_gc::*;
use pairs_aux::*;

fn main() {
    with_heap(|hs1| {
        with_heap(|hs2| {
            let obj2 = alloc_null_pair(hs2);
            //~^ ERROR cannot infer an appropriate lifetime for lifetime parameter 'h in function call due to conflicting requirements
            let obj1 = alloc_pair(hs1, Value::Null, Value::Pair(obj2));
        });
    });
}
