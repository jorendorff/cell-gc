//! Check that Rust does not let us drop the heap while references into it
//! still exist (they would be dangling pointers).

extern crate cell_gc;
#[macro_use] extern crate cell_gc_derive;
mod pairs_aux;
use cell_gc::*;
use pairs_aux::*;

fn main() {
    let mut heap = GcHeap::new();
    heap.enter(|hs| {
        let obj = alloc_null_pair(hs);

        // note: dropping `hs` would just drop the reference, which is no problem
        drop(heap);
        //~^ ERROR: cannot move `heap` into closure because it is borrowed

        let val = Value::Pair(obj.clone());
        obj.set_head(val); // occurs after the heap session is gone
    });
}
