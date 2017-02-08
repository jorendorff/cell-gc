//! Test try_alloc()'s behavior when the heap is full and every Object is
//! reachable.

#[macro_use] extern crate cell_gc;
#[macro_use] extern crate cell_gc_derive;
mod pairs_aux;
use pairs_aux::*;

fn null_pair<'h>() -> Pair<'h> {
    Pair { head: Value::Null, tail: Value::Null }
}

fn main() {
    cell_gc::with_heap(|heap| {
        // Fill up the heap by allocating HEAP_SIZE objects.
        let mut v = Value::Null;
        for _ in 0 .. cell_gc::page_capacity::<Pair>() {
            v = Value::Pair(alloc_pair(heap, Value::Null, v));
        }

        // The whole heap is reachable.  Now try_alloc() should return null every
        // time it's called.
        for _ in 0 .. 4 {
            let attempt: Option<PairRef> = heap.try_alloc(null_pair());
            assert_eq!(attempt, None);
        }
    });
}
