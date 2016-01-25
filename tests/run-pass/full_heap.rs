//! Test try_alloc()'s behavior when the heap is full and every Object is
//! reachable.

#[macro_use] extern crate toy_gc;
mod pairs_aux;
use toy_gc::*;
use pairs_aux::*;

fn null_pair<'a>() -> Pair<'a> {
    Pair { head: Value::Null, tail: Value::Null }
}

fn main() {
    with_heap(|heap| {
        // Fill up the heap by allocating HEAP_SIZE objects.
        let mut v = Value::Null;
        for _ in 0 .. HEAP_SIZE {
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
