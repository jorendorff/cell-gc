//! Test allocate()'s behavior when the heap is only *almost* full.

#[macro_use] extern crate toy_gc;
mod pairs_aux;
use toy_gc::*;
use pairs_aux::*;

fn main() {
    with_heap(|heap| {
        // Make the heap nearly full by allocating (HEAP_SIZE - 1) objects.
        let mut v = Value::Null;
        for _ in 0 .. HEAP_SIZE - 1 {
            v = Value::Pair(alloc_pair(heap, Value::Null, v));
        }

        // Now the entire heap is reachable except for one Object.  We should
        // be able to call allocate() successfully, repeatedly.  It returns
        // that one object every time it's called!
        let last = alloc_null_pair(heap).as_mut_ptr();
        for _ in 0 .. 10 {
            assert_eq!(alloc_null_pair(heap).as_mut_ptr(), last);
        }
    });
}
