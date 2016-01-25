//! Test that the GC is not confused by an object that contains pointers to
//! itself.

#[macro_use] extern crate toy_gc;
mod pairs_aux;
use toy_gc::*;
use pairs_aux::*;

fn main() {
    with_heap(|heap| {
        // Create a root object that contains pointers to itself.
        let root = alloc_null_pair(heap);
        root.set_head(Value::Pair(root.clone()));
        root.set_tail(Value::Pair(root.clone()));

        heap.force_gc();

        // After GC, the root object should be unchanged.
        assert_eq!(root.head(), Value::Pair(root.clone()));
        assert_eq!(root.tail(), Value::Pair(root.clone()));
    });
}
