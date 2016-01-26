//! Test that the GC is not confused by cycles in the reachable object graph.

#[macro_use] extern crate toy_gc;
mod pairs_aux;
use pairs_aux::*;

fn main() {
    toy_gc::with_heap(|heap| {
        // Set up obj1 and obj2 to point to each other.
        let obj1 = alloc_null_pair(heap);
        let obj2 = alloc_pair(heap,
                              Value::Pair(obj1.clone()),
                              Value::Pair(obj1.clone()));  // obj2 points to obj1
        obj1.set_head(Value::Pair(obj2.clone()));  // and vice versa
        obj1.set_tail(Value::Pair(obj2.clone()));

        heap.force_gc();

        // After GC, the two objects are unchanged.
        assert_eq!(obj1.head(), Value::Pair(obj2.clone()));
        assert_eq!(obj1.tail(), Value::Pair(obj2.clone()));
        assert_eq!(obj2.head(), Value::Pair(obj1.clone()));
        assert_eq!(obj2.tail(), Value::Pair(obj1.clone()));
    });
}
