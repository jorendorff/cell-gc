//! Test that rooted objects are not collected and reused.

#[macro_use] extern crate cellgc;
mod pairs_aux;
use pairs_aux::*;
use std::rc::Rc;

fn main() {
    cellgc::with_heap(|heap| {
        // Create and root one object.
        let root = alloc_pair(heap, Value::Int(1), Value::Str(Rc::new("hello world".to_string())));

        // Subsequent allocations never return root.
        for _ in 0 .. cellgc::page_capacity::<Pair>() * 2 {
            let tmp = alloc_null_pair(heap);
            assert!(tmp != root);
        }
    });
}
