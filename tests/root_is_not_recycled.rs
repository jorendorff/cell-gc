//! Test that rooted objects are not collected and reused.

extern crate cell_gc;
#[macro_use]
extern crate cell_gc_derive;
mod aux;
use aux::pairs::*;
use std::sync::Arc;

fn main() {
    cell_gc::with_heap(|hs| {
        // Create and root one object.
        let root = alloc_pair(
            hs,
            Value::Int(1),
            Value::Str(Arc::new("hello world".to_string())),
        );

        // Subsequent allocations never return root.
        for _ in 0..cell_gc::page_capacity::<Pair>() * 2 {
            let tmp = alloc_null_pair(hs);
            assert!(tmp != root);
        }
    });
}
