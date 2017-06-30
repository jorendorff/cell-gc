//! Test that the GC is not confused by an object that contains pointers to
//! itself.

extern crate cell_gc;
#[macro_use]
extern crate cell_gc_derive;
mod aux;
use aux::pairs::*;

fn main() {
    cell_gc::with_heap(|hs| {
        // Create a root object that contains pointers to itself.
        let root = alloc_null_pair(hs);
        root.set_head(Value::Pair(root.clone()));
        root.set_tail(Value::Pair(root.clone()));

        hs.force_gc();

        // After GC, the root object should be unchanged.
        assert_eq!(root.head(), Value::Pair(root.clone()));
        assert_eq!(root.tail(), Value::Pair(root.clone()));
    });
}
