extern crate cell_gc;
#[macro_use] extern crate cell_gc_derive;
mod pairs_aux;
use pairs_aux::*;

/// Test that a heap can at least allocate two objects.
fn main() {
    cell_gc::with_heap(|hs| {
        let obj1 = alloc_pair(hs, Value::Int(1), Value::Null);
        let obj2 = alloc_pair(hs, Value::Int(2), Value::Null);
        assert_eq!(obj1.head(), Value::Int(1));
        assert_eq!(obj2.head(), Value::Int(2));
    });
}
