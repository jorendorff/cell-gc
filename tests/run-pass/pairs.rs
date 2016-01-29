#[macro_use] extern crate cellgc;
mod pairs_aux;
use pairs_aux::*;

/// Test that a Heap can at least allocate two objects.
fn main() {
    cellgc::with_heap(|heap| {
        let obj1 = alloc_pair(heap, Value::Int(1), Value::Null);
        let obj2 = alloc_pair(heap, Value::Int(2), Value::Null);
        assert_eq!(obj1.head(), Value::Int(1));
        assert_eq!(obj2.head(), Value::Int(2));
    });
}
