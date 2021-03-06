//! Test allocate()'s behavior when the heap is only *almost* full.

extern crate cell_gc;
#[macro_use]
extern crate cell_gc_derive;
mod aux;
use aux::pairs::*;

#[test]
fn nearly_full_heap() {
    cell_gc::with_heap(|hs| {
        hs.set_page_limit::<Pair>(Some(1));

        // Make the heap nearly full by allocating (HEAP_SIZE - 1) objects.
        let mut v = Value::Null;
        for _ in 0..cell_gc::page_capacity::<Pair>() - 1 {
            v = Value::Pair(alloc_pair(hs, Value::Null, v));
        }

        // Now the entire heap is reachable except for one Object.  We should
        // be able to call allocate() successfully, repeatedly.  It returns
        // that one object every time it's called!
        let last = alloc_null_pair(hs).as_mut_ptr();
        for _ in 0..10 {
            assert_eq!(alloc_null_pair(hs).as_mut_ptr(), last);
        }
    });
}
