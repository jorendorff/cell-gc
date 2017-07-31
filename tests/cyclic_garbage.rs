//! Test that the GC is not confused by cycles that are garbage.

extern crate cell_gc;
#[macro_use]
extern crate cell_gc_derive;
mod aux;
use aux::pairs::*;

#[test]
fn cyclic_garbage() {
    cell_gc::with_heap(|hs| {
        hs.set_page_limit::<Pair>(Some(1));

        // Make a cycle.
        let (p1, p2);
        {
            let obj1 = alloc_null_pair(hs);
            p1 = obj1.as_mut_ptr();
            let obj2 = alloc_null_pair(hs);
            p2 = obj2.as_mut_ptr();
            obj2.set_tail(Value::Pair(obj1.clone()));
            obj1.set_tail(Value::Pair(obj2.clone()));
        } // Make the cycle unreachable.

        // Allocation should eventually recycle both objects.
        let mut recycled1 = 0;
        let mut recycled2 = 0;
        let mut root = Value::Null;
        for _ in 0..cell_gc::page_capacity::<Pair>() {
            let p = alloc_pair(hs, Value::Null, root);
            root = Value::Pair(p.clone());
            if p.as_mut_ptr() == p1 {
                recycled1 += 1;
            }
            if p.as_mut_ptr() == p2 {
                recycled2 += 1;
            }
        }
        assert_eq!(recycled1, 1);
        assert_eq!(recycled2, 1);
    });
}
