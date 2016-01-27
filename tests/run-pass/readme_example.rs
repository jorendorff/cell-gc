//! The mini-example from README.md actually works.

#[macro_use] extern crate toy_gc;

/// A linked list of numbers that lives in the GC heap.
gc_ref_type! {
    // This declares four different related structs, but the last two are
    // for the GC's internal use. Read on to see the first two in action.
    struct IntList / RefIntList / InHeapIntList / InHeapRefIntList <'a> {
        head / set_head: i64,
        tail / set_tail: Option<RefIntList<'a>>
    }
}

fn main() {
    // Create a heap (you'll only do this once in your whole program)
    toy_gc::with_heap(|heap| {
        // Allocate an object (returns a RefIntList)
        let obj1 = heap.alloc(IntList { head: 17, tail: None });
        assert_eq!(obj1.head(), 17);
        assert_eq!(obj1.tail(), None);

        // Allocate another object
        let obj2 = heap.alloc(IntList { head: 33, tail: Some(obj1) });
        assert_eq!(obj2.head(), 33);
        assert_eq!(obj2.tail().unwrap().head(), 17);
    });
}
