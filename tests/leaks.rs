// It's OK to destroy a heap when some GcRefs still exist. (!)
//
// Rust's safety rules ensure that the GcRefs won't still be accessible
// afterwards, so if this happens, it means the values were leaked.

extern crate cell_gc;

use cell_gc::Heap;
use std::mem;
use std::sync::Arc;

#[test]
fn test_leaked_ref() {
    let counted = Arc::new(false);
    assert_eq!(Arc::strong_count(&counted), 1);

    let mut heap = Heap::new();
    heap.enter(|hs| {
        let a = hs.alloc(counted.clone());
        assert_eq!(Arc::strong_count(&counted), 2);
        mem::forget(a);

        // The Arc in the heap remains pinned.
        hs.force_gc();
        assert_eq!(Arc::strong_count(&counted), 2);
    });

    // This destroys all remaining in-heap values, even though we leaked a
    // strong reference.
    drop(heap);

    assert_eq!(Arc::strong_count(&counted), 1);
}
