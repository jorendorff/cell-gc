//! Test equality properties of GCRef values.

#[macro_use] extern crate cell_gc;
#[macro_use] extern crate cell_gc_derive;
use cell_gc::with_heap;

/// A linked list of valueless nodes that lives in the GC heap.
#[derive(IntoHeap)]
struct List<'h> {
    tail: Option<ListRef<'h>>
}

fn main() {
    // Create a heap (do this only once)
    cell_gc::with_heap(|heap| {
        let a = heap.alloc(List { tail: None });
        // Two clones of a ref are equal.
        assert_eq!(a.clone(), a.clone());

        let b = heap.alloc(List { tail: Some(a.clone()) });
        // Refs that point to different objects are not equal.
        assert!(b.clone() != a.clone());
        assert!(b.tail() != Some(b.clone()));
        // But round-tripping a ref through the heap produces an equal ref.
        assert_eq!(b.tail(), Some(a));
    });
}
