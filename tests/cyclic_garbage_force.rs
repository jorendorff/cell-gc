//! Test that the GC is not confused by cycles that are garbage.

extern crate cell_gc;
#[macro_use]
extern crate cell_gc_derive;

#[derive(IntoHeap)]
struct Chain<'h> {
    id: char,
    next: Option<ChainRef<'h>>,
}

fn main() {
    cell_gc::with_heap(|hs| {
        // Make a cycle.
        {
            let a = hs.alloc(Chain { id: 'a', next: None });
            let b = hs.alloc(Chain { id: 'b', next: Some(a.clone()) });
            a.set_next(Some(b.clone()));
        } // Make the cycle unreachable.

        hs.force_gc();
        assert!(hs.is_empty());
    });
}
