extern crate cell_gc;

use cell_gc::{GcFrozenRef, GcLeaf, GcRef, GcHeap};

type Point = GcLeaf<(f64, f64)>;

#[test]
fn test_freeze_basic() {
    let mut heap = GcHeap::new();
    let frozen_pt: GcFrozenRef<Point> = heap.enter(|hs| {
        let pt = hs.alloc(GcLeaf::new((3.0, 4.0)));
        hs.freeze(pt)
    });
    heap.enter(|hs| {
        hs.force_gc();
        let pt: GcRef<Point> = hs.thaw(frozen_pt);
        assert_eq!(pt.get(), (3.0, 4.0));
    });
}

#[test]
fn test_freeze_drop() {
    // When the last reference to a value in the heap is a frozen reference,
    // dropping it makes the value collectable.
    //
    // This would be an integration test, but it uses GcHeapSession::is_empty()
    // which is a test-only feature.

    let mut heap = GcHeap::new();
    let frozen_pt: GcFrozenRef<Point> = heap.enter(|hs| {
        let pt = hs.alloc(GcLeaf::new((3.0, 4.0)));
        hs.freeze(pt)
    });

    heap.enter(|hs| {
        hs.force_gc();
        assert!(!hs.is_empty());
    });

    drop(frozen_pt);

    heap.enter(|hs| {
        hs.force_gc();
        assert!(hs.is_empty());
    });
}

#[test]
fn test_freeze_drop_heap() {
    // It's ok for a frozen reference to outlive the heap, as long as it's not
    // used again.
    let mut heap = GcHeap::new();
    let frozen_pt: GcFrozenRef<Point> = heap.enter(|hs| {
        let pt = hs.alloc(GcLeaf::new((3.0, 4.0)));
        hs.freeze(pt)
    });
    drop(heap);
    drop(frozen_pt);
}

#[test]
#[should_panic(expected = "can't thaw a frozen reference into a different heap")]
fn test_thawing_across_heaps() {
    // Frozen references can't be thawed in the wrong heap.
    let mut source_heap = GcHeap::new();
    let mut target_heap = GcHeap::new();
    let frozen_pt: GcFrozenRef<Point> = source_heap.enter(|hs| {
        let pt = hs.alloc(GcLeaf::new((4.0, 3.0)));
        hs.freeze(pt)
    });
    target_heap.enter(|hs| {
        hs.thaw(frozen_pt); // panics
    });
}

#[test]
#[should_panic(expected = "can't thaw a reference into a heap that has been dropped")]
fn test_thawing_across_heaps_2() {
    // The same, when the source heap is gone.
    let mut source_heap = GcHeap::new();
    let mut target_heap = GcHeap::new();
    let frozen_pt: GcFrozenRef<Point> = source_heap.enter(|hs| {
        let pt = hs.alloc(GcLeaf::new((4.0, 3.0)));
        hs.freeze(pt)
    });
    drop(source_heap);

    target_heap.enter(|hs| {
        hs.thaw(frozen_pt); // panics
    });
}
