// Tests for GcLeaf.

extern crate cell_gc;
#[macro_use]
extern crate cell_gc_derive;

use cell_gc::{GcLeaf, Heap};
use std::marker::PhantomData;
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq)]
struct Point2d(f64, f64);

#[test]
fn test_allocating_gc_leaf() {
    let p = Point2d(3.0, 4.0);

    let mut heap = Heap::new();
    heap.enter(|hs| {
        let point = hs.alloc(GcLeaf::new(p));
        assert_eq!(point.get(), Point2d(3.0, 4.0));
    });
}

#[test]
fn test_gc_leaf_field() {
    #[derive(IntoHeap)]
    struct Rect<'h> {
        point0: GcLeaf<Point2d>,
        point1: GcLeaf<Point2d>,
        ignore: PhantomData<&'h ()>,
    }

    let rect = Rect {
        point0: GcLeaf::new(Point2d(3.0, 4.0)),
        point1: GcLeaf::new(Point2d(6.0, 8.0)),
        ignore: PhantomData,
    };

    let mut heap = Heap::new();
    heap.enter(|hs| {
        let hrect = hs.alloc(rect);
        assert_eq!(*hrect.point0(), Point2d(3.0, 4.0));
        assert_eq!(*hrect.point1(), Point2d(6.0, 8.0));
    });
}

#[test]
fn test_gc_leaf_drop() {
    // GcLeaf values are dropped when the Heap is dropped.
    let point = Arc::new(Point2d(3.0, 4.0));

    let mut heaps = vec![];
    for _ in 0..2 {
        let mut heap = Heap::new();
        heap.enter(|hs| { let _ = hs.alloc(GcLeaf::new(point.clone())); });
        heaps.push(heap);
    }
    assert_eq!(Arc::strong_count(&point), 3);
    drop(heaps);
    assert_eq!(Arc::strong_count(&point), 1);
}
