//! Test try_alloc()'s behavior when the heap is full and every Object is
//! reachable.

extern crate cell_gc;
#[macro_use]
extern crate cell_gc_derive;
mod aux;
use aux::pairs::*;

fn null_pair<'h>() -> Pair<'h> {
    Pair {
        head: Value::Null,
        tail: Value::Null,
    }
}

#[test]
fn full_heap() {
    cell_gc::with_heap(|hs| {
        // Fill up the heap by setting a limit of 1 page and filling that page.
        hs.set_page_limit::<Pair>(Some(1));
        let mut v = Value::Null;
        for _ in 0..cell_gc::page_capacity::<Pair>() {
            v = Value::Pair(alloc_pair(hs, Value::Null, v));
        }

        // The whole heap is reachable.  Now try_alloc() should return None
        // every time it's called.
        for _ in 0..4 {
            let attempt: Option<PairRef> = hs.try_alloc(null_pair());
            assert_eq!(attempt, None);
        }
    });
}

#[test]
fn implicit_gc() {
    cell_gc::with_heap(|hs| {
        // Fill up the heap by setting a limit of 1 page and filling that page.
        hs.set_page_limit::<Pair>(Some(1));
        let mut v = Value::Null;
        let n = cell_gc::page_capacity::<Pair>();
        for i in 0..n {
            v = Value::Pair(alloc_pair(hs, Value::Int(i as i32), v));
        }

        // The whole heap is rooted.  Now trigger a GC that will free up exactly one cell.
        for _ in 0..4 {
            // Pop an element from the list v.
            let (popped_element, tail) = match v {
                Value::Pair(r) => (r.head(), r.tail()),
                _ => panic!("v corrupted, or else n == 0"), // bad either way
            };
            v = tail;

            // Now push it again, triggering GC.
            v = Value::Pair(alloc_pair(hs, popped_element, v));
        }

        // Check that GC did not wipe out the data.
        for i in (0..n).rev() {
            let (popped_element, tail) = match v {
                Value::Pair(r) => (r.head(), r.tail()),
                _ => panic!("v corrupted, or else N == 0"), // bad either way
            };
            assert_eq!(popped_element, Value::Int(i as i32));
            v = tail;
        }
        assert_eq!(v, Value::Null);
    });
}
