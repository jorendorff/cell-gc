use super::with_heap;
use super::Value;
use super::HEAP_SIZE;
use std::rc::Rc;

/// Test that a Heap can at least allocate two objects.
#[test]
fn can_allocate_twice() {
    with_heap(|mut heap| {
        let obj1 = heap.alloc((Value::Int(1), Value::Null));
        let obj2 = heap.alloc((Value::Int(2), Value::Null));
        assert_eq!(obj1.head(), Value::Int(1));
        assert_eq!(obj2.head(), Value::Int(2));
    });
}

/// Test that rooted objects are not collected and reused.
#[test]
fn root_is_not_recycled() {
    with_heap(|mut heap| {
        // Create and root one object.
        let root = heap.alloc((Value::Int(1), Value::Str(Rc::new("hello world".to_string()))));

        // Subsequent allocations never return root.
        for _ in 0 .. HEAP_SIZE * 2 {
            let tmp = heap.alloc_null();
            assert!(tmp != root);
        }
    });
}

/// Test try_alloc()'s behavior when the heap is full and every Object is
/// reachable.
#[test]
fn full_heap() {
    with_heap(|mut heap| {
        // Fill up the heap by allocating HEAP_SIZE objects.
        let mut v = Value::Null;
        for _ in 0 .. HEAP_SIZE {
            v = Value::Pair(heap.alloc((Value::Null, v)));
        }

        // The whole heap is reachable.  Now try_alloc() should return null every
        // time it's called.
        for _ in 0 .. 4 {
            assert_eq!(heap.try_alloc((Value::Null, Value::Null)), None);
        }
    });
}

/// Test allocate()'s behavior when the heap is only almost full.
#[test]
fn nearly_full_heap() {
    with_heap(|mut heap| {
        // Make the heap nearly full by allocating (HEAP_SIZE - 1) objects.
        let mut v = Value::Null;
        for _ in 0 .. HEAP_SIZE - 1 {
            v = Value::Pair(heap.alloc((Value::Null, v)));
        }

        // Now the entire heap is reachable except for one Object.  We should
        // be able to call allocate() successfully, repeatedly.  It returns
        // that one object every time it's called!
        let last = heap.alloc_null().address();
        for _ in 0 .. 10 {
            assert_eq!(heap.alloc_null().address(), last);
        }
    });
}

/// Test that objects reachable from a root's `.head` or `.tail` are not
/// collected.
#[test]
fn reachable_objects_not_collected() {
    with_heap(|mut heap| {
        let obj1 = heap.alloc_null();
        let (p2, p3, p4, p5);
        {
            let obj2 = heap.alloc_null();
            p2 = obj2.address();
            obj1.set_head(Value::Pair(obj2.clone()));
            let obj3 = heap.alloc_null();
            p3 = obj3.address();
            obj1.set_tail(Value::Pair(obj3));
            let obj4 = heap.alloc_null();
            p4 = obj4.address();
            obj2.set_head(Value::Pair(obj4));
            let obj5 = heap.alloc_null();
            p5 = obj5.address();
            obj2.set_tail(Value::Pair(obj5));
        }

        heap.force_gc();

        let h = match obj1.head() {
            Value::Pair(p) => p,
            _ => panic!("expected pair in obj1.head")
        };
        assert_eq!(h.address(), p2);

        let t = match obj1.tail() {
            Value::Pair(p) => p,
            _ => panic!("expected pair in obj1.tail")
        };
        assert_eq!(t.address(), p3);

        let hh = match h.head() {
            Value::Pair(p) => p,
            _ => panic!("expected pair in obj1.head.head")
        };
        assert_eq!(hh.address(), p4);

        let ht = match h.tail() {
            Value::Pair(p) => p,
            _ => panic!("expected pair in obj1.head.tail")
        };
        assert_eq!(ht.address(), p5);
    });
}

/// Test that the GC is not confused by an object that contains pointers to
/// itself.
#[test]
fn root_self_references() {
    with_heap(|mut heap| {
        // Create a root object that contains pointers to itself.
        let root = heap.alloc_null();
        root.set_head(Value::Pair(root.clone()));
        root.set_tail(Value::Pair(root.clone()));

        heap.force_gc();

        // After GC, the root object should be unchanged.
        assert_eq!(root.head(), Value::Pair(root.clone()));
        assert_eq!(root.tail(), Value::Pair(root.clone()));
    });
}

/// Test that the GC is not confused by cycles in the reachable object graph.
#[test]
fn test_root_cycle() {
    with_heap(|mut heap| {
        // Set up obj1 and obj2 to point to each other.
        let obj1 = heap.alloc_null();
        let obj2 = heap.alloc((Value::Pair(obj1.clone()),
                               Value::Pair(obj1.clone())));  // obj2 points to obj1
        obj1.set_head(Value::Pair(obj2.clone()));  // and vice versa
        obj1.set_tail(Value::Pair(obj2.clone()));

        heap.force_gc();

        // After GC, the two objects are unchanged.
        assert_eq!(obj1.head(), Value::Pair(obj2.clone()));
        assert_eq!(obj1.tail(), Value::Pair(obj2.clone()));
        assert_eq!(obj2.head(), Value::Pair(obj1.clone()));
        assert_eq!(obj2.tail(), Value::Pair(obj1.clone()));
    });
}

/// Test that the GC is not confused by cycles that are garbage.
#[test]
fn unreachable_cycle() {
    with_heap(|mut heap| {
        // Make a cycle.
        let (p1, p2);
        {
            let obj1 = heap.alloc_null();
            p1 = obj1.address();
            let obj2 = heap.alloc_null();
            p2 = obj2.address();
            obj2.set_tail(Value::Pair(obj1.clone()));
            obj1.set_tail(Value::Pair(obj2.clone()));
        }  // Make the cycle unreachable.

        // Allocation should eventually recycle both objects.
        let mut recycled1 = 0;
        let mut recycled2 = 0;
        let mut root = Value::Null;
        for _ in 0 .. HEAP_SIZE {
            let p = heap.alloc((Value::Null, root));
            root = Value::Pair(p.clone());
            if p.address() == p1 {
                recycled1 += 1;
            }
            if p.address() == p2 {
                recycled2 += 1;
            }
        }
        assert_eq!(recycled1, 1);
        assert_eq!(recycled2, 1);
    });
}

// Tests that you can't share values across separate heaps. Amazingly,
// Rust enforces this rule at compile time; unfortunately, I don't fully
// understand *all* the parts of the enforcement mechanism, so it's worth
// testing.
//
// Try uncommenting the tests below: if any of them compile successfully,
// consider it a test failure!
/*

/// The most obvious case: you can't just straight-up pass an object from
/// one heap to another heap's `alloc` method.
#[test]
fn bug_cross_heap_edges_1() {
    with_heap(|mut heap1| {
        with_heap(|mut heap2| {
            let obj1 = heap1.alloc_null();  // error: cannot infer an appropriate lifetime
            let obj2 = heap2.alloc((Value::Null, Value::Pair(obj1)));
        });
    });
}

/// `with_heap` callers can't smuggle the heap out of the lifetime of the
/// closure callback. (I am not sure but I think it would be unsafe if you
/// could.)
#[test]
fn bug_cross_heap_edges_2() {
    let mut heap1 = with_heap(|mut h| h);  // error: cannot infer an appropriate lifetime
    let mut heap2 = with_heap(|mut h| h);
    let obj1 = heap1.alloc_null();
    let obj2 = heap2.alloc((Value::Null, Value::Pair(obj1)));
}

#[test]
fn bug_cross_heap_edges_2a() {
    with_heap(|mut heap1| {
        let mut heap2 = with_heap(|mut h| h);  // error: cannot infer an appropriate lifetime
        let obj1 = heap1.alloc_null();
        let obj2 = heap2.alloc((Value::Null, Value::Pair(obj1)));
    });
}

/// Being able to swap two heap bindings would utterly break the safety
/// enforcement regime. :)
#[test]
fn bug_cross_heap_edges_3() {
    use std;
    with_heap(|mut heap1| {
        with_heap(|mut heap2| {
            let obj1 = heap1.alloc_null();
            std::mem::swap(&mut heap1, &mut heap2);  // error: mismatched types ... (lifetime mismatch)
            let obj2 = heap1.alloc((Value::Null, Value::Pair(obj1)));
        });
    });
}
*/

