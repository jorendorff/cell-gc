use std::default::Default;
use std::rc::Rc;
use super::*;

gc_ref_type! {
    pub struct Pair / PairFields / PairStorage<'a> {
        head / set_head: Value<'a>,
        tail / set_tail: Value<'a>
    }
}

impl<'a> Default for PairStorage<'a> {
    fn default() -> PairStorage<'a> {
        PairStorage {
            head: ValueStorage::Null,
            tail: ValueStorage::Null
        }
    }
}

gc_inline_enum! {
    pub enum Value / ValueStorage <'a> {
        Null,
        Int(i32),
        Str(Rc<String>),  // <-- equality is by value
        Pair(Pair<'a>)  // <-- equality is by pointer
    }
}

/// Helper function to avoid having to write out `PairFields` literals all over the place.
fn alloc_pair<'a>(heap: &mut Heap<'a>, head: Value<'a>, tail: Value<'a>) -> Pair<'a> {
    heap.alloc(PairFields { head: head, tail: tail })
}

/// Allocate a pair with the values `(null, null)`.
fn alloc_null_pair<'a>(heap: &mut Heap<'a>) -> Pair<'a> {
    alloc_pair(heap, Value::Null, Value::Null)
}


/// Test that a Heap can at least allocate two objects.
#[test]
fn can_allocate_twice() {
    with_heap(|heap| {
        let obj1 = alloc_pair(heap, Value::Int(1), Value::Null);
        let obj2 = alloc_pair(heap, Value::Int(2), Value::Null);
        assert_eq!(obj1.head(), Value::Int(1));
        assert_eq!(obj2.head(), Value::Int(2));
    });
}

/// Test that rooted objects are not collected and reused.
#[test]
fn root_is_not_recycled() {
    with_heap(|heap| {
        // Create and root one object.
        let root = alloc_pair(heap, Value::Int(1), Value::Str(Rc::new("hello world".to_string())));

        // Subsequent allocations never return root.
        for _ in 0 .. HEAP_SIZE * 2 {
            let tmp = alloc_null_pair(heap);
            assert!(tmp != root);
        }
    });
}

fn null_pair<'a>() -> PairFields<'a> {
    PairFields { head: Value::Null, tail: Value::Null }
}

/// Test try_alloc()'s behavior when the heap is full and every Object is
/// reachable.
#[test]
fn full_heap() {
    with_heap(|heap| {
        // Fill up the heap by allocating HEAP_SIZE objects.
        let mut v = Value::Null;
        for _ in 0 .. HEAP_SIZE {
            v = Value::Pair(alloc_pair(heap, Value::Null, v));
        }

        // The whole heap is reachable.  Now try_alloc() should return null every
        // time it's called.
        for _ in 0 .. 4 {
            let attempt: Option<Pair> = heap.try_alloc(null_pair());
            assert_eq!(attempt, None);
        }
    });
}

/// Test allocate()'s behavior when the heap is only almost full.
#[test]
fn nearly_full_heap() {
    with_heap(|heap| {
        // Make the heap nearly full by allocating (HEAP_SIZE - 1) objects.
        let mut v = Value::Null;
        for _ in 0 .. HEAP_SIZE - 1 {
            v = Value::Pair(alloc_pair(heap, Value::Null, v));
        }

        // Now the entire heap is reachable except for one Object.  We should
        // be able to call allocate() successfully, repeatedly.  It returns
        // that one object every time it's called!
        let last = alloc_null_pair(heap).address();
        for _ in 0 .. 10 {
            assert_eq!(alloc_null_pair(heap).address(), last);
        }
    });
}

/// Test that objects reachable from a root's `.head` or `.tail` are not
/// collected.
#[test]
fn reachable_objects_not_collected() {
    with_heap(|heap| {
        let obj1 = alloc_null_pair(heap);
        let (p2, p3, p4, p5);
        {
            let obj2 = alloc_null_pair(heap);
            p2 = obj2.address();
            obj1.set_head(Value::Pair(obj2.clone()));
            let obj3 = alloc_null_pair(heap);
            p3 = obj3.address();
            obj1.set_tail(Value::Pair(obj3));
            let obj4 = alloc_null_pair(heap);
            p4 = obj4.address();
            obj2.set_head(Value::Pair(obj4));
            let obj5 = alloc_null_pair(heap);
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
    with_heap(|heap| {
        // Create a root object that contains pointers to itself.
        let root = alloc_null_pair(heap);
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
    with_heap(|heap| {
        // Set up obj1 and obj2 to point to each other.
        let obj1 = alloc_null_pair(heap);
        let obj2 = alloc_pair(heap,
                              Value::Pair(obj1.clone()),
                              Value::Pair(obj1.clone()));  // obj2 points to obj1
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
    with_heap(|heap| {
        // Make a cycle.
        let (p1, p2);
        {
            let obj1 = alloc_null_pair(heap);
            p1 = obj1.address();
            let obj2 = alloc_null_pair(heap);
            p2 = obj2.address();
            obj2.set_tail(Value::Pair(obj1.clone()));
            obj1.set_tail(Value::Pair(obj2.clone()));
        }  // Make the cycle unreachable.

        // Allocation should eventually recycle both objects.
        let mut recycled1 = 0;
        let mut recycled2 = 0;
        let mut root = Value::Null;
        for _ in 0 .. HEAP_SIZE {
            let p = alloc_pair(heap, Value::Null, root);
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
    with_heap(|heap1| {
        with_heap(|heap2| {
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
    let mut heap1 = with_heap(|h| *h);  // error: cannot infer an appropriate lifetime
    let mut heap2 = with_heap(|h| *h);
    let obj1 = heap1.alloc_null();
    let obj2 = heap2.alloc((Value::Null, Value::Pair(obj1)));
}

#[test]
fn bug_cross_heap_edges_2a() {
    with_heap(|heap1| {
        let mut heap2 = with_heap(|h| *h);  // error: cannot infer an appropriate lifetime
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

#[test]
fn bug_heap_dropping() {
    // Check that Rust does not let us drop the heap while references into it
    // still exist (they would be dangling pointers). The first iteration of
    // `with_heap` passed the heap to the callback by value, and thus was
    // susceptible to this bug! It caused:
    //
    //     thread panicked while panicking. aborting.
    //
    // The fix was to pass the heap to the closure by reference.
    //
    with_heap(|heap| {
        let obj = alloc_null_pair(heap);

        // note: dropping `heap` would just drop the reference, which is no problem
        ::std::mem::drop(*heap);  // error: cannot move out of borrowed content

        let val = Value::Pair(obj.clone());
        obj.set_head(val); // occurs after the heap is gone
    });
}

#[test]
fn bug_outliving_1() {
    let obj = with_heap(|heap| {
        alloc_null_pair(heap)  // error: cannot infer an appropriate lifetime
    });

    let val = Value::Pair(obj.clone());
    obj.set_head(val);  // occurs after the heap is gone
}

#[test]
fn bug_outliving_2() {
    let obj;
    with_heap(|heap| {
        obj = alloc_null_pair(heap);  // error: cannot infer an appropriate lifetime
    });

    let val = Value::Pair(obj.clone());
    obj.set_head(val);  // occurs after the heap is gone
}

*/

