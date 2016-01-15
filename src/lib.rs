use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::marker::PhantomData;

// What does this do? You'll never guess!
type HeapId<'a> = PhantomData<std::cell::Cell<&'a mut ()>>;

// Pair is the only type that is actually allocated inside the heap (private)
struct Pair<'a> {
    mark: bool,
    head: XValue<'a>,
    tail: XValue<'a>
}

// Values inside the heap (GC heap-to-heap cross-references) (private)
enum XValue<'a> { Null, Int(i32), Str(Rc<String>), Pair(*mut Pair<'a>, HeapId<'a>) }

// The heap is (for now) just a big Vec of Pairs
pub struct Heap<'a> {
    id: HeapId<'a>,
    storage: Vec<Pair<'a>>,
    freelist: *mut Pair<'a>,
    pins: RefCell<HashMap<*mut Pair<'a>, usize>>
}

pub const HEAP_SIZE: usize = 10000;

pub fn with_heap<F, O>(f: F) -> O
    where F: for<'a> FnOnce(Heap<'a>) -> O
{
    f(Heap::new())
}

impl<'a> Heap<'a> {
    fn new() -> Heap<'a> {
        // Allocate with the full capacity so that allocated Pairs never move
        let mut h = Heap {
            id: PhantomData,
            storage: Vec::with_capacity(HEAP_SIZE),
            freelist: std::ptr::null_mut(),
            pins: RefCell::new(HashMap::new())
        };
        for i in 0 .. HEAP_SIZE {
            h.storage.push(Pair {
                mark: false,
                head: XValue::Null,
                tail: XValue::Null
            });
            let p = &mut h.storage[i] as *mut Pair<'a>;
            unsafe {
                h.add_to_free_list(p);
            }
        }
        h
    }

    fn pin(&self, p: *mut Pair<'a>) {
        let mut pins = self.pins.borrow_mut();
        let entry = pins.entry(p).or_insert(0);
        *entry += 1;
    }

    fn unpin(&self, p: *mut Pair<'a>) {
        let mut pins = self.pins.borrow_mut();
        if {
            let entry = pins.entry(p).or_insert(0);
            assert!(*entry != 0);
            *entry -= 1;
            *entry == 0
        } {
            pins.remove(&p);
        }
    }

    unsafe fn add_to_free_list(&mut self, p: *mut Pair<'a>) {
        let listp: *mut *mut Pair<'a> = std::mem::transmute(p);
        *listp = self.freelist;
        assert_eq!(*listp, self.freelist);
        self.freelist = p;
    }

    pub fn try_alloc(&mut self, pair: (Value<'a>, Value<'a>)) -> Option<PairRoot<'a>> {
        if self.freelist.is_null() {
            unsafe {
                self.gc();
            }
            if self.freelist.is_null() {
                return None;
            }
        }

        let p = self.freelist;
        unsafe {
            let listp: *mut *mut Pair<'a> = std::mem::transmute(p);
            self.freelist = *listp;

            let (h, t) = pair;
            *p = Pair {
                mark: false,
                head: value_to_heap(h),
                tail: value_to_heap(t)
            };
            Some(PairRoot::new(self, p))
        }
    }

    pub fn alloc(&mut self, pair: (Value<'a>, Value<'a>)) -> PairRoot<'a> {
        self.try_alloc(pair).expect("out of memory (gc did not collect anything)")
    }

    pub fn alloc_null(&mut self) -> PairRoot<'a> {
        self.alloc((Value::Null, Value::Null))
    }

    unsafe fn mark(p: *mut Pair<'a>) {
        if !p.is_null() && !(*p).mark {
            (*p).mark = true;
            Heap::mark_value(&(*p).head);
            Heap::mark_value(&(*p).tail);
        }
    }

    unsafe fn mark_value(v: &XValue<'a>) {
        match v {
            &XValue::Pair(p, _) => Heap::mark(p),
            _ => {}
        }
    }

    unsafe fn gc(&mut self) {
        // clear mark bits
        for p in &mut self.storage {
            p.mark = false;
        }

        // mark phase
        for (&p, _) in self.pins.borrow().iter() {
            Heap::mark(p);
        }

        // sweep phase
        self.freelist = std::ptr::null_mut();
        for i in 0 .. HEAP_SIZE {
            let p = &mut self.storage[i] as *mut Pair<'a>;
            if !(*p).mark {
                self.add_to_free_list(p);
            }
        }
    }

    #[cfg(test)]
    pub fn force_gc(&mut self) {
        unsafe { self.gc(); }
    }
}




// === Values outside the heap that may refer to heap values

// Handle to a Pair that lives in the heap.
#[allow(raw_pointer_derive)]
#[derive(Debug, PartialEq)]
pub struct PairRoot<'a>(*mut Pair<'a>, *const Heap<'a>, HeapId<'a>);

impl<'a> Drop for PairRoot<'a> {
    fn drop(&mut self) {
        unsafe {
            (*self.1).unpin(self.0);
        }
    }
}

impl<'a> Clone for PairRoot<'a> {
    fn clone(&self) -> PairRoot<'a> {
        let p = self.0;
        let heap = self.1;
        unsafe {
            (*heap).pin(p);
        }
        PairRoot(p, heap, self.2)
    }
}

impl<'a> PairRoot<'a> {
    unsafe fn new(heap: &Heap<'a>, p: *mut Pair<'a>) -> PairRoot<'a> {
        heap.pin(p);
        PairRoot(p, heap as *const Heap<'a>, heap.id)
    }

    pub fn head(&self) -> Value<'a> {
        let p = self.0;
        unsafe { value_from_heap(&*self.1, &(*p).head) }
    }

    pub fn tail(&self) -> Value<'a> {
        let p = self.0;
        unsafe { value_from_heap(&*self.1, &(*p).tail) }
    }

    pub fn set_head(&self, v: Value<'a>) {
        let p = self.0;
        unsafe { (*p).head = value_to_heap(v); }
    }

    pub fn set_tail(&self, v: Value<'a>) {
        let p = self.0;
        unsafe { (*p).tail = value_to_heap(v); }
    }

    #[cfg(test)]
    pub fn address(&self) -> usize {
        unsafe { std::mem::transmute(self.0) }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value<'a> {
    Null,
    Int(i32),
    Str(Rc<String>),  // <-- equality is by value
    Pair(PairRoot<'a>)  // <-- equality is by pointer
}


// === Getting data into and out of the heap

fn value_to_heap<'a>(v: Value<'a>) -> XValue<'a> {
    match v {
        Value::Null => XValue::Null,
        Value::Int(n) => XValue::Int(n),
        Value::Str(rcstr) => XValue::Str(rcstr),
        Value::Pair(PairRoot(p, _, heap_id)) => XValue::Pair(p, heap_id)
    }
}

unsafe fn value_from_heap<'a>(heap: &Heap<'a>, v: &XValue<'a>) -> Value<'a> {
    match v {
        &XValue::Null => Value::Null,
        &XValue::Int(n) => Value::Int(n),
        &XValue::Str(ref rcstr) => Value::Str(rcstr.clone()),
        &XValue::Pair(p, _) => Value::Pair(PairRoot::new(heap, p))
    }
}


#[cfg(test)]
mod test {
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

    // Tests that you can't share values across separate heaps.
    // Try uncommenting the tests below: if any of them compile successfully,
    // consider it a test failure!
    /*
    #[test]
    fn bug_swapping() {
        with_heap(|heap1| {
            with_heap(|mut heap2| {
                let obj1 = heap1.alloc_null();  // error: cannot infer an appropriate lifetime parameter
                let obj2 = heap2.alloc((Value::Null, Value::Pair(obj1)));
            });
        });
    }

    #[test]
    fn bug_swapping_2() {
        let heap1 = with_heap(|h| h);  // error: cannot infer an appropriate lifetime
        let heap2 = with_heap(|h| h);
        let obj1 = heap1.alloc_null();
        let obj2 = heap2.alloc((Value::Null, Value::Pair(obj1)));
    }

    #[test]
    fn bug_swapping_3() {
        with_heap(|heap1| {
            let heap2 = with_heap(|h| h);  // error: cannot infer an appropriate lifetime
            let obj1 = heap1.alloc_null();
            let obj2 = heap2.alloc((Value::Null, Value::Pair(obj1)));
        });
    }
    */
}
