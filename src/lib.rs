use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::marker::PhantomData;

// What does this do? You'll never guess!
type HeapId<'a> = PhantomData<std::cell::Cell<&'a mut ()>>;

// The heap is (for now) just a big Vec of Pairs
pub struct Heap<'a> {
    id: HeapId<'a>,
    storage: Vec<Pair<'a>>,
    freelist: *mut Pair<'a>,
    pins: RefCell<HashMap<*mut Pair<'a>, usize>>
}

pub const HEAP_SIZE: usize = 10000;

pub fn with_heap<F, O>(f: F) -> O
    where F: for<'a> FnOnce(&mut Heap<'a>) -> O
{
    f(&mut Heap::new())
}

unsafe trait Mark {
    unsafe fn mark(ptr: *mut Self);
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
                marked: false,
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
                marked: false,
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

    unsafe fn gc(&mut self) {
        // clear mark bits
        for p in &mut self.storage {
            p.marked = false;
        }

        // mark phase
        for (&p, _) in self.pins.borrow().iter() {
            Mark::mark(p);
        }

        // sweep phase
        self.freelist = std::ptr::null_mut();
        for i in 0 .. HEAP_SIZE {
            let p = &mut self.storage[i] as *mut Pair<'a>;
            if !(*p).marked {
                self.add_to_free_list(p);
            }
        }
    }

    #[cfg(test)]
    pub fn force_gc(&mut self) {
        unsafe { self.gc(); }
    }
}


// === Pair, the reference type

// Pair is the only type that is actually allocated inside the heap (private)
struct Pair<'a> {
    marked: bool,
    head: XValue<'a>,
    tail: XValue<'a>
}

unsafe impl<'a> Mark for Pair<'a> {
    unsafe fn mark(ptr: *mut Pair<'a>) {
        if !ptr.is_null() && !(*ptr).marked {
            (*ptr).marked = true;
            Mark::mark(&mut (*ptr).head as *mut XValue<'a>);
            Mark::mark(&mut (*ptr).tail as *mut XValue<'a>);
        }
    }
}

// Handle to a Pair that lives in the heap.
#[allow(raw_pointer_derive)]
#[derive(Debug, PartialEq)]
pub struct PairRoot<'a> {
    ptr: *mut Pair<'a>,
    heap: *const Heap<'a>,
    heap_id: HeapId<'a>
}

impl<'a> Drop for PairRoot<'a> {
    fn drop(&mut self) {
        unsafe {
            (*self.heap).unpin(self.ptr);
        }
    }
}

impl<'a> Clone for PairRoot<'a> {
    fn clone(&self) -> PairRoot<'a> {
        let &PairRoot { ptr, heap, heap_id } = self;
        unsafe {
            (*heap).pin(ptr);
        }
        PairRoot {
            ptr: ptr,
            heap: heap,
            heap_id: heap_id
        }
    }
}

impl<'a> PairRoot<'a> {
    unsafe fn new(heap: &Heap<'a>, p: *mut Pair<'a>) -> PairRoot<'a> {
        heap.pin(p);
        PairRoot {
            ptr: p,
            heap: heap as *const Heap<'a>,
            heap_id: heap.id
        }
    }

    pub fn head(&self) -> Value<'a> {
        let ptr = self.ptr;
        unsafe { value_from_heap(&*self.heap, &(*ptr).head) }
    }

    pub fn tail(&self) -> Value<'a> {
        let ptr = self.ptr;
        unsafe { value_from_heap(&*self.heap, &(*ptr).tail) }
    }

    pub fn set_head(&self, v: Value<'a>) {
        let ptr = self.ptr;
        unsafe { (*ptr).head = value_to_heap(v); }
    }

    pub fn set_tail(&self, v: Value<'a>) {
        let ptr = self.ptr;
        unsafe { (*ptr).tail = value_to_heap(v); }
    }

    #[cfg(test)]
    pub fn address(&self) -> usize {
        unsafe { std::mem::transmute(self.ptr) }
    }
}


// === Values (a heap-inline enum)

// Values inside the heap (GC heap-to-heap cross-references) (private)
enum XValue<'a> {
    Null,
    Int(i32),
    Str(Rc<String>),
    Pair(*mut Pair<'a>, HeapId<'a>)
}

unsafe impl<'a> Mark for XValue<'a> {
    unsafe fn mark(ptr: *mut XValue<'a>) {
        match *ptr {
            XValue::Pair(p, _) => Mark::mark(p),
            _ => {}
        }
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
        Value::Pair(PairRoot{ptr, heap_id, ..}) => XValue::Pair(ptr, heap_id)
    }
}

unsafe fn value_from_heap<'a>(heap: &Heap<'a>, v: &XValue<'a>) -> Value<'a> {
    match v {
        &XValue::Null => Value::Null,
        &XValue::Int(n) => Value::Int(n),
        &XValue::Str(ref rcstr) => Value::Str(rcstr.clone()),
        &XValue::Pair(ptr, _) => Value::Pair(PairRoot::new(heap, ptr))
    }
}


#[cfg(test)]
mod test;
