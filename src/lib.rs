use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::marker::PhantomData;

// What does this do? You'll never guess!
type HeapId<'a> = PhantomData<std::cell::Cell<&'a mut ()>>;

// The heap is (for now) just a big Vec of Pairs
pub struct Heap<'a> {
    id: HeapId<'a>,
    storage: Vec<PairStorage<'a>>,
    freelist: *mut PairStorage<'a>,
    pins: RefCell<HashMap<*mut PairStorage<'a>, usize>>
}

pub const HEAP_SIZE: usize = 10000;

/// Create a heap, pass it to a callback, then destroy the heap.
///
/// The heap's lifetime is directly tied to this function call, for safety. (So
/// the API is a little wonky --- but how many heaps were you planning on
/// creating?)
pub fn with_heap<F, O>(f: F) -> O
    where F: for<'a> FnOnce(&mut Heap<'a>) -> O
{
    f(&mut Heap::new())
}

/// Trait implemented by all types that can be stored directly in the GC heap:
/// the `Storage` types associated with any `HeapInline` or `HeapRef` type.
///
/// XXX maybe this should not be its own trait - fold into HeapInline?
///
unsafe trait Mark {
    unsafe fn mark(ptr: *mut Self);
}

unsafe trait HeapInline {
    type Heap;
    type Storage;
    unsafe fn from_heap(heap: &Self::Heap, ptr: &Self::Storage) -> Self;
    fn to_heap(self) -> Self::Storage;
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
            h.storage.push(PairStorage {
                marked: false,
                head: ValueStorage::Null,
                tail: ValueStorage::Null
            });
            let p = &mut h.storage[i] as *mut PairStorage<'a>;
            unsafe {
                h.add_to_free_list(p);
            }
        }
        h
    }

    fn pin(&self, p: *mut PairStorage<'a>) {
        let mut pins = self.pins.borrow_mut();
        let entry = pins.entry(p).or_insert(0);
        *entry += 1;
    }

    fn unpin(&self, p: *mut PairStorage<'a>) {
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

    unsafe fn add_to_free_list(&mut self, p: *mut PairStorage<'a>) {
        let listp: *mut *mut PairStorage<'a> = std::mem::transmute(p);
        *listp = self.freelist;
        assert_eq!(*listp, self.freelist);
        self.freelist = p;
    }

    pub fn try_alloc(&mut self, pair: (Value<'a>, Value<'a>)) -> Option<Pair<'a>> {
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
            let listp: *mut *mut PairStorage<'a> = std::mem::transmute(p);
            self.freelist = *listp;

            let (h, t) = pair;
            *p = PairStorage {
                marked: false,
                head: h.to_heap(),
                tail: t.to_heap()
            };
            Some(Pair::new(self, p))
        }
    }

    pub fn alloc(&mut self, pair: (Value<'a>, Value<'a>)) -> Pair<'a> {
        self.try_alloc(pair).expect("out of memory (gc did not collect anything)")
    }

    pub fn alloc_null(&mut self) -> Pair<'a> {
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
            let p = &mut self.storage[i] as *mut PairStorage<'a>;
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

// PairStorage is the only type that is actually allocated inside the heap (private)
struct PairStorage<'a> {
    marked: bool,
    head: ValueStorage<'a>,
    tail: ValueStorage<'a>
}

unsafe impl<'a> Mark for PairStorage<'a> {
    unsafe fn mark(ptr: *mut PairStorage<'a>) {
        if !ptr.is_null() && !(*ptr).marked {
            (*ptr).marked = true;
            Mark::mark(&mut (*ptr).head as *mut ValueStorage<'a>);
            Mark::mark(&mut (*ptr).tail as *mut ValueStorage<'a>);
        }
    }
}

// Handle to a PairStorage that lives in the heap.
#[allow(raw_pointer_derive)]
#[derive(Debug, PartialEq)]
pub struct Pair<'a> {
    ptr: *mut PairStorage<'a>,
    heap: *const Heap<'a>,
    heap_id: HeapId<'a>
}

impl<'a> Drop for Pair<'a> {
    fn drop(&mut self) {
        unsafe {
            (*self.heap).unpin(self.ptr);
        }
    }
}

impl<'a> Clone for Pair<'a> {
    fn clone(&self) -> Pair<'a> {
        let &Pair { ptr, heap, heap_id } = self;
        unsafe {
            (*heap).pin(ptr);
        }
        Pair {
            ptr: ptr,
            heap: heap,
            heap_id: heap_id
        }
    }
}

impl<'a> Pair<'a> {
    unsafe fn new(heap: &Heap<'a>, p: *mut PairStorage<'a>) -> Pair<'a> {
        heap.pin(p);
        Pair {
            ptr: p,
            heap: heap as *const Heap<'a>,
            heap_id: heap.id
        }
    }

    pub fn head(&self) -> Value<'a> {
        let ptr = self.ptr;
        unsafe { Value::from_heap(&*self.heap, &(*ptr).head) }
    }

    pub fn tail(&self) -> Value<'a> {
        let ptr = self.ptr;
        unsafe { Value::from_heap(&*self.heap, &(*ptr).tail) }
    }

    pub fn set_head(&self, v: Value<'a>) {
        let ptr = self.ptr;
        unsafe { (*ptr).head = v.to_heap(); }
    }

    pub fn set_tail(&self, v: Value<'a>) {
        let ptr = self.ptr;
        unsafe { (*ptr).tail = v.to_heap(); }
    }

    #[cfg(test)]
    pub fn address(&self) -> usize {
        unsafe { std::mem::transmute(self.ptr) }
    }
}


// === Values (a heap-inline enum)

// Values inside the heap (GC heap-to-heap cross-references) (private)
enum ValueStorage<'a> {
    Null,
    Int(i32),
    Str(Rc<String>),
    Pair(*mut PairStorage<'a>, HeapId<'a>)
}

unsafe impl<'a> Mark for ValueStorage<'a> {
    unsafe fn mark(ptr: *mut ValueStorage<'a>) {
        match *ptr {
            ValueStorage::Pair(p, _) => Mark::mark(p),
            _ => {}
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value<'a> {
    Null,
    Int(i32),
    Str(Rc<String>),  // <-- equality is by value
    Pair(Pair<'a>)  // <-- equality is by pointer
}

unsafe impl<'a> HeapInline for Value<'a> {
    type Heap = Heap<'a>;
    type Storage = ValueStorage<'a>;

    fn to_heap(self) -> ValueStorage<'a> {
        match self {
            Value::Null => ValueStorage::Null,
            Value::Int(n) => ValueStorage::Int(n),
            Value::Str(rcstr) => ValueStorage::Str(rcstr),
            Value::Pair(Pair{ptr, heap_id, ..}) => ValueStorage::Pair(ptr, heap_id)
        }
    }

    unsafe fn from_heap(heap: &Heap<'a>, v: &ValueStorage<'a>) -> Value<'a> {
        match v {
            &ValueStorage::Null => Value::Null,
            &ValueStorage::Int(n) => Value::Int(n),
            &ValueStorage::Str(ref rcstr) => Value::Str(rcstr.clone()),
            &ValueStorage::Pair(ptr, _) => Value::Pair(Pair::new(heap, ptr))
        }
    }
}


#[cfg(test)]
mod test;
