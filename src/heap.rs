//! GC heap allocator.

use std::cell::RefCell;
use std::collections::HashMap;
use std::marker::PhantomData;
use std::{fmt, mem, ptr};
use bit_vec::BitVec;

// What does this do? You'll never guess!
type HeapId<'a> = PhantomData<::std::cell::Cell<&'a mut ()>>;

/// Total number of Pair objects that can be allocated in a Heap at once.
///
/// XXX BOGUS: At present, this is number is carefully selected (with insider
/// knowledge of both `size_of::<PairStorage>()` on my platform and the `Box`
/// allocator's behavior) so that a HeapStorage object fits in a page and thus
/// the assertions below about HEAP_STORAGE_ALIGN pass.
pub const HEAP_SIZE: usize = 125;

/// We rely on all bits to the right of this bit being 0 in addresses of
/// HeapStorage instances.
const HEAP_STORAGE_ALIGN: usize = 0x1000;

// The heap is (for now) just a big array of Pairs
struct HeapStorage<'a> {
    mark_bits: BitVec,
    mark_entry_point: unsafe fn(*mut ()),
    objects: [PairStorage<'a>; HEAP_SIZE]
}

impl<'a> HeapStorage<'a> {
    unsafe fn new() -> HeapStorage<'a> {
        HeapStorage {
            mark_bits: BitVec::from_elem(HEAP_SIZE, false),
            mark_entry_point: mark_entry_point::<PairStorage>,
            objects: mem::uninitialized()
        }
    }
}

pub struct Heap<'a> {
    id: HeapId<'a>,
    storage: Box<HeapStorage<'a>>,
    freelist: *mut PairStorage<'a>,
    pins: RefCell<HashMap<*mut (), usize>>
}

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
pub unsafe trait Mark<'a> {
    unsafe fn mark(ptr: *mut Self);
}

/// Non-inlined function that serves as an entry point to marking. This is used
/// for marking root set entries.
unsafe fn mark_entry_point<'a, T: Mark<'a>>(addr: *mut ()) {
    Mark::mark(addr as *mut T);
}


/// Trait implemented by all types that can be stored in fields of structs (or,
/// eventually, elements of GCVecs) that are stored in the GC heap.
pub unsafe trait HeapInline<'a> {
    /// The type of the value when it is physically stored in the heap.
    type Storage;

    /// Extract the value from the heap. Do not under any circumstances call
    /// this.  It is for macro-generated code to call; it is impossible for
    /// ordinary users to call this safely, because `ptr` must be a direct,
    /// unwrapped reference to a value stored in the GC heap, which ordinary
    /// users cannot obtain.
    ///
    /// This turns any raw pointers in the `Storage` value into safe
    /// references, so while it's a dangerous function, the result of a correct
    /// call can be safely handed out to user code.
    ///
    unsafe fn from_heap(heap: &Heap<'a>, ptr: &Self::Storage) -> Self;

    /// Convert the value to the form it should have in the heap.
    /// This is for macro-generated code to call.
    fn to_heap(self) -> Self::Storage;
}

impl<'a> Heap<'a> {
    fn new() -> Heap<'a> {
        let mut h = Heap {
            id: PhantomData,
            storage: Box::new(unsafe { HeapStorage::new() }),
            freelist: ptr::null_mut(),
            pins: RefCell::new(HashMap::new())
        };

        // These assertions will likely fail on 32-bit platforms or if someone
        // is somehow using a custom Box allocator. If either one fails, this
        // GC will not work.
        assert_eq!(&mut *h.storage as *mut HeapStorage<'a> as usize & (HEAP_STORAGE_ALIGN - 1), 0);
        assert!(mem::size_of::<HeapStorage<'a>>() <= HEAP_STORAGE_ALIGN);

        for i in 0 .. HEAP_SIZE {
            let p = &mut h.storage.objects[i] as *mut PairStorage<'a>;
            unsafe {
                h.add_to_free_list(p);
            }
        }
        h
    }

    /// Add the object `*p` to the root set, protecting it from GC.
    ///
    /// An object that has been pinned *n* times stays in the root set
    /// until it has been unpinned *n* times.
    ///
    /// (Unsafe because if the argument is garbage, a later GC will
    /// crash. Called only from `impl PinnedRef`.)
    unsafe fn pin<T: Mark<'a>>(&self, p: *mut T) {
        let p = p as *mut ();
        let mut pins = self.pins.borrow_mut();
        let entry = pins.entry(p).or_insert(0);
        *entry += 1;
    }

    /// Unpin an object (see `pin`).
    ///
    /// (Unsafe because unpinning an object that other code is still using
    /// causes dangling pointers. Called only from `impl PinnedRef`.)
    unsafe fn unpin<T: Mark<'a>>(&self, p: *mut T) {
        let p = p as *mut ();
        let mut pins = self.pins.borrow_mut();
        if {
            let entry = pins.entry(p as *mut ()).or_insert(0);
            assert!(*entry != 0);
            *entry -= 1;
            *entry == 0
        } {
            pins.remove(&p);
        }
    }

    unsafe fn add_to_free_list(&mut self, p: *mut PairStorage<'a>) {
        let listp = p as *mut *mut PairStorage<'a>;
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
            let listp = p as *mut *mut PairStorage<'a>;
            self.freelist = *listp;

            let (h, t) = pair;
            *p = PairStorage {
                head: h.to_heap(),
                tail: t.to_heap()
            };
            Some(Pair(PinnedRef::new(self, p)))
        }
    }

    unsafe fn find_header<T>(_ptr: *const T) -> *mut HeapStorage<'a> {
        let storage_addr = _ptr as usize & !(HEAP_STORAGE_ALIGN - 1);
        storage_addr as *mut HeapStorage<'a>
    }

    unsafe fn find_mark_bit<T>(_ptr: *const T) -> (*mut BitVec, usize) {
        assert_eq!(mem::size_of::<T>(), mem::size_of::<PairStorage<'a>>());
        let storage = Heap::find_header(_ptr);
        let objects_addr = &mut (*storage).objects[0] as *mut PairStorage<'a> as usize;
        let index = (_ptr as usize - objects_addr) / mem::size_of::<PairStorage<'a>>();
        (&mut (*storage).mark_bits as *mut BitVec, index)
    }

    pub unsafe fn get_mark_bit<T>(_ptr: *const T) -> bool {
        let (bits, index) = Heap::find_mark_bit(_ptr);
        (*bits)[index]
    }

    pub unsafe fn set_mark_bit<T>(_ptr: *const T) {
        let (bits, index) = Heap::find_mark_bit(_ptr);
        (*bits).set(index, true);
    }

    pub fn alloc(&mut self, pair: (Value<'a>, Value<'a>)) -> Pair<'a> {
        self.try_alloc(pair).expect("out of memory (gc did not collect anything)")
    }

    pub fn alloc_null(&mut self) -> Pair<'a> {
        self.alloc((Value::Null, Value::Null))
    }

    unsafe fn mark(ptr: *mut ()) {
        let storage = Heap::find_header(ptr);
        let mark_fn = (*storage).mark_entry_point;
        mark_fn(ptr);
    }

    unsafe fn gc(&mut self) {
        // mark phase
        self.storage.mark_bits.clear();
        for (&p, _) in self.pins.borrow().iter() {
            Heap::mark(p);
        }

        // sweep phase
        self.freelist = ptr::null_mut();
        for i in 0 .. HEAP_SIZE {
            let p = &mut self.storage.objects[i] as *mut PairStorage<'a>;
            if !self.storage.mark_bits[i] {
                self.add_to_free_list(p);
            }
        }
    }

    #[cfg(test)]
    pub fn force_gc(&mut self) {
        unsafe { self.gc(); }
    }
}

pub struct PinnedRef<'a, T> {
    ptr: *mut T,
    heap: *const Heap<'a>,
    heap_id: HeapId<'a>
}

impl<'a, T> PinnedRef<'a, T> {
    unsafe fn new(heap: &Heap<'a>, p: *mut T) -> PinnedRef<'a, T> {
        heap.pin(p as *mut PairStorage<'a>);  // XXX BOGUS
        PinnedRef {
            ptr: p,
            heap: heap as *const Heap<'a>,
            heap_id: heap.id
        }
    }
}

impl<'a, T> Drop for PinnedRef<'a, T> {
    fn drop(&mut self) {
        unsafe {
            (*self.heap).unpin(self.ptr as *mut PairStorage<'a>);  // XXX BOGUS
        }
    }
}

impl<'a, T> Clone for PinnedRef<'a, T> {
    fn clone(&self) -> PinnedRef<'a, T> {
        let &PinnedRef { ptr, heap, heap_id } = self;
        unsafe {
            (*heap).pin(ptr as *mut PairStorage<'a>);  // XXX BOGUS
        }
        PinnedRef {
            ptr: ptr,
            heap: heap,
            heap_id: heap_id
        }
    }
}

impl<'a, T> fmt::Debug for PinnedRef<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "PinnedRef {{ ptr: {:p} }}", self.ptr)
    }
}

impl<'a, T> PartialEq for PinnedRef<'a, T> {
    fn eq(&self, other: &PinnedRef<'a, T>) -> bool {
        self.ptr == other.ptr
    }
}

impl<'a, T> Eq for PinnedRef<'a, T> {}

pub trait GCRef {
    #[cfg(test)]
    fn address(&self) -> usize;
}

// === Pair, the reference type

gc_ref_type! {
    pub struct Pair / PairStorage<'a> {
        head / set_head: Value<'a>,
        tail / set_tail: Value<'a>
    }
}


// === Values (a heap-inline enum)

use std::rc::Rc;

// Values inside the heap (GC heap-to-heap cross-references) (private)
enum ValueStorage<'a> {
    Null,
    Int(i32),
    Str(Rc<String>),
    Pair(*mut PairStorage<'a>)
}

unsafe impl<'a> Mark<'a> for ValueStorage<'a> {
    unsafe fn mark(ptr: *mut ValueStorage<'a>) {
        match *ptr {
            ValueStorage::Pair(p) => Mark::mark(p),
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

unsafe impl<'a> HeapInline<'a> for Value<'a> {
    type Storage = ValueStorage<'a>;

    fn to_heap(self) -> ValueStorage<'a> {
        match self {
            Value::Null => ValueStorage::Null,
            Value::Int(n) => ValueStorage::Int(n),
            Value::Str(rcstr) => ValueStorage::Str(rcstr),
            Value::Pair(pair) => ValueStorage::Pair(HeapInline::<'a>::to_heap(pair))
        }
    }

    unsafe fn from_heap(heap: &Heap<'a>, v: &ValueStorage<'a>) -> Value<'a> {
        match v {
            &ValueStorage::Null => Value::Null,
            &ValueStorage::Int(n) => Value::Int(n),
            &ValueStorage::Str(ref rcstr) => Value::Str(rcstr.clone()),
            &ValueStorage::Pair(ref ptr) => Value::Pair(HeapInline::<'a>::from_heap(heap, ptr))
        }
    }
}

unsafe impl<'a, T: Copy + 'static> Mark<'a> for T {
    unsafe fn mark(_ptr: *mut T) {}
}

unsafe impl<'a, T: Copy + 'static> HeapInline<'a> for T {
    type Storage = Self;

    fn to_heap(self) -> T { self }

    unsafe fn from_heap(_heap: &Heap<'a>, v: &T) -> T { *v }
}
