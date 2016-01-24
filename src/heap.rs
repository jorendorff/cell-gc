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
    freelist: *mut (),
    objects: [PairStorage<'a>; HEAP_SIZE]
}

impl<'a> HeapStorage<'a> {
    unsafe fn new() -> Box<HeapStorage<'a>> {
        let mut storage = Box::new(HeapStorage {
            mark_bits: BitVec::from_elem(HEAP_SIZE, false),
            mark_entry_point: mark_entry_point::<PairStorage>,
            freelist: ptr::null_mut(),
            objects: mem::uninitialized()
        });

        // These assertions will likely fail on 32-bit platforms or if someone
        // is somehow using a custom Box allocator. If either one fails, this
        // GC will not work.
        assert_eq!(&mut *storage as *mut HeapStorage<'a> as usize & (HEAP_STORAGE_ALIGN - 1), 0);
        assert!(mem::size_of::<HeapStorage<'a>>() <= HEAP_STORAGE_ALIGN);

        for i in 0 .. HEAP_SIZE {
            let p = &mut storage.objects[i] as *mut PairStorage<'a>;
            storage.add_to_free_list(p);
        }
        storage
    }

    unsafe fn add_to_free_list(&mut self, p: *mut PairStorage<'a>) {
        let listp = p as *mut *mut ();
        *listp = self.freelist;
        assert_eq!(*listp, self.freelist);
        self.freelist = p as *mut ();
    }

    unsafe fn try_alloc(&mut self) -> Option<*mut PairStorage<'a>> {
        let p = self.freelist;
        if p.is_null() {
            None
        } else {
            let listp = p as *mut *mut ();
            self.freelist = *listp;
            Some(p as *mut PairStorage<'a>)
        }
    }

    unsafe fn sweep(&mut self) {
        self.freelist = ptr::null_mut();
        for i in 0 .. HEAP_SIZE {
            let p = &mut self.objects[i] as *mut PairStorage<'a>;
            if !self.mark_bits[i] {
                self.add_to_free_list(p);
            }
        }
    }
}

pub struct Heap<'a> {
    id: HeapId<'a>,
    storage: Box<HeapStorage<'a>>,
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
///
/// This trait is unsafe to implement for several reasons, ranging from:
///
/// *   The common-sense: API users aren't supposed to know or care about this.
///     It is only public so that the public macros can see it.
///     Use `gc_ref_type!` and `gc_inline_enum!`.
///
/// *   To the obvious: `Storage` objects are full of pointers and if `to_heap`
///     puts garbage into them, GC will crash.
///
/// *   To the subtle: `from_heap` receives a non-mut reference to a heap
///     value. But there may exist gc-references to that value, in which case
///     `from_heap` (or other code it calls) could modify the value while this
///     direct, non-mut reference exists, which could lead to crashes (due to
///     changing enums if nothing else) - all without using any unsafe code.
///
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
        Heap {
            id: PhantomData,
            storage: unsafe { HeapStorage::new() },
            pins: RefCell::new(HashMap::new())
        }
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

    pub fn try_alloc(&mut self, pair: (Value<'a>, Value<'a>)) -> Option<Pair<'a>> {
        unsafe {
            self.storage.try_alloc()
                .or_else(|| {
                    self.gc();
                    self.storage.try_alloc()
                })
                .map(move |p| {
                    *p = PairStorage {
                        head: pair.0.to_heap(),
                        tail: pair.1.to_heap()
                    };
                    Pair(PinnedRef::new(self, p))
                })
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
        self.storage.sweep();
    }

    #[cfg(test)]
    pub fn force_gc(&mut self) {
        unsafe { self.gc(); }
    }
}

pub struct PinnedRef<'a, T: Mark<'a>> {
    ptr: *mut T,
    heap: *const Heap<'a>,
    heap_id: HeapId<'a>
}

impl<'a, T: Mark<'a>> PinnedRef<'a, T> {
    /// Pin an object, returning a new `PinnedRef` that will unpin it when
    /// dropped. Unsafe because if `p` is not a pointer to a live allocation of
    /// type `T` --- and a complete allocation, not a sub-object of one --- then
    /// later unsafe code will explode.
    unsafe fn new(heap: &Heap<'a>, p: *mut T) -> PinnedRef<'a, T> {
        heap.pin(p);
        PinnedRef {
            ptr: p,
            heap: heap as *const Heap<'a>,
            heap_id: heap.id
        }
    }
}

impl<'a, T: Mark<'a>> Drop for PinnedRef<'a, T> {
    fn drop(&mut self) {
        unsafe {
            (*self.heap).unpin(self.ptr);
        }
    }
}

impl<'a, T: Mark<'a>> Clone for PinnedRef<'a, T> {
    fn clone(&self) -> PinnedRef<'a, T> {
        let &PinnedRef { ptr, heap, heap_id } = self;
        unsafe {
            (*heap).pin(ptr);
        }
        PinnedRef {
            ptr: ptr,
            heap: heap,
            heap_id: heap_id
        }
    }
}

impl<'a, T: Mark<'a>> fmt::Debug for PinnedRef<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "PinnedRef {{ ptr: {:p} }}", self.ptr)
    }
}

impl<'a, T: Mark<'a>> PartialEq for PinnedRef<'a, T> {
    fn eq(&self, other: &PinnedRef<'a, T>) -> bool {
        self.ptr == other.ptr
    }
}

impl<'a, T: Mark<'a>> Eq for PinnedRef<'a, T> {}

pub trait GCRef {
    #[cfg(test)]
    fn address(&self) -> usize;
}

macro_rules! gc_trivial_impl {
    ($t:ty) => {
        unsafe impl<'a> Mark<'a> for $t {
            unsafe fn mark(_ptr: *mut $t) {}
        }

        unsafe impl<'a> HeapInline<'a> for $t {
            type Storage = Self;
            fn to_heap(self) -> $t { self }
            unsafe fn from_heap(_heap: &Heap<'a>, v: &$t) -> $t { (*v).clone() }
        }
    }
}

gc_trivial_impl!(bool);
gc_trivial_impl!(char);
gc_trivial_impl!(i8);
gc_trivial_impl!(u8);
gc_trivial_impl!(i16);
gc_trivial_impl!(u16);
gc_trivial_impl!(i32);
gc_trivial_impl!(u32);
gc_trivial_impl!(i64);
gc_trivial_impl!(u64);
gc_trivial_impl!(isize);
gc_trivial_impl!(usize);
gc_trivial_impl!(f32);
gc_trivial_impl!(f64);

use std::rc::Rc;
gc_trivial_impl!(Rc<String>);

/*
// 'static types are heap-safe because ref types are never 'static.
// Unfortunately I can't make the compiler understand this: the rules
// to prevent conflicting trait impls make this conflict with almost
// everything.
unsafe impl<'a, T: Clone + 'static> Mark<'a> for T { ...trivial... }
unsafe impl<'a, T: Clone + 'static> HeapInline<'a> for T { ...trivial... }
*/


// === Pair, the reference type

gc_ref_type! {
    pub struct Pair / PairStorage<'a> {
        head / set_head: Value<'a>,
        tail / set_tail: Value<'a>
    }
}


// === Values (a heap-inline enum)

gc_inline_enum! {
    pub enum Value / ValueStorage <'a> {
        Null,
        Int(i32),
        Str(Rc<String>),  // <-- equality is by value
        Pair(Pair<'a>)  // <-- equality is by pointer
    }
}
