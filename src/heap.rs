//! GC heap allocator.
//!
//! ### Safety
//!
//! First, a few general observations about safety in Rust.
//!
//! *   If a `mut` reference to a value exists, and other references to the value
//!     also exist, we can definitely crash.
//!
//!     One way it can happen is that the `mut` value is or contains an
//!     `enum`. Using the other reference, we can borrow a refrence to a
//!     current field of the `enum`. Then, using the `mut` reference, we can
//!     assign some other variant to the `enum`, invalidating the reference.
//!     Rust does not detect that the reference is invalid, so when we use it,
//!     we are accessing gibberish.
//!
//!     Another way is if a callback is called while we're in the middle of
//!     mutating a value, while it's in an invalid state. When you read
//!     "callback", think of `deref` and `drop` methods, which are called
//!     implicitly all over the place. These callbacks are normally safe,
//!     because they simply can't have a reference to the value that's in an
//!     intermediate state. But if another reference exists, it might be used
//!     to try to read from the half-mutated value.
//!
//! *   If a data structure contains two paths to a value, under Rust's usual
//!     rules, you can get two `mut` references to that value.
//!
//!     This is why you tend to build ownership trees in Rust and it's why GC
//!     is particularly challenging in Rust: GCs build an arbitrary graph of
//!     values and references.
//!
//! This GC takes the following approach to ensure safety.
//!
//! *   Minimize access to values stored in the heap. For the most part,
//!     application code *never* sees a direct reference to any value that is
//!     physically stored someplace where it's subject to GC.
//!
//! *   Minimize the times when direct references to in-heap values exist at
//!     all, and during these operations, prevent control from escaping to
//!     arbitrary application code.
//!
//! *   Ensure that when any direct references to in-heap values exist, they
//!     obey Rust's rules: for any given value, either only non-`mut`
//!     references, or at most one `mut` reference, exists at a time.
//!
//! Thus we are particularly keen to avoid the possibility of "reentering" the
//! heap, creating new references to in-heap values while others already exist.
//!
//! References to heap values therefore exist only during the following
//! operations:
//!
//! *   Allocation - That is, moving values into the heap. This is safe because
//!     it never triggers any user code at all while heap references exist.
//!
//! * - Heap reads and writes - The only way to do these is via macro-generated
//!     accessors which do not expose references.  Reads call `from_heap()` on
//!     in-heap values, which is dangerous because `from_heap()` receives a
//!     direct reference.  Writes call `drop()`, which is even more dangerous:
//!     (1) it receives a direct `mut` reference; and (2) it leaves in-heap
//!     values uninitialized.
//!
//! *   GC marking - The code for this is all macro-generated.
//!
//! *   GC sweeping - This calls `drop()`, which is dangerous for the reasons
//!     noted above.
//!
//! To make this scheme safe, `from_heap()` and `drop()` must be tightly controlled.
//! `from_heap()` is therefore in an unsafe trait; users are expected to use
//! the `gc_heap_type!` to autogenerate instances.
//!
//! However, **we leave it up to the user to exercise care with `drop()`.**
//! We suggest *never* implementing `Drop` for a heap type. If you must,
//! avoid reading pointer fields while dropping, and avoid calling into
//! arbitrary code.

use std::cell::RefCell;
use std::collections::HashMap;
use std::marker::PhantomData;
use std::ptr;
use traits::{IntoHeap, IntoHeapAllocation};
use pages::{heap_type_id, PageHeader, TypedPage, PageBox};
use gcref::GCRef;

pub struct Heap {
    pages: HashMap<usize, PageBox>,
    pins: RefCell<HashMap<*mut (), usize>>
}

// What does this do? You'll never guess!
pub type HeapSessionId<'h> = PhantomData<::std::cell::Cell<&'h mut ()>>;

pub struct HeapSession<'h> {
    id: HeapSessionId<'h>,
    heap: Heap
}

/// Create a heap, pass it to a callback, then destroy the heap.
///
/// The heap's lifetime is directly tied to this function call, for safety. (So
/// the API is a little wonky --- but how many heaps were you planning on
/// creating?)
pub fn with_heap<F, O>(f: F) -> O
    where F: for<'h> FnOnce(&mut HeapSession<'h>) -> O
{
    f(&mut HeapSession::new())
}

impl Heap {
    /// Add the value `*p` to the root set, protecting it from GC.
    ///
    /// A value that has been pinned *n* times stays in the root set
    /// until it has been unpinned *n* times.
    ///
    /// # Safety
    ///
    /// `p` must point to a live allocation of type `T` in this heap.
    pub unsafe fn pin<'h, T: IntoHeap<'h>>(&self, p: *mut T::In) {
        let p = p as *mut ();
        let mut pins = self.pins.borrow_mut();
        let entry = pins.entry(p).or_insert(0);
        *entry += 1;
    }

    /// Unpin a heap-allocated value (see `pin`).
    ///
    /// # Safety
    ///
    /// `p` must point to a pinned allocation of type `T` in this heap.
    pub unsafe fn unpin<'h, T: IntoHeap<'h>>(&self, p: *mut T::In) {
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

    pub unsafe fn from_allocation<'h, T: IntoHeap<'h>>(ptr: *const T::In) -> *const Heap {
        (*TypedPage::find(ptr)).header.heap
    }

    pub unsafe fn get_mark_bit<'h, T: IntoHeap<'h>>(ptr: *const T::In) -> bool {
        (*TypedPage::find(ptr)).get_mark_bit(ptr)
    }

    pub unsafe fn set_mark_bit<'h, T: IntoHeap<'h>>(ptr: *const T::In) {
        (*TypedPage::find(ptr)).set_mark_bit(ptr);
    }

    fn gc(&mut self) {
        unsafe {
            // mark phase
            for (_, page) in self.pages.iter_mut() {
                page.clear_mark_bits();
            }
            for (&ptr, _) in self.pins.borrow().iter() {
                (*PageHeader::find(ptr)).mark(ptr);
            }

            // sweep phase
            for (_, page) in self.pages.iter_mut() {
                page.sweep();
            }
        }
    }
}

impl Drop for Heap {
    fn drop(&mut self) {
        // Perform a final GC to call destructors on any remaining allocations.
        assert!(self.pins.borrow().is_empty());
        self.gc();

        for (_, page) in self.pages.iter() {
            assert!(page.is_empty());
        }
    }
}

impl<'h> HeapSession<'h> {
    fn new() -> HeapSession<'h> {
        HeapSession {
            id: PhantomData,
            heap: Heap {
                pages: HashMap::new(),
                pins: RefCell::new(HashMap::new())
            }
        }
    }

    fn get_page<'a, T: IntoHeap<'h> + 'a>(&'a mut self) -> &'a mut TypedPage<T::In> {
        let key = heap_type_id::<T>();
        let heap_ptr = &mut self.heap as *mut Heap;
        self.heap.pages
            .entry(key)
            .or_insert_with(|| PageBox::new::<T>(heap_ptr))
            .downcast_mut::<T>()
            .unwrap()
    }

    pub fn try_alloc<T: IntoHeapAllocation<'h>>(&mut self, value: T) -> Option<T::Ref> {
        // For now, this is done very early, so that if it panics, the heap is
        // left in an OK state. Better wrapping of raw pointers would make it
        // possible to do this later, closer to the `ptr::write()` call. (And
        // the compiler might optimize away this temporary if we do it that
        // way.) Looking forward to placement new!
        let u = value.into_heap();
        unsafe {
            let alloc = self.get_page::<T>().try_alloc();
            alloc
                .or_else(|| {
                    self.heap.gc();
                    self.get_page::<T>().try_alloc()
                })
                .map(move |p| {
                    ptr::write(p, u);
                    T::wrap_gcref(GCRef::new(p))
                })
        }
    }

    pub fn alloc<T: IntoHeapAllocation<'h>>(&mut self, value: T) -> T::Ref {
        self.try_alloc(value).expect("out of memory (gc did not collect anything)")
    }

    pub fn force_gc(&mut self) {
        self.heap.gc();
    }
}
