//! GC heap allocator.

use std::cell::RefCell;
use std::collections::HashMap;
use std::marker::PhantomData;
use std::ptr;
use traits::{Mark, ToHeap, GCThing, GCRef, gcthing_type_id};
use pages::{PageHeader, TypedPage, PageBox};
use refs::PinnedRef;

// What does this do? You'll never guess!
pub type HeapId<'a> = PhantomData<::std::cell::Cell<&'a mut ()>>;

pub struct Heap<'a> {
    pub id: HeapId<'a>,
    pages: HashMap<usize, PageBox<'a>>,
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

impl<'a> Heap<'a> {
    fn new() -> Heap<'a> {
        Heap {
            id: PhantomData,
            pages: HashMap::new(),
            pins: RefCell::new(HashMap::new())
        }
    }

    fn get_page<T: GCThing<'a>>(&mut self) -> &mut TypedPage<'a, T> {
        let key = gcthing_type_id::<T>();
        let heap_ptr = self as *mut Heap<'a>;
        self.pages
            .entry(key)
            .or_insert_with(|| PageBox::new::<T>(heap_ptr))
            .downcast_mut::<T>()
            .unwrap()
    }

    /// Add the object `*p` to the root set, protecting it from GC.
    ///
    /// An object that has been pinned *n* times stays in the root set
    /// until it has been unpinned *n* times.
    ///
    /// (Unsafe because if the argument is garbage, a later GC will
    /// crash. Called only from `impl PinnedRef`.)
    pub unsafe fn pin<T: GCThing<'a>>(&self, p: *mut T) {
        let p = p as *mut ();
        let mut pins = self.pins.borrow_mut();
        let entry = pins.entry(p).or_insert(0);
        *entry += 1;
    }

    /// Unpin an object (see `pin`).
    ///
    /// (Unsafe because unpinning an object that other code is still using
    /// causes dangling pointers. Called only from `impl PinnedRef`.)
    pub unsafe fn unpin<T: GCThing<'a>>(&self, p: *mut T) {
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

    pub fn try_alloc<T: GCRef<'a>>(&mut self, fields: T::Fields) -> Option<T> {
        unsafe {
            let alloc = self.get_page::<T::ReferentStorage>().try_alloc();
            alloc
                .or_else(|| {
                    self.gc();
                    self.get_page::<T::ReferentStorage>().try_alloc()
                })
                .map(move |p| {
                    ptr::write(p, fields.to_heap());
                    T::from_pinned_ref(PinnedRef::new(p))
                })
        }
    }

    pub unsafe fn from_allocation<T: GCThing<'a>>(ptr: *const T) -> *const Heap<'a> {
        (*TypedPage::find(ptr)).header.heap
    }

    pub unsafe fn get_mark_bit<T: GCThing<'a>>(ptr: *const T) -> bool {
        (*TypedPage::find(ptr)).get_mark_bit(ptr)
    }

    pub unsafe fn set_mark_bit<T: GCThing<'a>>(ptr: *const T) {
        (*TypedPage::find(ptr)).set_mark_bit(ptr);
    }

    pub fn alloc<T: GCRef<'a>>(&mut self, fields: T::Fields) -> T {
        self.try_alloc(fields).expect("out of memory (gc did not collect anything)")
    }

    unsafe fn gc(&mut self) {
        // mark phase
        for (_, page) in self.pages.iter_mut() {
            page.clear_mark_bits();
        }
        for (&ptr, _) in self.pins.borrow().iter() {
            (*PageHeader::<'a>::find(ptr)).mark(ptr);
        }

        // sweep phase
        for (_, page) in self.pages.iter_mut() {
            page.sweep();
        }
    }

    #[cfg(test)]
    pub fn force_gc(&mut self) {
        unsafe { self.gc(); }
    }
}

impl<'a> Drop for Heap<'a> {
    fn drop(&mut self) {
        // Perform a final GC to call destructors on any remaining allocations.
        assert!(self.pins.borrow().is_empty());
        unsafe { self.gc(); }

        for (_, page) in self.pages.iter() {
            assert!(page.is_empty());
        }
    }
}
