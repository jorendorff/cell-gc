use heap::{Heap, HeapSessionId};
use ptr::Pointer;
use std::fmt;
use std::marker::PhantomData;
use traits::IntoHeapAllocation;
use gc_leaf::GcLeaf;

pub struct GcRef<'h, T: IntoHeapAllocation<'h>> {
    ptr: Pointer<T::In>, // never null
    heap_id: HeapSessionId<'h>,
}

impl<'h, T: IntoHeapAllocation<'h>> GcRef<'h, T> {
    /// Pin an object, returning a new `GcRef` that will unpin it when
    /// dropped. Unsafe because if `p` is not a pointer to a live allocation of
    /// type `T::In` --- and a complete allocation, not a sub-object of one ---
    /// then later unsafe code will explode.
    pub unsafe fn new(p: Pointer<T::In>) -> GcRef<'h, T> {
        let heap: *const Heap = Heap::from_allocation::<T>(p);
        (*heap).pin::<T>(p);
        GcRef {
            ptr: p,
            heap_id: PhantomData,
        }
    }

    pub fn ptr(&self) -> Pointer<T::In> {
        self.ptr
    }

    pub fn as_ptr(&self) -> *const T::In {
        self.ptr.as_raw()
    }

    pub fn as_mut_ptr(&self) -> *mut T::In {
        self.ptr.as_raw() as *mut T::In
    }
}

impl<'h, T: Clone + Send + 'static> GcRef<'h, GcLeaf<T>> {
    pub fn get(&self) -> T {
        // XXX TODO I think this `unsafe` block is sound, I'm not totally
        // sure. It's OK as long as we can't trigger GC in the middle of
        // cloning...  which would ordinarily be the case since the 'static
        // type T won't contain a reference to the HeapSession<'h>.
        //
        // But one can imagine some kind of wrapper type that could store a
        // HeapSession<'h> within it, then let you get it back out as an Option
        // if we're somehow sure the lifetime `'h` isn't expired yet.  If that
        // primitive is safe, then this is unsound, and we'd have to restrict
        // GcLeaf to Copy types (ewww!)
        T::clone(unsafe { self.ptr.as_ref() })
    }
}

impl<'h, T: IntoHeapAllocation<'h>> Drop for GcRef<'h, T> {
    fn drop(&mut self) {
        unsafe {
            let heap = Heap::from_allocation::<T>(self.ptr);
            (*heap).unpin::<T>(self.ptr);
        }
    }
}

impl<'h, T: IntoHeapAllocation<'h>> Clone for GcRef<'h, T> {
    fn clone(&self) -> GcRef<'h, T> {
        let &GcRef { ptr, heap_id } = self;
        unsafe {
            let heap = Heap::from_allocation::<T>(ptr);
            (*heap).pin::<T>(ptr);
        }
        GcRef {
            ptr: ptr,
            heap_id: heap_id,
        }
    }
}

impl<'h, T: IntoHeapAllocation<'h>> fmt::Debug for GcRef<'h, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "GcRef {{ ptr: {:p} }}", self.ptr.as_raw())
    }
}

impl<'h, T: IntoHeapAllocation<'h>> PartialEq for GcRef<'h, T> {
    fn eq(&self, other: &GcRef<'h, T>) -> bool {
        self.ptr == other.ptr
    }
}

impl<'h, T: IntoHeapAllocation<'h>> Eq for GcRef<'h, T> {}
