use traits::IntoHeap;
use heap::{Heap, HeapId};
use std::fmt;
use std::marker::PhantomData;

pub struct GCRef<'h, T: IntoHeap<'h>> {
    ptr: *mut T::In,
    heap_id: HeapId<'h>
}

impl<'h, T: IntoHeap<'h>> GCRef<'h, T> {
    /// Pin an object, returning a new `GCRef` that will unpin it when
    /// dropped. Unsafe because if `p` is not a pointer to a live allocation of
    /// type `T::In` --- and a complete allocation, not a sub-object of one ---
    /// then later unsafe code will explode.
    pub unsafe fn new(p: *mut T::In) -> GCRef<'h, T> {
        let heap: *const Heap<'h> = Heap::from_allocation::<T>(p);
        (*heap).pin::<T>(p);
        GCRef {
            ptr: p,
            heap_id: PhantomData
        }
    }

    pub fn as_ptr(&self) -> *const T::In { self.ptr }
    pub fn as_mut_ptr(&self) -> *mut T::In { self.ptr }
}

impl<'h, T: IntoHeap<'h>> Drop for GCRef<'h, T> {
    fn drop(&mut self) {
        unsafe {
            let heap = Heap::from_allocation::<T>(self.ptr);
            (*heap).unpin::<T>(self.ptr);
        }
    }
}

impl<'h, T: IntoHeap<'h>> Clone for GCRef<'h, T> {
    fn clone(&self) -> GCRef<'h, T> {
        let &GCRef { ptr, heap_id } = self;
        unsafe {
            let heap = Heap::from_allocation::<T>(ptr);
            (*heap).pin::<T>(ptr);
        }
        GCRef {
            ptr: ptr,
            heap_id: heap_id
        }
    }
}

impl<'h, T: IntoHeap<'h>> fmt::Debug for GCRef<'h, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "GCRef {{ ptr: {:p} }}", self.ptr)
    }
}

impl<'h, T: IntoHeap<'h>> PartialEq for GCRef<'h, T> {
    fn eq(&self, other: &GCRef<'h, T>) -> bool {
        self.ptr == other.ptr
    }
}

impl<'h, T: IntoHeap<'h>> Eq for GCRef<'h, T> {}
