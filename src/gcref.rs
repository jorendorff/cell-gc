use traits::IntoHeap;
use heap::{Heap, HeapId};
use std::fmt;
use std::marker::PhantomData;

pub struct GCRef<'a, T: IntoHeap<'a>> {
    ptr: *mut T::In,
    heap_id: HeapId<'a>
}

impl<'a, T: IntoHeap<'a>> GCRef<'a, T> {
    /// Pin an object, returning a new `GCRef` that will unpin it when
    /// dropped. Unsafe because if `p` is not a pointer to a live allocation of
    /// type `T::In` --- and a complete allocation, not a sub-object of one ---
    /// then later unsafe code will explode.
    pub unsafe fn new(p: *mut T::In) -> GCRef<'a, T> {
        let heap: *const Heap<'a> = Heap::from_allocation::<T>(p);
        (*heap).pin::<T>(p);
        GCRef {
            ptr: p,
            heap_id: PhantomData
        }
    }

    pub fn as_ptr(&self) -> *const T::In { self.ptr }
    pub fn as_mut_ptr(&self) -> *mut T::In { self.ptr }
}

impl<'a, T: IntoHeap<'a>> Drop for GCRef<'a, T> {
    fn drop(&mut self) {
        unsafe {
            let heap = Heap::from_allocation::<T>(self.ptr);
            (*heap).unpin::<T>(self.ptr);
        }
    }
}

impl<'a, T: IntoHeap<'a>> Clone for GCRef<'a, T> {
    fn clone(&self) -> GCRef<'a, T> {
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

impl<'a, T: IntoHeap<'a>> fmt::Debug for GCRef<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "GCRef {{ ptr: {:p} }}", self.ptr)
    }
}

impl<'a, T: IntoHeap<'a>> PartialEq for GCRef<'a, T> {
    fn eq(&self, other: &GCRef<'a, T>) -> bool {
        self.ptr == other.ptr
    }
}

impl<'a, T: IntoHeap<'a>> Eq for GCRef<'a, T> {}
