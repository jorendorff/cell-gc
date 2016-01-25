use traits::InHeap;
use heap::{Heap, HeapId};
use std::fmt;

pub struct GCRef<'a, T: InHeap<'a>> {
    ptr: *mut T,
    heap_id: HeapId<'a>
}

impl<'a, T: InHeap<'a>> GCRef<'a, T> {
    /// Pin an object, returning a new `GCRef` that will unpin it when
    /// dropped. Unsafe because if `p` is not a pointer to a live allocation of
    /// type `T` --- and a complete allocation, not a sub-object of one --- then
    /// later unsafe code will explode.
    pub unsafe fn new(p: *mut T) -> GCRef<'a, T> {
        let heap = Heap::from_allocation(p);
        (*heap).pin(p);
        GCRef {
            ptr: p,
            heap_id: (*heap).id
        }
    }

    pub fn as_ptr(&self) -> *mut T { self.ptr }
}

impl<'a, T: InHeap<'a>> Drop for GCRef<'a, T> {
    fn drop(&mut self) {
        unsafe {
            let heap = Heap::from_allocation(self.ptr);
            (*heap).unpin(self.ptr);
        }
    }
}

impl<'a, T: InHeap<'a>> Clone for GCRef<'a, T> {
    fn clone(&self) -> GCRef<'a, T> {
        let &GCRef { ptr, heap_id } = self;
        unsafe {
            let heap = Heap::from_allocation(ptr);
            (*heap).pin(ptr);
        }
        GCRef {
            ptr: ptr,
            heap_id: heap_id
        }
    }
}

impl<'a, T: InHeap<'a>> fmt::Debug for GCRef<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "GCRef {{ ptr: {:p} }}", self.ptr)
    }
}

impl<'a, T: InHeap<'a>> PartialEq for GCRef<'a, T> {
    fn eq(&self, other: &GCRef<'a, T>) -> bool {
        self.ptr == other.ptr
    }
}

impl<'a, T: InHeap<'a>> Eq for GCRef<'a, T> {}
