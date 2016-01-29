//! Collections for use with GC references.

use traits::{InHeap, IntoHeap, IntoHeapAllocation};
use gcref::GCRef;
use heap::HeapId;
use std::marker::PhantomData;
use std::mem;

/// An implementation detail.
pub struct VecStorage<'a, U: InHeap<'a>>(Vec<U>, HeapId<'a>);

unsafe impl<'a, U: InHeap<'a>> InHeap<'a> for VecStorage<'a, U> {
    type Out = Vec<U::Out>;

    unsafe fn mark(&self) {
        for r in &self.0 {
            r.mark();
        }
    }

    unsafe fn from_heap(&self) -> Vec<U::Out> {
        self.0.iter().map(|x| x.from_heap()).collect()
    }
}

unsafe impl<'a, T: IntoHeap<'a>> IntoHeap<'a> for Vec<T> {
    type In = VecStorage<'a, T::In>;

    fn into_heap(self) -> VecStorage<'a, T::In> {
        VecStorage(self.into_iter().map(|x| x.into_heap()).collect(), PhantomData)
    }
}

impl<'a, T: IntoHeap<'a>> IntoHeapAllocation<'a> for Vec<T> {
    type Ref = VecRef<'a, T>;

    fn wrap_gcref(gcref: GCRef<'a, VecStorage<'a, T::In>>) -> VecRef<'a, T> {
        VecRef(gcref)
    }
}

/// A reference to a GC-heap-allocated `Vec`.
///
/// To allocate a vector in the heap, simply call `heap.alloc` on a plain old `Vec`.
/// It will return a `VecRef` object which has many of the familiar `Vec` methods,
/// as well as element accessors `get_at` and `set_at`.
///
///     #[macro_use] extern crate toy_gc;
///     use toy_gc::collections::VecRef;
///
///     fn main() {
///         toy_gc::with_heap(|heap| {
///             let r: VecRef<i32> = heap.alloc(vec![20, 10, 40, 30]);
///             assert_eq!(r.length(), 4);
///             assert_eq!(r.get(0), 20);
///             r.push(100);
///             r.sort();
///             assert_eq!(r.get_all(), vec![10, 20, 30, 40, 100]);
///         });
///     }
///
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VecRef<'a, T: IntoHeap<'a>>(GCRef<'a, VecStorage<'a, T::In>>);

unsafe impl<'a, U: InHeap<'a>> InHeap<'a> for *mut VecStorage<'a, U> {
    type Out = VecRef<'a, U::Out>;

    unsafe fn mark(&self) {
        for r in &(**self).0 {
            r.mark();
        }
    }

    unsafe fn from_heap(&self) -> VecRef<'a, U::Out> {
        VecRef(GCRef::new(*self))
    }
}

unsafe impl<'a, T: IntoHeap<'a>> IntoHeap<'a> for VecRef<'a, T> {
    type In = *mut VecStorage<'a, T::In>;

    fn into_heap(self) -> *mut VecStorage<'a, T::In> {
        self.0.as_mut_ptr()
    }
}

impl<'a, T: IntoHeap<'a>> VecRef<'a, T> {
    /// Unsafe to call: During the lifetime of the reference passed to the
    /// callback, the caller must ensure that the vector is not modified. Look
    /// out for destructors and `to_heap/from_heap` methods.
    fn unsafe_deref<R, F>(&self, f: F) -> R
        where F: for<'b> FnOnce(&'b Vec<T::In>) -> R
    {
        // NOTE: the signature above is overly restrictive.
        // I should be able to figure out a better one when I have a little time.
        // TODO lock here!
        f(unsafe { &(*self.0.as_ptr()).0 })
        // TODO release lock here!
    }

    /// Unsafe to call: During the lifetime of the reference passed to the
    /// callback, the caller must ensure that no other references exist to the
    /// vector.  Destructors and `from_heap` methods are presumed safe. Avoid
    /// `to_heap`.
    fn unsafe_deref_mut<R, F>(&self, f: F) -> R
        where F: for<'b> FnOnce(&'b mut Vec<T::In>) -> R
    {
        // TODO lock here!
        f(unsafe { &mut (*self.0.as_mut_ptr()).0 })
        // TODO release lock here!
    }

    pub fn length(&self) -> usize {
        self.unsafe_deref(|v| v.len())
    }

    pub fn capacity(&self) -> usize {
        self.unsafe_deref(|v| v.capacity())
    }
 
    pub fn get(&self, i: usize) -> T {
        self.unsafe_deref(|v| unsafe { v[i].from_heap() })
    }

    pub fn get_all(&self) -> Vec<T> {
        // NOTE: not using unsafe_deref because of the `for<'b>` in its signature.
        unsafe { (*self.0.as_ptr()).from_heap() }
    }

    pub fn set(&self, i: usize, value: T) {
        let u = value.into_heap();
        self.unsafe_deref_mut(|v| v[i] = u);
    }

    pub fn truncate(&self, len: usize) {
        self.unsafe_deref_mut(|v| v.truncate(len));
    }

    pub fn swap_remove(&self, index: usize) -> T {
        let u = self.unsafe_deref_mut(|v| v.swap_remove(index));
        unsafe { u.from_heap() }
    }

    pub fn insert(&self, index: usize, element: T) {
        let u = element.into_heap();
        self.unsafe_deref_mut(|v| v.insert(index, u));
    }

    pub fn remove(&self, index: usize) -> T {
        let u = self.unsafe_deref_mut(|v| v.remove(index));
        unsafe { u.from_heap() }
    }

    pub fn push(&self, value: T) {
        let u = value.into_heap();
        self.unsafe_deref_mut(|v| v.push(u));
    }

    pub fn pop(&self) -> Option<T> {
        self.unsafe_deref_mut(|v| v.pop()).map(|u| unsafe { u.from_heap() })
    }

    // BUG: the `T::In: Ord` bound here is going to be weird for users.
    pub fn sort(&self) where T::In: Ord {
        // Make the heap vector empty while we're sorting it.  This way, the
        // user can do whatever malicious nonsense they like to the heap in
        // Ord::compare, and our copy of the vector is unaffected.
        let mut tmp = vec![];
        self.unsafe_deref_mut(|v| mem::swap(&mut tmp, v));
        tmp.sort();
        self.unsafe_deref_mut(|v| mem::swap(&mut tmp, v));
    }
}
