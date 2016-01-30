//! Collections for use with GC references.

use traits::{IntoHeap, IntoHeapAllocation};
use gcref::GCRef;
use heap::HeapId;
use std::marker::PhantomData;
use std::mem;
use std::cmp::Ordering;

/// An implementation detail.
pub struct VecStorage<'h, T: IntoHeap<'h>>(Vec<T::In>, HeapId<'h>);

unsafe impl<'h, T: IntoHeap<'h>> IntoHeap<'h> for Vec<T> {
    type In = VecStorage<'h, T>;

    fn into_heap(self) -> VecStorage<'h, T> {
        VecStorage(self.into_iter().map(|x| x.into_heap()).collect(), PhantomData)
    }

    unsafe fn from_heap(storage: &VecStorage<'h, T>) -> Vec<T> {
        storage.0.iter().map(|x| T::from_heap(x)).collect()
    }

    unsafe fn mark(storage: &VecStorage<'h, T>) {
        for r in &storage.0 {
            T::mark(r);
        }
    }
}

impl<'h, T: IntoHeap<'h>> IntoHeapAllocation<'h> for Vec<T> {
    type Ref = VecRef<'h, T>;

    fn wrap_gcref(gcref: GCRef<'h, Vec<T>>) -> VecRef<'h, T> {
        VecRef(gcref)
    }
}

/// A reference to a GC-heap-allocated `Vec`.
///
/// To allocate a vector in the heap, simply call `heap.alloc` on a plain old `Vec`.
/// It will return a `VecRef` object which has many of the familiar `Vec` methods,
/// as well as element accessors `get_at` and `set_at`.
///
/// ```rust
/// use cell_gc::collections::VecRef;
///
/// cell_gc::with_heap(|heap| {
///     let r: VecRef<i32> = heap.alloc(vec![20, 10, 40, 30]);
///     assert_eq!(r.len(), 4);
///     assert_eq!(r.get(0), 20);
///     r.push(100);
///     r.sort();
///     assert_eq!(r.get_all(), vec![10, 20, 30, 40, 100]);
/// });
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VecRef<'h, T: IntoHeap<'h>>(GCRef<'h, Vec<T>>);

unsafe impl<'h, T: IntoHeap<'h>> IntoHeap<'h> for VecRef<'h, T> {
    type In = *mut VecStorage<'h, T>;

    fn into_heap(self) -> *mut VecStorage<'h, T> {
        self.0.as_mut_ptr()
    }

    unsafe fn from_heap(storage: &*mut VecStorage<'h, T>) -> VecRef<'h, T> {
        VecRef(GCRef::new(*storage))
    }

    unsafe fn mark(storage: &*mut VecStorage<'h, T>) {
        // BUG - should call a method mark_ref that checks the mark bit before
        // doing anything
        for r in &(**storage).0 {
            T::mark(r);
        }
    }
}

impl<'h, T: IntoHeap<'h>> VecRef<'h, T> {
    /// Unsafe to call: During the lifetime of the reference passed to the
    /// callback, the caller must ensure that the vector is not modified.
    /// Destructors and `from_heap` methods are presumed safe. Avoid
    /// `to_heap`.
    fn unsafe_deref<R, F>(&self, f: F) -> R
        where F: for <'b> FnOnce(&'b Vec<T::In>) -> R
    {
        // NOTE: the signature above is overly restrictive.
        // I should be able to figure out a better one when I have a little time.
        // TODO: in debug builds at least there should be a RefCell-like "lock" here,
        // asserting that no direct Rust references exist to anything in the GC heap.
        f(unsafe { &(*self.0.as_ptr()).0 })
    }

    /// Unsafe to call: During the lifetime of the reference passed to the
    /// callback, the caller must ensure that no other references exist to the
    /// vector.  Destructors and `from_heap` methods are presumed safe. Avoid
    /// `to_heap`.
    fn unsafe_deref_mut<R, F>(&self, f: F) -> R
        where F: for <'b> FnOnce(&'b mut Vec<T::In>) -> R
    {
        // TODO should assert here, like unsafe_deref
        f(unsafe { &mut (*self.0.as_mut_ptr()).0 })
    }

    /// Get the element `index` from the vector.
    ///
    /// This clones the element, so `VecRef<LargeStruct>::get()` would be slow.
    /// `VecRef<LargeStructRef>::get()` only clones a reference, so it is
    /// relatively quick.
    ///
    /// # Panics
    ///
    /// Panics if `index` is out of bounds.
    pub fn get(&self, index: usize) -> T {
        self.unsafe_deref(|v| unsafe { IntoHeap::from_heap(&v[index]) })
    }

    /// Copy the vector out of the GC-managed heap. This returns an ordinary,
    /// non-GC-managed Rust `Vec`.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use cell_gc::collections::VecRef;
    /// # cell_gc::with_heap(|heap| {
    /// let gc_vec: VecRef<i32> = heap.alloc(vec![]);
    /// for i in 0 .. 4 {
    ///     gc_vec.push(i);
    /// }
    /// let plain_vec = gc_vec.get_all();
    /// assert_eq!(plain_vec, vec![0, 1, 2, 3]);
    /// # });
    /// ```
    pub fn get_all(&self) -> Vec<T> {
        unsafe { IntoHeap::from_heap(&*self.0.as_ptr()) }
    }

    /// Set element `index` of the vector to `value`.
    ///
    /// # Panics
    ///
    /// Panics if `index` is out of bounds.
    pub fn set(&self, index: usize, value: T) {
        let u = value.into_heap();
        self.unsafe_deref_mut(|v| v[index] = u);
    }

    pub fn capacity(&self) -> usize {
        self.unsafe_deref(|v| v.capacity())
    }

    pub fn reserve(&self, additional: usize) {
        self.unsafe_deref_mut(|v| v.reserve(additional));
    }

    pub fn reserve_exact(&self, additional: usize) {
        self.unsafe_deref_mut(|v| v.reserve_exact(additional));
    }

    pub fn shrink_to_fit(&self) {
        self.unsafe_deref_mut(|v| v.shrink_to_fit());
    }

    pub fn truncate(&self, len: usize) {
        self.unsafe_deref_mut(|v| v.truncate(len));
    }

    pub fn swap_remove(&self, index: usize) -> T {
        let u = self.unsafe_deref_mut(|v| v.swap_remove(index));
        unsafe { IntoHeap::from_heap(&u) }
    }

    pub fn insert(&self, index: usize, element: T) {
        let u = element.into_heap();
        self.unsafe_deref_mut(|v| v.insert(index, u));
    }

    pub fn remove(&self, index: usize) -> T {
        let u = self.unsafe_deref_mut(|v| v.remove(index));
        unsafe { IntoHeap::from_heap(&u) }
    }

    pub fn push(&self, value: T) {
        let u = value.into_heap();
        self.unsafe_deref_mut(|v| v.push(u));
    }

    pub fn pop(&self) -> Option<T> {
        self.unsafe_deref_mut(|v| v.pop().map(|u| unsafe { IntoHeap::from_heap(&u) }))
    }

    pub fn append(&self, other: &VecRef<'h, T>) {
        let mut tmp: Vec<T::In> = vec![];
        other.unsafe_deref_mut(|src| mem::swap(src, &mut tmp));
        self.unsafe_deref_mut(|dest| dest.append(&mut tmp));
    }

    pub fn clear(&self) {
        self.unsafe_deref_mut(|v| v.clear());
    }

    pub fn len(&self) -> usize {
        self.unsafe_deref(|v| v.len())
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Get the first element of the vector, or `None` if the vector is empty.
    ///
    /// The standard `Vec<T>::first` returns an `&T` reference into the vector,
    /// but cell-gc never hands out Rust references to heap values. Instead
    /// this returns a clone of the value.
    pub fn first(&self) -> Option<T> {
        self.unsafe_deref(|v| v.first().map(|u| unsafe { IntoHeap::from_heap(u) }))
    }

    /// Get the last element of the vector, or `None` if the vector is empty.
    ///
    /// Like `first()`, this clones the value.
    pub fn last(&self) -> Option<T> {
        self.unsafe_deref(|v| v.last().map(|u| unsafe { IntoHeap::from_heap(u) }))
    }

    /// Sort the vector in place.
    pub fn sort(&self) where T: Ord {
        self.sort_by(Ord::cmp);
    }

    pub fn sort_by<F>(&self, mut compare: F) where F: FnMut(&T, &T) -> Ordering {
        // Make the heap vector empty while we're sorting it.  This way, the
        // user can do whatever malicious nonsense they like to the heap in
        // `compare`, and our copy of the vector is unaffected.
        let mut tmp = vec![];
        self.unsafe_deref_mut(|v| mem::swap(&mut tmp, v));
        tmp.sort_by(|ru1, ru2| {
            let t1 = unsafe { IntoHeap::from_heap(ru1) };
            let t2 = unsafe { IntoHeap::from_heap(ru2) };
            compare(&t1, &t2)
        });
        self.unsafe_deref_mut(|v| mem::swap(&mut tmp, v));
    }
}
