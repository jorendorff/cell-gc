//! Collections for use with GC references.

use traits::{InHeap, IntoHeap, IntoHeapAllocation};
use gcref::GCRef;
use heap::HeapId;
use std::marker::PhantomData;
use std::mem;
use std::cmp::Ordering;

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
    /// callback, the caller must ensure that the vector is not modified.
    /// Destructors and `from_heap` methods are presumed safe. Avoid
    /// `to_heap`.
    fn unsafe_deref<'b, R, F>(&'b self, f: F) -> R
        where F: FnOnce(&'b Vec<T::In>) -> R
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
    fn unsafe_deref_mut<'b, R, F>(&'b self, f: F) -> R
        where F: FnOnce(&'b mut Vec<T::In>) -> R
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
        self.unsafe_deref(|v| unsafe { v[index].from_heap() })
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
        unsafe { (*self.0.as_ptr()).from_heap() }
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

    pub fn append(&self, other: &VecRef<'a, T>) {
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
        let r = self.unsafe_deref(|v| v.first());
        r.map(|u| unsafe { u.from_heap() })
    }

    /// Get the last element of the vector, or `None` if the vector is empty.
    ///
    /// Like `first()`, this clones the value.
    pub fn last(&self) -> Option<T> {
        let r = self.unsafe_deref(|v| v.last());
        r.map(|u| unsafe { u.from_heap() })
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
            let t1 = unsafe { ru1.from_heap() };
            let t2 = unsafe { ru2.from_heap() };
            compare(&t1, &t2)
        });
        self.unsafe_deref_mut(|v| mem::swap(&mut tmp, v));
    }
}
