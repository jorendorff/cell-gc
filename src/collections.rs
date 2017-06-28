//! Collections for use with GC references.

use traits::{IntoHeap, IntoHeapAllocation};
use gcref::GcRef;
use ptr::Pointer;
use std::mem;
use std::cmp::Ordering;

unsafe impl<'h, T: IntoHeap<'h>> IntoHeap<'h> for Vec<T> {
    type In = Vec<T::In>;

    fn into_heap(self) -> Vec<T::In> {
        self.into_iter().map(|x| x.into_heap()).collect()
    }

    unsafe fn from_heap(storage: &Vec<T::In>) -> Vec<T> {
        storage.iter().map(|x| T::from_heap(x)).collect()
    }

    unsafe fn mark(storage: &Vec<T::In>) {
        for r in storage {
            T::mark(r);
        }
    }
}

impl<'h, T: IntoHeap<'h>> IntoHeapAllocation<'h> for Vec<T> {
    type Ref = VecRef<'h, T>;

    fn wrap_gcref(gcref: GcRef<'h, Vec<T>>) -> VecRef<'h, T> {
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
pub struct VecRef<'h, T: IntoHeap<'h>>(GcRef<'h, Vec<T>>);

unsafe impl<'h, T: IntoHeap<'h>> IntoHeap<'h> for VecRef<'h, T> {
    type In = Pointer<Vec<T::In>>;

    fn into_heap(self) -> Pointer<Vec<T::In>> {
        self.0.ptr()
    }

    unsafe fn from_heap(storage: &Pointer<Vec<T::In>>) -> VecRef<'h, T> {
        VecRef(GcRef::new(*storage))
    }

    unsafe fn mark(storage: &Pointer<Vec<T::In>>) {
        // BUG - should call a method mark_ref that checks the mark bit before
        // doing anything
        for r in storage.as_ref() {
            T::mark(r);
        }
    }
}

impl<'h, T: IntoHeap<'h>> VecRef<'h, T> {
    /// Run the given closure, passing it a reference to the heap-internal
    /// storage vector.
    ///
    /// # Safety
    ///
    /// Anything that provides direct access to any heap storage is unsafe
    /// because Rust can't guarantee there aren't other ways to obtain `mut`
    /// references to that data at the same time.
    ///
    /// Therefore, during the call to `f`, it's the caller responsibility to
    /// ensure that no `mut` references to the vector exist. In other words,
    /// don't nest `with_storage` calls. Destructors and `from_heap` methods
    /// may be presumed not to nest. Avoid `to_heap`.
    ///
    unsafe fn with_storage<'v, 'b, R, F>(&'v self, f: F) -> R
        where F: FnOnce(&'b Vec<T::In>) -> R,
              'v: 'b
    {
        // TODO: in debug builds at least there should be a RefCell-like "lock" here,
        // asserting that no direct Rust references exist to anything in the GC heap.
        f(&*self.0.as_ptr())
    }

    /// Run the given closure, passing it a `mut` reference to the heap-internal
    /// storage vector.
    ///
    /// # Safety
    ///
    /// Like `with_storage`, except the caller must ensure that no other
    /// references to the vector exist. It still amounts to: don't nest.
    unsafe fn with_storage_mut<'v, 'b, R, F>(&'v self, f: F) -> R
        where F: FnOnce(&'b mut Vec<T::In>) -> R,
              'v: 'b
    {
        // TODO should assert here, like unsafe_deref
        f(&mut *self.0.as_mut_ptr())
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
        unsafe {
            self.with_storage(|v| IntoHeap::from_heap(&v[index]))
        }
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
        unsafe {
            self.with_storage_mut(|v| v[index] = u);
        }
    }

    pub fn capacity(&self) -> usize {
        unsafe {
            self.with_storage(|v| v.capacity())
        }
    }

    pub fn reserve(&self, additional: usize) {
        unsafe {
            self.with_storage_mut(|v| v.reserve(additional));
        }
    }

    pub fn reserve_exact(&self, additional: usize) {
        unsafe {
            self.with_storage_mut(|v| v.reserve_exact(additional));
        }
    }

    pub fn shrink_to_fit(&self) {
        unsafe {
            self.with_storage_mut(|v| v.shrink_to_fit());
        }
    }

    pub fn truncate(&self, len: usize) {
        unsafe {
            self.with_storage_mut(|v| v.truncate(len));
        }
    }

    pub fn swap_remove(&self, index: usize) -> T {
        unsafe {
            let u = self.with_storage_mut(|v| v.swap_remove(index));
            IntoHeap::from_heap(&u)
        }
    }

    pub fn insert(&self, index: usize, element: T) {
        let u = element.into_heap();
        unsafe {
            self.with_storage_mut(|v| v.insert(index, u));
        }
    }

    pub fn remove(&self, index: usize) -> T {
        unsafe {
            let u = self.with_storage_mut(|v| v.remove(index));
            IntoHeap::from_heap(&u)
        }
    }

    pub fn push(&self, value: T) {
        let u = value.into_heap();
        unsafe {
            self.with_storage_mut(|v| v.push(u));
        }
    }

    pub fn pop(&self) -> Option<T> {
        unsafe {
            self.with_storage_mut(|v| v.pop().map(|u| IntoHeap::from_heap(&u)))
        }
    }

    pub fn append(&self, other: &VecRef<'h, T>) {
        let mut tmp: Vec<T::In> = vec![];
        unsafe {
            other.with_storage_mut(|src| mem::swap(src, &mut tmp));
            self.with_storage_mut(|dest| dest.append(&mut tmp));
        }
    }

    pub fn clear(&self) {
        unsafe {
            self.with_storage_mut(|v| v.clear());
        }
    }

    pub fn len(&self) -> usize {
        unsafe {
            self.with_storage(|v| v.len())
        }
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
        unsafe {
            self.with_storage(|v| v.first().map(|u| IntoHeap::from_heap(u)))
        }
    }

    /// Get the last element of the vector, or `None` if the vector is empty.
    ///
    /// Like `first()`, this clones the value.
    pub fn last(&self) -> Option<T> {
        unsafe {
            self.with_storage(|v| v.last().map(|u| IntoHeap::from_heap(u)))
        }
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
        unsafe {
            self.with_storage_mut(|v| mem::swap(&mut tmp, v));
        }
        tmp.sort_by(|ru1, ru2| {
            let t1 = unsafe { IntoHeap::from_heap(ru1) };
            let t2 = unsafe { IntoHeap::from_heap(ru2) };
            compare(&t1, &t2)
        });
        unsafe {
            self.with_storage_mut(|v| mem::swap(&mut tmp, v));
        }
    }
}
