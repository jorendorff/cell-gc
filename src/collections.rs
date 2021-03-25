//! Collections for use with GC references.

use gc_ref::GcRef;
use heap::{NoGc, post_write_barrier};
use ptr::Pointer;
use std::cmp::Ordering;
use std::hash::{Hash, Hasher};
use std::mem;
use std::ops::Range;
use traits::{InHeap, IntoHeap, IntoHeapAllocation, IntoHeapBase, Tracer};

impl<U: InHeap> InHeap for Vec<U> {
    unsafe fn trace<R: Tracer>(&self, tracer: &mut R) {
        for r in self {
            r.trace(tracer);
        }
    }
}

impl<T: IntoHeapBase> IntoHeapBase for Vec<T> {
    type In = Vec<T::In>;

    fn into_heap(self) -> Vec<T::In> {
        self.into_iter().map(|x| x.into_heap()).collect()
    }

    unsafe fn from_heap(storage: &Vec<T::In>) -> Vec<T> {
        storage.iter().map(|x| T::from_heap(x)).collect()
    }
}

unsafe impl<'h, T: IntoHeap<'h>> IntoHeap<'h> for Vec<T> {}

impl<'h, T: IntoHeap<'h>> IntoHeapAllocation<'h> for Vec<T> {
    type Ref = VecRef<'h, T>;

    fn wrap_gc_ref(gc_ref: GcRef<'h, Vec<T>>) -> VecRef<'h, T> {
        VecRef(gc_ref)
    }

    fn into_gc_ref(wrapped_ref: VecRef<'h, T>) -> GcRef<'h, Vec<T>> {
        wrapped_ref.0
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

impl<'h, T: IntoHeap<'h>> Hash for VecRef<'h, T> {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}


impl<'h, T: IntoHeap<'h>> IntoHeapBase for VecRef<'h, T> {
    type In = Pointer<Vec<T::In>>;

    fn into_heap(self) -> Pointer<Vec<T::In>> {
        self.0.ptr()
    }

    unsafe fn from_heap(storage: &Pointer<Vec<T::In>>) -> VecRef<'h, T> {
        VecRef(GcRef::new(*storage))
    }
}

unsafe impl<'h, T: IntoHeap<'h>> IntoHeap<'h> for VecRef<'h, T> {}

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
    where
        F: FnOnce(&'b Vec<T::In>) -> R,
        'v: 'b,
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
    ///
    /// Additionally, it is the responsibility of the caller to ensure that
    /// generational post-write barriers are invoked on the the storage's
    /// elements, if they are modified.
    unsafe fn with_storage_mut<'v, 'b, R, F>(&'v self, f: F) -> R
    where
        F: FnOnce(&'b mut Vec<T::In>) -> R,
        'v: 'b,
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
        unsafe { self.with_storage(|v| T::from_heap(&v[index])) }
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
        unsafe { <Vec<T>>::from_heap(&*self.0.as_ptr()) }
    }

    /// Set element `index` of the vector to `value`.
    ///
    /// # Panics
    ///
    /// Panics if `index` is out of bounds.
    pub fn set(&self, index: usize, value: T) {
        let u = value.into_heap();
        unsafe {
            self.with_storage_mut(|v| {
                v[index] = u;
                // We can't allow this post-barrier to trigger a nursery
                // collection because it might re-enter this `with_storage`.
                post_write_barrier::<Vec<T>, T, NoGc>(&self.0.ptr(), &v[index]);
            })
        };
    }

    /// The capacity of this vector.
    pub fn capacity(&self) -> usize {
        unsafe { self.with_storage(|v| v.capacity()) }
    }

    /// Reserve at least enough space for `additional` more elements in this
    /// vector.
    pub fn reserve(&self, additional: usize) {
        unsafe {
            self.with_storage_mut(|v| v.reserve(additional));
        }
    }

    /// Reserve space for exactly `additional` more elements in this vector.
    pub fn reserve_exact(&self, additional: usize) {
        unsafe {
            self.with_storage_mut(|v| v.reserve_exact(additional));
        }
    }

    /// Reclaim any excess storage that has been reserved.
    pub fn shrink_to_fit(&self) {
        unsafe {
            self.with_storage_mut(|v| v.shrink_to_fit());
        }
    }

    /// Truncate this vector to only `len` elements, dropping any others.
    pub fn truncate(&self, len: usize) {
        unsafe {
            self.with_storage_mut(|v| v.truncate(len));
        }
    }

    /// Removes an element from the vector and returns it.
    ///
    /// The removed element is replaced by the last element of the vector.
    ///
    /// This does not preserve ordering, but is O(1).
    pub fn swap_remove(&self, index: usize) -> T {
        unsafe {
            let u = self.with_storage_mut(|v| v.swap_remove(index));
            T::from_heap(&u)
        }
    }

    /// Inserts an element at position index within the vector, shifting all elements after it to the right.
    ///
    /// ### Panics
    ///
    /// Panics if `index` is out of bounds.
    pub fn insert(&self, index: usize, element: T) {
        let u = element.into_heap();
        unsafe {
            self.with_storage_mut(|v| {
                v.insert(index, u);
                // We can't allow this post-barrier to trigger a nursery
                // collection because it might re-enter this `with_storage`.
                post_write_barrier::<Vec<T>, T, NoGc>(&self.0.ptr(), &v[index]);
            });
        }
    }

    /// Removes and returns the element at position index within the vector,
    /// shifting all elements after it to the left.
    ///
    /// ### Panics
    ///
    /// Panics if `index` is out of bounds.
    pub fn remove(&self, index: usize) -> T {
        unsafe {
            let u = self.with_storage_mut(|v| v.remove(index));
            T::from_heap(&u)
        }
    }

    /// Appends an element to the back of a collection.
    ///
    /// ### Panics
    ///
    /// Panics if the number of elements in the vector overflows a `usize`.
    pub fn push(&self, value: T) {
        let u = value.into_heap();
        unsafe {
            self.with_storage_mut(|v| {
                v.push(u);
                // We can't allow this post-barrier to trigger a nursery
                // collection because it might re-enter this `with_storage`.
                post_write_barrier::<Vec<T>, T, NoGc>(&self.0.ptr(), v.last().unwrap());
            });
        }
    }

    /// Removes the last element from a vector and returns it, or `None` if it
    /// is empty.
    pub fn pop(&self) -> Option<T> {
        unsafe { self.with_storage_mut(|v| v.pop().map(|u| T::from_heap(&u))) }
    }

    /// Moves all the elements of other into Self, leaving other empty.
    ///
    /// ### Panics
    ///
    /// Panics if the number of elements in the vector overflows a `usize`.
    pub fn append(&self, other: &VecRef<'h, T>) {
        let mut tmp: Vec<T::In> = vec![];
        unsafe {
            other.with_storage_mut(|src| mem::swap(src, &mut tmp));
            self.with_storage_mut(|dest| {
                let n = dest.len();
                dest.append(&mut tmp);
                for x in dest.iter().skip(n) {
                    // We can't allow this post-barrier to trigger a nursery
                    // collection because it might re-enter this `with_storage`.
                    post_write_barrier::<Vec<T>, T, NoGc>(&self.0.ptr(), x);
                }
            });
        }
    }

    /// Clears the vector, removing all values.
    ///
    /// Note that this method has no effect on the allocated capacity of the
    /// vector.
    pub fn clear(&self) {
        unsafe {
            self.with_storage_mut(|v| v.clear());
        }
    }

    /// Returns the number of elements in the vector, also referred to as its
    /// 'length'.
    pub fn len(&self) -> usize {
        unsafe { self.with_storage(|v| v.len()) }
    }

    /// Returns `true` if the vector contains no elements.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Get the first element of the vector, or `None` if the vector is empty.
    ///
    /// The standard `Vec<T>::first` returns an `&T` reference into the vector,
    /// but cell-gc never hands out Rust references to heap values. Instead
    /// this returns a clone of the value.
    pub fn first(&self) -> Option<T> {
        unsafe { self.with_storage(|v| v.first().map(|u| T::from_heap(u))) }
    }

    /// Get the last element of the vector, or `None` if the vector is empty.
    ///
    /// Like `first()`, this clones the value.
    pub fn last(&self) -> Option<T> {
        unsafe { self.with_storage(|v| v.last().map(|u| T::from_heap(u))) }
    }

    /// Sort the vector in place.
    pub fn sort(&self)
    where
        T: Ord,
    {
        self.sort_by(Ord::cmp);
    }

    /// Sorts the slice with a comparator function.
    ///
    /// This sort is stable (i.e. does not reorder equal elements) and O(n log
    /// n) worst-case.
    pub fn sort_by<F>(&self, mut compare: F)
    where
        F: FnMut(&T, &T) -> Ordering,
    {
        // Make the heap vector empty while we're sorting it.  This way, the
        // user can do whatever malicious nonsense they like to the heap in
        // `compare`, and our copy of the vector is unaffected.
        let mut tmp = vec![];
        unsafe {
            self.with_storage_mut(|v| mem::swap(&mut tmp, v));
        }
        tmp.sort_by(|ru1, ru2| {
            let t1 = unsafe { T::from_heap(ru1) };
            let t2 = unsafe { T::from_heap(ru2) };
            compare(&t1, &t2)
        });
        unsafe {
            self.with_storage_mut(|v| mem::swap(&mut tmp, v));
        }
    }
}

/// An iterator over a GC-heap-allocated vector.
pub struct VecRefIter<'h, T: IntoHeap<'h>> {
    indexes: Range<usize>,
    data: VecRef<'h, T>,
}

impl<'h, T: IntoHeap<'h>> IntoIterator for VecRef<'h, T> {
    type Item = T;
    type IntoIter = VecRefIter<'h, T>;

    fn into_iter(self) -> VecRefIter<'h, T> {
        VecRefIter {
            indexes: 0..self.len(),
            data: self
        }
    }
}

impl<'h, T: IntoHeap<'h>> Iterator for VecRefIter<'h, T> {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        self.indexes.next().map(|i| self.data.get(i))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.indexes.size_hint()
    }
}

impl<'h, T: IntoHeap<'h>> DoubleEndedIterator for VecRefIter<'h, T> {
    fn next_back(&mut self) -> Option<T> {
        self.indexes.next_back().map(|i| self.data.get(i))
    }
}
