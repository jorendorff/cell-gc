use gc_leaf::GcLeaf;
use heap::{GcHeap, HeapId, GcHeapSession, HeapSessionId};
use pages;
use poison;
use ptr::Pointer;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::mem;
use traits::{IntoHeapAllocation, IntoHeapBase};

/// A reference to something inside the GC heap, that is valid for the current
/// GC heap session.
pub struct GcRef<'h, T: IntoHeapAllocation<'h>> {
    heap_id: HeapSessionId<'h>,
    ptr: Pointer<T::In>, // never null
}

impl<'h, T: IntoHeapAllocation<'h>> GcRef<'h, T> {
    /// Pin an object, returning a new `GcRef` that will unpin it when
    /// dropped. Unsafe because if `p` is not a pointer to a live allocation of
    /// type `T::In` --- and a complete allocation, not a sub-object of one ---
    /// then later unsafe code will explode.
    pub unsafe fn new(p: Pointer<T::In>) -> GcRef<'h, T> {
        poison::assert_is_not_poisoned(p.as_raw());
        pages::pin(p);
        GcRef {
            ptr: p,
            heap_id: PhantomData,
        }
    }

    /// Get an untyped GC pointer to the referent.
    pub fn ptr(&self) -> Pointer<T::In> {
        unsafe {
            poison::assert_is_not_poisoned(self.ptr.as_raw());
        }

        self.ptr
    }

    /// Get a raw, untyped const pointer to the referent.
    pub fn as_ptr(&self) -> *const T::In {
        unsafe {
            poison::assert_is_not_poisoned(self.ptr.as_raw());
        }

        self.ptr.as_raw()
    }

    /// Get a raw, untyped mut pointer to the referent.
    pub fn as_mut_ptr(&self) -> *mut T::In {
        unsafe {
            poison::assert_is_not_poisoned(self.ptr.as_raw());
        }

        self.ptr.as_raw() as *mut T::In
    }

    /// Consume this reference and return it as an untyped GC pointer without
    /// unpinning its referent. The referent will be considered a GC root until
    /// manually unpinned.
    pub fn into_pinned_ptr(self) -> Pointer<T::In> {
        unsafe {
            poison::assert_is_not_poisoned(self.ptr.as_raw());
        }

        let ptr = self.ptr;
        mem::forget(self); // skip unpinning destructor
        ptr
    }
}

impl<'h, T: IntoHeapAllocation<'h>> Hash for GcRef<'h, T> {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.ptr.hash(state);
    }
}

impl<'h, T: Clone + Send + 'static> GcRef<'h, GcLeaf<T>> {
    /// Get this reference's referent.
    pub fn get(&self) -> T {
        unsafe {
            poison::assert_is_not_poisoned(self.ptr.as_raw());
        }

        // XXX TODO I think this `unsafe` block is sound, I'm not totally
        // sure. It's OK as long as we can't trigger GC in the middle of
        // cloning...  which would ordinarily be the case since the 'static
        // type T won't contain a reference to the GcHeapSession<'h>.
        //
        // But one can imagine some kind of wrapper type that could store a
        // GcHeapSession<'h> within it, then let you get it back out as an Option
        // if we're somehow sure the lifetime `'h` isn't expired yet.  If that
        // primitive is safe, then this is unsound, and we'd have to restrict
        // GcLeaf to Copy types (ewww!)
        T::clone(unsafe { self.ptr.as_ref() })
    }
}

impl<'h, T: IntoHeapAllocation<'h>> Drop for GcRef<'h, T> {
    fn drop(&mut self) {
        unsafe {
            poison::assert_is_not_poisoned(self.ptr.as_raw());
            pages::unpin(self.ptr);
        }
    }
}

impl<'h, T: IntoHeapAllocation<'h>> Clone for GcRef<'h, T> {
    fn clone(&self) -> GcRef<'h, T> {
        let &GcRef { ptr, heap_id } = self;
        unsafe {
            poison::assert_is_not_poisoned(ptr.as_raw());
            pages::pin(ptr);
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


/// References into the heap that survive across sessions. A `GcFrozenRef<T>`
/// can't access the `T` value it points to, but it keeps it alive so you can
/// access it again later.
///
/// Use `GcHeapSession::freeze()` to convert normal `GcRef` values to
/// `GcFrozenRef`. Use `GcHeapSession::thaw()` to convert the frozen reference
/// back to a `GcRef` later, either in the same session or a different one.
///
/// Dropping a `GcFrozenRef` is slightly inefficient, as the pointer is added
/// to an internal buffer. The assumption is that dropping frozen references
/// will be rare, as they're typically stored with the intention of thawing and
/// using them later.
///
/// `GcFrozenRef`s, like `GcHeap`s, are `Send` (they can be moved from one thread
/// to another).
pub struct GcFrozenRef<T: IntoHeapBase> {
    /// Identifies the heap into which this frozen ref points.
    ///
    /// Implementation note: This has to be an Option in order to support both
    /// thaw() and drop(), annoyingly.
    heap_id: Option<HeapId>,

    /// Actual pointer to the referent.
    ptr: Pointer<T::In>,
}

unsafe impl<T: IntoHeapBase> Send for GcFrozenRef<T> {}

impl<T: IntoHeapBase> GcFrozenRef<T> {
    pub(crate) fn new<'h>(session: &GcHeapSession<'h>, t: T::Ref) -> GcFrozenRef<T>
    where
        T: IntoHeapAllocation<'h>,
    {
        GcFrozenRef {
            heap_id: Some(session.heap_id()),
            ptr: T::into_gc_ref(t).into_pinned_ptr(),
        }
    }

    /// Convert back to a GcRef.
    ///
    /// # Panics
    ///
    /// Panics if `**heap` is not the heap where this reference was frozen.
    pub(crate) fn thaw<'h>(mut self, session: &GcHeapSession<'h>) -> GcRef<'h, T>
    where
        T: IntoHeapAllocation<'h>,
    {
        session.check_heap_id(self.heap_id.take().unwrap());
        GcRef {
            heap_id: PhantomData,
            ptr: self.ptr,
        }
    }
}

impl<T: IntoHeapBase> Drop for GcFrozenRef<T> {
    fn drop(&mut self) {
        if let Some(heap_id) = self.heap_id.take() {
            GcHeap::drop_frozen_ptr(heap_id, self.ptr.into());
        }
    }
}
