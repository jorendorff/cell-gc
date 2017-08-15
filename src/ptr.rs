//! Non-pinning pointers into the GC heap.

use pages::{PAGE_ALIGN, TypedPage};
use std::fmt;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::mem;
use traits::InHeap;

/// A pointer to some `U` in the GC heap.
///
/// # Safety
///
/// A `Pointer<U>` is a small step up in safety from a raw pointer. A valid
/// `Pointer<U>` instance is a guarantee that:
///
/// * The pointer points into the GC heap, within a GC-allocated page, and not
/// on top of the `PageHeader`.
///
/// * The pointer has the alignment required by the type and GC.
///
/// Note that `Pointer<U>` does **not** pin its referent like `GcRef<T>`
/// does. Therefore, a `Pointer<U>` can still easily be made to dangle or point
/// at uninitialized memory! The `Pointer<U>` type is not for general use (use
/// `GcRef<T>` for that instead), only for GC internals. We have to make it
/// `pub` so that `#[derive(IntoHeap)]` can generate code that uses it, but no
/// one else should!
#[derive(PartialOrd, Ord)]
pub struct Pointer<U: InHeap> {
    ptr: UntypedPointer,
    phantom: PhantomData<*const U>,
}

impl<U: InHeap> Hash for Pointer<U> {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.ptr.hash(state);
    }
}

impl<U: InHeap> Pointer<U> {
    /// Construct a new `Pointer<U>` from a raw `*const U`.
    ///
    /// # Safety
    ///
    /// It is the responsibility of callers to ensure that the guarantees
    /// mentioned above hold true.
    ///
    /// # Panics
    ///
    /// Panics if the pointer is not aligned properly, or if it would clobber
    /// the GC's internal `PageHeader`.
    #[inline]
    pub unsafe fn new(ptr: *const U) -> Pointer<U> {
        assert_eq!(
            ptr as usize & (mem::align_of::<U>() - 1),
            0,
            "heap pointers respect U's alignment"
        );
        assert!(
            {
                let ptr = ptr as usize;
                let page = ptr & !(PAGE_ALIGN - 1);
                let allocs = page + TypedPage::<U>::first_allocation_offset();
                ptr >= allocs
            },
            "heap pointers shouldn't clobber the PageHeader"
        );
        Pointer {
            ptr: UntypedPointer::new(ptr as *const ()),
            phantom: PhantomData,
        }
    }

    /// Get a reference to the pointed-to `U` instance.
    ///
    /// # Safety
    ///
    /// If a GC happens and reclaims the referent while the returned reference
    /// is in use, it will result in use-after-free.
    ///
    /// If this pointer doesn't point at a valid `U` instance, then all hell
    /// will break loose.
    #[inline]
    pub unsafe fn as_ref(&self) -> &U {
        assert!(!self.ptr.0.is_null());
        &*self.as_raw()
    }

    /// Get the underlying raw pointer.
    #[inline]
    pub fn as_raw(&self) -> *const U {
        self.ptr.as_void() as *const U
    }

    /// Get the underlying raw, mutable pointer.
    #[inline]
    pub fn as_mut(&self) -> *mut U {
        self.ptr.as_void() as *mut U
    }

    /// Get the underlying raw pointer as a `*const ()`.
    #[inline]
    pub fn as_void(&self) -> *const () {
        self.ptr.as_void()
    }

    /// Get the underlying raw pointer as a `usize`.
    #[inline]
    pub fn as_usize(&self) -> usize {
        self.ptr.as_usize()
    }
}

impl<U: InHeap> fmt::Debug for Pointer<U> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Pointer {{ {:p} }}", self.ptr.as_void())
    }
}

impl<U: InHeap> Clone for Pointer<U> {
    fn clone(&self) -> Pointer<U> {
        *self
    }
}

impl<U: InHeap> Copy for Pointer<U> {}

impl<U: InHeap> PartialEq for Pointer<U> {
    fn eq(&self, other: &Pointer<U>) -> bool {
        self.ptr == other.ptr
    }
}

impl<U: InHeap> Eq for Pointer<U> {}

impl<U: InHeap> From<Pointer<U>> for UntypedPointer {
    fn from(p: Pointer<U>) -> UntypedPointer {
        p.ptr
    }
}

impl<U: InHeap> From<Pointer<U>> for usize {
    fn from(p: Pointer<U>) -> usize {
        p.ptr.as_void() as usize
    }
}

/// Similar to `Pointer<U>` but provides no guarantees other than that it points
/// to some allocation inside the GC heap, and has the minimal GC-required
/// alignment.
///
/// # Safety
///
/// See `Pointer<U>`.
///
// TODO: The pointer should probably be wrapped in `Option<Shared<...>>` once
// `Shared` and `NonZero` are stabilized.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct UntypedPointer(*const ());

impl UntypedPointer {
    /// Construct a new untyped pointer into the GC heap.
    ///
    /// # Safety
    ///
    /// See the type's documentation, as well as the documentation for
    /// `Pointer<U>`.
    #[inline]
    pub unsafe fn new(ptr: *const ()) -> UntypedPointer {
        assert!(!ptr.is_null(), "GC heap pointers can't be null.");
        assert_eq!(
            ptr as usize & (mem::size_of::<usize>() - 1),
            0,
            "All GC heap pointers are at least word-aligned"
        );
        assert!(
            {
                let ptr = ptr as usize;
                let page = ptr & !(PAGE_ALIGN - 1);
                let allocs = page + TypedPage::<usize>::first_allocation_offset();
                ptr >= allocs
            },
            "heap pointers shouldn't clobber the PageHeader"
        );
        UntypedPointer(ptr)
    }

    /// Convert this `UntypedPointer` into a `Pointer<U>`.
    ///
    /// # Safety
    ///
    /// You had better be damn sure that this pointer points at a `U`
    /// instance. See also the `Pointer<U>` safety rules and its `new` method's
    /// safety rules.
    #[inline]
    pub unsafe fn as_typed_ptr<U: InHeap>(&self) -> Pointer<U> {
        Pointer::new(self.0 as *const U)
    }

    /// Get the underlying raw pointer.
    #[inline]
    pub fn as_void(&self) -> *const () {
        self.0
    }

    /// Get the underlying raw pointer as a `usize`.
    #[inline]
    pub fn as_usize(&self) -> usize {
        self.0 as usize
    }
}
