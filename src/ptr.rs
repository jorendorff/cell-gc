//! Non-pinning pointers into the GC heap.

use pages::{PAGE_ALIGN, TypedPage};
use std::fmt;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::mem;

/// A pointer to some `T` in the GC heap.
///
/// # Safety
///
/// A `Pointer<T>` is a small step up in safety from a raw pointer. A valid
/// `Pointer<T>` instance is a guarantee that:
///
/// * The pointer points into the GC heap, within a GC-allocated page, and not
/// on top of the `PageHeader`.
///
/// * The pointer has the alignment required by the type and GC.
///
/// Note that `Pointer<T>` does **not** pin its referent like `GcRef<T>`
/// does. Therefore, a `Pointer<T>` can still easily be made to dangle or point
/// at uninitialized memory! The `Pointer<T>` type is not for general use (use
/// `GcRef<T>` for that instead), only for GC internals. We have to make it
/// `pub` so that `#[derive(IntoHeap)]` can generate code that uses it, but no
/// one else should!
#[derive(PartialOrd, Ord)]
pub struct Pointer<T> {
    ptr: UntypedPointer,
    phantom: PhantomData<*const T>,
}

impl<'h, T> Hash for Pointer<T> {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.ptr.hash(state);
    }
}

impl<T> Pointer<T> {
    /// Construct a new `Pointer<T>` from a raw `*const T`.
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
    pub unsafe fn new(ptr: *const T) -> Pointer<T> {
        assert_eq!(
            ptr as usize & (mem::align_of::<T>() - 1),
            0,
            "heap pointers respect T's alignment"
        );
        assert!(
            {
                let ptr = ptr as usize;
                let page = ptr & !(PAGE_ALIGN - 1);
                let allocs = page + TypedPage::<T>::first_allocation_offset();
                ptr >= allocs
            },
            "heap pointers shouldn't clobber the PageHeader"
        );
        Pointer {
            ptr: UntypedPointer::new(ptr as *const ()),
            phantom: PhantomData,
        }
    }

    /// Get a reference to the pointed-to `T` instance.
    ///
    /// # Safety
    ///
    /// If a GC happens and reclaims the referent while the returned reference
    /// is in use, it will result in use-after-free.
    ///
    /// If this pointer doesn't point at a valid `T` instance, then all hell
    /// will break loose.
    #[inline]
    pub unsafe fn as_ref(&self) -> &T {
        assert!(!self.ptr.0.is_null());
        &*self.as_raw()
    }

    /// Get the underlying raw pointer.
    #[inline]
    pub fn as_raw(&self) -> *const T {
        self.ptr.as_void() as *const T
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

impl<T> fmt::Debug for Pointer<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Pointer {{ {:p} }}", self.ptr.as_void())
    }
}

impl<T> Clone for Pointer<T> {
    fn clone(&self) -> Pointer<T> {
        *self
    }
}

impl<T> Copy for Pointer<T> {}

impl<T> PartialEq for Pointer<T> {
    fn eq(&self, other: &Pointer<T>) -> bool {
        self.ptr == other.ptr
    }
}

impl<T> Eq for Pointer<T> {}

impl<T> From<Pointer<T>> for UntypedPointer {
    fn from(p: Pointer<T>) -> UntypedPointer {
        p.ptr
    }
}

impl<T> From<Pointer<T>> for usize {
    fn from(p: Pointer<T>) -> usize {
        p.ptr.as_void() as usize
    }
}

/// Similar to `Pointer<T>` but provides no guarantees other than that it points
/// to some allocation inside the GC heap, and has the minimal GC-required
/// alignment.
///
/// # Safety
///
/// See `Pointer<T>`.
///
// TODO: The pointer should probably be wrapped in `Option<Shared<...>>` once
// `Shared` and `NonZero` are stabilized.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct UntypedPointer(*const ());

impl UntypedPointer {
    #[inline]
    unsafe fn new(ptr: *const ()) -> UntypedPointer {
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

    /// Convert this `UntypedPointer` into a `Pointer<T>`.
    ///
    /// # Safety
    ///
    /// You had better be damn sure that this pointer points at a `T`
    /// instance. See also the `Pointer<T>` safety rules and its `new` method's
    /// safety rules.
    #[inline]
    pub unsafe fn as_typed_ptr<T>(&self) -> Pointer<T> {
        Pointer::new(self.0 as *const T)
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
