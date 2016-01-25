use refs::PinnedRef;

/// Trait implemented by all types that can be stored directly in the GC heap:
/// the `Storage` types associated with any `ToHeap` type.
///
pub unsafe trait InHeap<'a>: Sized {
    type Out: ToHeap<'a>;

    unsafe fn mark(ptr: *mut Self);

    /// Extract the value from the heap. This is for macro-generated code to
    /// call; it is impossible for ordinary users to call this safely, because
    /// `self` must be a direct, unwrapped reference to a value stored in the
    /// GC heap, which ordinary users cannot obtain.
    ///
    /// This turns any raw pointers in the `Storage` value into safe
    /// references, so while it's a dangerous function, the result of a correct
    /// call can be safely handed out to user code.
    ///
    unsafe fn from_heap(&self) -> Self::Out;
}

/// Non-inlined function that serves as an entry point to marking. This is used
/// for marking root set entries.
pub unsafe fn mark_entry_point<'a, T: InHeap<'a>>(addr: *mut ()) {
    InHeap::mark(addr as *mut T);
}

/// `ToHeap` types are safe to use and can be stored in a field of a GC struct.
///
/// "Safe to use" means they don't expose raw pointers to GC memory, and they
/// obey Rust's safety and aliasing rules. If a `ToHeap` value contains a
/// pointer to a GC allocation, then that allocation (and everything reachable
/// from it) is protected from GC.
///
/// "Stored in a field of a GC struct" - This includes primitive types like `i32`,
/// but also GC structs themselves (because they can be nested).
///
/// This trait is unsafe to implement for several reasons, ranging from:
///
/// *   The common-sense: API users aren't supposed to know or care about this.
///     It is only public so that the public macros can see it. Use the macros.
///
/// *   ...To the obvious: `Storage` objects are full of pointers and if
///     `to_heap` puts garbage into them, GC will crash.
///
/// *   ...To the subtle: `from_heap` receives a non-mut reference to a heap
///     value. But there may exist gc-references to that value, in which case
///     `from_heap` (or other code it calls) could modify the value while this
///     direct, non-mut reference exists, which could lead to crashes (due to
///     changing enums if nothing else) - all without using any unsafe code.
///
pub unsafe trait ToHeap<'a>: Sized {
    /// The type of the value when it is physically stored in the heap.
    type Storage: InHeap<'a, Out=Self>;

    /// Convert the value to the form it should have in the heap.
    /// This is for macro-generated code to call.
    fn to_heap(self) -> Self::Storage;
}

pub fn heap_type_id<'a, T: InHeap<'a>>() -> usize {
    mark_entry_point::<T> as *const () as usize
}

pub trait GCRef<'a>: ToHeap<'a> {
    type Target: ToHeap<'a, Storage=Self::TargetStorage>;
    type TargetStorage: InHeap<'a, Out=Self::Target>;

    fn from_pinned_ref(r: PinnedRef<'a, Self::TargetStorage>) -> Self;

    #[cfg(test)]
    fn address(&self) -> usize;
}

macro_rules! gc_trivial_impl {
    ($t:ty) => {
        unsafe impl<'a> InHeap<'a> for $t {
            type Out = $t;
            unsafe fn mark(_ptr: *mut $t) {}
            unsafe fn from_heap(&self) -> $t { self.clone() }
        }

        unsafe impl<'a> ToHeap<'a> for $t {
            type Storage = $t;
            fn to_heap(self) -> $t { self }
        }
    }
}

gc_trivial_impl!(bool);
gc_trivial_impl!(char);
gc_trivial_impl!(i8);
gc_trivial_impl!(u8);
gc_trivial_impl!(i16);
gc_trivial_impl!(u16);
gc_trivial_impl!(i32);
gc_trivial_impl!(u32);
gc_trivial_impl!(i64);
gc_trivial_impl!(u64);
gc_trivial_impl!(isize);
gc_trivial_impl!(usize);
gc_trivial_impl!(f32);
gc_trivial_impl!(f64);

use std::rc::Rc;
gc_trivial_impl!(Rc<String>);

/*
// 'static types are heap-safe because ref types are never 'static.
// Unfortunately I can't make the compiler understand this: the rules
// to prevent conflicting trait impls make this conflict with almost
// everything.
unsafe impl<'a, T: Clone + 'static> InHeap<'a> for T { ...trivial... }
unsafe impl<'a, T: Clone + 'static> ToHeap<'a> for T { ...trivial... }
*/
