use refs::PinnedRef;

/// Trait implemented by all types that can be stored directly in the GC heap:
/// the `Storage` types associated with any `HeapInline` or `HeapRef` type.
///
/// XXX maybe this should not be its own trait - fold into HeapInline?
///
pub unsafe trait Mark<'a> {
    unsafe fn mark(ptr: *mut Self);
}

/// Non-inlined function that serves as an entry point to marking. This is used
/// for marking root set entries.
pub unsafe fn mark_entry_point<'a, T: Mark<'a>>(addr: *mut ()) {
    Mark::mark(addr as *mut T);
}

/// Trait implemented by all types that can be stored in fields of structs (or,
/// eventually, elements of GCVecs) that are stored in the GC heap.
///
/// This trait is unsafe to implement for several reasons, ranging from:
///
/// *   The common-sense: API users aren't supposed to know or care about this.
///     It is only public so that the public macros can see it.
///     Use `gc_ref_type!` and `gc_inline_enum!`.
///
/// *   To the obvious: `Storage` objects are full of pointers and if `to_heap`
///     puts garbage into them, GC will crash.
///
/// *   To the subtle: `from_heap` receives a non-mut reference to a heap
///     value. But there may exist gc-references to that value, in which case
///     `from_heap` (or other code it calls) could modify the value while this
///     direct, non-mut reference exists, which could lead to crashes (due to
///     changing enums if nothing else) - all without using any unsafe code.
///
pub unsafe trait HeapInline<'a> {
    /// The type of the value when it is physically stored in the heap.
    type Storage;

    /// Extract the value from the heap. Do not under any circumstances call
    /// this.  It is for macro-generated code to call; it is impossible for
    /// ordinary users to call this safely, because `ptr` must be a direct,
    /// unwrapped reference to a value stored in the GC heap, which ordinary
    /// users cannot obtain.
    ///
    /// This turns any raw pointers in the `Storage` value into safe
    /// references, so while it's a dangerous function, the result of a correct
    /// call can be safely handed out to user code.
    ///
    unsafe fn from_heap(ptr: &Self::Storage) -> Self;

    /// Convert the value to the form it should have in the heap.
    /// This is for macro-generated code to call.
    fn to_heap(self) -> Self::Storage;
}

/// Things that can be allocated in the heap (the backing store for a GCRef type).
pub trait GCThing<'a>: Mark<'a> + Sized {
    type RefType: GCRef<'a, ReferentStorage=Self>;
}

pub fn gcthing_type_id<'a, T: GCThing<'a>>() -> usize {
    mark_entry_point::<T> as *const () as usize
}

pub trait GCRef<'a>: HeapInline<'a> {
    type ReferentStorage: GCThing<'a>;
    type Fields;

    fn from_pinned_ref(r: PinnedRef<'a, Self::ReferentStorage>) -> Self;

    fn fields_to_heap(fields: Self::Fields) -> Self::ReferentStorage;

    #[cfg(test)]
    fn address(&self) -> usize;
}

macro_rules! gc_trivial_impl {
    ($t:ty) => {
        unsafe impl<'a> Mark<'a> for $t {
            unsafe fn mark(_ptr: *mut $t) {}
        }

        unsafe impl<'a> HeapInline<'a> for $t {
            type Storage = Self;
            fn to_heap(self) -> $t { self }
            unsafe fn from_heap(v: &$t) -> $t { (*v).clone() }
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
unsafe impl<'a, T: Clone + 'static> Mark<'a> for T { ...trivial... }
unsafe impl<'a, T: Clone + 'static> HeapInline<'a> for T { ...trivial... }
*/
