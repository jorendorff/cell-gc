use gcref::GCRef;

/// `InHeap` types can be stored directly in the GC heap.
///
/// Application code does not need to use this trait. It is only public so that
/// the public macros can use it. Use the friendly macros!
///
/// GC types come in pairs: an `InHeap` type, which is stored physically inside
/// the heap, and may be packed with pointer fields and unsafe methods; and an
/// `IntoHeap` type, which is safe, is used in application code, and lives on
/// the stack.
///
/// Users never get a direct `&` reference to any value stored in the heap. All
/// access is through safe `IntoHeap` types, like `GCRef`.
///
/// *   For primitive types, like `i32`, the two types are the same.
///
/// *   `GCRef<'a, T>` is an `IntoHeap` type; the corresponding `InHeap` type
///     is the pointer type `*mut T`.
///
/// *   For macro-generated structs and enums, the user specifies the names of
///     both types. Both types have identical fields or variants, but fields of
///     the `InHeap` type are `InHeap`, and fields of the `IntoHeap` type are
///     `IntoHeap`.
///
/// Unsafe to implement: `from_heap` receives a non-mut reference to a heap
/// value. There may exist gc-references to that value, in which case
/// `from_heap` (or other code it calls) could *without using any unsafe code*
/// modify the value while this direct, non-mut reference exists. This breaks
/// Rust's aliasing rules and could cause crashes due to changing enums, if
/// nothing else.
///
pub unsafe trait InHeap<'a>: Sized {
    type Out: IntoHeap<'a, In=Self>;

    unsafe fn mark(ptr: *mut Self);

    /// Extract the value from the heap. This turns any raw pointers in the
    /// `InHeap` value into safe references, so while it's an unsafe function,
    /// the result of a correct call can be safely handed out to user code.
    ///
    /// Unsafe to call: It is impossible for ordinary users to call this
    /// safely, because `self` must be a direct, unwrapped reference to a value
    /// stored in the GC heap, which ordinary users cannot obtain.
    ///
    unsafe fn from_heap(&self) -> Self::Out;
}

/// Non-inlined function that serves as an entry point to marking. This is used
/// for marking root set entries.
pub unsafe fn mark_entry_point<'a, T: InHeap<'a>>(addr: *mut ()) {
    InHeap::mark(addr as *mut T);
}

/// `IntoHeap` types live on the stack, in application code. They are the
/// safe, user-friendly analogues to pointer-filled `InHeap` types. When you
/// use `gc_ref_type!` to declare a new GC struct or enum, `IntoHeap` types are
/// the types you use for fields.
///
/// Application code does not need to use this trait. It is only public so that
/// the public macros can use it. Use the friendly macros!
///
/// "Safe to use" means they don't expose pointers or references to GC memory;
/// and they obey Rust's safety and aliasing rules. If an `IntoHeap` value
/// contains a pointer to a GC allocation, then that allocation (and everything
/// reachable from it) is protected from GC.
///
/// "Stored in a field of a GC struct" - This includes primitive types like
/// `i32`, but also GC structs and enums (they can nest).
///
/// Unsafe to implement: `InHeap` objects are full of pointers; if `into_heap`
/// puts garbage into them, GC will crash.
///
pub unsafe trait IntoHeap<'a>: Sized {
    /// The type of the value when it is physically stored in the heap.
    type In: InHeap<'a, Out=Self>;

    /// Convert the value to the form it should have in the heap.
    /// This is for macro-generated code to call.
    fn into_heap(self) -> Self::In;
}

pub fn heap_type_id<'a, T: InHeap<'a>>() -> usize {
    mark_entry_point::<T> as *const () as usize
}

/// Relate an `IntoHeap` type to the corresponding safe reference type.
///
/// Application code does not need to use this trait. It is only public so that
/// the public macros can use it. Use the friendly macros!
///
pub trait IntoHeapAllocation<'a>: IntoHeap<'a>
{
    type Ref: IntoHeap<'a>;

    fn wrap_gcref(gcref: GCRef<'a, Self::In>) -> Self::Ref;
}


// === Provided implmentations for primitive types

macro_rules! gc_trivial_impl {
    ($t:ty) => {
        unsafe impl<'a> InHeap<'a> for $t {
            type Out = $t;
            unsafe fn mark(_ptr: *mut $t) {}
            unsafe fn from_heap(&self) -> $t { self.clone() }
        }

        unsafe impl<'a> IntoHeap<'a> for $t {
            type In = $t;
            fn into_heap(self) -> $t { self }
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
unsafe impl<'a, T: Clone + 'static> IntoHeap<'a> for T { ...trivial... }
*/
