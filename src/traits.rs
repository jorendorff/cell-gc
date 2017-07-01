//! The traits defined here are implementation details of cell_gc.
//!
//! Application code does not need to use these traits. They are public only so
//! that `#[derive(IntoHeap)]` can use it. Use the friendly macro!

use gcref::GcRef;
use ptr::Pointer;

/// Base trait for values that can be moved into a GC heap.
pub trait IntoHeapBase: Sized {
    /// The type of the value when it is physically stored in the heap.
    ///
    /// GC types come in pairs: an `IntoHeap` type and an `In` type. Both
    /// types have the same layout, bit for bit.
    ///
    /// The `In` type is stored physically inside the heap, and may be packed
    /// with pointer fields and unsafe methods. The `IntoHeap` type is safe, is
    /// used in application code, and lives on the stack.
    ///
    /// *   For primitive types, like `i32`, the two types are the same.
    ///
    /// *   Macro-generated "Ref" types are `IntoHeap` types; the corresponding
    ///     in-heap types are effectively raw pointers.
    ///
    /// *   For user-defined structs and enums, the `#[derive(IntoHeap)]` macro
    ///     autogenerates an `In` type for you. It has identical fields or
    ///     variants, but fields of the `IntoHeap` type are `IntoHeap`, and
    ///     fields of the `In` type are the corresponding `In` types.
    ///
    /// (The word "safe" above means that, as with everything generally in Rust, you
    /// can hack without fear. `IntoHeap` types don't expose raw Rust pointers or
    /// references to GC memory, they don't expose the unsafe in-heap types, and
    /// they obey Rust's safety and aliasing rules.)
    ///
    /// Users never get a direct `&` reference to any in-heap value. All access is
    /// through safe `IntoHeap` types, like the `Ref` type that is automatically
    /// declared for you when you use `#[derive(IntoHeap)]` on a struct or union.
    type In;

    /// Convert the value to the form it should have in the heap.
    /// This is for cell-gc to call.
    ///
    /// This method must not be called while any direct Rust references to
    /// heap objects exist. (However, user code never runs while such
    /// references exist, so the method is not marked `unsafe`.)
    fn into_heap(self) -> Self::In;

    /// Extract the value from the heap. This turns any raw pointers in the
    /// in-heap value into safe references, so while it's an unsafe function,
    /// the result of a correct call can be safely handed out to user code.
    ///
    /// # Safety
    ///
    /// It is impossible for ordinary users to call this safely, because `self`
    /// must be a direct, unwrapped reference to a value stored in the GC heap,
    /// which ordinary users cannot obtain.
    unsafe fn from_heap(&Self::In) -> Self;

    /// Traverse a value in the heap.
    ///
    /// This calls `tracer.visit(ptr)` for each edge from this value
    /// to other heap values.
    ///
    /// For user-defined structs, this is achieved by calling the
    /// `field.trace(tracer)` method of each field of this value. (They are
    /// typically all `In` types.) The implementation of this method for
    /// `Pointer<T>` calls `tracer.visit()` (if the pointer is non-null?).
    ///
    /// # Safety
    ///
    /// It is impossible for ordinary users to call this safely, because `self`
    /// must be a direct, unwrapped reference to a value stored in the GC heap,
    /// which ordinary users cannot obtain.
    unsafe fn trace<R>(&Self::In, tracer: &mut R)
    where
        R: Tracer;
}

/// Trait for values that can be moved into a GC heap.
///
/// Cell-gc does not support GC allocation of arbitrary values: only values of
/// types that implement `IntoHeap`. This trait is **not** meant to be
/// implemented by hand: use `#[derive(IntoHeap)]` instead. All primitive types
/// and many standard types support `IntoHeap`.
///
/// That is all you need to know in order to use cell-gc. The rest is really
/// implementation detail.
///
/// Implementation note: every type with `'static` lifetime should be safe to
/// store in the heap, because ref types are never `'static`. Unfortunately
/// rustc cannot be made to understand this. It would be *legal* to write this:
///
/// ```ignore
/// unsafe impl<'h, T: Clone + 'static> IntoHeap<'h> for T { ...trivial... }
/// ```
///
/// but alas, Rust thinks this impl conflicts with almost all others, making it
/// impossible to get the macro to work. So instead we `impl IntoHeap for` a
/// lot of individual types by hand.
///
/// **Liveness.** Some `IntoHeap` types ("Ref" types) are effectively smart
/// pointers to in-heap values. To preserve safety, if an `IntoHeap` value is
/// (or contains) a smart pointer to an in-heap value, then that value (and
/// everything reachable from it) is protected from GC.
///
/// # Safety
///
/// Implementing `IntoHeap<'h>` tells cell-gc that a type integrates with
/// garbage collection. All the methods in the type's `IntoHeapBase` impl must
/// be correct or it will explode.
///
/// *   In-heap values are typically full of pointers; if `into_heap` puts
///     garbage into them, GC will crash.
///
/// *   `trace` must be implemented with care in order to preserve the
///     invariants of the GC graph-walking algorithm. Bugs there are very
///     likely to lead to dangling pointers and hard-to-debug crashes down the
///     road.
///
/// *   And this probably goes without saying, but none of these methods may
///     allocate or do anything else that could trigger garbage collection.
///
pub unsafe trait IntoHeap<'h>: IntoHeapBase {
}

/// Types that can be allocated in the heap.
pub trait IntoHeapAllocation<'h>: IntoHeap<'h> {
    /// The safe reference type that's returned when a value of this type is
    /// moved into the heap (allocated).
    type Ref: IntoHeap<'h>;

    fn wrap_gcref(gcref: GcRef<'h, Self>) -> Self::Ref;
}

pub trait Tracer {
    fn visit<'h, T>(&mut self, Pointer<T::In>)
    where
        T: IntoHeapAllocation<'h>;
}

// === Provided implmentations for primitive types

macro_rules! gc_trivial_impl {
    ($t:ty) => {
        impl IntoHeapBase for $t {
            type In = $t;
            fn into_heap(self) -> $t { self }
            unsafe fn from_heap(storage: &$t) -> $t { storage.clone() }
            unsafe fn trace<R>(_storage: &$t, _tracer: &mut R) where R: Tracer {}
        }
        unsafe impl<'h> IntoHeap<'h> for $t {}
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

gc_trivial_impl!(String);

macro_rules! gc_generic_trivial_impl {
    (@as_item $it:item) => { $it };
    ([$($x:tt)*] $t:ty) => {
        gc_generic_trivial_impl! {
            @as_item
            impl<$($x)*> IntoHeapBase for $t {
                type In = $t;
                fn into_heap(self) -> $t { self }
                unsafe fn from_heap(storage: &$t) -> $t { (*storage).clone() }
                unsafe fn trace<R>(_storage: &$t, _tracer: &mut R) where R: Tracer {}
            }
        }
        gc_generic_trivial_impl! {
            @as_item
            unsafe impl<'h, $($x)*> IntoHeap<'h> for $t {}
        }
    }
}

gc_generic_trivial_impl!([T: ?Sized] &'static T);
gc_generic_trivial_impl!([T: ?Sized] ::std::marker::PhantomData<T>);
gc_generic_trivial_impl!([T: Clone + 'static] ::GCLeaf<T>);
gc_generic_trivial_impl!([T: Clone + 'static] Box<T>);
gc_generic_trivial_impl!([T: Clone + 'static] ::std::rc::Rc<T>);

// Transitive implementations ("this particular kind of struct/enum is IntoHeap
// if all its fields are") are slightly less trivial.

impl<T: IntoHeapBase> IntoHeapBase for Option<T> {
    type In = Option<T::In>;

    fn into_heap(self) -> Option<T::In> {
        self.map(|t| t.into_heap())
    }

    unsafe fn trace<R>(storage: &Option<T::In>, tracer: &mut R)
    where
        R: Tracer,
    {
        match storage {
            &None => (),
            &Some(ref u) => T::trace(u, tracer),
        }
    }

    unsafe fn from_heap(storage: &Option<T::In>) -> Option<T> {
        match storage {
            &None => None,
            &Some(ref u) => Some(T::from_heap(u)),
        }
    }
}

unsafe impl<'h, T: IntoHeap<'h>> IntoHeap<'h> for Option<T> {}

macro_rules! gc_trivial_tuple_impl {
    (@as_item $it:item) => { $it };
    ($($t:ident),*) => {
        gc_trivial_tuple_impl! {
            @as_item
            impl<$($t: IntoHeapBase,)*> IntoHeapBase for ($($t,)*) {
                type In = ($($t::In,)*);

                #[allow(non_snake_case)]  // because we use the type names as variable names (!)
                fn into_heap(self) -> Self::In {
                    let ($($t,)*) = self;
                    ($($t.into_heap(),)*)
                }

                #[allow(non_snake_case)]
                unsafe fn trace<R>(storage: &Self::In, tracer: &mut R)
                    where R: Tracer
                {
                    let &($(ref $t,)*) = storage;

                    $(
                        <$t as $crate::traits::IntoHeapBase>::trace($t, tracer);
                    )*

                    // If the above `$(...)*` expansion is empty, we need this
                    // to quiet unused variable warnings for `tracer`.
                    let _ = tracer;
                }

                #[allow(non_snake_case)]
                unsafe fn from_heap(storage: &Self::In) -> Self {
                    let &($(ref $t,)*) = storage;
                    ($( <$t as $crate::traits::IntoHeapBase>::from_heap($t), )*)
                }
            }
        }

        gc_trivial_tuple_impl! {
            @as_item
            unsafe impl<'h, $($t: IntoHeap<'h>,)*> IntoHeap<'h> for ($($t,)*) {}
        }
    }
}

gc_trivial_tuple_impl!();
gc_trivial_tuple_impl!(T);
gc_trivial_tuple_impl!(T, U);
gc_trivial_tuple_impl!(T, U, V);
gc_trivial_tuple_impl!(T, U, V, W);
gc_trivial_tuple_impl!(T, U, V, W, X);
gc_trivial_tuple_impl!(T, U, V, W, X, Y);
gc_trivial_tuple_impl!(T, U, V, W, X, Y, Z);
