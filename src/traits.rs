//! The traits defined here are implementation details of cell_gc.
//!
//! Application code does not need to use these traits. They are public only so
//! that `#[derive(IntoHeap)]` can use them. Use the friendly macro!
//!
//! Summary of the types that are allowed in the GC heap:
//!
//! ```text
//! machine types
//!     ----> themselves
//! String, &'static T, GcLeaf<T: Clone>, Box<T: Clone>, Arc<T: Sync>
//!     ----> themselves
//! PhantomData<&'h T>
//!     ----> itself
//! GcRef<'h, T: IntoHeapAllocation>
//!     ----> Pointer<T::In>
//! Option<T: IntoHeap>
//!     ----> Option<T::In>
//! FooRef stack-to-gc-heap smart pointer
//!     ----> Pointer<FooStorage>
//! tuples of IntoHeap types
//!     ----> tuples of corresponding InHeap types
//! structs/enums with IntoHeap fields and #[derive(IntoHeap)]
//!     ----> structs/enums with corresponding InHeap fields
//! Vec<T: IntoHeap>, VecRef<'h, T>
//!     ----> Vec<T::In>, Pointer<Vec<T::In>>
//! ```

use gc_leaf::GcLeaf;
use gc_ref::GcRef;
use poison;
use ptr::Pointer;
use std::any::Any;
use std::hash::Hash;
use std::marker::PhantomData;

/// Trait for values actually stored inside the heap.
pub trait InHeap: Any + 'static {
    /// Traverse a value in the heap.
    ///
    /// This calls `tracer.visit(ptr)` for each edge from this value
    /// to other heap values.
    ///
    /// For user-defined structs and enums, this is achieved by calling the
    /// `field.trace(tracer)` method of each field of this value. (They are
    /// typically all `In` types.) The implementation of this method for
    /// `Pointer<T>` calls `tracer.visit()`.
    ///
    /// # Safety
    ///
    /// This must be safe to call if `u` is a valid in-heap value and the
    /// caller has read access to the heap that contains it.
    ///
    /// This method exists for the benefit of the garbage collector. It is
    /// impossible for ordinary users to call this safely, because `self` must
    /// be a direct, unwrapped reference to a value stored in the GC heap,
    /// which ordinary users cannot obtain.
    unsafe fn trace<R>(&self, tracer: &mut R)
    where
        R: Tracer;
}

/// Base trait for values that can be moved into a GC heap.
///
/// An implementation of this trait is safe if its `from_heap` and `trace`
/// methods are safe under the circumstances described in their documentation.
pub trait IntoHeapBase: Sized {
    /// The type of the value when it is physically stored in the heap.
    ///
    /// GC types come in pairs: an `IntoHeap` type and an `In` type. Ideally
    /// both types would have the same layout, bit for bit, but Rust makes no
    /// promises about struct and enum layout generally, so we can't guarantee
    /// this.
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
    type In: InHeap;

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
    /// This must be safe to call if `u` is a valid in-heap value and the
    /// caller has read access to the heap that contains it.
    ///
    /// The implementation of this method for `GcRef`, for example, is safe to
    /// call if `u` is a valid pointer—it points to an initialized allocation
    /// of the correct type in the same heap.
    ///
    /// This method exists for the benefit of cell-gc-derive. It is impossible
    /// for ordinary users to call it safely, because `self` must be a
    /// direct, unwrapped reference to a value stored in the GC heap, which
    /// ordinary users cannot obtain.
    unsafe fn from_heap(u: &Self::In) -> Self;
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
/// *   `GcHeap` implements `Send`. This means any value that can be stored in
///     the heap can be sent to other threads (by sending the whole heap). So
///     don't implement this trait for any type that isn't itself already
///     `Send`. (The reason this trait doesn't *require* `Send` is that at
///     present, `Ref` types must *not* be `Send` but must implement
///     `IntoHeap`.)  It's OK to store non-`Sync` values in the heap.
///
/// *   And this probably goes without saying, but none of these methods may
///     allocate or do anything else that could trigger garbage collection.
///
pub unsafe trait IntoHeap<'h>: IntoHeapBase {}

/// Types that can be allocated in the heap.
pub trait IntoHeapAllocation<'h>: IntoHeap<'h> {
    /// The safe reference type that's returned when a value of this type is
    /// moved into the heap (i.e. when it's allocated).
    type Ref: Hash + IntoHeap<'h>;

    /// Wrap a `GcRef` into the associated `Self::Ref` type.
    fn wrap_gc_ref(gc_ref: GcRef<'h, Self>) -> Self::Ref;

    /// Unwrap an associated `Self::Ref` type into a `GcRef`.
    fn into_gc_ref(r: Self::Ref) -> GcRef<'h, Self>;
}

/// A `Tracer` visits each of an object's outgoing edges pointing to other GC
/// things.
///
/// The primary `Tracer` is the marking tracer, which walks the heap starting
/// from the roots and sets the mark bits of every live object it finds. While
/// this is currently the only `Tracer` impl that exists, one could imagine a
/// `HeapSnapshotTracer` that serializes the heap graph to disk for offline
/// processing and analysis in some devtool, for example.
pub trait Tracer {
    /// Tell the `Tracer` about an outgoing edge of the object currently being
    /// traced.
    fn visit<U: InHeap>(&mut self, Pointer<U>);
}

// === Provided implmentations for primitive types

macro_rules! gc_trivial_impl {
    ($t:ty, $h:expr) => {
        impl InHeap for $t {
            #[inline] unsafe fn trace<R: Tracer>(&self, _tracer: &mut R) {}
        }
        impl IntoHeapBase for $t {
            type In = $t;
            #[inline] fn into_heap(self) -> $t { self }
            #[inline] unsafe fn from_heap(storage: &$t) -> $t { storage.clone() }
        }
        unsafe impl<'h> IntoHeap<'h> for $t {}
        impl<'h> IntoHeapAllocation<'h> for $t {
            type Ref = GcRef<'h, Self>;
            #[inline] fn wrap_gc_ref(gc_ref: GcRef<'h, Self>) -> Self::Ref { gc_ref }
            #[inline] fn into_gc_ref(gc_ref: Self::Ref) -> GcRef<'h, Self> { gc_ref }
        }
    }
}

gc_trivial_impl!(bool, 0xa1200afc6a46e9b);
gc_trivial_impl!(char, 0xbd439e198d8c84ec);
gc_trivial_impl!(i8, 0x2b057772479916bd);
gc_trivial_impl!(u8, 0xef7d2bcff18c9c1f);
gc_trivial_impl!(i16, 0xabcb6c243dfe6d88);
gc_trivial_impl!(u16, 0x2eb8e1dca4d9b8f3);
gc_trivial_impl!(i32, 0xf9df7e1ee1249e8d);
gc_trivial_impl!(u32, 0xfdd13ffbed3f8c6f);
gc_trivial_impl!(i64, 0x4d81963faaf90a33);
gc_trivial_impl!(u64, 0xf301072ea4b22a88);
gc_trivial_impl!(isize, 0x17a31e16220b9ec8);
gc_trivial_impl!(usize, 0xbc3d03b0a285f9a7);
gc_trivial_impl!(f32, 0xd08d8d94baf44a74);
gc_trivial_impl!(f64, 0x80bff0f49d51f22);

gc_trivial_impl!(String, 0x1c66d28939b11111);

macro_rules! gc_generic_trivial_impl {
    (@as_item $it:item) => { $it };
    ([$($x:tt)*] $t:ty) => {
        gc_generic_trivial_impl! {
            @as_item
            impl<$($x)*> InHeap for $t {
                unsafe fn trace<R: Tracer>(&self, _tracer: &mut R) {}
            }
        }
        gc_generic_trivial_impl! {
            @as_item
            impl<$($x)*> IntoHeapBase for $t {
                type In = $t;
                fn into_heap(self) -> $t { self }
                unsafe fn from_heap(storage: &$t) -> $t { (*storage).clone() }
            }
        }
        gc_generic_trivial_impl! {
            @as_item
            unsafe impl<'h, $($x)*> IntoHeap<'h> for $t {}
        }
        gc_generic_trivial_impl! {
            @as_item
            impl<'h, $($x)*> IntoHeapAllocation<'h> for $t {
                type Ref = GcRef<'h, Self>;
                fn wrap_gc_ref(gc_ref: GcRef<'h, Self>) -> Self::Ref { gc_ref }
                fn into_gc_ref(gc_ref: Self::Ref) -> GcRef<'h, Self> { gc_ref }
            }
        }
    }
}

gc_generic_trivial_impl!([T: ?Sized + Sync] &'static T);
gc_generic_trivial_impl!([T: Clone + Send + 'static] GcLeaf<T>);
gc_generic_trivial_impl!([T: Clone + Send + ?Sized + 'static] Box<T>);
gc_generic_trivial_impl!([T: Sync + ?Sized + 'static] ::std::sync::Arc<T>);

/// Currently, `#[derive(IntoHeap)]` only works for types that have a lifetime
/// parameter.  This poses a problem because sometimes you want to store stuff
/// in the heap that doesn't contain any `GcRef`s or other heap lifetimes.
/// As a hackaround, we allow `PhantomData<&'h T>` fields (for now).
impl<T: 'static> InHeap for PhantomData<&'static T> {
    unsafe fn trace<R: Tracer>(&self, _tracer: &mut R) {}
}

impl<'h, T: 'static> IntoHeapBase for PhantomData<&'h T> {
    type In = PhantomData<&'static T>;
    fn into_heap(self) -> Self::In { PhantomData }
    unsafe fn from_heap(_storage: &Self::In) -> Self { PhantomData }
}

impl<U: InHeap> InHeap for Pointer<U> {
    unsafe fn trace<R: Tracer>(&self, tracer: &mut R) {
        tracer.visit::<U>(*self);
    }
}

// GCRef has a special implementation.
impl<'h, T: IntoHeapAllocation<'h>> IntoHeapBase for GcRef<'h, T> {
    type In = Pointer<T::In>;

    fn into_heap(self) -> Self::In {
        self.ptr()
    }

    unsafe fn from_heap(storage: &Self::In) -> Self {
        poison::assert_is_not_poisoned(storage.as_raw());
        Self::new(*storage)
    }

    // unsafe fn trace<R: Tracer>(storage: &Self::In, tracer: &mut R) {
    //     poison::assert_is_not_poisoned(storage.as_raw());
    //     tracer.visit::<T>(*storage);
    // }
}

unsafe impl<'h, T: IntoHeapAllocation<'h>> IntoHeap<'h> for GcRef<'h, T> {}

// Transitive implementations ("this particular kind of struct/enum is IntoHeap
// if all its fields are") are slightly less trivial.

impl<U: InHeap> InHeap for Option<U> {
    unsafe fn trace<R: Tracer>(&self, tracer: &mut R) {
        if let &Some(ref u) = self {
            u.trace(tracer);
        }
    }
}

impl<T: IntoHeapBase> IntoHeapBase for Option<T> {
    type In = Option<T::In>;

    fn into_heap(self) -> Option<T::In> {
        self.map(|t| t.into_heap())
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
            impl<$($t: InHeap,)*> InHeap for ($($t,)*) {
                #[allow(non_snake_case)]
                unsafe fn trace<R: Tracer>(&self, tracer: &mut R) {
                    let &($(ref $t,)*) = self;

                    $(
                        <$t as $crate::traits::InHeap>::trace($t, tracer);
                    )*

                    // If the above `$(...)*` expansion is empty, we need this
                    // to quiet unused variable warnings for `tracer`.
                    let _ = tracer;
                }
            }
        }

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
