//! The traits defined here are implementation details of cell_gc.
//!
//! Application code does not need to use these traits. They are public only so
//! that `gc_heap_type!` can use it. Use the friendly macro!

use gcref::GcRef;

/// Trait for values that can be moved into a GC heap.
///
/// Cell-gc does not support GC allocation of arbitrary values: only values of
/// types that implement `IntoHeap`. This trait is **not** meant to be
/// implemented by hand: use the `gc_heap_type!` macro instead. All primitive
/// types and many standard types support `IntoHeap`.
///
/// GC types come in pairs: an *in-heap* type, which is stored physically inside
/// the heap, and may be packed with pointer fields and unsafe methods; and an
/// `IntoHeap` type, which is safe, is used in application code, and lives on
/// the stack. Both types have the same layout, bit for bit.
///
/// (The word "safe" above means that, as with everything generally in Rust, you
/// can hack without fear. `IntoHeap` types don't expose raw Rust pointers or
/// references to GC memory, they don't expose the unsafe in-heap types, and
/// they obey Rust's safety and aliasing rules.)
///
/// Users never get a direct `&` reference to any in-heap value. All access is
/// through safe `IntoHeap` types, like the `Ref` type that is automatically
/// declared for you when you use `gc_heap_type!` to declare a struct.
///
/// *   For primitive types, like `i32`, the two types are the same.
///
/// *   Macro-generated "Ref" types are `IntoHeap` types; the corresponding
///     in-heap types are effectively raw pointers.
///
/// *   For macro-generated structs and enums, the user specifies the names of
///     both types. Both types have identical fields or variants, but fields of
///     the in-heap type are in-heap, and fields of the `IntoHeap` type are
///     `IntoHeap`.
///
/// Implementation note: every type with `'static` lifetime should be safe to
/// store in the heap, because ref types are never `'static`. Unfortunately I
/// can't make rustc understand this. It would be *legal* to write this:
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
/// In-heap objects are full of pointers; if `into_heap` puts garbage into
/// them, GC will crash.
///
/// `mark` must be implmented with care in order to preserve the invariants of
/// the GC graph-walking algorithm. Bugs there are very likely to lead to
/// dangling pointers and hard-to-debug crashes down the road.
///
pub unsafe trait IntoHeap<'h>: Sized {
    /// The type of the value when it is physically stored in the heap.
    type In;

    /// Convert the value to the form it should have in the heap.
    /// This is for macro-generated code to call.
    ///
    /// This method must not be called while any direct Rust references to
    /// heap objects exist. (However, user code never runs while such
    /// references exist, so the method is not marked `unsafe`.)
    fn into_heap(self) -> Self::In;

    /// Extract the value from the heap. This turns any raw pointers in the
    /// in-heap value into safe references, so while it's an unsafe function,
    /// the result of a correct call can be safely handed out to user code.
    ///
    /// Unsafe to call: It is impossible for ordinary users to call this
    /// safely, because `self` must be a direct, unwrapped reference to a value
    /// stored in the GC heap, which ordinary users cannot obtain.
    unsafe fn from_heap(&Self::In) -> Self;

    /// Unsafe to call: It is impossible for ordinary users to call this
    /// safely, because `self` must be a direct, unwrapped reference to a value
    /// stored in the GC heap, which ordinary users cannot obtain.
    unsafe fn mark(&Self::In);
}

/// Relate an `IntoHeap` type to the corresponding safe reference type.
pub trait IntoHeapAllocation<'h>: IntoHeap<'h> {
    type Ref: IntoHeap<'h>;

    fn wrap_gcref(gcref: GcRef<'h, Self>) -> Self::Ref;
}


// === Provided implmentations for primitive types

macro_rules! gc_trivial_impl {
    ($t:ty) => {
        unsafe impl<'h> IntoHeap<'h> for $t {
            type In = $t;
            fn into_heap(self) -> $t { self }
            unsafe fn from_heap(storage: &$t) -> $t { storage.clone() }
            unsafe fn mark(_storage: &$t) {}
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

gc_trivial_impl!(String);

macro_rules! gc_generic_trivial_impl {
    (@as_item $it:item) => { $it };
    ([$($x:tt)*] $t:ty) => {
        gc_generic_trivial_impl! {
            @as_item
            unsafe impl<'h, $($x)*> IntoHeap<'h> for $t {
                type In = $t;
                fn into_heap(self) -> $t { self }
                unsafe fn from_heap(storage: &$t) -> $t { (*storage).clone() }
                unsafe fn mark(_storage: &$t) {}
            }
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

unsafe impl<'h, T: IntoHeap<'h>> IntoHeap<'h> for Option<T> {
    type In = Option<T::In>;

    fn into_heap(self) -> Option<T::In> {
        self.map(|t| t.into_heap())
    }

    unsafe fn mark(storage: &Option<T::In>) {
        match storage {
            &None => (),
            &Some(ref u) => T::mark(u),
        }
    }

    unsafe fn from_heap(storage: &Option<T::In>) -> Option<T> {
        match storage {
            &None => None,
            &Some(ref u) => Some(T::from_heap(u)),
        }
    }
}

macro_rules! gc_trivial_tuple_impl {
    (@as_item $it:item) => { $it };
    ($($t:ident),*) => {
        gc_trivial_tuple_impl! {
            @as_item
            unsafe impl<'h, $($t: IntoHeap<'h>,)*> IntoHeap<'h> for ($($t,)*) {
                type In = ($($t::In,)*);

                #[allow(non_snake_case)]  // because we use the type names as variable names (!)
                fn into_heap(self) -> Self::In {
                    let ($($t,)*) = self;
                    ($($t.into_heap(),)*)
                }

                #[allow(non_snake_case)]
                unsafe fn mark(storage: &Self::In) {
                    let &($(ref $t,)*) = storage;
                    $( <$t as $crate::traits::IntoHeap>::mark($t); )*
                }

                #[allow(non_snake_case)]
                unsafe fn from_heap(storage: &Self::In) -> Self {
                    let &($(ref $t,)*) = storage;
                    ($( <$t as $crate::traits::IntoHeap>::from_heap($t), )*)
                }
            }
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
