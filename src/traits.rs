//! The traits defined here are implementation details of cell_gc.
//!
//! Application code does not need to use these traits. They are public only so
//! that the public macros can use it. Use the friendly macros!

use gcref::GCRef;

/// `InHeap` types can be stored directly in the GC heap. All primitive types
/// and many standard types implement `InHeap`.
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
/// Unsafe to implement: `from_heap` is incredibly dangerous. It receives a
/// non-mut reference to a heap value. That value may contain gc-references to
/// itself, directly or indirectly, in which case `from_heap` (or other code it
/// calls) could *without using any unsafe code* modify the heap while this
/// direct, non-mut reference exists. This breaks Rust's aliasing rules and
/// could cause crashes.
///
/// Implementation note: every type with `'static` lifetime should be safe to
/// store in the heap, because ref types are never `'static`. Unfortunately I
/// can't make rustc understand this. It would be *legal* to write this:
///
/// ```ignore
/// unsafe impl<'a, T: Clone + 'static> InHeap<'a> for T { ...trivial... }
/// unsafe impl<'a, T: Clone + 'static> IntoHeap<'a> for T { ...trivial... }
/// ```
///
/// but alas, Rust thinks those impls conflict with almost all others, making
/// it impossible to get the macros to work. So instead we `impl InHeap for` a
/// lot of individual types by hand.
pub unsafe trait InHeap<'a>: Sized {
    type Out: IntoHeap<'a, In=Self>;

    /// Unsafe to call: It is impossible for ordinary users to call this
    /// safely, because `self` must be a direct, unwrapped reference to a value
    /// stored in the GC heap, which ordinary users cannot obtain.
    unsafe fn mark(&self);

    /// Extract the value from the heap. This turns any raw pointers in the
    /// `InHeap` value into safe references, so while it's an unsafe function,
    /// the result of a correct call can be safely handed out to user code.
    ///
    /// Unsafe to call: It is impossible for ordinary users to call this
    /// safely, because `self` must be a direct, unwrapped reference to a value
    /// stored in the GC heap, which ordinary users cannot obtain.
    unsafe fn from_heap(&self) -> Self::Out;
}

/// `IntoHeap` types live on the stack, in application code. They are the
/// safe, user-friendly analogues to pointer-filled `InHeap` types. When you
/// use `gc_ref_type!` to declare a new GC struct or enum, `IntoHeap` types are
/// the types you use for fields.
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
pub unsafe trait IntoHeap<'a>: Sized {
    /// The type of the value when it is physically stored in the heap.
    type In: InHeap<'a, Out=Self>;

    /// Convert the value to the form it should have in the heap.
    /// This is for macro-generated code to call.
    ///
    /// This method must not be called while any direct Rust references to
    /// heap objects exist. (However, user code never runs while such
    /// references exist, so the method is not marked `unsafe`.)
    fn into_heap(self) -> Self::In;
}

/// Relate an `IntoHeap` type to the corresponding safe reference type.
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
            unsafe fn mark(&self) {}
            unsafe fn from_heap(&self) -> $t { self.clone() }
        }

        unsafe impl<'a> IntoHeap<'a> for $t {
            type In = $t;
            fn into_heap(self) -> $t { self }
        }
    }
}

macro_rules! gc_generic_trivial_impl {
    (#[as_item] $it:item) => { $it};
    ([$($x:tt)*] $t:ty) => {
        gc_generic_trivial_impl! {
            #[as_item]
            unsafe impl<'a, $($x)*> InHeap<'a> for $t {
                type Out = $t;
                unsafe fn mark(&self) {}
                unsafe fn from_heap(&self) -> $t { self.clone() }
            }
        }

        gc_generic_trivial_impl! {
            #[as_item]
            unsafe impl<'a, $($x)*> IntoHeap<'a> for $t {
                type In = $t;
                fn into_heap(self) -> $t { self }
            }
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

gc_generic_trivial_impl!([T: ?Sized] &'static T);
gc_generic_trivial_impl!([T: ?Sized] ::std::marker::PhantomData<T>);
gc_generic_trivial_impl!([T: Clone + 'static] Box<T>);
gc_generic_trivial_impl!([T: Clone + 'static] ::std::rc::Rc<T>);

// Transitive implementations ("this particular kind of struct/enum is InHeap
// if all its fields are") are slightly less trivial.

unsafe impl<'a, U: InHeap<'a>> InHeap<'a> for Option<U> {
    type Out = Option<U::Out>;

    unsafe fn mark(&self) {
        match self {
            &None => (),
            &Some(ref u) => u.mark()
        }
    }

    unsafe fn from_heap(&self) -> Option<U::Out> {
        match self {
            &None => None,
            &Some(ref u) => Some(u.from_heap())
        }
    }
}

unsafe impl<'a, T: IntoHeap<'a>> IntoHeap<'a> for Option<T> {
    type In = Option<T::In>;
    fn into_heap(self) -> Option<T::In> {
        self.map(|t| t.into_heap())
    }
}

macro_rules! gc_trivial_tuple_impl {
    (#[as_item] $it:item) => { $it };
    ($($t:ident),*) => {
        gc_trivial_tuple_impl! {
            #[as_item]
            unsafe impl<'a $(, $t: InHeap<'a>)*> InHeap<'a> for ($($t,)*) {
                type Out = ($($t::Out,)*);

                #[allow(non_snake_case)]  // because we use the type names as variable names (!)
                unsafe fn mark(&self) {
                    let &($(ref $t,)*) = self;
                    $( $t.mark(); )*
                }

                #[allow(non_snake_case)]
                unsafe fn from_heap(&self) -> Self::Out {
                    let &($(ref $t,)*) = self;
                    ($($t.from_heap(),)*)
                }
            }
        }

        gc_trivial_tuple_impl! {
            #[as_item]
            unsafe impl<'a, $($t: IntoHeap<'a>,)*> IntoHeap<'a> for ($($t,)*) {
                type In = ($($t::In,)*);

                #[allow(non_snake_case)]
                fn into_heap(self) -> Self::In {
                    let ($($t,)*) = self;
                    ($($t.into_heap(),)*)
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
