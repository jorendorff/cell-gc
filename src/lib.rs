//! A simple garbage collector for use in Rust.
//!
//! The goal is to help you quickly build a VM in Rust.
//! So this GC is designed for:
//!
//! *   Safety
//!
//! *   No dependency on linters or compiler plugins
//!
//! *   An API that's consistent with a high-performance implementation
//!     (though right now cell-gc is not speedy)
//!
//! *   Fun
//!
//!
//! # Caveats
//!
//! **cell-gc is for use in VMs.** So the assumption is that the data the GC is
//! managing is not really *your* data; it's your end user's data. If you don't
//! want every field of every GC-managed object to be public and mutable, cell-gc
//! is not the GC for your project!
//!
//! **The API is completely unstable.** I promise I will change it in ways
//! that will break code; you'll just have to keep up until things stabilize.
//!
//! cell-gc is not designed to support multithread access to a single heap (like Java).
//! Instead, you can create one heap per thread (like JavaScript).
//!
//! Currently it does not support lots of small heaps with random lifetimes (like Erlang),
//! but I have some ideas on how to get there.
//! [See issue #7.](https://github.com/jorendorff/rust-toy-gc/issues/7)
//!
//!
//! # How to use it
//!
//! There are two parts to using `cell_gc`: GC types and GC heaps.
//!
//! ## Declaring GC types
//!
//! Declaring GC types is actually super easy. Just add `#[derive(IntoHeap)]`
//! to any struct:
//!
//! ```rust
//! extern crate cell_gc;
//! #[macro_use] extern crate cell_gc_derive;
//!
//! /// A linked list of numbers that lives in the GC heap.
//! /// The `#[derive(IntoHeap)]` here causes Rust to define an additional
//! /// type, `IntListRef`.
//! #[derive(IntoHeap)]
//! struct IntList<'h> {
//!     head: i64,
//!     tail: Option<IntListRef<'h>>
//! }
//! # fn main(){}
//! ```
//!
//! `cell_gc` does several things:
//!
//! *   Behind the scenes, it generates some code used for garbage collection,
//!     such as marking code. You never need to worry about that stuff.
//!
//! *   It checks that `IntList`'s fields are all GC-safe.
//!
//!     Not every type is safe to use as a field of a heap struct or enum.
//!     Here are the allowed field types:
//!
//!     * primitive types, like `i32`
//!     * types declared with `#[derive(IntoHeap)]`, like `IntList<'h>` and `IntListRef<'h>`
//!     * `Box<T>` where `T` has `'static` lifetime
//!     * `Rc<T>` where `T` has `'static` lifetime
//!     * `Option<T>` where `T` is any of these types
//!
//!     If you try to use anything else, you'll get bizarre error messages
//!     from `rustc`.
//!
//! *   It declare a `Ref` type for you, in this case `IntListRef`.
//!     `cell_gc` names this type by gluing `Ref` to the end of the struct
//!     name. `IntListRef` is a smart pointer to a GC-managed `IntList`. You
//!     need this because `cell_gc` doesn't let you have normal Rust references
//!     to stuff in the GC heap.
//!
//!     `IntListRef` values keep in-heap `IntList` values alive; once the last
//!     `IntListRef` pointing at an object is gone, it becomes available for
//!     garbage collection, and eventually it'll be recycled.
//!
//!     `IntListRef` is like `std::rc::Rc`: it's `Clone` but not `Copy`, and
//!     calling `.clone()` copies the Ref, not the object it points to.
//!
//!     `Ref` types have accessor methods for getting and setting each
//!     field of the struct. For example, `IntList` has methods `.head()`, `.tail()`,
//!     `.set_head(i64)`, and `.set_tail(Option<IntListRef>)`.
//!
//! You can also derive `IntoHeap` for an enum, but support is incomplete: no
//! `Ref` type is generated for enums. Tuple structs are not supported.
//!
//! ## Understanding heaps
//!
//! This part isn't documented well yet. But here's an example,
//! using the `IntList` from above:
//!
//! ```rust
//! # extern crate cell_gc;
//! # #[macro_use] extern crate cell_gc_derive;
//! # #[derive(IntoHeap)]
//! # struct IntList<'h> {
//! #     head: i64,
//! #     tail: Option<IntListRef<'h>>
//! # }
//! use cell_gc::GcHeap;
//!
//! fn main() {
//!     // Create a heap (you'll only do this once in your whole program)
//!     let mut heap = GcHeap::new();
//!
//!     heap.enter(|hs| {
//!         // Allocate an object (returns an IntListRef)
//!         let obj1 = hs.alloc(IntList { head: 17, tail: None });
//!         assert_eq!(obj1.head(), 17);
//!         assert_eq!(obj1.tail(), None);
//!
//!         // Allocate another object
//!         let obj2 = hs.alloc(IntList { head: 33, tail: Some(obj1) });
//!         assert_eq!(obj2.head(), 33);
//!         assert_eq!(obj2.tail().unwrap().head(), 17);
//!     });
//! }
//! ```
//!
//! Use `GcHeap::new()` in your `main()` function to create a heap.
//! Use `heap.enter()` to gain access to the heap (opening a "heap session", `hs`).
//! Use `hs.alloc(v)` to allocate values in the heap.
//!
//! # Vectors in the GC heap
//!
//! A very simple "object" type for a text adventure game:
//!
//! ```rust
//! extern crate cell_gc;
//! #[macro_use] extern crate cell_gc_derive;
//!
//! use cell_gc::collections::VecRef;
//!
//! #[derive(IntoHeap)]
//! struct Object<'h> {
//!     name: String,
//!     description: String,
//!     children: VecRef<'h, ObjectRef<'h>>
//! }
//! # fn main() {}
//! ```
//!
//! Note that `children` is a `VecRef<'h, ObjectRef<'h>>`; that is, it is
//! a reference to a separately GC-allocated `Vec<ObjectRef<'h>>`, which is
//! a vector of references to other objects. In other words, this is exactly
//! what you would have in Java for a field declared like this:
//!
//! ```java
//! public ArrayList<Object> children;
//! ```
//!
//! The API generated by this macro looks like this:
//!
//! ```rust
//! # struct VecRef<'h, T: 'h>(&'h T);  // hack to make this compile
//! struct Object<'h> {
//!     name: String,
//!     description: String,
//!     children: VecRef<'h, ObjectRef<'h>>
//! }
//!
//! struct ObjectRef<'h> {
//!    /* all fields private */
//! #  target: &'h Object<'h>     // hack to make this compile
//! }
//!
//! impl<'h> ObjectRef<'h> {
//!     fn name(&self) -> String
//! #       { unimplemented!(); }
//!     fn set_name(&self, name: String)
//! #       { unimplemented!(); }
//!     fn description(&self) -> String
//! #       { unimplemented!(); }
//!     fn set_description(&self, description: String)
//! #       { unimplemented!(); }
//!     fn children(&self) -> VecRef<'h, ObjectRef<'h>>
//! #       { unimplemented!(); }
//!     fn set_children(&self, children: VecRef<'h, ObjectRef<'h>>)
//! #       { unimplemented!(); }
//! }
//! ```
//!
//! (You might never actually use that `set_children()` method.
//! Instead, you'll initialize the `children` field with a vector when you
//! create the object, and then you'll most likely mutate that existing vector
//! rather than ever creating a new one.)
//!
//! You can allocate `Object`s in the heap using `hs.alloc(Object { ... })`,
//! and make one `Object` a child of another by using `obj1.children().push(obj2)`.
//!
//! # Safety
//!
//! As long as you don't type the keyword `unsafe` in your code,
//! this GC is safe.<sup>[citation needed]</sup>
//!
//! Still, there's one weird rule to be aware of:
//! **Don't implement `Drop` or `Clone`
//! for any type declared using `derive(IntoHeap)`.**
//! It's safe in the full Rust sense of that word
//! (it won't cause crashes or undefined behavior,
//! as long as your `.drop()` or `.clone()` method does nothing `unsafe`),
//! but it won't do what you want.
//! Your `.drop()` and `.clone()` methods simply will not be called when you expect;
//! and they'll be called at other times that make no sense.
//!
//! So don't do that!
//! The safe alternative is to put a `Box` or `Rc` around your value
//! (the one that implements `Drop` or `Clone`)
//! and use that as a field of a GC heap struct.
//!
//!
//! # Why is it called "cell-gc"?
//!
//! In cell-gc, every field of every GC-managed object is mutable.
//! You can't get *direct* references to the data;
//! instead you use methods to get and set values.
//!
//! It's as though every field were a [Cell](http://doc.rust-lang.org/std/cell/struct.Cell.html).

#![deny(missing_docs)]

pub mod traits;
mod pages;
mod heap;
mod gc_ref;
mod gc_leaf;
pub mod collections;
pub mod ptr;
mod marking;
pub mod poison;
mod signposts;
mod ssb;

pub use gc_leaf::GcLeaf;
pub use gc_ref::{GcFrozenRef, GcRef};
pub use heap::{GcHeap, GcHeapSession, with_heap, post_write_barrier, YesGc};

/// Return the number of allocations of a given type that fit in a "page".
/// (Unstable. This is a temporary hack for testing.)
pub fn page_capacity<'h, T: traits::IntoHeapAllocation<'h>>() -> usize {
    pages::TypedPage::<T::In>::capacity()
}
