# cellgc

This crate contains a simple garbage collector for use in Rust.
The goal is to help you quickly build a VM in Rust.
So this GC is designed for:

*   Safety

*   No dependency on linters or compiler plugins

*   An API that's consistent with a high-performance implementation
    (though right now cellgc is not speedy)

*   Fun


## Caveats

**cellgc only works for toy-sized programs at present.**
[See issue #4.](https://github.com/jorendorff/rust-toy-gc/issues/4)

**cellgc is for use in VMs.** So the assumption is that the data the GC is
managing is not really *your* data; it's your end user's data. If you don't
want every field of every GC-managed object to be public and mutable, cellgc
is not the GC for your project!

cellgc isn't on crates.io yet. **The API is completely unstable.**

cellgc is not designed to support multithread access to a single heap (like Java).
Instead, you can create one heap per thread (like JavaScript).

Currently it does not support lots of small heaps with random lifetimes (like Erlang),
but I have some ideas on how to get there.


## How to use it

Good luck!

```rust
#[macro_use] extern crate cellgc;

/// A linked list of numbers that lives in the GC heap.
gc_ref_type! {
    // This declares four different related structs, but the last two are
    // for the GC's internal use. Read on to see the first two in action.
    struct IntList / RefIntList / InHeapIntList / InHeapRefIntList <'a> {
        head / set_head: i64,
        tail / set_tail: Option<RefIntList<'a>>
    }
}

fn main() {
    // Create a heap (you'll only do this once in your whole program)
    cellgc::with_heap(|heap| {
        // Allocate an object (returns a RefIntList)
        let obj1 = heap.alloc(IntList { head: 17, tail: None });
        assert_eq!(obj1.head(), 17);
        assert_eq!(obj1.tail(), None);

        // Allocate another object
        let obj2 = heap.alloc(IntList { head: 33, tail: Some(obj1) });
        assert_eq!(obj2.head(), 33);
        assert_eq!(obj2.tail().unwrap().head(), 17);
    });
}
```

`RefIntList` values keep in-heap `IntList` values alive;
once the last `RefIntList` pointing at an object is gone,
it becomes available for garbage collection,
and eventually it'll be recycled.

`RefIntList` is like `std::rc::Rc`: it's `Clone` but not `Copy`,
and calling `.clone()` copies the Ref, not the object it points to.


# Heap types

Right now, there is a separate macro for declaring enums: `heap_inline_enum!{}`.
(But I hope to merge these two macros, so this will go away.)

Not every type is safe to use as a field of a heap struct or enum.
Here are the allowed field types:

* primitive types, like `i32`
* macro-declared GC types like `IntList<'a>` and `RefIntList<'a>`
* macro-declared enum types
* `Box<T>` where `T` has `'static` lifetime
* `Rc<T>` where `T` has `'static` lifetime
* `Option<T>` where `T` is any of these types

If you try to use anything else, you'll get bizarre `rustc` error messages.


## Safety

As long as you don't type the keyword `unsafe` in your code,
this GC is safe.<sup>[citation needed]</sup>

Still, there's one weird rule to be aware of:
**Don't implement `Drop` or `Clone`
for any type declared using the `gc` macros.**
It's safe in the full Rust sense of that word
(it won't cause crashes or undefined behavior,
as long as your `.drop()` or `.clone()` method does nothing `unsafe`),
but it won't do what you want.
Your `.drop()` and `.clone()` methods simply will not be called when you expect;
and they'll be called at other times that make no sense.

So don't do that!
The safe alternative is to put a `Box` or `Rc` around your value
(the one that implements `Drop` or `Clone`)
and use that as a field of a GC heap struct.

## Why is it called `cellgc`?

In cellgc, every field of every GC-managed object is public and mutable.

It's as though every field were a [Cell](http://doc.rust-lang.org/std/cell/struct.Cell.html).
