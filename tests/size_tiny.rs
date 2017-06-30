//! The GC can work with objects that only take up one byte.

extern crate cell_gc;
#[macro_use]
extern crate cell_gc_derive;

use std::marker::PhantomData;

#[derive(IntoHeap)]
struct Tiny<'h> {
    bit: bool,
    phantom: PhantomData<&'h u8>,
}

fn main() {
    cell_gc::with_heap(|hs| {
        let n = cell_gc::page_capacity::<Tiny>();

        // see comment in size_medium.rs
        assert!(n >= 500);
        assert!(n <= 1024);

        let refs: Vec<TinyRef> = (0..n)
            .map(|i| {
                hs.alloc(Tiny {
                    bit: i & 1 == 1,
                    phantom: PhantomData,
                })
            })
            .collect();

        for i in 0..n {
            assert_eq!(refs[i].bit(), i % 2 != 0);
            refs[i].set_bit(i % 5 == 3);
        }

        hs.force_gc();

        for i in 0..n {
            assert_eq!(refs[i].bit(), i % 5 == 3);
        }
    });
}
