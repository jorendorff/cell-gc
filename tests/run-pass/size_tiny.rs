//! The GC can work with objects that only take up one byte.

#[macro_use] extern crate toy_gc;
use std::marker::PhantomData;

gc_ref_type! {
    struct Tiny / TinyRef / TinyStorage / TinyRefStorage <'a> {
        bit / set_bit: bool,
        phantom / set_phantom: PhantomData<&'a u8>
    }
}

fn main () {
    toy_gc::with_heap(|heap| {
        let n = toy_gc::page_capacity::<Tiny>();

        // see comment in size_medium.rs
        assert!(n >= 500);
        assert!(n <= 1024);

        let refs: Vec<TinyRef> =
            (0 .. n)
            .map(|i| {
                heap.alloc(Tiny {
                    bit: i & 1 == 1,
                    phantom: PhantomData
                })
            })
            .collect();

        for i in 0 .. n {
            assert_eq!(refs[i].bit(), i % 2 != 0);
            refs[i].set_bit(i % 5 == 3);
        }

        heap.force_gc();

        for i in 0 .. n {
            assert_eq!(refs[i].bit(), i % 5 == 3);
        }
    });
}
