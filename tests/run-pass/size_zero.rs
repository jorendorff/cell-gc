//! The GC can allocate objects that are zero-size.

#[macro_use] extern crate toy_gc;
use std::marker::PhantomData;

gc_ref_type! {
    pub struct Unit / UnitRef / UnitStorage / UnitRefStorage <'a> {
        phantom / set_phantom: PhantomData<&'a u8>
    }
}

fn main () {
    assert_eq!(std::mem::size_of::<UnitStorage>(), 0);

    toy_gc::with_heap(|heap| {
        let n = toy_gc::page_capacity::<Unit>();

        // see comment in size_medium.rs
        assert!(n >= 500);
        assert!(n <= 1024);

        let refs: Vec<UnitRef> =
            (0 .. n)
            .map(|i| heap.alloc(Unit { phantom: PhantomData }))
            .collect();

        heap.force_gc();

        assert_eq!(heap.try_alloc(Unit { phantom: PhantomData }),
                   None);
    });
}
