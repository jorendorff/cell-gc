//! The GC can work with objects that are a few hundred bytes big.

#[macro_use] extern crate toy_gc;

gc_ref_type! {
    struct Chunk / ChunkRef / ChunkStorage / ChunkRefStorage <'a> {
        field_0 / set_field_0: (u64, u64, u64, u64),
        field_32 / set_field_32: (u64, u64, u64, u64),
        field_64 / set_field_64: (u64, u64, u64, u64),
        field_96 / set_field_96: (u64, u64, u64, u64),
        field_128 / set_field_128: (u64, u64, u64, u64),
        field_160 / set_field_160: (u64, u64, u64, u64),
        field_192 / set_field_192: (u64, u64, u64, u64),
        field_224 / set_field_224: (u64, u64, u64, u64),
        next / set_next: Option<ChunkRef<'a>>
    }
}

fn main() {
    toy_gc::with_heap(|heap| {
        let n = toy_gc::page_capacity::<Chunk>();

        // Users don't care about the exact value here, but test it anyway
        // since it would be weird if it changed (or turned out to be different
        // on some platform). If this is failing for you and the actual value
        // of `n` is reasonable, just weaken the assertion.
        assert!(n >= 14);
        assert!(n <= 15);

        let mut root = None;
        for i in 0 .. n as u64 {
            root = Some(heap.alloc(Chunk {
                field_0: (i, i, i, i),
                field_32: (i, i, i, i),
                field_64: (i, i, i, i),
                field_96: (i, i, i, i),
                field_128: (i, i, i, i),
                field_160: (i, i, i, i),
                field_192: (i, i, i, i),
                field_224: (i, i, i, i),
                next: root
            }));
        }

        // Heap is full.
        assert_eq!(heap.try_alloc(Chunk {
            field_0: (99, 99, 99, 99),
            field_32: (99, 99, 99, 99),
            field_64: (99, 99, 99, 99),
            field_96: (99, 99, 99, 99),
            field_128: (99, 99, 99, 99),
            field_160: (99, 99, 99, 99),
            field_192: (99, 99, 99, 99),
            field_224: (99, 99, 99, 99),
            next: root.clone()
        }), None);

        // Spot-check that the objects are still good.
        let mut j = n as u64;
        for i in 0 .. n {
            j -= 1;
            let chunk = root.expect("references aren't working or something");
            assert_eq!(chunk.field_0().0, j);
            assert_eq!(chunk.field_96().2, j);
            root = chunk.next();
        }
        assert_eq!(root, None);

        // Now, having discarded that refrence, we should be able to allocate.
        root = heap.try_alloc(Chunk {
            field_0: (99, 99, 99, 99),
            field_32: (99, 99, 99, 99),
            field_64: (99, 99, 99, 99),
            field_96: (99, 99, 99, 99),
            field_128: (99, 99, 99, 99),
            field_160: (99, 99, 99, 99),
            field_192: (99, 99, 99, 99),
            field_224: (99, 99, 99, 99),
            next: root
        });
        assert_eq!(root.expect("gc should have freed up memory").field_128().1, 99);
    });
}
