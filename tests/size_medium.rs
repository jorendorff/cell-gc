//! The GC can work with objects that are a few hundred bytes big.

extern crate cell_gc;
#[macro_use] extern crate cell_gc_derive;

#[derive(IntoHeap)]
struct Chunk<'h> {
    field_0: (u64, u64, u64, u64),
    field_32: (u64, u64, u64, u64),
    field_64: (u64, u64, u64, u64),
    field_96: (u64, u64, u64, u64),
    field_128: (u64, u64, u64, u64),
    field_160: (u64, u64, u64, u64),
    field_192: (u64, u64, u64, u64),
    field_224: (u64, u64, u64, u64),
    next: Option<ChunkRef<'h>>
}

fn main() {
    cell_gc::with_heap(|hs| {
        hs.set_page_limit::<Chunk>(Some(1));

        let n = cell_gc::page_capacity::<Chunk>();

        // Users don't care about the exact value here, but test it anyway
        // since it would be weird if it changed (or turned out to be different
        // on some platform). If this is failing for you and the actual value
        // of `n` is reasonable, just weaken the assertion.
        assert!(n >= 14);
        assert!(n <= 15);

        let mut root = None;
        for i in 0 .. n as u64 {
            root = Some(hs.alloc(Chunk {
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
        assert_eq!(hs.try_alloc(Chunk {
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
        for _ in 0 .. n {
            j -= 1;
            let chunk = root.expect("references aren't working or something");
            assert_eq!(chunk.field_0().0, j);
            assert_eq!(chunk.field_96().2, j);
            root = chunk.next();
        }
        assert_eq!(root, None);

        // Now, having discarded that refrence, we should be able to allocate.
        root = hs.try_alloc(Chunk {
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
