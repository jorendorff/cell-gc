//! Destructors are called.

#[macro_use] extern crate cell_gc;

gc_ref_type! {
    struct Dropper / DropperRef / DropperStorage / DropperRefStorage <'a> {
        addr / set_addr: usize,
        ignore / set_ignore: SomethingWithLifetime<'a>
    }
}

gc_inline_enum! {
    enum SomethingWithLifetime / SomethingWithLifetimeStorage <'a> {
        Another(DropperRef<'a>),
        Nothing
    }
}

use SomethingWithLifetime::Nothing;

impl<'a> Drop for DropperStorage<'a> {
    fn drop(&mut self) {
        unsafe {
            *(self.addr as *mut i32) += 1;
        }
    }
}

fn main() {
    cell_gc::with_heap(|heap| {
        let mut drop_count: i32 = 0;
        let ptr: *mut i32 = &mut drop_count;

        let mut r = heap.alloc(Dropper { addr: ptr as usize, ignore: Nothing });
        for i in 1..7 {
            r = heap.alloc(Dropper { addr: ptr as usize, ignore: Nothing });
        }

        assert_eq!(drop_count, 0);
        heap.force_gc();
        assert_eq!(drop_count, 6);
        std::mem::drop(r);
        heap.force_gc();
        assert_eq!(drop_count, 7);
    });
}
