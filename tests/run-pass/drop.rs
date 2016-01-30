//! Destructors are called.

#[macro_use] extern crate cell_gc;

gc_heap_type! {
    struct Dropper / DropperRef / DropperStorage <'h> {
        addr / set_addr: usize,
        ignore / set_ignore: SomethingWithLifetime<'h>
    }
}

gc_heap_type! {
    enum SomethingWithLifetime / SomethingWithLifetimeStorage <'h> {
        Another(DropperRef<'h>),
        Nothing
    }
}

use SomethingWithLifetime::Nothing;

impl<'h> Drop for DropperStorage<'h> {
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
