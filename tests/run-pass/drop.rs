//! Destructors are called.

#[macro_use] extern crate cell_gc;
#[macro_use] extern crate cell_gc_derive;

#[derive(IntoHeap)]
struct Dropper<'h> {
    addr: usize,
    ignore: SomethingWithLifetime<'h>
}

#[derive(IntoHeap)]
enum SomethingWithLifetime<'h> {
    Another(DropperRef<'h>),
    Nothing
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
    cell_gc::with_heap(|hs| {
        let mut drop_count: i32 = 0;
        let ptr: *mut i32 = &mut drop_count;

        let mut r = hs.alloc(Dropper { addr: ptr as usize, ignore: Nothing });
        for i in 1..7 {
            r = hs.alloc(Dropper { addr: ptr as usize, ignore: Nothing });
        }

        assert_eq!(drop_count, 0);
        hs.force_gc();
        assert_eq!(drop_count, 6);
        std::mem::drop(r);
        hs.force_gc();
        assert_eq!(drop_count, 7);
    });
}
