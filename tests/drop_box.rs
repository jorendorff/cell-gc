//! Destructors are called when a heap struct with a Box field is dropped.

extern crate cell_gc;
#[macro_use]
extern crate cell_gc_derive;

#[derive(Clone, Debug)]
struct Dropper {
    addr: *mut i32,
}

impl Drop for Dropper {
    fn drop(&mut self) {
        unsafe {
            *(self.addr) += 1;
        }
    }
}

unsafe impl Send for Dropper {}

#[derive(IntoHeap)]
struct Obj<'h> {
    frob: Box<Dropper>,
    more: Option<ObjRef<'h>>,
}

fn main() {
    cell_gc::with_heap(|hs| {
        let mut drop_count: i32 = 0;
        let ptr: *mut i32 = &mut drop_count;

        hs.alloc(Obj {
            frob: Box::new(Dropper { addr: ptr }),
            more: None,
        });
        assert_eq!(drop_count, 0);
        hs.force_gc();
        assert_eq!(drop_count, 1);
    });
}
