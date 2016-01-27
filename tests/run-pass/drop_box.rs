//! Destructors are called when a heap struct with a Box field is dropped.

#[macro_use] extern crate toy_gc;

#[derive(Clone, Debug)]
struct Dropper {
    addr: *mut i32,
}

impl Drop for Dropper {
    fn drop(&mut self) {
        unsafe { *(self.addr) += 1; }
    }
}

gc_ref_type! {
    struct Obj / ObjRef / ObjStorage / ObjRefStorage <'a> {
        frob / set_frob: Box<Dropper>,
        more / set_more: Option<ObjRef<'a>>
    }
}

fn main() {
    toy_gc::with_heap(|heap| {
        let mut drop_count: i32 = 0;
        let ptr: *mut i32 = &mut drop_count;

        heap.alloc(Obj {
            frob: Box::new(Dropper { addr: ptr }),
            more: None
        });
        assert_eq!(drop_count, 0);
        heap.force_gc();
        assert_eq!(drop_count, 1);
    });
}
