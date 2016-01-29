//! A Box can be a field of a heap struct only if the boxed type has 'static lifetime.

// error-pattern: the type `core::option::Option<ThingRef<'a>>` does not fulfill the required lifetime

#[macro_use] extern crate cell_gc;
mod pairs_aux;
use cell_gc::*;
use pairs_aux::*;

gc_heap_type! {
    struct Thing / ThingRef / ThingStorage / ThingRefStorage <'a> {
        boxed_ref / set_boxed_ref: Box<Option<ThingRef<'a>>>
    }
}

fn main() {
    with_heap(|heap| {
        let thing_1 = heap.alloc(Thing { boxed_ref: Box::new(None) });
        let thing_2 = heap.alloc(Thing { boxed_ref: Box::new(Some(thing_1)) });
        std::mem::drop(thing_1);
        heap.force_gc();  // Boxes aren't marked; thing_1 is collected!
        let thing_1_revived = (*thing_2.boxed_ref()).unwrap();  // bad
    });
}
