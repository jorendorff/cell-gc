//! A Box can be a field of a heap struct only if the boxed type has 'static lifetime.

// error-pattern: the trait bound `*const (): std::marker::Send` is not satisfied in `std::option::Option<ThingRef<'_>>`

#[macro_use] extern crate cell_gc;
#[macro_use] extern crate cell_gc_derive;
mod pairs_aux;
use cell_gc::*;
use pairs_aux::*;

#[derive(IntoHeap)]
struct Thing<'h> {
    boxed_ref: Box<Option<ThingRef<'h>>>
}

fn main() {
    with_heap(|hs| {
        let thing_1 = hs.alloc(Thing { boxed_ref: Box::new(None) });
        let thing_2 = hs.alloc(Thing { boxed_ref: Box::new(Some(thing_1)) });
        std::mem::drop(thing_1);
        hs.force_gc();  // Boxes aren't marked; thing_1 is collected!
        let thing_1_revived = (*thing_2.boxed_ref()).unwrap();  // bad
    });
}
