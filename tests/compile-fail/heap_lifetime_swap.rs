//! Being able to swap two heap bindings would utterly break the safety
//! enforcement regime. :)

extern crate cell_gc;
#[macro_use] extern crate cell_gc_derive;
mod pairs_aux;
use cell_gc::*;
use pairs_aux::*;

fn main() {
    with_heap(|mut hs1| {
        with_heap(|mut hs2| {
            let obj1 = alloc_null_pair(hs1);
            std::mem::swap(&mut hs1, &mut hs2);
            //~^ ERROR mismatched types
            //~| ERROR mismatched types
            //~| ERROR mismatched types
            //~| ERROR mismatched types
            let obj2 = alloc_pair(hs1, Value::Null, Value::Pair(obj1));
        });
    });
}
