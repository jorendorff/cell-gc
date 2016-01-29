//! Being able to swap two heap bindings would utterly break the safety
//! enforcement regime. :)

#[macro_use] extern crate cell_gc;
mod pairs_aux;
use cell_gc::*;
use pairs_aux::*;

fn main() {
    with_heap(|mut heap1| {
        with_heap(|mut heap2| {
            let obj1 = alloc_null_pair(heap1);
            std::mem::swap(&mut heap1, &mut heap2);
            //~^ ERROR mismatched types
            //~| expected `&mut &mut cell_gc::heap::Heap<'a>`
            //~| found `&mut &mut cell_gc::heap::Heap<'a>`
            //~| lifetime mismatch
            //~| ERROR mismatched types
            //~| expected `&mut &mut cell_gc::heap::Heap<'a>`
            //~| found `&mut &mut cell_gc::heap::Heap<'a>`
            //~| lifetime mismatch
            //~| ERROR mismatched types
            //~| expected `&mut &mut cell_gc::heap::Heap<'a>`
            //~| found `&mut &mut cell_gc::heap::Heap<'a>`
            //~| lifetime mismatch
            //~| ERROR mismatched types
            //~| expected `&mut &mut cell_gc::heap::Heap<'a>`
            //~| found `&mut &mut cell_gc::heap::Heap<'a>`
            //~| lifetime mismatch
            let obj2 = alloc_pair(heap1, Value::Null, Value::Pair(obj1));
        });
    });
}
