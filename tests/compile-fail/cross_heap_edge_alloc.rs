//! You can't pass an object from one heap to another heap's `alloc` method.

#[macro_use] extern crate cellgc;
mod pairs_aux;
use cellgc::*;
use pairs_aux::*;

fn main() {
    with_heap(|heap1| {
        with_heap(|heap2| {
            let obj1 = alloc_null_pair(heap1);
            //~^ ERROR cannot infer an appropriate lifetime for lifetime parameter 'a in function call due to conflicting requirements
            let obj2 = alloc_pair(heap2, Value::Null, Value::Pair(obj1));
        });
    });
}
