// You can't store a non-`Send` value in the heap and then `Send` the heap.

extern crate cell_gc;

use cell_gc::{Heap, GcLeaf};
use std::rc::Rc;
use std::thread;
use std::sync::mpsc::channel;

fn main() {
    let (sender, receiver) = channel();
    let j = thread::spawn(move || {
        let mut heap = Heap::new();
        let r = Rc::new(true);
        heap.enter(|hs| {
            let _ = hs.alloc(GcLeaf::new(r.clone()));
            //~^ ERROR: the trait bound `std::rc::Rc<bool>: std::marker::Send` is not satisfied
        });
        sender.send(heap).unwrap();
        for i in 0..1000 {
            let _ = r.clone();
        }
    });

    j.join().unwrap();
}
