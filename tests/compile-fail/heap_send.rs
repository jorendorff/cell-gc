// You can't store a non-`Send` value in the heap and then `Send` the heap.

extern crate cell_gc;

use cell_gc::{Heap, GcLeaf};
use std::rc::Rc;
use std::thread;
use std::sync::mpsc::channel;

fn main() {
    let (sender, receiver) = channel();
    let j = thread::spawn(move || {
        //~^ ERROR: the trait bound `*const (): std::marker::Send` is not satisfied in `cell_gc::pages::TypeId`
        //~| ERROR: the trait bound `*mut cell_gc::Heap: std::marker::Send` is not satisfied in `cell_gc::pages::PageSet`
        //~| ERROR: the trait bound `*mut cell_gc::pages::PageHeader: std::marker::Send` is not satisfied in `cell_gc::pages::PageSet`


        let mut heap = Heap::new();
        let r = Rc::new(true);
        heap.enter(|hs| {
            let _ = hs.alloc(GcLeaf::new(r.clone()));
        });
        sender.send(heap).unwrap();
        for i in 0..1000 {
            let _ = r.clone();
        }
    });

    j.join().unwrap();
}
