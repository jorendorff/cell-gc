// ignore-test Not a test. Used by other tests

#![allow(dead_code)]  // Tests don't ordinarily use every feature and every accessor.

use std::rc::Rc;
use cell_gc::Heap;

gc_heap_type! {
    pub struct Pair / PairRef / PairStorage / PairRefStorage <'a> {
        head / set_head: Value<'a>,
        tail / set_tail: Value<'a>
    }
}

gc_heap_type! {
    pub enum Value / ValueStorage <'a> {
        Null,
        Int(i32),
        Str(Rc<String>),  // <-- equality is by value
        Pair(PairRef<'a>)  // <-- equality is by pointer
    }
}

/// Helper function to avoid having to write out `Pair` literals all over the place.
pub fn alloc_pair<'a>(heap: &mut Heap<'a>, head: Value<'a>, tail: Value<'a>) -> PairRef<'a> {
    heap.alloc(Pair { head: head, tail: tail })
}

/// Allocate a pair with the values `(null, null)`.
pub fn alloc_null_pair<'a>(heap: &mut Heap<'a>) -> PairRef<'a> {
    alloc_pair(heap, Value::Null, Value::Null)
}
