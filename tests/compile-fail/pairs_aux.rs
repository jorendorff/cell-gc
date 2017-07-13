// ignore-test Not a test. Used by other tests

#![allow(dead_code)] // Tests don't ordinarily use every feature and every accessor.

use std::sync::Arc;
use cell_gc::*;

#[derive(IntoHeap)]
pub struct Pair<'h> {
    pub head: Value<'h>,
    pub tail: Value<'h>,
}

#[derive(IntoHeap)]
pub enum Value<'h> {
    Null,
    Int(i32),
    Str(Arc<String>), // <-- equality is by value
    Pair(PairRef<'h>), // <-- equality is by pointer
}

/// Helper function to avoid having to write out `Pair` literals all over the place.
pub fn alloc_pair<'h>(hs: &mut GcHeapSession<'h>, head: Value<'h>, tail: Value<'h>) -> PairRef<'h> {
    hs.alloc(Pair {
                 head: head,
                 tail: tail,
             })
}

/// Allocate a pair with the values `(null, null)`.
pub fn alloc_null_pair<'h>(hs: &mut GcHeapSession<'h>) -> PairRef<'h> {
    alloc_pair(hs, Value::Null, Value::Null)
}
