//! Sequential store buffers.

use pages;
use poison;
use ptr::{Pointer, UntypedPointer};
use std::vec;
use traits::InHeap;

/// The default capacity for the store buffer. When the store buffer fills, we
/// tell the post-write barrier to do a nursery collection.
const STORE_BUFFER_CAPACITY: usize = 4096;

/// A sequential store buffer, remembering cross generational edges from the
/// tenured heap into the nursery heap.
#[derive(Debug)]
pub struct StoreBuffer {
    rememembered: Vec<UntypedPointer>,
}

impl Default for StoreBuffer {
    fn default() -> StoreBuffer {
        StoreBuffer {
            rememembered: Vec::with_capacity(STORE_BUFFER_CAPACITY),
        }
    }
}

impl StoreBuffer {
    /// Insert the given nursery pointer into the remembered set, as it is
    /// referenced from a tenured object.
    pub unsafe fn insert<U>(&mut self, ptr: Pointer<U>) -> bool
    where
        U: InHeap
    {
        poison::assert_is_not_poisoned(ptr.as_raw());
        debug_assert!(!pages::get_mark_bit(ptr));

        pages::set_mark_bit(ptr);
        self.rememembered.push(ptr.into());
        self.rememembered.len() >= STORE_BUFFER_CAPACITY
    }

    /// Drain the store buffer's remembered pointers.
    ///
    /// # Safety
    ///
    /// This must only be called during GC, otherwise nursery collection roots
    /// will be lost and memory safety will eventually ensue.
    pub unsafe fn drain(&mut self) -> vec::Drain<UntypedPointer> {
        self.rememembered.drain(..)
    }
}
