extern crate bit_vec;

mod traits;
#[macro_use] mod macros;
mod pages;
mod heap;
mod refs;

pub use heap::{Heap, with_heap};
pub use refs::PinnedRef;
pub use traits::{InHeap, ToHeap};
pub use pages::HEAP_SIZE;

#[cfg(test)]
mod test;
