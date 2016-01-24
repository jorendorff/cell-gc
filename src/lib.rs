extern crate bit_vec;

mod traits;
#[macro_use] mod macros;
mod pages;
mod heap;

pub use heap::*;
pub use traits::{Mark, HeapInline, GCThing, GCRef};
pub use pages::HEAP_SIZE;

#[cfg(test)]
mod test;
