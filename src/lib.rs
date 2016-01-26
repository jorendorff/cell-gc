extern crate bit_vec;

pub mod traits;
#[macro_use] mod macros;
mod pages;
mod heap;
mod gcref;

pub use heap::{Heap, with_heap};
pub use gcref::GCRef;
pub use pages::HEAP_SIZE;
