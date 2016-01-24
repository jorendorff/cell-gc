extern crate bit_vec;

#[macro_use] mod macros;
mod pages;
mod heap;

pub use heap::*;
pub use pages::HEAP_SIZE;

#[cfg(test)]
mod test;
