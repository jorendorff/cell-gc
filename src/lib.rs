extern crate bit_vec;

#[macro_use] mod macros;
mod heap;

pub use heap::*;

#[cfg(test)]
mod test;
