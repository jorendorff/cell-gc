extern crate bit_vec;

pub mod traits;
#[macro_use] mod macros;
mod pages;
mod heap;
mod gcref;

pub use heap::{Heap, with_heap};
pub use gcref::GCRef;

/// Return the number of allocations of a given type that fit in a "page".
/// (Unstable. This is a temporary hack for testing.)
pub fn page_capacity<'a, T: traits::IntoHeapAllocation<'a>>() -> usize {
    pages::TypedPage::<'a, T::In>::capacity()
}
