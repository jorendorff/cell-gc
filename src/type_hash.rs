//! Hashes to identify a particular type, that are statically generated at
//! compile time.

use std::hash::{Hash, Hasher};

/// A pre-computed hash that is generated at compilation time, and constant
/// during runtime.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct PreComputedTypeHash(u64);

impl PreComputedTypeHash {
    /// Construct a new pre-computed hash. Only use compile-time generated
    /// random constants here!
    #[inline]
    pub fn new(h: u64) -> PreComputedTypeHash {
        PreComputedTypeHash(h)
    }
}

impl Hash for PreComputedTypeHash {
    #[inline]
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher
    {
        // No need to hash anything, just write the u64 directly.
        state.write_u64(self.0);
    }
}
