use std;

/// GcLeaf can be used to embed just about anything in a GC heap type.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct GcLeaf<T: Clone + 'static> {
    // This private field has an underscore in the hopes that it is
    // less likely to collide with useful features of T.
    value_: T,
}

impl<T: Clone + 'static> GcLeaf<T> {
    /// Create a new GcLeaf wrapping a given value.
    pub fn new(value: T) -> GcLeaf<T> {
        GcLeaf { value_: value }
    }

    pub fn unwrap(self) -> T {
        self.value_
    }
}

impl<T: Clone + 'static> std::ops::Deref for GcLeaf<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.value_
    }
}

impl<T: Clone + 'static> std::ops::DerefMut for GcLeaf<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.value_
    }
}
