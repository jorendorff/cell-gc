use std;

/// GCLeaf can be used to embed just about anything in a GC heap type.
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct GCLeaf<T: Clone + 'static> {
    // This private field has an underscore in the hopes that it is
    // less likely to collide with useful features of T.
    value_: T
}

impl<T: Clone + 'static> GCLeaf<T> {
    /// Create a new GCLeaf wrapping a given value.
    pub fn new(value: T) -> GCLeaf<T> { GCLeaf { value_: value } }
}

impl<T: Clone + 'static> std::ops::Deref for GCLeaf<T> {
    type Target = T;
    fn deref(&self) -> &T { &self.value_ }
}

impl<T: Clone + 'static> std::ops::DerefMut for GCLeaf<T> {
    fn deref_mut(&mut self) -> &mut T { &mut self.value_ }
}
