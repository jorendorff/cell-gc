#[macro_export]
macro_rules! gc_ref_type {
    (pub struct $ref_type: ident / $storage_type: ident <'a> {
        $($field_name: ident / $field_setter_name: ident : $field_type: ty),*
    }) => {
        struct $storage_type <'a> {
            $($field_name: <$field_type as HeapInline<'a>>::Storage,)*
        }

        unsafe impl<'a> $crate::Mark<'a> for $crate::Markable<$storage_type<'a>> {
            unsafe fn mark(ptr: *mut $crate::Markable<$storage_type<'a>>) {
                if !ptr.is_null() && !(*ptr).marked {
                    (*ptr).marked = true;
                    $(
                        $crate::Mark::mark(
                            &mut (*ptr).value.$field_name
                                as *mut <$field_type as $crate::HeapInline<'a>>::Storage);
                    )*
                }
            }
        }

        #[allow(raw_pointer_derive)]
        #[derive(Clone, Debug, PartialEq)]
        pub struct $ref_type<'a>($crate::PinnedRef<'a, $crate::Markable<$storage_type<'a>>>);

        impl<'a> $ref_type<'a> {
            $(
                pub fn $field_name(&self) -> $field_type {
                    let ptr = self.0.ptr;
                    unsafe {
                        <$field_type as $crate::HeapInline<'a>>::from_heap(
                            &*self.0.heap,
                            &(*ptr).value.$field_name)
                    }
                }

                pub fn $field_setter_name(&self, v: $field_type) {
                    let ptr = self.0.ptr;
                    unsafe {
                        (*ptr).value.$field_name =
                            <$field_type as $crate::HeapInline<'a>>::to_heap(v);
                    }
                }
            )*
        }

        unsafe impl<'a> $crate::HeapInline<'a> for $ref_type<'a> {
            type Storage = *mut $crate::Markable<$storage_type<'a>>;

            fn to_heap(self) -> Self::Storage {
                self.0.ptr
            }

            unsafe fn from_heap(heap: &$crate::Heap<'a>, v: &Self::Storage) -> Self {
                $ref_type($crate::PinnedRef::new(heap, *v))
            }
        }

        impl<'a> $crate::GCRef for $ref_type<'a> {
            #[cfg(test)]
            fn address(&self) -> usize {
                unsafe { ::std::mem::transmute(self.0.ptr) }
            }
        }
    }
}


