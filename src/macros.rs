#[macro_export]
macro_rules! gc_ref_type {
    (pub struct $ref_type:ident / $storage_type:ident <'a> {
        $($field_name:ident / $field_setter_name:ident : $field_type: ty),*
    }) => {
        struct $storage_type <'a> {
            $($field_name: <$field_type as $crate::HeapInline<'a>>::Storage,)*
        }

        unsafe impl<'a> $crate::Mark<'a> for $storage_type<'a> {
            unsafe fn mark(ptr: *mut $storage_type<'a>) {
                if !$crate::Heap::get_mark_bit(ptr) {
                    $crate::Heap::set_mark_bit(ptr);
                    $(
                        $crate::Mark::mark(
                            &mut (*ptr).$field_name
                                as *mut <$field_type as $crate::HeapInline<'a>>::Storage);
                    )*
                }
            }
        }

        unsafe impl<'a> $crate::Mark<'a> for *mut $storage_type<'a> {
            unsafe fn mark(field_ptr: *mut *mut $storage_type<'a>) {
                let ptr = *field_ptr;
                if !ptr.is_null() {
                    Mark::mark(ptr);
                }
            }
        }

        #[allow(raw_pointer_derive)]
        #[derive(Clone, Debug, PartialEq)]
        pub struct $ref_type<'a>($crate::PinnedRef<'a, $storage_type<'a>>);

        impl<'a> $ref_type<'a> {
            $(
                pub fn $field_name(&self) -> $field_type {
                    let ptr = self.0.ptr;
                    unsafe {
                        HeapInline::from_heap(&(*ptr).$field_name)
                    }
                }

                pub fn $field_setter_name(&self, v: $field_type) {
                    let ptr = self.0.ptr;
                    unsafe {
                        (*ptr).$field_name = HeapInline::to_heap(v);
                    }
                }
            )*
        }

        unsafe impl<'a> $crate::HeapInline<'a> for $ref_type<'a> {
            type Storage = *mut $storage_type<'a>;

            fn to_heap(self) -> Self::Storage {
                self.0.ptr
            }

            unsafe fn from_heap(v: &Self::Storage) -> Self {
                $ref_type($crate::PinnedRef::new(*v))
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

#[macro_export]
macro_rules! gc_inline_enum {
    { AS_ITEM $x:item } => { $x };
    { AS_EXPR $x:expr } => { $x };

    {
        PARSE_VARIANTS $helper_name:ident
        {}
        $( $etc:tt )*
    } => {
        gc_inline_enum! {
            $helper_name DONE
            $($etc)*
        }
    };

    {
        PARSE_VARIANTS $helper_name:ident
        { $variant_name:ident }
        $( $etc:tt )*
    } => {
        gc_inline_enum! {
            $helper_name VARIANT $variant_name NO_FIELDS
            {}
            $($etc)*
        }
    };

    {
        PARSE_VARIANTS $helper_name:ident
        { $variant_name:ident , $($more_variants:tt)* }
        $( $etc:tt )*
    } => {
        gc_inline_enum! {
            $helper_name VARIANT $variant_name NO_FIELDS
            { $($more_variants)* }
            $($etc)*
        }
    };

    {
        PARSE_VARIANTS $helper_name:ident
        { $variant_name:ident ( $($field_types:tt)* ) }
        $( $etc:tt )*
    } => {
        gc_inline_enum! {
            $helper_name VARIANT $variant_name ( $($field_types)* )
            { }
            $($etc)*
        }
    };

    {
        PARSE_VARIANTS $helper_name:ident
        { $variant_name:ident ( $($field_types:tt)* ), $($more_variants:tt)* }
        $( $etc:tt )*
    } => {
        gc_inline_enum! {
            $helper_name VARIANT $variant_name ( $($field_types)* )
            { $($more_variants)* }
            $($etc)*
        }
    };

    {
        DECLARE_STORAGE_TYPE DONE { $($accumulated_output:tt)* } $storage_type:ident
    } => {
        gc_inline_enum! {
            AS_ITEM
            enum $storage_type<'a> {
                $($accumulated_output)*
            }
        }
    };

    {
        DECLARE_STORAGE_TYPE VARIANT $variant_name:ident NO_FIELDS
        $more_variants:tt
        { $($accumulated_output:tt)* }
        $storage_type:ident
    } => {
        gc_inline_enum! {
            PARSE_VARIANTS DECLARE_STORAGE_TYPE $more_variants {
                $($accumulated_output)*
                $variant_name,
            }
            $storage_type
        }
    };

    {
        DECLARE_STORAGE_TYPE VARIANT $variant_name:ident ( $($field_type:ty),* )
        $more_variants:tt
        { $($accumulated_output:tt)* }
        $storage_type:ident
    } => {
        gc_inline_enum! {
            PARSE_VARIANTS DECLARE_STORAGE_TYPE $more_variants {
                $($accumulated_output)*
                $variant_name($(<$field_type as $crate::HeapInline<'a>>::Storage),*),
            }
            $storage_type
        }
    };

    {
        IMPL_MARK DONE { $($accumulated_output:tt)* } $storage_type:ident
    } => {
        gc_inline_enum! {
            AS_ITEM
            unsafe impl<'a> $crate::Mark<'a> for $storage_type<'a> {
                unsafe fn mark(ptr: *mut $storage_type<'a>) {
                    match *ptr {
                        $($accumulated_output)*
                    }
                }
            }
        }
    };

    {
        IMPL_MARK VARIANT $name:ident NO_FIELDS
        $more_variants:tt
        { $($accumulated_output:tt)* }
        $storage_type:ident
    } => {
        gc_inline_enum! {
            PARSE_VARIANTS IMPL_MARK $more_variants {
                $($accumulated_output)*
                $storage_type::$name => (),
            }
            $storage_type
        }
    };

    {
        IMPL_MARK VARIANT $name:ident ( $($field_type:ty),* )
        $($etc:tt)*
    } => {
        gc_inline_enum! {
            TYPES_TO_IDENTS ( $($field_type),*, ) () (a b c d e f g h i j k l m n o p q r s t u v w x y z)
            (IMPL_MARK CONTINUE_VARIANT $name $($etc)*)
        }
    };

    {
        IMPL_MARK CONTINUE_VARIANT $name:ident
        $more_variants:tt
        { $($accumulated_output:tt)* }
        $storage_type:ident
        ( $(($binding:ident : $field_type:ty))* )
    } => {
        gc_inline_enum! {
            PARSE_VARIANTS IMPL_MARK $more_variants {
                $($accumulated_output)*
                $storage_type::$name ( $(ref mut $binding),* ) => {
                    $( $crate::Mark::mark($binding as *mut <$field_type as HeapInline<'a>>::Storage); )*
                }
            }
            $storage_type
        }
    };

    { TYPES_TO_IDENTS () ($(($binding:ident : $btype:ty))*) $_leftovers:tt ($($ctn:tt)*) } => {
        gc_inline_enum! { $($ctn)* ($(($binding : $btype))*) }
    };
    {
        TYPES_TO_IDENTS
        ($t:ty, $($ts:ty),*)
        ($(($binding:ident : $btype:ty))*)
        ($id:ident $($ids:tt)*)
        ($($ctn:tt)*)
    } => {
        gc_inline_enum! {
            TYPES_TO_IDENTS
            ($($ts),*)
            ($(($binding : $btype))* ($id : $t))
            ($($ids)*)
            ($($ctn)*)
        }
    };

    {
        TO_HEAP DONE { $($accumulated_output:tt)* }
        $self_:expr, $_stack_type:ident / $_storage_type:ident
    } => {
        gc_inline_enum! {
            AS_EXPR
            match $self_ {
                $($accumulated_output)*
            }
        }
    };

    {
        TO_HEAP VARIANT $name:ident NO_FIELDS
        $more_variants:tt
        { $($accumulated_output:tt)* }
        $self_:expr, $stack_type:ident / $storage_type:ident
    } => {
        gc_inline_enum! {
            PARSE_VARIANTS TO_HEAP $more_variants {
                $($accumulated_output)*
                $stack_type::$name => $storage_type::$name,
            }
            $self_, $stack_type / $storage_type
        }
    };

    {
        TO_HEAP VARIANT $name:ident ( $($field_type:ty),* )
        $($etc:tt)*
    } => {
        gc_inline_enum! {
            TYPES_TO_IDENTS ( $($field_type),*, ) () (a b c d e f g h i j k l m n o p q r s t u v w x y z)
            (TO_HEAP CONTINUE_VARIANT $name $($etc)*)
        }
    };

    {
        TO_HEAP CONTINUE_VARIANT $name:ident
        $more_variants:tt
        { $($accumulated_output:tt)* }
        $self_:expr, $stack_type:ident / $storage_type:ident
        ( $(($binding:ident : $field_type:ty))* )
    } => {
        gc_inline_enum! {
            PARSE_VARIANTS TO_HEAP $more_variants {
                $($accumulated_output)*
                $stack_type::$name ( $($binding),* ) =>
                    $storage_type::$name( $(HeapInline::to_heap($binding)),* ),
            }
            $self_, $stack_type / $storage_type
        }
    };

    {
        FROM_HEAP DONE { $($accumulated_output:tt)* }
        $value:expr, $_stack_type:ident / $_storage_type:ident
    } => {
        gc_inline_enum! {
            AS_EXPR
            match $value {
                $($accumulated_output)*
            }
        }
    };

    {
        FROM_HEAP VARIANT $name:ident NO_FIELDS
        $more_variants:tt
        { $($accumulated_output:tt)* }
        $value:expr, $stack_type:ident / $storage_type:ident
    } => {
        gc_inline_enum! {
            PARSE_VARIANTS FROM_HEAP $more_variants {
                $($accumulated_output)*
                &$storage_type::$name => $stack_type::$name,
            }
            $value, $stack_type / $storage_type
        }
    };

    {
        FROM_HEAP VARIANT $name:ident ( $($field_type:ty),* )
        $($etc:tt)*
    } => {
        gc_inline_enum! {
            TYPES_TO_IDENTS ( $($field_type),*, ) () (a b c d e f g h i j k l m n o p q r s t u v w x y z)
            (FROM_HEAP CONTINUE_VARIANT $name $($etc)*)
        }
    };

    {
        FROM_HEAP CONTINUE_VARIANT $name:ident
        $more_variants:tt
        { $($accumulated_output:tt)* }
        $value:expr, $stack_type:ident / $storage_type:ident
        ( $(($binding:ident : $field_type:ty))* )
    } => {
        gc_inline_enum! {
            PARSE_VARIANTS FROM_HEAP $more_variants {
                $($accumulated_output)*
                &$storage_type::$name ( ref $($binding),* ) =>
                    $stack_type::$name( $(HeapInline::from_heap($binding)),* ),
            }
            $value, $stack_type / $storage_type
        }
    };

    {
        pub enum $stack_type:ident / $storage_type:ident <'a>
            $variants:tt
    } => {
        gc_inline_enum! {
            PARSE_VARIANTS DECLARE_STORAGE_TYPE $variants {}
            $storage_type
        }

        gc_inline_enum! {
            AS_ITEM
            #[derive(Debug, Clone, PartialEq)]
            pub enum $stack_type<'a>
                $variants
        }

        gc_inline_enum! {
            PARSE_VARIANTS IMPL_MARK $variants {}
            $storage_type
        }

        unsafe impl<'a> $crate::HeapInline<'a> for $stack_type<'a> {
            type Storage = $storage_type<'a>;

            fn to_heap(self) -> $storage_type<'a> {
                gc_inline_enum! {
                    PARSE_VARIANTS TO_HEAP $variants {}
                    self, $stack_type / $storage_type
                }
            }

            unsafe fn from_heap(v: &$storage_type<'a>) -> $stack_type<'a> {
                gc_inline_enum! {
                    PARSE_VARIANTS FROM_HEAP $variants {}
                    v, $stack_type / $storage_type
                }
            }
        }
    }
}
