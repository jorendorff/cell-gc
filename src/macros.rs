#[macro_export]
macro_rules! gc_ref_type {
    {
        pub struct $fields_type:ident / $ref_type:ident / $storage_type:ident / $ref_storage_type:ident <'a> {
            $($field_name:ident / $field_setter_name:ident : $field_type: ty),*
        }
    } => {
        // === $storage_type: the InHeap representation of the struct
        pub struct $storage_type<'a> {
            $($field_name: <$field_type as $crate::IntoHeap<'a>>::In),*
        }

        unsafe impl<'a> $crate::InHeap<'a> for $storage_type<'a> {
            type Out = $fields_type<'a>;

            unsafe fn mark(ptr: *mut $storage_type<'a>) {
                if !$crate::Heap::get_mark_bit(ptr) {
                    $crate::Heap::set_mark_bit(ptr);
                    $(
                        $crate::InHeap::mark(&mut (*ptr).$field_name);
                    )*
                }
            }

            unsafe fn from_heap(&self) -> $fields_type<'a> {
                $fields_type {
                    $( $field_name: $crate::InHeap::from_heap(&self.$field_name) ),*
                }
            }
        }

        // === $fields_type: A safe version of the struct
        #[derive(Clone, Debug, PartialEq)]
        pub struct $fields_type<'a> {
            $(pub $field_name: $field_type),*
        }

        unsafe impl<'a> $crate::IntoHeap<'a> for $fields_type<'a> {
            type In = $storage_type<'a>;

            fn into_heap(self) -> $storage_type<'a> {
                $storage_type {
                    $( $field_name: $crate::IntoHeap::into_heap(self.$field_name) ),*
                }
            }
        }

        impl<'a> $crate::IntoHeapAllocation<'a> for $fields_type<'a> {
            type Ref = $ref_type<'a>;

            fn wrap_gcref(gcref: GCRef<'a, $storage_type<'a>>) -> $ref_type<'a> {
                $ref_type(gcref)
            }
        }

        // === $ref_storage_type: The InHeap representation of a reference to the struct
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        pub struct $ref_storage_type<'a>(*mut $storage_type<'a>);

        unsafe impl<'a> $crate::InHeap<'a> for $ref_storage_type<'a> {
            type Out = $ref_type<'a>;

            unsafe fn mark(ptr_to_ptr: *mut $ref_storage_type<'a>) {
                let $ref_storage_type(ptr) = *ptr_to_ptr;
                if !ptr.is_null() {
                    InHeap::mark(ptr);
                }
            }

            unsafe fn from_heap(&self) -> $ref_type<'a> {
                $ref_type($crate::GCRef::new(self.0))
            }
        }

        // === $ref_type: A safe reference to the struct
        #[derive(Clone, Debug, PartialEq, Eq)]
        pub struct $ref_type<'a>($crate::GCRef<'a, $storage_type<'a>>);

        unsafe impl<'a> $crate::IntoHeap<'a> for $ref_type<'a> {
            type In = $ref_storage_type<'a>;

            fn into_heap(self) -> $ref_storage_type<'a> {
                $ref_storage_type(self.0.as_mut_ptr())
            }
        }

        impl<'a> $ref_type<'a> {
            // Field accessors.
            $(
                pub fn $field_name(&self) -> $field_type {
                    let ptr = self.0.as_ptr();
                    unsafe {
                        InHeap::from_heap(&(*ptr).$field_name)
                    }
                }

                pub fn $field_setter_name(&self, v: $field_type) {
                    let ptr = self.0.as_mut_ptr();
                    unsafe {
                        (*ptr).$field_name = IntoHeap::into_heap(v);
                    }
                }
            )*

            pub fn as_mut_ptr(&self) -> *mut $storage_type<'a> {
                self.0.as_mut_ptr()
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
            pub enum $storage_type<'a> {
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
                $variant_name($(<$field_type as $crate::IntoHeap<'a>>::In),*),
            }
            $storage_type
        }
    };

    {
        MARK DONE { $($accumulated_output:tt)* } $ptr:ident, $_storage_type:ty
    } => {
        gc_inline_enum! {
            AS_EXPR
            match *$ptr {
                $($accumulated_output)*
            }
        }
    };

    {
        MARK VARIANT $name:ident NO_FIELDS
        $more_variants:tt
        { $($accumulated_output:tt)* }
        $ptr:ident, $storage_type:ident
    } => {
        gc_inline_enum! {
            PARSE_VARIANTS MARK $more_variants {
                $($accumulated_output)*
                $storage_type::$name => (),
            }
            $ptr, $storage_type
        }
    };

    {
        MARK VARIANT $name:ident ( $($field_type:ty),* )
        $($etc:tt)*
    } => {
        gc_inline_enum! {
            TYPES_TO_IDENTS ( $($field_type),*, ) () (a b c d e f g h i j k l m n o p q r s t u v w x y z)
            (MARK CONTINUE_VARIANT $name $($etc)*)
        }
    };

    {
        MARK CONTINUE_VARIANT $name:ident
        $more_variants:tt
        { $($accumulated_output:tt)* }
        $ptr:ident, $storage_type:ident
        ( $(($binding:ident : $field_type:ty))* )
    } => {
        gc_inline_enum! {
            PARSE_VARIANTS MARK $more_variants {
                $($accumulated_output)*
                $storage_type::$name ( $(ref mut $binding),* ) => {
                    $( $crate::InHeap::mark($binding as *mut <$field_type as IntoHeap<'a>>::In); )*
                }
            }
            $ptr, $storage_type
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
                    $storage_type::$name( $(IntoHeap::into_heap($binding)),* ),
            }
            $self_, $stack_type / $storage_type
        }
    };

    {
        FROM_HEAP DONE { $($accumulated_output:tt)* }
        $self_ref:expr, $_stack_type:ident / $_storage_type:ident
    } => {
        gc_inline_enum! {
            AS_EXPR
            match $self_ref {
                $($accumulated_output)*
            }
        }
    };

    {
        FROM_HEAP VARIANT $name:ident NO_FIELDS
        $more_variants:tt
        { $($accumulated_output:tt)* }
        $self_ref:expr, $stack_type:ident / $storage_type:ident
    } => {
        gc_inline_enum! {
            PARSE_VARIANTS FROM_HEAP $more_variants {
                $($accumulated_output)*
                &$storage_type::$name => $stack_type::$name,
            }
            $self_ref, $stack_type / $storage_type
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
        $self_ref:expr, $stack_type:ident / $storage_type:ident
        ( $(($binding:ident : $field_type:ty))* )
    } => {
        gc_inline_enum! {
            PARSE_VARIANTS FROM_HEAP $more_variants {
                $($accumulated_output)*
                &$storage_type::$name ( ref $($binding),* ) =>
                    $stack_type::$name( $($crate::InHeap::from_heap($binding)),* ),
            }
            $self_ref, $stack_type / $storage_type
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

        unsafe impl<'a> $crate::InHeap<'a> for $storage_type<'a> {
            type Out = $stack_type<'a>;

            unsafe fn mark(ptr: *mut $storage_type<'a>) {
                gc_inline_enum! {
                    PARSE_VARIANTS MARK $variants {}
                    ptr, $storage_type
                }
            }

            unsafe fn from_heap(&self) -> $stack_type<'a> {
                gc_inline_enum! {
                    PARSE_VARIANTS FROM_HEAP $variants {}
                    self, $stack_type / $storage_type
                }
            }
        }

        unsafe impl<'a> $crate::IntoHeap<'a> for $stack_type<'a> {
            type In = $storage_type<'a>;

            fn into_heap(self) -> $storage_type<'a> {
                gc_inline_enum! {
                    PARSE_VARIANTS TO_HEAP $variants {}
                    self, $stack_type / $storage_type
                }
            }
        }
    }
}
