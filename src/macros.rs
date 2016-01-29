#[macro_export]
macro_rules! gc_ref_type {
    { @as_item $it:item } => { $it };

    {
        pub struct $($x:tt)*
    } => {
        gc_ref_type! {
            @gc_heap_struct (pub) struct $($x)*
        }
    };
    {
        struct $($x:tt)*
    } => {
        gc_ref_type! {
            @gc_heap_struct () struct $($x)*
        }
    };

    {
        @gc_heap_struct ( $($maybe_pub:tt)* )
        struct $fields_type:ident / $ref_type:ident / $storage_type:ident / $ref_storage_type:ident <'a> {
            $($field_name:ident / $field_setter_name:ident : $field_type: ty),*
        }
    } => {
        // === $storage_type: the InHeap representation of the struct
        gc_ref_type! {
            @as_item
            $($maybe_pub)* struct $storage_type<'a> {
                $( pub $field_name: <$field_type as $crate::traits::IntoHeap<'a>>::In ),*
            }
        }

        unsafe impl<'a> $crate::traits::InHeap<'a> for $storage_type<'a> {
            type Out = $fields_type<'a>;

            unsafe fn mark(&self) {
                if !$crate::Heap::get_mark_bit(self) {
                    $crate::Heap::set_mark_bit(self);
                    $(
                        $crate::traits::InHeap::mark(&self.$field_name);
                    )*
                }
            }

            unsafe fn from_heap(&self) -> $fields_type<'a> {
                $fields_type {
                    $( $field_name: $crate::traits::InHeap::from_heap(&self.$field_name) ),*
                }
            }
        }

        // === $fields_type: A safe version of the struct
        gc_ref_type! {
            @as_item
            #[derive(Clone, Debug)]
            $($maybe_pub)* struct $fields_type<'a> {
                $( pub $field_name: $field_type ),*
            }
        }

        unsafe impl<'a> $crate::traits::IntoHeap<'a> for $fields_type<'a> {
            type In = $storage_type<'a>;

            fn into_heap(self) -> $storage_type<'a> {
                $storage_type {
                    $( $field_name: $crate::traits::IntoHeap::into_heap(self.$field_name) ),*
                }
            }
        }

        impl<'a> $crate::traits::IntoHeapAllocation<'a> for $fields_type<'a> {
            type Ref = $ref_type<'a>;

            fn wrap_gcref(gcref: $crate::GCRef<'a, $storage_type<'a>>) -> $ref_type<'a> {
                $ref_type(gcref)
            }
        }

        // === $ref_storage_type: The InHeap representation of a reference to the struct
        gc_ref_type! {
            @as_item
            #[derive(Clone, Copy, Debug, PartialEq, Eq)]
            $($maybe_pub)* struct $ref_storage_type<'a>(*mut $storage_type<'a>);
        }

        unsafe impl<'a> $crate::traits::InHeap<'a> for $ref_storage_type<'a> {
            type Out = $ref_type<'a>;

            unsafe fn mark(&self) {
                let ptr = self.0;
                if !ptr.is_null() {
                    $crate::traits::InHeap::mark(&*ptr);
                }
            }

            unsafe fn from_heap(&self) -> $ref_type<'a> {
                $ref_type($crate::GCRef::new(self.0))
            }
        }

        // === $ref_type: A safe reference to the struct
        gc_ref_type! {
            @as_item
            #[derive(Clone, Debug, PartialEq, Eq)]
            $($maybe_pub)* struct $ref_type<'a>($crate::GCRef<'a, $storage_type<'a>>);
        }

        unsafe impl<'a> $crate::traits::IntoHeap<'a> for $ref_type<'a> {
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
                        $crate::traits::InHeap::from_heap(&(*ptr).$field_name)
                    }
                }

                pub fn $field_setter_name(&self, v: $field_type) {
                    let ptr = self.0.as_mut_ptr();
                    let u = $crate::traits::IntoHeap::into_heap(v);
                    unsafe {
                        (*ptr).$field_name = u;
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
    { @as_item $x:item } => { $x };
    { @as_expr $x:expr } => { $x };

    {
        @parse_variants $helper_name:ident
        {}
        $( $etc:tt )*
    } => {
        gc_inline_enum! {
            $helper_name DONE
            $($etc)*
        }
    };

    {
        @parse_variants $helper_name:ident
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
        @parse_variants $helper_name:ident
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
        @parse_variants $helper_name:ident
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
        @parse_variants $helper_name:ident
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
        @for_each_variant $_helper:tt {} $all_results:tt ($($ctn:tt)*)
    } => {
        gc_inline_enum! { $($ctn)* $all_results }
    };

    {
        @for_each_variant ($($helper:tt)*)
        { $variant_name:ident } $acc:tt $ctn:tt
    } => {
        gc_inline_enum! {
            $($helper)* $variant_name NO_FIELDS
                (@next_variant ($($helper)*) {} $acc $ctn)
        }
    };

    {
        @for_each_variant ($($helper:tt)*)
        { $variant_name:ident , $($more_variants:tt)* }
        $acc:tt $ctn:tt
    } => {
        gc_inline_enum! {
            $($helper)* $variant_name NO_FIELDS
                (@next_variant ($($helper)*) { $($more_variants)* } $acc $ctn)
        }
    };

    {
        @for_each_variant ($($helper:tt)*)
        { $variant_name:ident ( $($field_types:tt)* ) }
        $acc:tt $ctn:tt
    } => {
        gc_inline_enum! {
            $($helper)* $variant_name ( $($field_types)* )
                (@next_variant ($($helper)*) {} $acc $ctn)
        }
    };

    {
        @for_each_variant ($($helper:tt)*)
        { $variant_name:ident ( $($field_types:tt)* ), $($more_variants:tt)*  }
        $acc:tt $ctn:tt
    } => {
        gc_inline_enum! {
            $($helper)* $variant_name ( $($field_types)* )
                (@next_variant ($($helper)*) { $($more_variants)* } $acc $ctn)
        }
    };

    {
        @next_variant $helper:tt $more_variants:tt { $($acc:tt)* } $ctn:tt { $($rv:tt)* }
    } => {
        gc_inline_enum! {
            @for_each_variant $helper $more_variants { $($acc)* $($rv)* } $ctn
        }
    };

    {
        DECLARE_STORAGE_TYPE DONE { $($accumulated_output:tt)* }
        ( $($maybe_pub:tt)* ) $storage_type:ident
    } => {
        gc_inline_enum! {
            @as_item
            $($maybe_pub)*
            enum $storage_type<'a> {
                $($accumulated_output)*
            }
        }
    };

    {
        DECLARE_STORAGE_TYPE VARIANT $variant_name:ident NO_FIELDS
        $more_variants:tt
        { $($accumulated_output:tt)* }
        ( $($maybe_pub:tt)* ) $storage_type:ident
    } => {
        gc_inline_enum! {
            @parse_variants DECLARE_STORAGE_TYPE $more_variants {
                $($accumulated_output)*
                $variant_name,
            }
            ( $($maybe_pub)* ) $storage_type
        }
    };

    {
        DECLARE_STORAGE_TYPE VARIANT $variant_name:ident ( $($field_type:ty),* )
        $more_variants:tt
        { $($accumulated_output:tt)* }
        ( $($maybe_pub:tt)* ) $storage_type:ident
    } => {
        gc_inline_enum! {
            @parse_variants DECLARE_STORAGE_TYPE $more_variants {
                $($accumulated_output)*
                $variant_name($(<$field_type as $crate::traits::IntoHeap<'a>>::In),*),
            }
            ( $($maybe_pub)* ) $storage_type
        }
    };

    {
        @mark_expr ($self_ref:expr) { $($arms:tt)* }
    } => {
        gc_inline_enum! {
            @as_expr
            match *$self_ref {
                $($arms)*
            }
        }
    };

    {
        @mark_variant $storage_type:ident
            $name:ident NO_FIELDS ($($ctn:tt)*)
    } => {
        gc_inline_enum! {
            $($ctn)* { $storage_type::$name => (), }
        }
    };

    {
        @mark_variant $storage_type:ident
            $name:ident ( $($field_type:ty),* ) $ctn:tt
    } => {
        gc_inline_enum! {
            @zip_idents_with_types (a b c d e f g h i j k l m n o p q r s t u v w x y z)
                ( $($field_type),*, ) ()
                (@mark_variant_continued $storage_type $name $ctn)
        }
    };

    {
        @mark_variant_continued $storage_type:ident $name:ident ($($ctn:tt)*)
            ( $(($binding:ident : $field_type:ty))* )
    } => {
        gc_inline_enum! {
            $($ctn)* {
                $storage_type::$name ( $(ref $binding),* ) => {
                    $( $crate::traits::InHeap::mark($binding); )*
                },
            }
        }
    };

    {
        @into_heap_expr ($self_:expr)
        { $($accumulated_output:tt)* }
    } => {
        gc_inline_enum! {
            @as_expr
            match $self_ {
                $($accumulated_output)*
            }
        }
    };

    {
        @variant_into_heap $stack_type:ident $storage_type:ident
            $name:ident NO_FIELDS
            ($($ctn:tt)*)
    } => {
        gc_inline_enum! {
            $($ctn)* { $stack_type::$name => $storage_type::$name, }
        }
    };

    {
        @variant_into_heap $stack_type:ident $storage_type:ident
            $name:ident ( $($field_type:ty),* )
            $ctn:tt
    } => {
        gc_inline_enum! {
            @zip_idents_with_types (a b c d e f g h i j k l m n o p q r s t u v w x y z)
                ( $($field_type),*, ) ()
                (@variant_into_heap_continued $stack_type $storage_type $name $ctn)
        }
    };

    {
        @zip_idents_with_types $_leftovers:tt () ($(($binding:ident : $btype:ty))*) ($($ctn:tt)*)
    } => {
        gc_inline_enum! { $($ctn)* ($(($binding : $btype))*) }
    };
    {
        @zip_idents_with_types
        ($id:ident $($ids:tt)*)
        ($t:ty, $($ts:ty),*)
        ($($acc:tt)*)
        $ctn:tt
    } => {
        gc_inline_enum! {
            @zip_idents_with_types
            ($($ids)*)
            ($($ts),*)
            ($($acc)* ($id : $t))
            $ctn
        }
    };

    {
        @variant_into_heap_continued $stack_type:ident $storage_type:ident $name:ident ($($ctn:tt)*)
            ( $(($binding:ident : $field_type:ty))* )
    } => {
        gc_inline_enum! {
            $($ctn)* {
                $stack_type::$name ( $($binding),* ) =>
                    $storage_type::$name( $($crate::traits::IntoHeap::into_heap($binding)),* ),
            }
        }
    };

    {
        @from_heap_expr ($self_ref:expr) { $($arms:tt)* }
    } => {
        gc_inline_enum! {
            @as_expr
            match $self_ref {
                $($arms)*
            }
        }
    };

    {
        @from_heap_variant $stack_type:ident $storage_type:ident
            $name:ident NO_FIELDS ($($ctn:tt)*)
    } => {
        gc_inline_enum! {
            $($ctn)* { &$storage_type::$name => $stack_type::$name, }
        }
    };

    {
        @from_heap_variant $stack_type:ident $storage_type:ident
            $name:ident ( $($field_type:ty),* ) $ctn:tt
    } => {
        gc_inline_enum! {
            @zip_idents_with_types (a b c d e f g h i j k l m n o p q r s t u v w x y z)
                ( $($field_type),*, ) ()
                (@from_heap_variant_continued $stack_type $storage_type $name $ctn)
        }
    };

    {
        @from_heap_variant_continued $stack_type:ident $storage_type:ident $name:ident ($($ctn:tt)*)
            ( $(($binding:ident : $field_type:ty))* )
    } => {
        gc_inline_enum! {
            $($ctn)* {
                &$storage_type::$name ( ref $($binding),* ) =>
                    $stack_type::$name( $($crate::traits::InHeap::from_heap($binding)),* ),
            }
        }
    };

    {
        pub enum $($x:tt)*
    } => {
        gc_inline_enum! {
            @gc_heap_enum (pub) enum $($x)*
        }
    };

    {
        enum $($x:tt)*
    } => {
        gc_inline_enum! {
            @gc_heap_enum () enum $($x)*
        }
    };

    {
        @gc_heap_enum
        ($($maybe_pub:tt)*)
        enum $stack_type:ident / $storage_type:ident <'a>
        $variants:tt
    } => {
        gc_inline_enum! {
            @parse_variants DECLARE_STORAGE_TYPE $variants {}
            ( $($maybe_pub)* ) $storage_type
        }

        gc_inline_enum! {
            @as_item
            #[derive(Debug, Clone, PartialEq)]
            $($maybe_pub)*
            enum $stack_type<'a>
                $variants
        }

        unsafe impl<'a> $crate::traits::InHeap<'a> for $storage_type<'a> {
            type Out = $stack_type<'a>;

            unsafe fn mark(&self) {
                gc_inline_enum! {
                    @for_each_variant (@mark_variant $storage_type) $variants {}
                    (@mark_expr (self))
                }
            }

            unsafe fn from_heap(&self) -> $stack_type<'a> {
                gc_inline_enum! {
                    @for_each_variant (@from_heap_variant $stack_type $storage_type) $variants {}
                    (@from_heap_expr (self))
                }
            }
        }

        unsafe impl<'a> $crate::traits::IntoHeap<'a> for $stack_type<'a> {
            type In = $storage_type<'a>;

            fn into_heap(self) -> $storage_type<'a> {
                gc_inline_enum! {
                    @for_each_variant (@variant_into_heap $stack_type $storage_type) $variants {}
                    (@into_heap_expr (self))
                }
            }
        }
    }
}
