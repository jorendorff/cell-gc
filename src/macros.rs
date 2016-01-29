#[macro_export]
macro_rules! gc_heap_type {
    // Top-level macros.
    { pub enum $($etc:tt)* } =>
    { gc_heap_type! { @gc_heap_enum (pub) enum $($etc)* } };

    { enum $($etc:tt)* } =>
    { gc_heap_type! { @gc_heap_enum () enum $($etc)* } };

    { pub struct $($etc:tt)* } =>
    { gc_heap_type! { @gc_heap_struct (pub) struct $($etc)* } };

    { struct $($etc:tt)* } =>
    { gc_heap_type! { @gc_heap_struct () struct $($etc)* } };

    // Helpers used by almost every macro.
    { @as_item $x:item } => { $x };
    { @as_expr $x:expr } => { $x };

    // The main helper macro for expanding a struct.
    {
        @gc_heap_struct ( $($maybe_pub:tt)* )
        struct $fields_type:ident / $ref_type:ident / $storage_type:ident / $ref_storage_type:ident <'a> {
            $($field_name:ident / $field_setter_name:ident : $field_type: ty),*
        }
    } => {
        // === $storage_type: the in-heap representation of the struct
        gc_heap_type! {
            @as_item
            $($maybe_pub)* struct $storage_type<'a> {
                $( pub $field_name: <$field_type as $crate::traits::IntoHeap<'a>>::In ),*
            }
        }

        // === $fields_type: A safe version of the struct
        gc_heap_type! {
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

            unsafe fn mark(storage: &$storage_type<'a>) {
                if !$crate::Heap::get_mark_bit::<Self>(storage) {
                    $crate::Heap::set_mark_bit::<Self>(storage);
                    $(
                        <$field_type as $crate::traits::IntoHeap>::mark(&storage.$field_name);
                    )*
                }
            }

            unsafe fn from_heap(storage: &$storage_type<'a>) -> $fields_type<'a> {
                $fields_type {
                    $( $field_name: $crate::traits::IntoHeap::from_heap(&storage.$field_name) ),*
                }
            }
        }

        impl<'a> $crate::traits::IntoHeapAllocation<'a> for $fields_type<'a> {
            type Ref = $ref_type<'a>;

            fn wrap_gcref(gcref: $crate::GCRef<'a, $fields_type<'a>>) -> $ref_type<'a> {
                $ref_type(gcref)
            }
        }

        // === $ref_storage_type: The in-heap representation of a reference to the struct
        gc_heap_type! {
            @as_item
            #[derive(Clone, Copy, Debug, PartialEq, Eq)]
            $($maybe_pub)* struct $ref_storage_type<'a>(*mut $storage_type<'a>);
        }

        // === $ref_type: A safe reference to the struct
        gc_heap_type! {
            @as_item
            #[derive(Clone, Debug, PartialEq, Eq)]
            $($maybe_pub)* struct $ref_type<'a>($crate::GCRef<'a, $fields_type<'a>>);
        }

        unsafe impl<'a> $crate::traits::IntoHeap<'a> for $ref_type<'a> {
            type In = $ref_storage_type<'a>;

            fn into_heap(self) -> $ref_storage_type<'a> {
                $ref_storage_type(self.0.as_mut_ptr())
            }

            unsafe fn from_heap(storage: &$ref_storage_type<'a>) -> $ref_type<'a> {
                $ref_type($crate::GCRef::new(storage.0))
            }

            unsafe fn mark(storage: &$ref_storage_type<'a>) {
                let ptr = storage.0;
                if !ptr.is_null() {
                    <$fields_type<'a> as $crate::traits::IntoHeap>::mark(&*ptr);
                }
            }
        }

        impl<'a> $ref_type<'a> {
            // Field accessors.
            $(
                pub fn $field_name(&self) -> $field_type {
                    let ptr = self.0.as_ptr();
                    unsafe {
                        $crate::traits::IntoHeap::from_heap(&(*ptr).$field_name)
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
    };

    // `gc_heap_type! { @for_each_variant ($helper*) {$variants*} {} ($ctn*) }`
    //
    // This helper is like `concatMap` for mapping enum variants through
    // another helper macro. `{$variants*}` should be the body of an enum item.
    //
    // For each variant in $variants, this calls
    // `gc_heap_type! { $helper $variant_name $variant_fields ($ctn2*) }`.
    // For variants that have no fields, it passes `NO_FIELDS` to the
    // $variant_fields argument.  The helper must call back:
    // `gc_heap_type! { $ctn2* { ...tokens... } }`.
    //
    // @for_each_variant accumulates all the tokens passed back by the calls to
    // $helper. After the last call to $helper, it passes all the results to
    // its continuation, calling `gc_heap_type! { $ctn* { ...all tokens... } }`.
    {
        @for_each_variant $_helper:tt {} $all_results:tt ($($ctn:tt)*)
    } => {
        gc_heap_type! { $($ctn)* $all_results }
    };

    {
        @for_each_variant ($($helper:tt)*)
        { $variant_name:ident } $acc:tt $ctn:tt
    } => {
        gc_heap_type! {
            $($helper)* $variant_name NO_FIELDS
                (@next_variant ($($helper)*) {} $acc $ctn)
        }
    };

    {
        @for_each_variant ($($helper:tt)*)
        { $variant_name:ident , $($more_variants:tt)* }
        $acc:tt $ctn:tt
    } => {
        gc_heap_type! {
            $($helper)* $variant_name NO_FIELDS
                (@next_variant ($($helper)*) { $($more_variants)* } $acc $ctn)
        }
    };

    {
        @for_each_variant ($($helper:tt)*)
        { $variant_name:ident ( $($field_types:tt)* ) }
        $acc:tt $ctn:tt
    } => {
        gc_heap_type! {
            $($helper)* $variant_name ( $($field_types)* )
                (@next_variant ($($helper)*) {} $acc $ctn)
        }
    };

    {
        @for_each_variant ($($helper:tt)*)
        { $variant_name:ident ( $($field_types:tt)* ), $($more_variants:tt)*  }
        $acc:tt $ctn:tt
    } => {
        gc_heap_type! {
            $($helper)* $variant_name ( $($field_types)* )
                (@next_variant ($($helper)*) { $($more_variants)* } $acc $ctn)
        }
    };

    {
        @next_variant $helper:tt $more_variants:tt { $($acc:tt)* } $ctn:tt { $($rv:tt)* }
    } => {
        gc_heap_type! {
            @for_each_variant $helper $more_variants { $($acc)* $($rv)* } $ctn
        }
    };

    // `gc_heap_type! { @zip_idents_with_types ($alphabet*) ($types,*) () ($ctn*) }`
    //
    // This helper macro pairs each type in `$types,*` with a letter of the `$alphabet*`.
    // It passes the resulting pairs to `$ctn`. So, for example, this:
    //     @zip_idents_with_types (a b c d e f g) (i32, String) () (@continue_here)
    // boils down to this:
    //     @continue_here ((a: i32) (b: String))
    {
        @zip_idents_with_types $_leftovers:tt () ($(($binding:ident : $btype:ty))*) ($($ctn:tt)*)
    } => {
        gc_heap_type! { $($ctn)* ($(($binding : $btype))*) }
    };
    {
        @zip_idents_with_types
        ($id:ident $($ids:tt)*)
        ($t:ty, $($ts:ty),*)
        ($($acc:tt)*)
        $ctn:tt
    } => {
        gc_heap_type! {
            @zip_idents_with_types
            ($($ids)*)
            ($($ts),*)
            ($($acc)* ($id : $t))
            $ctn
        }
    };

    // Helper macros for declaring an in-heap enum.
    {
        @enum_in_heap_variant $variant_name:ident NO_FIELDS ($($ctn:tt)*)
    } => {
        gc_heap_type! {
            $($ctn)* { $variant_name, }
        }
    };

    {
        @enum_in_heap_variant $variant_name:ident ( $($field_type:ty),* ) ($($ctn:tt)*)
    } => {
        gc_heap_type! {
            $($ctn)* {
                $variant_name($(<$field_type as $crate::traits::IntoHeap<'a>>::In),*),
            }
        }
    };

    {
        @enum_declare_in_heap_type ( $($maybe_pub:tt)* ) $storage_type:ident
            { $($variants:tt)* }
    } => {
        gc_heap_type! {
            @as_item
            $($maybe_pub)*
            enum $storage_type<'a> {
                $($variants)*
            }
        }
    };

    // Helper macros for implementing the mark() method for an in-heap enum.
    {
        @enum_mark_variant $storage_type:ident
            $name:ident NO_FIELDS ($($ctn:tt)*)
    } => {
        gc_heap_type! {
            $($ctn)* { $storage_type::$name => (), }
        }
    };

    {
        @enum_mark_variant $storage_type:ident
            $name:ident ( $($field_type:ty),* ) $ctn:tt
    } => {
        gc_heap_type! {
            @zip_idents_with_types (a b c d e f g h i j k l m n o p q r s t u v w x y z)
                ( $($field_type),*, ) ()
                (@enum_mark_variant_continued $storage_type $name $ctn)
        }
    };

    {
        @enum_mark_variant_continued $storage_type:ident $name:ident ($($ctn:tt)*)
            ( $(($binding:ident : $field_type:ty))* )
    } => {
        gc_heap_type! {
            $($ctn)* {
                $storage_type::$name ( $(ref $binding),* ) => {
                    $( <$field_type as $crate::traits::IntoHeap>::mark($binding); )*
                },
            }
        }
    };

    {
        @enum_mark_expr ($self_ref:expr) { $($arms:tt)* }
    } => {
        gc_heap_type! {
            @as_expr
            match *$self_ref {
                $($arms)*
            }
        }
    };

    // Helper macros for implementing the into_heap() method for an IntoHeap
    // enum.
    {
        @enum_into_heap_variant $stack_type:ident $storage_type:ident
            $name:ident NO_FIELDS
            ($($ctn:tt)*)
    } => {
        gc_heap_type! {
            $($ctn)* { $stack_type::$name => $storage_type::$name, }
        }
    };

    {
        @enum_into_heap_variant $stack_type:ident $storage_type:ident
            $name:ident ( $($field_type:ty),* )
            $ctn:tt
    } => {
        gc_heap_type! {
            @zip_idents_with_types (a b c d e f g h i j k l m n o p q r s t u v w x y z)
                ( $($field_type),*, ) ()
                (@enum_into_heap_variant_continued $stack_type $storage_type $name $ctn)
        }
    };

    {
        @enum_into_heap_variant_continued $stack_type:ident $storage_type:ident $name:ident ($($ctn:tt)*)
            ( $(($binding:ident : $field_type:ty))* )
    } => {
        gc_heap_type! {
            $($ctn)* {
                $stack_type::$name ( $($binding),* ) =>
                    $storage_type::$name( $($crate::traits::IntoHeap::into_heap($binding)),* ),
            }
        }
    };

    {
        @enum_into_heap_expr ($self_:expr)
        { $($accumulated_output:tt)* }
    } => {
        gc_heap_type! {
            @as_expr
            match $self_ {
                $($accumulated_output)*
            }
        }
    };

    // Helper macros for implementing the from_heap() method of an in-heap enum.
    {
        @enum_from_heap_variant $stack_type:ident $storage_type:ident
            $name:ident NO_FIELDS ($($ctn:tt)*)
    } => {
        gc_heap_type! {
            $($ctn)* { &$storage_type::$name => $stack_type::$name, }
        }
    };

    {
        @enum_from_heap_variant $stack_type:ident $storage_type:ident
            $name:ident ( $($field_type:ty),* ) $ctn:tt
    } => {
        gc_heap_type! {
            @zip_idents_with_types (a b c d e f g h i j k l m n o p q r s t u v w x y z)
                ( $($field_type),*, ) ()
                (@enum_from_heap_variant_continued $stack_type $storage_type $name $ctn)
        }
    };

    {
        @enum_from_heap_variant_continued $stack_type:ident $storage_type:ident $name:ident ($($ctn:tt)*)
            ( $(($binding:ident : $field_type:ty))* )
    } => {
        gc_heap_type! {
            $($ctn)* {
                &$storage_type::$name ( ref $($binding),* ) =>
                    $stack_type::$name( $($crate::traits::IntoHeap::from_heap($binding)),* ),
            }
        }
    };

    {
        @enum_from_heap_expr ($self_ref:expr) { $($arms:tt)* }
    } => {
        gc_heap_type! {
            @as_expr
            match $self_ref {
                $($arms)*
            }
        }
    };

    {
        @gc_heap_enum
        ($($maybe_pub:tt)*)
        enum $stack_type:ident / $storage_type:ident <'a>
        $variants:tt
    } => {
        gc_heap_type! {
            @for_each_variant (@enum_in_heap_variant) $variants {}
                (@enum_declare_in_heap_type ( $($maybe_pub)* ) $storage_type)
        }

        gc_heap_type! {
            @as_item
            #[derive(Debug, Clone, PartialEq)]
            $($maybe_pub)*
            enum $stack_type<'a>
                $variants
        }

        unsafe impl<'a> $crate::traits::IntoHeap<'a> for $stack_type<'a> {
            type In = $storage_type<'a>;

            fn into_heap(self) -> $storage_type<'a> {
                gc_heap_type! {
                    @for_each_variant (@enum_into_heap_variant $stack_type $storage_type) $variants {}
                    (@enum_into_heap_expr (self))
                }
            }

            unsafe fn from_heap(storage: &$storage_type<'a>) -> $stack_type<'a> {
                gc_heap_type! {
                    @for_each_variant (@enum_from_heap_variant $stack_type $storage_type) $variants {}
                    (@enum_from_heap_expr (storage))
                }
            }

            unsafe fn mark(storage: &$storage_type<'a>) {
                gc_heap_type! {
                    @for_each_variant (@enum_mark_variant $storage_type) $variants {}
                    (@enum_mark_expr (storage))
                }
            }
        }
    }
}
