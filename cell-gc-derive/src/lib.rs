#![recursion_limit = "1000"]

extern crate proc_macro;
extern crate syn;
#[macro_use]
extern crate quote;

use proc_macro::TokenStream;
use syn::Ident;
use quote::Tokens;

#[proc_macro_derive(IntoHeap)]
pub fn derive_into_heap(input: TokenStream) -> TokenStream {
    let source = input.to_string();
    let ast = syn::parse_derive_input(&source).unwrap();
    let expanded = impl_into_heap(&ast);

    // Uncomment to debug the generated code...
    // println!("{}", expanded);

    expanded.parse().unwrap()
}

fn impl_into_heap(ast: &syn::DeriveInput) -> Tokens {
    match ast.body {
        syn::Body::Struct(ref data) => impl_into_heap_for_struct(ast, data),
        syn::Body::Enum(ref variants) => impl_into_heap_for_enum(ast, variants),
    }
}


// The `_to_static` functions below modify a type by replacing all instances of
// a given lifetime with `'static'.
//
// We want to be able to emit code like `<Thing<'h> as IntoHeapBase>::In` in
// contexts where there is no lifetime `'h`. That's not allowed.
// But we can write `<Thing<'static> as IntoHeapBase>::In` instead.

fn lifetime_to_static(lifetime: &mut syn::Lifetime, heap_lifetime: &syn::Lifetime) {
    if lifetime.ident == heap_lifetime.ident {
        lifetime.ident = "'static".into();
    } else if lifetime.ident.as_ref() != "'static" {
        panic!("Only {:?} and 'static are allowed in types here", heap_lifetime.ident);
    }
}

fn params_to_static(params: &mut syn::PathParameters, heap_lifetime: &syn::Lifetime) {
    match *params {
        syn::PathParameters::AngleBracketed(ref mut data) => {
            for lifetime in &mut data.lifetimes {
                lifetime_to_static(lifetime, heap_lifetime);
            }
            for ty in &mut data.types {
                ty_to_static(ty, heap_lifetime);
            }
            for binding in &mut data.bindings {
                ty_to_static(&mut binding.ty, heap_lifetime);
            }
        }
        syn::PathParameters::Parenthesized(ref mut data) => {
            for ty in &mut data.inputs {
                ty_to_static(ty, heap_lifetime);
            }
            for ty in &mut data.output {
                ty_to_static(ty, heap_lifetime);
            }
        }
    }
}

fn path_to_static(path: &mut syn::Path, heap_lifetime: &syn::Lifetime) {
    for segment in &mut path.segments {
        params_to_static(&mut segment.parameters, heap_lifetime);
    }
}

fn ty_param_bound_to_static(bound: &mut syn::TyParamBound, heap_lifetime: &syn::Lifetime) {
    match *bound {
        syn::TyParamBound::Trait(ref mut poly_trait_ref, ref _modifier) => {
            // If the lifetime is rebound, do not recurse.
            for lifetime_def in &mut poly_trait_ref.bound_lifetimes {
                if lifetime_def.lifetime == *heap_lifetime {
                    return;
                }
            }

            path_to_static(&mut poly_trait_ref.trait_ref, heap_lifetime);
        }
        syn::TyParamBound::Region(ref mut lifetime) =>
            lifetime_to_static(lifetime, heap_lifetime),
    }
}

fn ty_to_static(ty: &mut syn::Ty, heap_lifetime: &syn::Lifetime) {
    match *ty {
        syn::Ty::Slice(ref mut elem_ty) =>
            ty_to_static(elem_ty, heap_lifetime),
        syn::Ty::Array(ref mut elem_ty, _) =>
            ty_to_static(elem_ty, heap_lifetime),
        syn::Ty::Ptr(ref mut referent_ty) =>
            ty_to_static(&mut referent_ty.ty, heap_lifetime),
        syn::Ty::Rptr(ref mut opt_lifetime, ref mut referent_ty) => {
            for lifetime in opt_lifetime {
                lifetime_to_static(lifetime, heap_lifetime);
            }
            ty_to_static(&mut referent_ty.ty, heap_lifetime);
        }
        syn::Ty::BareFn(_) => panic!("unsupported: bare function types"),
        syn::Ty::Never => {}
        syn::Ty::Tup(ref mut elem_types) => {
            for elem_ty in elem_types {
                ty_to_static(elem_ty, heap_lifetime);
            }
        }
        syn::Ty::Path(ref mut opt_qself, ref mut path) => {
            for qself in opt_qself {
                ty_to_static(&mut qself.ty, heap_lifetime);
            }
            path_to_static(path, heap_lifetime);
        }
        syn::Ty::TraitObject(ref mut bounds) => {
            for bound in bounds {
                ty_param_bound_to_static(bound, heap_lifetime);
            }
        }
        syn::Ty::ImplTrait(ref mut bounds) => {
            for bound in bounds {
                ty_param_bound_to_static(bound, heap_lifetime);
            }
        }
        syn::Ty::Paren(ref mut boxed_ty) =>
            ty_to_static(boxed_ty, heap_lifetime),
        syn::Ty::Infer => {}
        syn::Ty::Mac(_) =>
            panic!("unsupported: macro in type position")
    }
}

fn field_storage_type(field_ty: &syn::Ty, heap_lifetime: &syn::Lifetime) -> Tokens {
    let mut field_ty_as_static = field_ty.clone();
    ty_to_static(&mut field_ty_as_static, heap_lifetime);
    quote! {
        <#field_ty_as_static as ::cell_gc::traits::IntoHeapBase>::In
    }
}

fn impl_into_heap_for_struct(ast: &syn::DeriveInput, data: &syn::VariantData) -> Tokens {
    let name = &ast.ident;
    let name_str: &str = name.as_ref();
    let storage_type_name: Ident = Ident::from(name_str.to_string() + "Storage");
    let vis = &ast.vis;
    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();
    let heap_lifetime = &ast.generics
        .lifetimes
        .first()
        .expect("lifetime parameter required")
        .lifetime;

    // The "Storage" type for a struct or enum must have the static lifetime.
    let mut storage_generics = ast.generics.clone();
    storage_generics.lifetimes.remove(0);  // Remove heap lifetime.
    assert!(storage_generics.lifetimes.is_empty(),
            "IntoHeap struct must have exactly one lifetime parameter");
    let (storage_impl_generics, storage_ty_generics, storage_where_clause) =
        storage_generics.split_for_impl();

    match *data {
        syn::VariantData::Struct(ref fields) => {
            let field_vis: &Vec<_> = &fields.iter().map(|f| &f.vis).collect();
            let field_names: &Vec<_> = &fields.iter().map(|f| &f.ident).collect();
            let field_types: &Vec<_> = &fields.iter().map(|f| &f.ty).collect();
            let field_storage_types: &Vec<_> = &fields.iter()
                .map(|f| field_storage_type(&f.ty, &heap_lifetime))
                .collect();

            // 1. The in-heap representation of the struct.
            let storage_struct = quote! {
                #vis struct #storage_type_name #storage_impl_generics #storage_where_clause {
                    #( #field_vis #field_names: #field_storage_types ),*
                }
            };

            // 2. IntoHeap implementation.
            // Body of the trace() method.
            let trace_fields: Vec<Tokens> = fields
                .iter()
                .map(|f| {
                    let name = &f.ident;
                    quote! {
                        ::cell_gc::traits::InHeap::trace(&self.#name, tracer);
                    }
                })
                .collect();

            // Oddly you can't use the same identifier more than once in the
            // same loop. So create an alias.
            let field_types_1 = field_types;
            let field_names_1 = field_names;

            let into_heap = quote! {
                impl #impl_generics ::cell_gc::traits::InHeap
                    for #storage_type_name #storage_ty_generics
                    #where_clause
                {
                    unsafe fn trace<R>(&self, tracer: &mut R)
                        where R: ::cell_gc::traits::Tracer
                    {
                        #( #trace_fields )*

                        // Quiet unused variable warnings when `$(...)*` expands
                        // to nothing.
                        let _ = tracer;
                    }
                }

                impl #impl_generics ::cell_gc::traits::IntoHeapBase
                    for #name #ty_generics
                    #where_clause
                {
                    type In = #storage_type_name #storage_ty_generics;

                    fn into_heap(self) -> Self::In {
                        #storage_type_name {
                            #(
                                #field_names:
                                    ::cell_gc::traits::IntoHeapBase::into_heap(
                                        self.#field_names_1)
                            ),*
                        }
                    }

                    unsafe fn from_heap(storage: &Self::In) -> Self {
                        #name {
                            #(
                                #field_names:
                                    ::cell_gc::traits::IntoHeapBase::from_heap(
                                        &storage.#field_names_1)
                            ),*
                        }
                    }
                }

                unsafe impl #impl_generics ::cell_gc::traits::IntoHeap<#heap_lifetime>
                    for #name #ty_generics
                    #where_clause
                {}
            };

            // 3. IntoHeapAllocation implementation.
            let ref_type_name: Ident = Ident::from(name_str.to_string() + "Ref");
            let into_heap_allocation = quote! {
                impl #impl_generics ::cell_gc::traits::IntoHeapAllocation<#heap_lifetime>
                    for #name #ty_generics
                    #where_clause
                {
                    type Ref = #ref_type_name #ty_generics;

                    fn wrap_gc_ref(gc_ref: ::cell_gc::GcRef<#heap_lifetime, #name #ty_generics>)
                        -> Self::Ref
                    {
                        #ref_type_name(gc_ref)
                    }

                    fn into_gc_ref(wrapped_ref: Self::Ref)
                        -> ::cell_gc::GcRef<#heap_lifetime, #name #ty_generics>
                    {
                        wrapped_ref.0
                    }
                }
            };

            // 4. #ref_type_name: A safe reference to the struct
            let ref_type = quote! {
                #[derive(Clone, Debug, PartialEq, Eq)]
                #vis struct #ref_type_name #impl_generics
                    (::cell_gc::GcRef<#heap_lifetime, #name #ty_generics>)
                    #where_clause;
            };

            // 5. The ref type also gets an IntoHeap impl...
            let ref_type_into_heap = quote! {
                impl #impl_generics ::cell_gc::traits::IntoHeapBase
                    for #ref_type_name #ty_generics
                    #where_clause
                {
                    type In = <::cell_gc::GcRef<#heap_lifetime, #name #ty_generics>
                               as ::cell_gc::traits::IntoHeapBase>::In;

                    fn into_heap(self) -> Self::In {
                        self.0.into_heap()
                    }

                    unsafe fn from_heap(storage: &Self::In) -> Self {
                        #ref_type_name(::cell_gc::GcRef::<#heap_lifetime, #name #ty_generics>::new(*storage))
                    }
                }

                unsafe impl #impl_generics ::cell_gc::traits::IntoHeap<#heap_lifetime>
                    for #ref_type_name #ty_generics
                    #where_clause
                {}
            };

            // 6. The ref type also hashes...
            let ref_type_hash = quote! {
                impl #impl_generics ::std::hash::Hash for #ref_type_name #ty_generics
                    #where_clause
                {
                    #[inline]
                    fn hash<H: ::std::hash::Hasher>(&self, state: &mut H) {
                        self.0.hash(state);
                    }
                }
            };

            // 7. Getters and setters.
            let field_setter_names: Vec<_> = fields
                .iter()
                .map(|f| {
                    let field_str: &str = f.ident.as_ref().unwrap().as_ref();
                    Ident::from(format!("set_{}", field_str))
                })
                .collect();
            let this_type = ::std::iter::repeat(quote! {
                #name #ty_generics
            });
            let accessors = quote! {
                impl #impl_generics #ref_type_name #ty_generics #where_clause {
                    #(
                        #[allow(dead_code)]
                        #field_vis fn #field_names(&self) -> #field_types {
                            let ptr = self.0.as_ptr();
                            unsafe {
                                ::cell_gc::traits::IntoHeapBase::from_heap(
                                    &(*ptr).#field_names_1)
                            }
                        }
                    )*

                    #(
                        #[allow(dead_code)]
                        #field_vis fn #field_setter_names(&self, v: #field_types) {
                            let ptr = self.0.as_mut_ptr();
                            let u = ::cell_gc::traits::IntoHeapBase::into_heap(v);
                            unsafe {
                                (*ptr).#field_names = u;

                                ::cell_gc::post_write_barrier::<
                                    #this_type,
                                    #field_types_1,
                                    ::cell_gc::YesGc
                                >(
                                    &self.0.ptr(),
                                    &(*ptr).#field_names_1
                                );
                            }
                        }
                    )*

                    ///// Get all fields at once.
                    //pub fn get(&self) -> #name {
                    //    ::cell_gc::traits::IntoHeapBase::from_heap(self.0.ptr())
                    //}

                    #[allow(dead_code)]
                    pub fn as_mut_ptr(&self) -> *mut #storage_type_name #storage_ty_generics {
                        self.0.as_mut_ptr()
                    }
                }
            };

            quote! {
                #storage_struct
                #into_heap
                #into_heap_allocation
                #ref_type
                #ref_type_into_heap
                #ref_type_hash
                #accessors
            }
        }
        syn::VariantData::Tuple(ref _fields) => {
            panic!("#[derive(IntoHeap)] does not support tuple structs");
        }
        syn::VariantData::Unit => {
            panic!("#[derive(IntoHeap)] does not support unit structs");
        }
    }
}

fn impl_into_heap_for_enum(ast: &syn::DeriveInput, variants: &[syn::Variant]) -> Tokens {
    let attrs = &ast.attrs;
    let name = &ast.ident;
    let name_str: &str = name.as_ref();
    let storage_type_name: Ident = Ident::from(name_str.to_string() + "Storage");
    let vis = &ast.vis;
    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();
    let heap_lifetime = &ast.generics
        .lifetimes
        .first()
        .expect("lifetime parameter required")
        .lifetime;

    // The "Storage" type for a struct or enum must have the static lifetime.
    let mut storage_generics = ast.generics.clone();
    storage_generics.lifetimes.remove(0);  // Remove heap lifetime.
    assert!(storage_generics.lifetimes.is_empty(),
            "IntoHeap enum must have exactly one lifetime parameter");
    let (storage_impl_generics, storage_ty_generics, storage_where_clause) =
        storage_generics.split_for_impl();

    let variant_storage = variants.iter().map(|v| {
        let attrs = &v.attrs;
        let ident = &v.ident;
        if v.discriminant.is_some() {
            panic!("#[derive(IntoHeap)] does not support enum variants with discriminants");
        }
        match v.data {
            syn::VariantData::Struct(ref fields) => {
                let field_decls = fields.iter().map(|f| {
                    let field_attrs = &f.attrs;
                    let field_name = &f.ident;
                    let field_ty = field_storage_type(&f.ty, &heap_lifetime);
                    quote! {
                        #( #field_attrs )*
                        #field_name: #field_ty
                    }
                });
                quote! {
                    #( #attrs )*
                    #ident { #( #field_decls ),* }
                }
            }
            syn::VariantData::Tuple(ref fields) => {
                let field_decls = fields.iter().map(|f| {
                    let field_ty = field_storage_type(&f.ty, &heap_lifetime);
                    let field_attrs = &f.attrs;
                    quote! {
                        #( #field_attrs )*
                        #field_ty
                    }
                });
                quote! {
                    #( #attrs )*
                    #ident( #( #field_decls ),* )
                }
            }
            syn::VariantData::Unit => {
                quote! { #( #attrs )* #ident }
            }
        }
    });
    let storage_enum = quote! {
        #( #attrs )*
        #vis enum #storage_type_name #storage_impl_generics
            #storage_where_clause
        {
            #( #variant_storage ),*
        }
    };

    let into_heap_arms = variants.iter().map(|v| {
        let ident = &v.ident;
        match v.data {
            syn::VariantData::Struct(ref fields) => {
                let field_names: &Vec<_> = &fields.iter().map(|f| &f.ident).collect();
                let field_names_1 = field_names;
                quote! {
                    #name::#ident { #(#field_names),* } => {
                        #storage_type_name::#ident {
                            #(
                                #field_names:
                                    ::cell_gc::traits::IntoHeapBase::into_heap(#field_names_1)
                            ),*
                        }
                    }
                }
            }
            syn::VariantData::Tuple(ref fields) => {
                let bindings: &Vec<Ident> = &(0..fields.len())
                    .map(|n| Ident::from(format!("x{}", n)))
                    .collect();
                quote! {
                    #name::#ident( #(#bindings),* ) => {
                        #storage_type_name::#ident(
                            #(
                                ::cell_gc::traits::IntoHeapBase::into_heap(#bindings)
                            ),*
                        )
                    }
                }
            }
            syn::VariantData::Unit => {
                quote! {
                    #name::#ident => #storage_type_name::#ident
                }
            }
        }
    });

    let from_heap_arms = variants.iter().map(|v| {
        let ident = &v.ident;
        match v.data {
            syn::VariantData::Struct(ref fields) => {
                let field_names: &Vec<_> = &fields.iter().map(|f| &f.ident).collect();
                let field_names_1 = field_names;
                let field_types = fields.iter().map(|f| &f.ty);
                quote! {
                    #storage_type_name::#ident { #(ref #field_names),* } => {
                        #name::#ident {
                            #(
                                #field_names:
                                    <#field_types as ::cell_gc::traits::IntoHeapBase>
                                    ::from_heap(#field_names_1)
                            ),*
                        }
                    }
                }
            }
            syn::VariantData::Tuple(ref fields) => {
                let field_types = fields.iter().map(|f| &f.ty);
                let bindings: &Vec<Ident> = &(0..fields.len())
                    .map(|n| Ident::from(format!("x{}", n)))
                    .collect();
                quote! {
                    #storage_type_name::#ident( #(ref #bindings),* ) => {
                        #name::#ident(
                            #(
                                <#field_types as ::cell_gc::traits::IntoHeapBase>
                                    ::from_heap(#bindings)
                            ),*
                        )
                    }
                }
            }
            syn::VariantData::Unit => {
                quote! {
                    #storage_type_name::#ident => #name::#ident
                }
            }
        }
    });

    let trace_arms = variants.iter().map(|v| {
        let ident = &v.ident;
        match v.data {
            syn::VariantData::Struct(ref fields) => {
                let field_names: &Vec<_> = &fields.iter().map(|f| &f.ident).collect();
                quote! {
                    #storage_type_name::#ident { #(ref #field_names),* } => {
                        #(
                            ::cell_gc::traits::InHeap::trace(#field_names, tracer);
                        )*
                    }
                }
            }
            syn::VariantData::Tuple(ref fields) => {
                let bindings: &Vec<Ident> = &(0..fields.len())
                    .map(|n| Ident::from(format!("x{}", n)))
                    .collect();
                quote! {
                    #storage_type_name::#ident( #(ref #bindings),* ) => {
                        #(
                            ::cell_gc::traits::InHeap::trace(#bindings, tracer);
                        )*
                    }
                }
            }
            syn::VariantData::Unit => {
                quote! { #storage_type_name::#ident => () }
            }
        }
    });

    let into_heap = quote! {
        impl #impl_generics
            ::cell_gc::traits::InHeap
            for #storage_type_name #storage_ty_generics
            #where_clause
        {
            unsafe fn trace<R>(&self, tracer: &mut R)
                where R: ::cell_gc::traits::Tracer
            {
                match *self {
                    #( #trace_arms ),*
                }

                // Quiet unused variable warnings when `$(...)*` expands to
                // nothing.
                let _ = tracer;
            }
        }

        impl #impl_generics
            ::cell_gc::traits::IntoHeapBase
            for #name #ty_generics
            #where_clause
        {
            type In = #storage_type_name #storage_ty_generics;

            fn into_heap(self) -> Self::In {
                match self {
                    #( #into_heap_arms ),*
                }
            }

            unsafe fn from_heap(storage: &Self::In) -> Self {
                match *storage {
                    #( #from_heap_arms ),*
                }
            }
        }

        unsafe impl #impl_generics
            ::cell_gc::traits::IntoHeap<#heap_lifetime>
            for #name #ty_generics
            #where_clause
        {}
    };

    quote! {
        #storage_enum
        #into_heap
    }
}
