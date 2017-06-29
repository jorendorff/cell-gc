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
    println!("{:?}", expanded);
    expanded.parse().unwrap()
}

fn impl_into_heap(ast: &syn::DeriveInput) -> Tokens {
    match ast.body {
        syn::Body::Struct(ref data) => impl_into_heap_for_struct(ast, data),
        syn::Body::Enum(ref variants) => impl_into_heap_for_enum(ast, variants),
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

    match *data {
        syn::VariantData::Struct(ref fields) => {
            let field_vis: &Vec<_> = &fields.iter().map(|f| &f.vis).collect();
            let field_names: &Vec<_> = &fields.iter().map(|f| &f.ident).collect();
            let field_types: &Vec<_> = &fields.iter().map(|f| &f.ty).collect();
            let field_storage_types: Vec<_> = fields
                .iter()
                .map(|f| {
                    let field_ty = &f.ty;
                    quote! {
                    <#field_ty as ::cell_gc::traits::IntoHeap<#heap_lifetime>>::In
                }
                })
                .collect();

            // 1. The in-heap representation of the struct.
            let storage_struct = quote! {
                #vis struct #storage_type_name #impl_generics #where_clause {
                    #( #field_vis #field_names: #field_storage_types ),*
                }
            };

            // 2. IntoHeap implementation.
            // Body of the trace() method.
            let trace_fields: Vec<Tokens> = fields
                .iter()
                .map(|f| {
                    let name = &f.ident;
                    let ty = &f.ty;
                    quote! {
                    <#ty as ::cell_gc::traits::IntoHeap<#heap_lifetime>>
                        ::trace(&storage.#name, tracer);
                }
                })
                .collect();

            // Oddly you can't use the same identifier more than once in the
            // same loop. So create an alias.
            let field_names_1 = field_names;

            let into_heap = quote! {
                unsafe impl #impl_generics ::cell_gc::traits::IntoHeap<#heap_lifetime>
                    for #name #ty_generics
                    #where_clause
                {
                    type In = #storage_type_name #ty_generics;

                    fn into_heap(self) -> Self::In {
                        #storage_type_name {
                            #(
                                #field_names:
                                    ::cell_gc::traits::IntoHeap::into_heap(
                                        self.#field_names_1)
                            ),*
                        }
                    }

                    unsafe fn trace<R>(storage: &Self::In, tracer: &mut R)
                        where R: ::cell_gc::traits::Tracer
                    {
                        #( #trace_fields )*

                        // Quiet unused variable warnings when `$(...)*` expands
                        // to nothing.
                        let _ = tracer;
                    }

                    unsafe fn from_heap(storage: &Self::In) -> Self {
                        #name {
                            #(
                                #field_names:
                                    ::cell_gc::traits::IntoHeap::from_heap(
                                        &storage.#field_names_1)
                            ),*
                        }
                    }
                }
            };

            // 3. IntoHeapAllocation implementation.
            let ref_type_name: Ident = Ident::from(name_str.to_string() + "Ref");
            let into_heap_allocation = quote! {
                impl #impl_generics ::cell_gc::traits::IntoHeapAllocation<#heap_lifetime>
                    for #name #ty_generics
                    #where_clause
                {
                    type Ref = #ref_type_name #ty_generics;

                    fn wrap_gcref(gcref: ::cell_gc::GcRef<#heap_lifetime,
                                                          #name #ty_generics>)
                        -> #ref_type_name #ty_generics
                    {
                        #ref_type_name(gcref)
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
                unsafe impl #impl_generics ::cell_gc::traits::IntoHeap<#heap_lifetime>
                    for #ref_type_name #ty_generics
                    #where_clause
                {
                    type In = ::cell_gc::ptr::Pointer<#storage_type_name #ty_generics>;

                    fn into_heap(self) -> Self::In {
                        self.0.ptr()
                    }

                    unsafe fn trace<R>(storage: &Self::In, tracer: &mut R)
                        where R: ::cell_gc::traits::Tracer
                    {
                        if !storage.is_null() {
                            tracer.visit::<#name #ty_generics>(*storage);
                        }
                    }

                    unsafe fn from_heap(storage: &Self::In) -> #ref_type_name #ty_generics {
                        #ref_type_name(::cell_gc::GcRef::<#name #ty_generics>::new(*storage))
                    }
                }
            };

            // 6. Getters and setters.
            let field_setter_names: Vec<_> = fields
                .iter()
                .map(|f| {
                    let field_str: &str = f.ident.as_ref().unwrap().as_ref();
                    Ident::from(format!("set_{}", field_str))
                })
                .collect();
            let accessors = quote! {
                impl #impl_generics #ref_type_name #ty_generics #where_clause {
                    #(
                        #[allow(dead_code)]
                        #field_vis fn #field_names(&self) -> #field_types {
                            let ptr = self.0.as_ptr();
                            unsafe {
                                ::cell_gc::traits::IntoHeap::from_heap(
                                    &(*ptr).#field_names_1)
                            }
                        }
                    )*

                    #(
                        #[allow(dead_code)]
                        #field_vis fn #field_setter_names(&self, v: #field_types) {
                            let ptr = self.0.as_mut_ptr();
                            let u = ::cell_gc::traits::IntoHeap::into_heap(v);
                            unsafe {
                                (*ptr).#field_names = u;
                            }
                        }
                    )*

                    ///// Get all fields at once.
                    //pub fn get(&self) -> #name {
                    //    ::cell_gc::traits::IntoHeap::from_heap(ptr)
                    //}

                    #[allow(dead_code)]
                    pub fn as_mut_ptr(&self) -> *mut #storage_type_name #ty_generics {
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
                    let field_ty = &f.ty;
                    quote! {
                        #( #field_attrs )*
                        #field_name:
                            <#field_ty as ::cell_gc::traits::IntoHeap<#heap_lifetime>>::In
                    }
                });
                quote! {
                    #( #attrs )*
                    #ident { #( #field_decls ),* }
                }
            }
            syn::VariantData::Tuple(ref fields) => {
                let field_decls = fields.iter().map(|f| {
                    let field_ty = &f.ty;
                    let field_attrs = &f.attrs;
                    quote! {
                        #( #field_attrs )*
                        <#field_ty as ::cell_gc::traits::IntoHeap<#heap_lifetime>>::In
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
        #vis enum #storage_type_name #impl_generics
            #where_clause
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
                let field_types = fields.iter().map(|f| &f.ty);
                quote! {
                    #name::#ident { #(#field_names),* } => {
                        #storage_type_name::#ident {
                            #(
                                #field_names:
                                    <#field_types as ::cell_gc::traits::IntoHeap>
                                    ::into_heap(#field_names_1)
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
                    #name::#ident( #(#bindings),* ) => {
                        #storage_type_name::#ident(
                            #(
                                <#field_types as ::cell_gc::traits::IntoHeap>
                                    ::into_heap(#bindings)
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
                                    <#field_types as ::cell_gc::traits::IntoHeap>
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
                                <#field_types as ::cell_gc::traits::IntoHeap>
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
                let field_types = fields.iter().map(|f| &f.ty);
                quote! {
                    #storage_type_name::#ident { #(ref #field_names),* } => {
                        #(
                            <#field_types as ::cell_gc::traits::IntoHeap>
                                ::trace(#field_names, tracer);
                        )*
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
                        #(
                            <#field_types as ::cell_gc::traits::IntoHeap>
                                ::trace(#bindings, tracer);
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
        unsafe impl #impl_generics
            ::cell_gc::traits::IntoHeap<#heap_lifetime>
            for #name #ty_generics
            #where_clause
        {
            type In = #storage_type_name #ty_generics;

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

            unsafe fn trace<R>(storage: &Self::In, tracer: &mut R)
                where R: ::cell_gc::traits::Tracer
            {
                match *storage {
                    #( #trace_arms ),*
                }

                // Quiet unused variable warnings when `$(...)*` expands to
                // nothing.
                let _ = tracer;
            }
        }
    };

    quote! {
        #storage_enum
        #into_heap
    }
}
