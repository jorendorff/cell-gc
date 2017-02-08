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
    expanded.parse().unwrap()
}

fn impl_into_heap(ast: &syn::DeriveInput) -> Tokens {
    match ast.body {
        syn::Body::Struct(ref data) => {
            impl_into_heap_for_struct(ast, data)
        },
        syn::Body::Enum(_) => {
            panic!("#[derive(IntoHeap)] can only be used with structs");
        }
    }
}

fn impl_into_heap_for_struct(ast: &syn::DeriveInput, data: &syn::VariantData) -> Tokens {
    let name = &ast.ident;
    let name_str: &str = name.as_ref();
    let ref_type_name: Ident = Ident::from(name_str.to_string() + "Ref");
    let storage_type_name: Ident = Ident::from(name_str.to_string() + "Storage");
    let vis = &ast.vis;
    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();
    let heap_lifetime = &ast.generics.lifetimes[0].lifetime;

    match *data {
        syn::VariantData::Struct(ref fields) => {
            let field_names: &Vec<_> =
                &fields.iter().map(|f| &f.ident).collect();
            let field_names_1 = field_names;
            let field_types: &Vec<_> =
                &fields.iter().map(|f| &f.ty).collect();
            let field_setter_names: Vec<_> = fields.iter().map(|f| {
                let field_str: &str = f.ident.as_ref().unwrap().as_ref();
                Ident::from(format!("set_{}", field_str))
            }).collect();
            let field_visibilities: Vec<_> =
                fields.iter().map(|f| &f.vis).collect();
            let field_storage_types: Vec<_> = fields.iter().map(|f| {
                let field_ty = &f.ty;
                quote! {
                    <#field_ty as ::cell_gc::traits::IntoHeap<#heap_lifetime>>::In
                }
            }).collect();

            // Statements for marking the fields of a struct.
            let mark_fields: Vec<Tokens> = fields.iter().map(|f| {
                let name = &f.ident;
                let ty = &f.ty;
                quote! {
                    <#ty as ::cell_gc::traits::IntoHeap<#heap_lifetime>>
                        ::mark(&storage.#name);
                }
            }).collect();

            let output = quote! {
                // 1. The in-heap representation of the struct.
                #vis struct #storage_type_name #impl_generics #where_clause {
                    #( #field_visibilities #field_names: #field_storage_types ),*
                }

                // 2. IntoHeap.
                unsafe impl #impl_generics ::cell_gc::traits::IntoHeap<#heap_lifetime>
                    for #name #ty_generics
                    #where_clause
                {
                    type In = #storage_type_name #ty_generics;
                    fn into_heap(self) -> #storage_type_name #ty_generics {
                        #storage_type_name {
                            #(
                                #field_names:
                                    ::cell_gc::traits::IntoHeap::into_heap(
                                        self.#field_names_1)
                            ),*
                        }
                    }

                    unsafe fn mark(storage: &#storage_type_name #ty_generics) {
                        if !::cell_gc::Heap::get_mark_bit::<Self>(storage) {
                            ::cell_gc::Heap::set_mark_bit::<Self>(storage);
                            #( #mark_fields )*
                        }
                    }

                    unsafe fn from_heap(storage: &#storage_type_name #ty_generics)
                            -> Self {
                        #name {
                            #(
                                #field_names: ::cell_gc::traits::IntoHeap::from_heap(
                                    &storage.#field_names_1)
                            ),*
                        }
                    }
                }

                // 3. IntoHeapAllocation.
                impl #impl_generics ::cell_gc::traits::IntoHeapAllocation<#heap_lifetime>
                    for #name #ty_generics
                    #where_clause
                {
                    type Ref = #ref_type_name #ty_generics;

                    fn wrap_gcref(gcref: ::cell_gc::GCRef<#heap_lifetime,
                                                          #name #ty_generics>)
                        -> #ref_type_name #ty_generics
                    {
                        #ref_type_name(gcref)
                    }
                }

                // 4. #ref_type_name: A safe reference to the struct
                #[derive(Clone, Debug, PartialEq, Eq)]
                #vis struct #ref_type_name #impl_generics
                    (::cell_gc::GCRef<#heap_lifetime, #name #ty_generics>)
                    #where_clause;

                unsafe impl #impl_generics ::cell_gc::traits::IntoHeap<#heap_lifetime>
                    for #ref_type_name #ty_generics
                    #where_clause
                {
                    type In = *mut #storage_type_name #ty_generics;

                    fn into_heap(self) -> *mut #storage_type_name #ty_generics {
                        self.0.as_mut_ptr()
                    }

                    unsafe fn mark(storage: &*mut #storage_type_name #ty_generics) {
                        let ptr = *storage;
                        if !ptr.is_null() {
                            <#name #ty_generics as ::cell_gc::traits::IntoHeap>
                                ::mark(&*ptr);
                        }
                    }

                    unsafe fn from_heap(storage: &*mut #storage_type_name #ty_generics)
                        -> #ref_type_name #ty_generics {
                        #ref_type_name(::cell_gc::GCRef::new(*storage))
                    }
                }

                // 5. Field accessors.
                impl #impl_generics #ref_type_name #ty_generics {
                    #(
                        pub fn #field_names(&self) -> #field_types {
                            let ptr = self.0.as_ptr();
                            unsafe {
                                ::cell_gc::traits::IntoHeap::from_heap(
                                    &(*ptr).#field_names_1)
                            }
                        }
                    )*

                    #(
                        pub fn #field_setter_names(&self, v: #field_types) {
                            let ptr = self.0.as_mut_ptr();
                            let u = ::cell_gc::traits::IntoHeap::into_heap(v);
                            unsafe {
                                (*ptr).#field_names = u;
                            }
                        }
                    )*

                    pub fn as_mut_ptr(&self) -> *mut #storage_type_name #ty_generics {
                        self.0.as_mut_ptr()
                    }
                }
            };

            println!("{:?}", output);
            output
        },
        syn::VariantData::Tuple(ref _fields) => {
            panic!("#[derive(IntoHeap)] does not support unit structs");
        },
        syn::VariantData::Unit => {
            panic!("#[derive(IntoHeap)] does not support unit structs");
        }
    }
}
