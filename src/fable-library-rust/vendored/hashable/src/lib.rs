use proc_macro::TokenStream;

use proc_macro_crate::{FoundCrate, crate_name};
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{format_ident, quote};
use syn::{Data, DeriveInput, Fields, GenericParam, PathArguments, Type, parse_macro_input, parse_quote};

#[proc_macro_derive(Hashable)]
pub fn derive_hashable(input: TokenStream) -> TokenStream {
    match expand(parse_macro_input!(input as DeriveInput)) {
        Ok(tokens) => tokens.into(),
        Err(error) => error.to_compile_error().into(),
    }
}

fn expand(input: DeriveInput) -> syn::Result<TokenStream2> {
    let runtime = runtime_path();
    let ident = input.ident;
    let generics = add_hash_bounds(input.generics);
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let hash_body = expand_hash_body(&runtime, &input.data)?;

    Ok(quote! {
        impl #impl_generics ::core::hash::Hash for #ident #ty_generics #where_clause {
            fn hash<H: ::core::hash::Hasher>(&self, state: &mut H) {
                #hash_body
            }
        }

        impl #impl_generics #runtime::Native_::Hashable for #ident #ty_generics #where_clause {
            #[inline]
            fn getHashCode(&self) -> i32 {
                #runtime::Native_::getHashCode(self)
            }
        }
    })
}

fn runtime_path() -> TokenStream2 {
    match crate_name("fable_library_rust") {
        Ok(FoundCrate::Itself) => quote!(crate),
        Ok(FoundCrate::Name(name)) => {
            let ident = syn::Ident::new(&name, Span::call_site());
            quote!(::#ident)
        }
        Err(_) => quote!(::fable_library_rust),
    }
}

fn add_hash_bounds(mut generics: syn::Generics) -> syn::Generics {
    for param in generics.params.iter_mut() {
        if let GenericParam::Type(param) = param {
            param.bounds.push(parse_quote!(::core::hash::Hash));
        }
    }

    generics
}

fn expand_hash_body(runtime: &TokenStream2, data: &Data) -> syn::Result<TokenStream2> {
    match data {
        Data::Struct(data_struct) => expand_struct_hash_body(runtime, &data_struct.fields),
        Data::Enum(data_enum) => expand_enum_hash_body(runtime, data_enum),
        Data::Union(_) => Err(syn::Error::new(Span::call_site(), "Hashable does not support unions")),
    }
}

fn expand_struct_hash_body(runtime: &TokenStream2, fields: &Fields) -> syn::Result<TokenStream2> {
    let statements = match fields {
        Fields::Named(fields_named) => fields_named
            .named
            .iter()
            .map(|field| {
                let field_name = field.ident.as_ref().expect("named field");
                hash_field_statement(runtime, quote!(&self.#field_name), &field.ty)
            })
            .collect::<Vec<_>>(),
        Fields::Unnamed(fields_unnamed) => fields_unnamed
            .unnamed
            .iter()
            .enumerate()
            .map(|(index, field)| {
                let index = syn::Index::from(index);
                hash_field_statement(runtime, quote!(&self.#index), &field.ty)
            })
            .collect::<Vec<_>>(),
        Fields::Unit => Vec::new(),
    };

    Ok(quote! {
        #(#statements)*
    })
}

fn expand_enum_hash_body(runtime: &TokenStream2, data_enum: &syn::DataEnum) -> syn::Result<TokenStream2> {
    let arms = data_enum
        .variants
        .iter()
        .enumerate()
        .map(|(index, variant)| {
            let variant_name = &variant.ident;
            let tag_hash = quote! {
                ::core::hash::Hash::hash(&(#index as usize), state);
            };

            match &variant.fields {
                Fields::Named(fields_named) => {
                    let bindings = fields_named
                        .named
                        .iter()
                        .map(|field| field.ident.clone().expect("named field"))
                        .collect::<Vec<_>>();

                    let hashes = fields_named
                        .named
                        .iter()
                        .map(|field| {
                            let field_name = field.ident.as_ref().expect("named field");
                            hash_field_statement(runtime, quote!(#field_name), &field.ty)
                        })
                        .collect::<Vec<_>>();

                    quote! {
                        Self::#variant_name { #(#bindings),* } => {
                            #tag_hash
                            #(#hashes)*
                        }
                    }
                }
                Fields::Unnamed(fields_unnamed) => {
                    let bindings = fields_unnamed
                        .unnamed
                        .iter()
                        .enumerate()
                        .map(|(field_index, _)| format_ident!("field_{field_index}"))
                        .collect::<Vec<_>>();

                    let hashes = fields_unnamed
                        .unnamed
                        .iter()
                        .enumerate()
                        .map(|(field_index, field)| {
                            let binding = format_ident!("field_{field_index}");
                            hash_field_statement(runtime, quote!(#binding), &field.ty)
                        })
                        .collect::<Vec<_>>();

                    quote! {
                        Self::#variant_name(#(#bindings),*) => {
                            #tag_hash
                            #(#hashes)*
                        }
                    }
                }
                Fields::Unit => {
                    quote! {
                        Self::#variant_name => {
                            #tag_hash
                        }
                    }
                }
            }
        })
        .collect::<Vec<_>>();

    Ok(quote! {
        match self {
            #(#arms),*
        }
    })
}

fn hash_field_statement(runtime: &TokenStream2, value: TokenStream2, ty: &Type) -> TokenStream2 {
    match hash_strategy(ty) {
        HashStrategy::F64 => quote! {
            #runtime::Native_::hash_f64(#value, state);
        },
        HashStrategy::F32 => quote! {
            #runtime::Native_::hash_f32(#value, state);
        },
        HashStrategy::MutCellF64 => quote! {
            #runtime::Native_::hash_mutcell_f64(#value, state);
        },
        HashStrategy::MutCellF32 => quote! {
            #runtime::Native_::hash_mutcell_f32(#value, state);
        },
        HashStrategy::Direct => quote! {
            ::core::hash::Hash::hash(#value, state);
        },
    }
}

fn hash_strategy(ty: &Type) -> HashStrategy {
    match float_kind(ty) {
        Some(FloatKind::F64) => HashStrategy::F64,
        Some(FloatKind::F32) => HashStrategy::F32,
        None => match mutcell_float_kind(ty) {
            Some(FloatKind::F64) => HashStrategy::MutCellF64,
            Some(FloatKind::F32) => HashStrategy::MutCellF32,
            None => HashStrategy::Direct,
        },
    }
}

fn float_kind(ty: &Type) -> Option<FloatKind> {
    let Type::Path(type_path) = ty else {
        return None;
    };

    if type_path.qself.is_some() {
        return None;
    }

    let segment = type_path.path.segments.last()?;
    match segment.ident.to_string().as_str() {
        "f64" => Some(FloatKind::F64),
        "f32" => Some(FloatKind::F32),
        _ => None,
    }
}

fn mutcell_float_kind(ty: &Type) -> Option<FloatKind> {
    let Type::Path(type_path) = ty else {
        return None;
    };

    if type_path.qself.is_some() {
        return None;
    }

    let segment = type_path.path.segments.last()?;
    if segment.ident != "MutCell" {
        return None;
    }

    let PathArguments::AngleBracketed(arguments) = &segment.arguments else {
        return None;
    };

    let syn::GenericArgument::Type(inner_ty) = arguments.args.first()? else {
        return None;
    };

    float_kind(inner_ty)
}

enum HashStrategy {
    Direct,
    F32,
    F64,
    MutCellF32,
    MutCellF64,
}

enum FloatKind {
    F32,
    F64,
}