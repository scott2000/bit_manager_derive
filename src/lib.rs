//! Provides the derive macro for [`BitStore`].
//!
//! # Usage
//!
//! ```
//! #[macro_use]
//! extern crate bit_manager_derive;
//! extern crate bit_manager;
//!
//! # #[allow(unused_imports)]
//! use bit_manager::*;
//!
//! #[derive(BitStore, PartialEq, Debug)]
//! struct Point {
//!     x: f64,
//!     y: f64,
//! }
//! # fn main() {}
//! ```
//!
//! After deriving, `Point` can be stored and read like this:
//!
//! ```
//! # #[macro_use]
//! # extern crate bit_manager_derive;
//! # extern crate bit_manager;
//! # use bit_manager::*;
//! # #[derive(BitStore, PartialEq, Debug)]
//! # struct Point {
//! #     x: f64,
//! #     y: f64,
//! # }
//! # fn main() { test().unwrap(); }
//! # fn test() -> Result<()> {
//! let point = Point {
//!     x: 6.28318531,
//!     y: 2.5,
//! };
//!
//! let mut writer = BitWriter::new(Vec::new());
//! writer.write(&point)?;
//! let vec = writer.into_inner()?;
//!
//! let mut reader = BitReader::new(&vec[..]);
//! assert_eq!(reader.read::<Point>()?, point);
//! # Ok(())
//! # }
//! ```
//!
//! # Attributes
//!
//! * `#[bit(align_enum="8")]` uses integer multiples of the provided number for the amount of bits to store
//! the indexes of enum variants, allowing future additions of items. Default number is zero. `#[bit(align_enum)]`
//! uses a number of 8 bits. Max is 32 bits.
//! * `#[bit(verbose)]` prints out the derived implementation and a record of which attributes were used.
//!
//! # Implemenation
//!
//! ## Enums
//!
//! Enum variants are stored by their index. The index is stored with the least amount of bits needed (by default)
//! or with the least amount of bytes (using `align_enum`). Then, the fields of each variant are stored one at a time,
//! like a struct. On reading, if the index doesn't match any variants, an `Error::ConversionFailed` is returned.
//! Enums with no fields cannot be stored because they cannot be instantiated.
//!
//! ## Structs
//!
//! Structs store their contents one field at a time. Unit structs are a no-op. A failure to read an item will
//! cause the reading to stop immediately and return the error.
//!
//! The actual implementation of `Point`, when simplified, looks like this:
//!
//! ```
//! # extern crate bit_manager;
//! # struct Point {
//! #     x: f64,
//! #     y: f64,
//! # }
//! # fn main() {}
//! impl bit_manager::data::BitStore for Point {
//!     fn read_from<R: bit_manager::BitRead>(reader: &mut R) -> bit_manager::Result<Self> {
//!         Ok(
//!             Point {
//!                 x: reader.read()?,
//!                 y: reader.read()?,
//!             }
//!         )
//!     }
//!     fn write_to<W: bit_manager::BitWrite>(&self, writer: &mut W) -> bit_manager::Result<()> {
//!         writer.write(&self.x)?;
//!         writer.write(&self.y)
//!     }
//! }
//! ```
//!
//! [`BitStore`]: http://docs.rs/bit_manager/0.5.2/bit_manager/data/trait.BitStore.html

#![recursion_limit="128"]
extern crate proc_macro;
extern crate syn;
#[macro_use]
extern crate quote;

use proc_macro::TokenStream;

#[derive(Debug, Default)]
struct BitAttribute {
    align_enum: u8,
    verbose: bool,
}

impl<'a> From<&'a syn::DeriveInput> for BitAttribute {
    fn from(ast: &'a syn::DeriveInput) -> BitAttribute {
        let mut attributes = BitAttribute::default();
        for attr in &ast.attrs {
            if attr.value.name() == "bit" {
                if let &syn::MetaItem::List(_, ref list) = &attr.value {
                    for item in list {
                        if let &syn::NestedMetaItem::MetaItem(ref meta) = item {
                            match meta {
                                &syn::MetaItem::Word(_) => {
                                    match meta.name() {
                                        "align_enum" => attributes.align_enum = 8,
                                        "verbose" => attributes.verbose = true,
                                        tag => panic!("unknown bit tag: {}", tag),
                                    }
                                }
                                &syn::MetaItem::NameValue(_, ref literal) => {
                                    match meta.name() {
                                        "align_enum" => attributes.align_enum = match literal {
                                            &syn::Lit::Str(ref string, _) => {
                                                let n = string.parse().expect("invalid index number");
                                                if n > 32 {
                                                    panic!("enums currently can only use up to 32 bit indices")
                                                }
                                                n
                                            },
                                            _ => panic!("the align_enum tag only takes a string with a number")
                                        },
                                        "verbose" => panic!("the verbose tag does not take a literal"),
                                        tag => panic!("unknown bit tag: {}", tag),
                                    }
                                }
                                &syn::MetaItem::List(_, _) => {
                                    match meta.name() {
                                        "align_enum" => panic!("the align_enum tag does not take a list"),
                                        "verbose" => panic!("the verbose tag does not take a list"),
                                        tag => panic!("unknown bit tag: {}", tag),
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        attributes
    }
}

/// The derive macro for [`BitStore`]
///
/// [`BitStore`]: http://docs.rs/bit_manager/0.5.2/bit_manager/data/trait.BitStore.html
#[proc_macro_derive(BitStore, attributes(bit))]
pub fn bit_store_derive(input: TokenStream) -> TokenStream {
    let mut ast = syn::parse_derive_input(&input.to_string()).unwrap();
    let attributes = BitAttribute::from(&ast);
    if attributes.verbose {
        println!("attributes: {:?}", attributes);
    }
    let gen = parse_impl(&mut ast, &attributes);
    if attributes.verbose {
        println!("derived: {}", gen);
    }
    gen.parse().unwrap()
}

fn parse_impl(ast: &mut syn::DeriveInput, attributes: &BitAttribute) -> quote::Tokens {
    let name = &ast.ident;
    let dummy_var = syn::Ident::new(format!("__impl_BitStore_for_{}", name));
    let type_impl = parse_type_impl(name, &ast.body, attributes);
    update_generics(&mut ast.generics);
    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();
    quote! {
        #[allow(non_upper_case_globals, unused_attributes, unused_qualifications)]
        const #dummy_var: () = {
            extern crate bit_manager as __bit;
            #[automatically_derived]
            impl #impl_generics __bit::data::BitStore for #name #ty_generics #where_clause {
                #type_impl
            }
        };
    }
}

fn parse_type_impl(name: &syn::Ident, body: &syn::Body, attributes: &BitAttribute) -> quote::Tokens {
    match body {
        &syn::Body::Struct(ref data) => {
            if attributes.align_enum > 0 {
                panic!("struct fields cannot be aligned with #[bit(align_enum)]");
            }
            if let &syn::VariantData::Struct(ref fields) = data {
                parse_struct_impl(name, fields)
            } else if let &syn::VariantData::Tuple(ref fields) = data {
                parse_tuple_struct_impl(name, fields)
            } else {
                quote! {
                    fn read_from<__R: __bit::BitRead>(_: &mut __R) -> __bit::Result<Self> {
                        ::std::result::Result::Ok(#name)
                    }
                    fn write_to<__W: __bit::BitWrite>(&self, _: &mut __W) -> __bit::Result<()> {
                        ::std::result::Result::Ok(())
                    }
                }
            }
        },
        &syn::Body::Enum(ref variants) => {
            parse_enum_impl(name, variants, attributes)
        },
    }
}

fn parse_enum_impl(name: &syn::Ident, variants: &Vec<syn::Variant>, attributes: &BitAttribute) -> quote::Tokens {
    assert!(variants.len() > 0, "enum {} has too few variants to store", name);
    assert!(variants.len() as u64 <= (u32::max_value() as u64) + 1, "enum {} has too many variants to store", name);
    let mut bits = (32 - (variants.len() as u32 - 1).leading_zeros()) as u8;
    if attributes.align_enum > 1 {
        bits += (attributes.align_enum - (bits % attributes.align_enum)) % attributes.align_enum;
    }
    let mut i = 0;
    let mut access_r = Vec::new();
    let mut access_w = Vec::new();
    for variant in variants {
        let (r, w) = match &variant.data {
            &syn::VariantData::Struct(_) => parse_enum_struct_variant(&i, name, variant),
            &syn::VariantData::Tuple(_) => parse_enum_tuple_variant(&i, name, variant),
            &syn::VariantData::Unit => parse_enum_unit_variant(&i, name, variant),
        };
        access_r.push(r);
        access_w.push(w);
        i += 1;
    }
    let iter_r = access_r.iter();
    let iter_w = access_w.iter();
    quote! {
        fn read_from<__R: __bit::BitRead>(__r: &mut __R) -> __bit::Result<Self> {
            let __mask = __bit::data::BitMask::bits(#bits);
            ::std::result::Result::Ok (
                match __r.read_using::<u32, _>(&__mask)? {
                    #(
                        #iter_r,
                    )*
                    _ => return ::std::result::Result::Err(__bit::Error::ConversionFailed),
                }
            )
        }
        fn write_to<__W: __bit::BitWrite>(&self, __w: &mut __W) -> __bit::Result<()> {
            let __mask = __bit::data::BitMask::bits(#bits);
            match self {
                #(
                    #iter_w,
                )*
            }
        }
    }
}

fn parse_enum_struct_variant(n: &u32, name: &syn::Ident, variant: &syn::Variant) -> (quote::Tokens, quote::Tokens) {
    let fields = variant.data.fields();
    let mut access = Vec::new();
    for field in fields {
        if let &Some(ref ident) = &field.ident {
            access.push(quote! { #ident });
        } else {
            unreachable!();
        }
    }
    let iter_r = access.iter();
    let iter_w = access.iter();
    let iter_w_v = access.iter();
    let ident = &variant.ident;
    (
        quote! {
            #n => #name::#ident {
                #(
                    #iter_r: __r.read()?,
                )*
            }
        },
        quote! {
            &#name::#ident { #( ref #iter_w, )* } => {
                __w.write_using(&#n, &__mask)?;
                #(
                    __w.write(#iter_w_v)?;
                )*
                ::std::result::Result::Ok(())
            }
        }
    )
}

fn parse_enum_tuple_variant(n: &u32, name: &syn::Ident, variant: &syn::Variant) -> (quote::Tokens, quote::Tokens) {
    let fields = variant.data.fields();
    let mut i = 0;
    let mut access_r = Vec::new();
    let mut access_w = Vec::new();
    for _ in fields {
        access_r.push(quote! { read });
        access_w.push(syn::Ident::new(format!("__{}", i)));
        i += 1;
    }
    let iter_r = access_r.iter();
    let iter_w = access_w.iter();
    let iter_w_v = access_w.iter();
    let ident = &variant.ident;
    (
        quote! {
            #n => #name::#ident(
                #( __r.#iter_r()?, )*
            )
        },
        quote! {
            &#name::#ident( #( ref #iter_w, )* ) => {
                __w.write_using(&#n, &__mask)
                #(
                    ?; __w.write(#iter_w_v)
                )*
            }
        }
    )
}

fn parse_enum_unit_variant(n: &u32, name: &syn::Ident, variant: &syn::Variant) -> (quote::Tokens, quote::Tokens) {
    let ident = &variant.ident;
    (
        quote! {
            #n => {
                #name::#ident
            }
        },
        quote! {
            &#name::#ident => __w.write_using(&#n, &__mask)
        }
    )
}

fn parse_struct_impl(name: &syn::Ident, fields: &[syn::Field]) -> quote::Tokens {
    let mut access = Vec::new();
    for field in fields {
        if let &Some(ref ident) = &field.ident {
            access.push(quote! { #ident });
        } else {
            unreachable!();
        }
    }
    let iter_r = access.iter();
    let iter_w = access.iter();
    quote! {
        fn read_from<__R: __bit::BitRead>(__r: &mut __R) -> __bit::Result<Self> {
            ::std::result::Result::Ok (
                #name {
                    #(
                        #iter_r: __r.read()?,
                    )*
                }
            )
        }
        fn write_to<__W: __bit::BitWrite>(&self, __w: &mut __W) -> __bit::Result<()> {
            ::std::result::Result::Ok(())
            #(
                ?; __w.write(&self.#iter_w)
            )*
        }
    }
}

fn parse_tuple_struct_impl(name: &syn::Ident, fields: &[syn::Field]) -> quote::Tokens {
    let mut i = 0;
    let mut access_r = Vec::new();
    let mut access_w = Vec::new();
    for _ in fields {
        access_r.push(quote! { read });
        access_w.push(syn::Ident::new(i.to_string()));
        i += 1;
    }
    let iter_r = access_r.iter();
    let iter_w = access_w.iter();
    quote! {
        fn read_from<__R: __bit::BitRead>(__r: &mut __R) -> __bit::Result<Self> {
            ::std::result::Result::Ok (
                #name (
                    #(
                        __r.#iter_r()?,
                    )*
                )
            )
        }
        fn write_to<__W: __bit::BitWrite>(&self, __w: &mut __W) -> __bit::Result<()> {
            ::std::result::Result::Ok(())
            #(
                ?; __w.write(&self.#iter_w)
            )*
        }
    }
}

fn update_generics(generics: &mut syn::Generics) {
    let params = &mut generics.ty_params;
    for mut ty_param in params {
        ty_param.bounds.push(
            syn::TyParamBound::Trait(
                syn::PolyTraitRef {
                    bound_lifetimes: Vec::new(),
                    trait_ref: syn::Path {
                        global: false,
                        segments: vec![
                            syn::PathSegment {
                                ident: syn::Ident::new("__bit"),
                                parameters: syn::PathParameters::AngleBracketed(syn::AngleBracketedParameterData::default()),
                            },
                            syn::PathSegment {
                                ident: syn::Ident::new("data"),
                                parameters: syn::PathParameters::AngleBracketed(syn::AngleBracketedParameterData::default()),
                            },
                            syn::PathSegment {
                                ident: syn::Ident::new("BitStore"),
                                parameters: syn::PathParameters::AngleBracketed(syn::AngleBracketedParameterData::default()),
                            },
                        ],
                    },
                },
                syn::TraitBoundModifier::None,
            )
        );
    }
}