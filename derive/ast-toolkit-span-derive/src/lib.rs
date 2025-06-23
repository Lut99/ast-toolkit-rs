//  LIB.rs
//    by Lut99
//
//  Description:
//!   Implements the derive macros for the `ast-toolkit-span`-crate.
//

// Modules
mod analyze;
mod generate;
mod spanning;
mod spanning_inf;
mod spanning_mut;
mod spanning_ref;

// Imports
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::ToTokens;
use syn::{Attribute, Data, DeriveInput, Error, Generics, Ident, parse_macro_input};

use crate::analyze::{ToplevelAttrs, analyze_fields};
use crate::generate::{GenericEnumImplInfo, inject_attrs_and_gens};
use crate::spanning::SpanningImplInfoEnumImplFinish;
use crate::spanning_inf::SpanningInfImplInfoEnumImplFinish;
use crate::spanning_mut::SpanningMutImplInfoEnumImplFinish;
use crate::spanning_ref::SpanningRefImplInfoEnumImplFinish;


/***** LIBRARY *****/
/// Implements a derive macro for the `Spanning`-trait.
#[proc_macro_derive(Spanning, attributes(span, spanning))]
pub fn spanning_derive(input: TokenStream) -> TokenStream {
    /// Internal wrapper for easier error handling.
    fn inner(attrs: Vec<Attribute>, ident: Ident, mut gens: Generics, data: Data) -> Result<TokenStream2, Error> {
        let mut attrs = ToplevelAttrs::parse(attrs)?;

        // Decide on the kind of input
        match data {
            Data::Struct(s) => {
                // Analyze the fields
                let info = analyze_fields(s.fields)?;
                inject_attrs_and_gens(&info, &mut attrs, &mut gens);
                Ok(info.spanning(&attrs).struct_impl(&ident, &gens).into_token_stream().into())
            },
            Data::Enum(e) => {
                let mut info = GenericEnumImplInfo::new();
                for variant in e.variants {
                    let vinfo = analyze_fields(variant.fields)?;
                    inject_attrs_and_gens(&vinfo, &mut attrs, &mut gens);
                    info.add_variant(
                        variant.ident,
                        vinfo.ty_pat(),
                        vinfo.spanning(&attrs).get_span(),
                        vinfo.spanning(&attrs).take_span(),
                        vinfo.where_preds(&attrs, "Spanning"),
                    );
                }
                Ok(info.finish::<SpanningImplInfoEnumImplFinish>(&attrs, &gens, &ident).into_token_stream().into())
            },

            // Unsupported
            Data::Union(u) => Err(Error::new(u.union_token.span, "Cannot derive Spanning on unions")),
        }
    }


    // Let the external one handle it
    let DeriveInput { attrs, vis: _, ident, generics, data } = parse_macro_input!(input);
    match inner(attrs, ident, generics, data) {
        Ok(res) => res.into(),
        Err(err) => err.into_compile_error().into(),
    }
}



/// Implements a derive macro for the `SpanningInf`-trait.
#[proc_macro_derive(SpanningInf, attributes(span, spanning))]
pub fn spanning_inv_derive(input: TokenStream) -> TokenStream {
    /// Internal wrapper for easier error handling.
    fn inner(attrs: Vec<Attribute>, ident: Ident, mut gens: Generics, data: Data) -> Result<TokenStream2, Error> {
        let mut attrs = ToplevelAttrs::parse(attrs)?;

        // Decide on the kind of input
        match data {
            Data::Struct(s) => {
                // Analyze the fields
                let info = analyze_fields(s.fields)?;
                if let Some((ident, _)) = info.joining().next() {
                    return Err(Error::new(ident.span(), "Cannot implement `SpanningInf` on a joinable type (the join may return `None`)"));
                }
                inject_attrs_and_gens(&info, &mut attrs, &mut gens);
                Ok(info.spanning_inf(&attrs).struct_impl(&ident, &gens).into_token_stream().into())
            },
            Data::Enum(e) => {
                let mut info = GenericEnumImplInfo::new();
                for variant in e.variants {
                    let vinfo = analyze_fields(variant.fields)?;
                    if let Some((ident, _)) = vinfo.joining().next() {
                        return Err(Error::new(ident.span(), "Cannot implement `SpanningInf` on a joinable type (the join may return `None`)"));
                    }
                    inject_attrs_and_gens(&vinfo, &mut attrs, &mut gens);
                    info.add_variant(
                        variant.ident,
                        vinfo.ty_pat(),
                        vinfo.spanning_inf(&attrs).span(),
                        vinfo.spanning_inf(&attrs).into_span(),
                        vinfo.where_preds(&attrs, "SpanningInf"),
                    );
                }
                Ok(info.finish::<SpanningInfImplInfoEnumImplFinish>(&attrs, &gens, &ident).into_token_stream().into())
            },

            // Unsupported
            Data::Union(u) => Err(Error::new(u.union_token.span, "Cannot derive SpanningInf on unions")),
        }
    }


    // Let the external one handle it
    let DeriveInput { attrs, vis: _, ident, generics, data } = parse_macro_input!(input);
    match inner(attrs, ident, generics, data) {
        Ok(res) => res.into(),
        Err(err) => err.into_compile_error().into(),
    }
}



/// Implements a derive macro for the `SpanningRef`-trait.
#[proc_macro_derive(SpanningRef, attributes(span, spanning))]
pub fn spanning_ref_derive(input: TokenStream) -> TokenStream {
    /// Internal wrapper for easier error handling.
    fn inner(attrs: Vec<Attribute>, ident: Ident, mut gens: Generics, data: Data) -> Result<TokenStream2, Error> {
        let mut attrs = ToplevelAttrs::parse(attrs)?;

        // Decide on the kind of input
        match data {
            Data::Struct(s) => {
                // Analyze the fields
                let info = analyze_fields(s.fields)?;
                if let Some((ident, _)) = info.joining().next() {
                    return Err(Error::new(ident.span(), "Cannot implement `SpanningRef` on a joinable type (the join may return `None`)"));
                }
                inject_attrs_and_gens(&info, &mut attrs, &mut gens);
                Ok(info.spanning_ref(&attrs).struct_impl(&ident, &gens).into_token_stream().into())
            },
            Data::Enum(e) => {
                let mut info = GenericEnumImplInfo::new();
                for variant in e.variants {
                    let vinfo = analyze_fields(variant.fields)?;
                    if let Some((ident, _)) = vinfo.joining().next() {
                        return Err(Error::new(ident.span(), "Cannot implement `SpanningRef` on a joinable type (the join may return `None`)"));
                    }
                    inject_attrs_and_gens(&vinfo, &mut attrs, &mut gens);
                    info.add_variant_one(
                        variant.ident,
                        vinfo.ty_pat(),
                        vinfo.spanning_ref(&attrs).span_ref(),
                        vinfo.where_preds(&attrs, "SpanningRef"),
                    );
                }
                Ok(info.finish::<SpanningRefImplInfoEnumImplFinish>(&attrs, &gens, &ident).into_token_stream().into())
            },

            // Unsupported
            Data::Union(u) => Err(Error::new(u.union_token.span, "Cannot derive SpanningRef on unions")),
        }
    }


    // Let the external one handle it
    let DeriveInput { attrs, vis: _, ident, generics, data } = parse_macro_input!(input);
    match inner(attrs, ident, generics, data) {
        Ok(res) => res.into(),
        Err(err) => err.into_compile_error().into(),
    }
}



/// Implements a derive macro for the `SpanningMut`-trait.
#[proc_macro_derive(SpanningMut, attributes(span, spanning))]
pub fn spanning_mut_derive(input: TokenStream) -> TokenStream {
    /// Internal wrapper for easier error handling.
    fn inner(attrs: Vec<Attribute>, ident: Ident, mut gens: Generics, data: Data) -> Result<TokenStream2, Error> {
        let mut attrs = ToplevelAttrs::parse(attrs)?;

        // Decide on the kind of input
        match data {
            Data::Struct(s) => {
                // Analyze the fields
                let info = analyze_fields(s.fields)?;
                if let Some((ident, _)) = info.joining().next() {
                    return Err(Error::new(ident.span(), "Cannot implement `SpanningMut` on a joinable type (the join may return `None`)"));
                }
                inject_attrs_and_gens(&info, &mut attrs, &mut gens);
                Ok(info.spanning_mut(&attrs).struct_impl(&ident, &gens).into_token_stream().into())
            },
            Data::Enum(e) => {
                let mut info = GenericEnumImplInfo::new();
                for variant in e.variants {
                    let vinfo = analyze_fields(variant.fields)?;
                    if let Some((ident, _)) = vinfo.joining().next() {
                        return Err(Error::new(ident.span(), "Cannot implement `SpanningMut` on a joinable type (the join may return `None`)"));
                    }
                    inject_attrs_and_gens(&vinfo, &mut attrs, &mut gens);
                    info.add_variant_one(
                        variant.ident,
                        vinfo.ty_pat(),
                        vinfo.spanning_mut(&attrs).span_mut(),
                        vinfo.where_preds(&attrs, "SpanningMut"),
                    );
                }
                Ok(info.finish::<SpanningMutImplInfoEnumImplFinish>(&attrs, &gens, &ident).into_token_stream().into())
            },

            // Unsupported
            Data::Union(u) => Err(Error::new(u.union_token.span, "Cannot derive SpanningMut on unions")),
        }
    }


    // Let the external one handle it
    let DeriveInput { attrs, vis: _, ident, generics, data } = parse_macro_input!(input);
    match inner(attrs, ident, generics, data) {
        Ok(res) => res.into(),
        Err(err) => err.into_compile_error().into(),
    }
}
