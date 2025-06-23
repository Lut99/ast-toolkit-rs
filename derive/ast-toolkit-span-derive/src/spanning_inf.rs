//  SPANNING INFALLIBLE.rs
//    by Lut99
//
//  Description:
//!   Implements structs that will generate `SpanningInf` impls.
//

use proc_macro2::TokenStream as TokenStream2;
use quote::{ToTokens, quote};
use syn::{Generics, Ident};

use crate::analyze::{GenericImplInfo, ToplevelAttrs};
use crate::generate::{GenericEnumImplInfo, WhereClauseWithoutWhereWithComma};


/***** LIBRARY *****/
/// Specializes [`GenericImplInfo`] for the `SpanningInf`-trait.
pub struct SpanningInfImplInfo<'i, 'a>(&'i GenericImplInfo, &'a ToplevelAttrs);
impl GenericImplInfo {
    /// Returns a wrapper struct that specializes this GenericImplInfo to the `SpanningInf`-trait.
    ///
    /// # Arguments
    /// - `attrs`: Some [`ToplevelAttrs`] that carry user-specific configuration for the
    ///   generation.
    ///
    /// # Returns
    /// A [`SpanningInfImplInfo`] that can generate parts of the implementation.
    pub const fn spanning_inf<'i, 'a>(&'i self, attrs: &'a ToplevelAttrs) -> SpanningInfImplInfo<'i, 'a> { SpanningInfImplInfo(self, attrs) }
}



/// Defines a wrapper around a [`SpanningInfImplInfo`] that uses its information to implement
/// `SpanningInf::span()` on a single struct or enum variant.
pub struct SpanningInfImplInfoSpan<'s, 'i, 'a>(&'s SpanningInfImplInfo<'i, 'a>);
impl<'s, 'i, 'a> ToTokens for SpanningInfImplInfoSpan<'s, 'i, 'a> {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let Self(SpanningInfImplInfo(info, attrs)) = self;
        let ast_toolkit_span = &attrs.ast_toolkit_span;

        // Return the proper innards
        let span = info.span_ident();
        let ty = info.span_ty();
        let s = &attrs.s;
        if !info.is_joining() {
            tokens.extend(quote! { <#ty as #ast_toolkit_span::SpanningInf<#s>>::span(#span) })
        } else {
            panic!("Cannot join for `SpanningInf` impls")
        }
    }
}
impl<'i, 'a> SpanningInfImplInfo<'i, 'a> {
    /// Returns a child struct of this SpanningInfImplInfo that generates the body of
    /// `SpanningInf::span()` for a single struct or variant.
    ///
    /// # Returns
    /// A [`SpanningInfImplInfoSpan`] that implements [`ToTokens`].
    #[inline]
    pub const fn span<'s>(&'s self) -> SpanningInfImplInfoSpan<'s, 'i, 'a> { SpanningInfImplInfoSpan(self) }
}



/// Defines a wrapper around a [`SpanningInfImplInfo`] that uses its information to implement
/// `SpanningInf::into_span()` on a single struct or enum variant.
pub struct SpanningInfImplInfoIntoSpan<'s, 'i, 'a>(&'s SpanningInfImplInfo<'i, 'a>);
impl<'s, 'i, 'a> ToTokens for SpanningInfImplInfoIntoSpan<'s, 'i, 'a> {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let Self(SpanningInfImplInfo(info, attrs)) = self;
        let ast_toolkit_span = &attrs.ast_toolkit_span;

        // Return the proper innards
        let span = info.span_ident();
        let ty = info.span_ty();
        let s = &attrs.s;
        if !info.is_joining() {
            tokens.extend(quote! { <#ty as #ast_toolkit_span::SpanningInf<#s>>::into_span(#span) })
        } else {
            panic!("Cannot join for `SpanningInf` impls")
        }
    }
}
impl<'i, 'a> SpanningInfImplInfo<'i, 'a> {
    /// Returns a child struct of this SpanningInfImplInfo that generates the body of
    /// `SpanningInf::into_span()` for a single struct or variant.
    ///
    /// # Returns
    /// A [`SpanningInfImplInfoIntoSpan`] that implements [`ToTokens`].
    #[inline]
    pub const fn into_span(&self) -> SpanningInfImplInfoIntoSpan { SpanningInfImplInfoIntoSpan(self) }
}



/// Defines a wrapper around a [`SpanningInfImplInfo`] that uses its information to implement
/// `SpanningInf` for some _struct_ type.
pub struct SpanningInfImplInfoStructImpl<'s, 'i, 'a, 'n, 'g>(&'s SpanningInfImplInfo<'i, 'a>, &'n Ident, &'g Generics);
impl<'s, 'i, 'a, 'n, 'g> ToTokens for SpanningInfImplInfoStructImpl<'s, 'i, 'a, 'n, 'g> {
    #[inline]
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let Self(sinfo @ SpanningInfImplInfo(info, attrs), ident, gens) = self;
        let ast_toolkit_span = &attrs.ast_toolkit_span;
        let span_impl = sinfo.span();
        let into_span_impl = sinfo.into_span();

        let (ig, ty_gen, _) = gens.split_for_impl();
        let impl_gen: TokenStream2 = match &attrs.impl_gen {
            Some(impl_gen) => quote! { < #impl_gen > },
            None => ig.to_token_stream(),
        };

        // Serialize to tokens
        let s = &attrs.s;
        let where_preds_old = gens.where_clause.as_ref().map(|wc| WhereClauseWithoutWhereWithComma(wc));
        let where_preds_new = info.where_preds(attrs, "SpanningInf");
        let ty_pat = info.ty_pat();
        tokens.extend(quote! {
            impl #impl_gen #ast_toolkit_span::SpanningInf<#s> for #ident #ty_gen where #where_preds_old #where_preds_new {
                #[inline]
                fn span(&self) -> ::std::borrow::Cow<#ast_toolkit_span::Span<#s>> {
                    let Self #ty_pat = self;
                    #span_impl
                }

                #[inline]
                fn into_span(self) -> #ast_toolkit_span::Span<#s> {
                    let Self #ty_pat = self;
                    #into_span_impl
                }
            }
        });
    }
}
impl<'i, 'a> SpanningInfImplInfo<'i, 'a> {
    /// Returns a child struct of this SpanningInfImplInfo that generates the body of
    /// a `SpanningInf` impl on a struct.
    ///
    /// # Returns
    /// A [`SpanningInfImplInfoStructImpl`] that implements [`ToTokens`].
    #[inline]
    pub const fn struct_impl<'s, 'n, 'g>(&'s self, ident: &'n Ident, gens: &'g Generics) -> SpanningInfImplInfoStructImpl<'s, 'i, 'a, 'n, 'g> {
        SpanningInfImplInfoStructImpl(self, ident, gens)
    }
}



/// Defines a wrapper around [`SpanningInfImplInfoEnum`] that encodes the finishing state (i.e., we
/// have enough info to generate the full impl).
pub struct SpanningInfImplInfoEnumImplFinish<'a, 'g, 'i>(GenericEnumImplInfo, &'a ToplevelAttrs, &'g Generics, &'i Ident);
impl<'a, 'g, 'i> ToTokens for SpanningInfImplInfoEnumImplFinish<'a, 'g, 'i> {
    #[inline]
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let Self(GenericEnumImplInfo(where_preds_new, span_tokens, into_span_tokens), attrs, gens, ident) = self;
        let ast_toolkit_span = &attrs.ast_toolkit_span;

        let (ig, ty_gen, _) = gens.split_for_impl();
        let impl_gen: TokenStream2 = match &attrs.impl_gen {
            Some(impl_gen) => quote! { < #impl_gen > },
            None => ig.to_token_stream(),
        };

        // Serialize to tokens
        let s = &attrs.s;
        let where_preds_old = gens.where_clause.as_ref().map(|wc| WhereClauseWithoutWhereWithComma(wc));
        tokens.extend(quote! {
            impl #impl_gen #ast_toolkit_span::SpanningInf<#s> for #ident #ty_gen where #where_preds_old #where_preds_new {
                #[inline]
                fn span(&self) -> ::std::borrow::Cow<#ast_toolkit_span::Span<#s>> {
                    match self {
                        #span_tokens
                    }
                }

                #[inline]
                fn into_span(self) -> #ast_toolkit_span::Span<#s> {
                    match self {
                        #into_span_tokens
                    }
                }
            }
        });
    }
}
impl<'a, 'g, 'i> From<(GenericEnumImplInfo, &'a ToplevelAttrs, &'g Generics, &'i Ident)> for SpanningInfImplInfoEnumImplFinish<'a, 'g, 'i> {
    #[inline]
    fn from((info, where_preds, func1_tokens, func2_tokens): (GenericEnumImplInfo, &'a ToplevelAttrs, &'g Generics, &'i Ident)) -> Self {
        Self(info, where_preds, func1_tokens, func2_tokens)
    }
}
