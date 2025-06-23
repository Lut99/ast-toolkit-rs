//  SPANNING REF.rs
//    by Lut99
//
//  Description:
//!   Implements structs that will generate `SpanningRef`-impls.
//

use proc_macro2::TokenStream as TokenStream2;
use quote::{ToTokens, quote};
use syn::{Generics, Ident};

use crate::analyze::{GenericImplInfo, ToplevelAttrs};
use crate::generate::{GenericEnumImplInfo, WhereClauseWithoutWhereWithComma};


/***** LIBRARY *****/
/// Specializes [`GenericImplInfo`] for the `SpanningRef`-trait.
pub struct SpanningRefImplInfo<'i, 'a>(&'i GenericImplInfo, &'a ToplevelAttrs);
impl GenericImplInfo {
    /// Returns a wrapper struct that specializes this GenericImplInfo to the `SpanningRef`-trait.
    ///
    /// # Arguments
    /// - `attrs`: Some [`ToplevelAttrs`] that carry user-specific configuration for the
    ///   generation.
    ///
    /// # Returns
    /// A [`SpanningRefImplInfo`] that can generate parts of the implementation.
    pub const fn spanning_ref<'i, 'a>(&'i self, attrs: &'a ToplevelAttrs) -> SpanningRefImplInfo<'i, 'a> { SpanningRefImplInfo(self, attrs) }
}



/// Defines a wrapper around a [`SpanningRefImplInfo`] that uses its information to implement
/// `SpanningRef::span()` on a single struct or enum variant.
pub struct SpanningRefImplInfoRef<'s, 'i, 'a>(&'s SpanningRefImplInfo<'i, 'a>);
impl<'s, 'i, 'a> ToTokens for SpanningRefImplInfoRef<'s, 'i, 'a> {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let Self(SpanningRefImplInfo(info, attrs)) = self;
        let ast_toolkit_span = &attrs.ast_toolkit_span;

        // Return the proper innards
        let span = info.span_ident();
        let ty = info.span_ty();
        let s = &attrs.s;
        if !info.is_joining() {
            tokens.extend(quote! { <#ty as #ast_toolkit_span::SpanningRef<#s>>::span_ref(#span) })
        } else {
            panic!("Cannot join for `SpanningRef` impls")
        }
    }
}
impl<'i, 'a> SpanningRefImplInfo<'i, 'a> {
    /// Returns a child struct of this SpanningRefImplInfo that generates the body of
    /// `SpanningRef::span_ref()` for a single struct or variant.
    ///
    /// # Returns
    /// A [`SpanningRefImplInfoRef`] that implements [`ToTokens`].
    #[inline]
    pub const fn span_ref<'s>(&'s self) -> SpanningRefImplInfoRef<'s, 'i, 'a> { SpanningRefImplInfoRef(self) }
}



/// Defines a wrapper around a [`SpanningRefImplInfo`] that uses its information to implement
/// `SpanningRef` for some _struct_ type.
pub struct SpanningRefImplInfoStructImpl<'s, 'i, 'a, 'n, 'g>(&'s SpanningRefImplInfo<'i, 'a>, &'n Ident, &'g Generics);
impl<'s, 'i, 'a, 'n, 'g> ToTokens for SpanningRefImplInfoStructImpl<'s, 'i, 'a, 'n, 'g> {
    #[inline]
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let Self(sinfo @ SpanningRefImplInfo(info, attrs), ident, gens) = self;
        let ast_toolkit_span = &attrs.ast_toolkit_span;
        let ref_impl = sinfo.span_ref();

        let (ig, ty_gen, _) = gens.split_for_impl();
        let impl_gen: TokenStream2 = match &attrs.impl_gen {
            Some(impl_gen) => quote! { < #impl_gen > },
            None => ig.to_token_stream(),
        };

        // Serialize to tokens
        let s = &attrs.s;
        let where_preds_old = gens.where_clause.as_ref().map(|wc| WhereClauseWithoutWhereWithComma(wc));
        let where_preds_new = info.where_preds(attrs, "SpanningRef");
        let ty_pat = info.ty_pat();
        tokens.extend(quote! {
            impl #impl_gen #ast_toolkit_span::SpanningRef<#s> for #ident #ty_gen where #where_preds_old #where_preds_new {
                #[inline]
                fn span_ref(&self) -> &#ast_toolkit_span::Span<#s> {
                    let Self #ty_pat = self;
                    #ref_impl
                }
            }
        });
    }
}
impl<'i, 'a> SpanningRefImplInfo<'i, 'a> {
    /// Returns a child struct of this SpanningRefImplInfo that generates the body of
    /// a `SpanningRef` impl on a struct.
    ///
    /// # Returns
    /// A [`SpanningRefImplInfoStructImpl`] that implements [`ToTokens`].
    #[inline]
    pub const fn struct_impl<'s, 'n, 'g>(&'s self, ident: &'n Ident, gens: &'g Generics) -> SpanningRefImplInfoStructImpl<'s, 'i, 'a, 'n, 'g> {
        SpanningRefImplInfoStructImpl(self, ident, gens)
    }
}



/// Defines a wrapper around [`SpanningRefImplInfoEnum`] that encodes the finishing state (i.e., we
/// have enough info to generate the full impl).
pub struct SpanningRefImplInfoEnumImplFinish<'a, 'g, 'i>(GenericEnumImplInfo, &'a ToplevelAttrs, &'g Generics, &'i Ident);
impl<'a, 'g, 'i> ToTokens for SpanningRefImplInfoEnumImplFinish<'a, 'g, 'i> {
    #[inline]
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let Self(GenericEnumImplInfo(where_preds_new, span_ref_tokens, _), attrs, gens, ident) = self;
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
            impl #impl_gen #ast_toolkit_span::SpanningRef<#s> for #ident #ty_gen where #where_preds_old #where_preds_new {
                #[inline]
                fn span_ref(&self) -> &#ast_toolkit_span::Span<#s> {
                    match self {
                        #span_ref_tokens
                    }
                }
            }
        });
    }
}
impl<'a, 'g, 'i> From<(GenericEnumImplInfo, &'a ToplevelAttrs, &'g Generics, &'i Ident)> for SpanningRefImplInfoEnumImplFinish<'a, 'g, 'i> {
    #[inline]
    fn from((info, where_preds, func1_tokens, func2_tokens): (GenericEnumImplInfo, &'a ToplevelAttrs, &'g Generics, &'i Ident)) -> Self {
        Self(info, where_preds, func1_tokens, func2_tokens)
    }
}
