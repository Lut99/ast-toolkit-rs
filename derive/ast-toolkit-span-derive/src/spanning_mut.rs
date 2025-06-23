//  SPANNING MUT.rs
//    by Lut99
//
//  Description:
//!   Implements structs that will generate `SpanningMut`-impls.
//

use proc_macro2::TokenStream as TokenStream2;
use quote::{ToTokens, quote};
use syn::{Generics, Ident};

use crate::analyze::{GenericImplInfo, ToplevelAttrs};
use crate::generate::{GenericEnumImplInfo, WhereClauseWithoutWhereWithComma};


/***** LIBRARY *****/
/// Specializes [`GenericImplInfo`] for the `SpanningMut`-trait.
pub struct SpanningMutImplInfo<'i, 'a>(&'i GenericImplInfo, &'a ToplevelAttrs);
impl GenericImplInfo {
    /// Returns a wrapper struct that specializes this GenericImplInfo to the `SpanningMut`-trait.
    ///
    /// # Arguments
    /// - `attrs`: Some [`ToplevelAttrs`] that carry user-specific configuration for the
    ///   generation.
    ///
    /// # Returns
    /// A [`SpanningMutImplInfo`] that can generate parts of the implementation.
    pub const fn spanning_mut<'i, 'a>(&'i self, attrs: &'a ToplevelAttrs) -> SpanningMutImplInfo<'i, 'a> { SpanningMutImplInfo(self, attrs) }
}



/// Defines a wrapper around a [`SpanningMutImplInfo`] that uses its information to implement
/// `SpanningMut::span()` on a single struct or enum variant.
pub struct SpanningMutImplInfoMut<'s, 'i, 'a>(&'s SpanningMutImplInfo<'i, 'a>);
impl<'s, 'i, 'a> ToTokens for SpanningMutImplInfoMut<'s, 'i, 'a> {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let Self(SpanningMutImplInfo(info, attrs)) = self;
        let ast_toolkit_span = &attrs.ast_toolkit_span;

        // Return the proper innards
        let span = info.span_ident();
        let ty = info.span_ty();
        let s = &attrs.s;
        if !info.is_joining() {
            tokens.extend(quote! { <#ty as #ast_toolkit_span::SpanningMut<#s>>::span_mut(#span) })
        } else {
            panic!("Cannot join for `SpanningMut` impls")
        }
    }
}
impl<'i, 'a> SpanningMutImplInfo<'i, 'a> {
    /// Returns a child struct of this SpanningMutImplInfo that generates the body of
    /// `SpanningMut::span_mut()` for a single struct or variant.
    ///
    /// # Returns
    /// A [`SpanningMutImplInfoMut`] that implements [`ToTokens`].
    #[inline]
    pub const fn span_mut<'s>(&'s self) -> SpanningMutImplInfoMut<'s, 'i, 'a> { SpanningMutImplInfoMut(self) }
}



/// Defines a wrapper around a [`SpanningMutImplInfo`] that uses its information to implement
/// `SpanningMut` for some _struct_ type.
pub struct SpanningMutImplInfoStructImpl<'s, 'i, 'a, 'n, 'g>(&'s SpanningMutImplInfo<'i, 'a>, &'n Ident, &'g Generics);
impl<'s, 'i, 'a, 'n, 'g> ToTokens for SpanningMutImplInfoStructImpl<'s, 'i, 'a, 'n, 'g> {
    #[inline]
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let Self(sinfo @ SpanningMutImplInfo(info, attrs), ident, gens) = self;
        let ast_toolkit_span = &attrs.ast_toolkit_span;
        let mut_impl = sinfo.span_mut();

        let (ig, ty_gen, _) = gens.split_for_impl();
        let impl_gen: TokenStream2 = match &attrs.impl_gen {
            Some(impl_gen) => quote! { < #impl_gen > },
            None => ig.to_token_stream(),
        };

        // Serialize to tokens
        let s = &attrs.s;
        let where_preds_old = gens.where_clause.as_ref().map(|wc| WhereClauseWithoutWhereWithComma(wc));
        let where_preds_new = info.where_preds(attrs, "SpanningMut");
        let ty_pat = info.ty_pat();
        tokens.extend(quote! {
            impl #impl_gen #ast_toolkit_span::SpanningMut<#s> for #ident #ty_gen where #where_preds_old #where_preds_new {
                #[inline]
                fn span_mut(&mut self) -> &mut #ast_toolkit_span::Span<#s> {
                    let Self #ty_pat = self;
                    #mut_impl
                }
            }
        });
    }
}
impl<'i, 'a> SpanningMutImplInfo<'i, 'a> {
    /// Returns a child struct of this SpanningMutImplInfo that generates the body of
    /// a `SpanningMut` impl on a struct.
    ///
    /// # Returns
    /// A [`SpanningMutImplInfoStructImpl`] that implements [`ToTokens`].
    #[inline]
    pub const fn struct_impl<'s, 'n, 'g>(&'s self, ident: &'n Ident, gens: &'g Generics) -> SpanningMutImplInfoStructImpl<'s, 'i, 'a, 'n, 'g> {
        SpanningMutImplInfoStructImpl(self, ident, gens)
    }
}



/// Defines a wrapper around [`SpanningMutImplInfoEnum`] that encodes the finishing state (i.e., we
/// have enough info to generate the full impl).
pub struct SpanningMutImplInfoEnumImplFinish<'a, 'g, 'i>(GenericEnumImplInfo, &'a ToplevelAttrs, &'g Generics, &'i Ident);
impl<'a, 'g, 'i> ToTokens for SpanningMutImplInfoEnumImplFinish<'a, 'g, 'i> {
    #[inline]
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let Self(GenericEnumImplInfo(where_preds_new, span_mut_tokens, _), attrs, gens, ident) = self;
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
            impl #impl_gen #ast_toolkit_span::SpanningMut<#s> for #ident #ty_gen where #where_preds_old #where_preds_new {
                #[inline]
                fn span_mut(&mut self) -> &mut #ast_toolkit_span::Span<#s> {
                    match self {
                        #span_mut_tokens
                    }
                }
            }
        });
    }
}
impl<'a, 'g, 'i> From<(GenericEnumImplInfo, &'a ToplevelAttrs, &'g Generics, &'i Ident)> for SpanningMutImplInfoEnumImplFinish<'a, 'g, 'i> {
    #[inline]
    fn from((info, where_preds, func1_tokens, func2_tokens): (GenericEnumImplInfo, &'a ToplevelAttrs, &'g Generics, &'i Ident)) -> Self {
        Self(info, where_preds, func1_tokens, func2_tokens)
    }
}
