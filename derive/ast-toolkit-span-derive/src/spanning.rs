//  SPANNING.rs
//    by Lut99
//
//  Description:
//!   Implements structs that will generate `Spanning` impls.
//

use proc_macro2::TokenStream as TokenStream2;
use quote::{ToTokens, quote};
use syn::{Generics, Ident};

use crate::analyze::{GenericImplInfo, ToplevelAttrs};
use crate::generate::{GenericEnumImplInfo, WhereClauseWithoutWhereWithComma};


/***** LIBRARY *****/
/// Specializes [`GenericImplInfo`] for the [`Spanning`]-trait.
pub struct SpanningImplInfo<'i, 'a>(&'i GenericImplInfo, &'a ToplevelAttrs);
impl GenericImplInfo {
    /// Returns a wrapper struct that specializes this GenericImplInfo to the `Spanning`-trait.
    ///
    /// # Arguments
    /// - `attrs`: Some [`ToplevelAttrs`] that carry user-specific configuration for the
    ///   generation.
    ///
    /// # Returns
    /// A [`SpanningImplInfo`] that can generate parts of the implementation.
    pub const fn spanning<'i, 'a>(&'i self, attrs: &'a ToplevelAttrs) -> SpanningImplInfo<'i, 'a> { SpanningImplInfo(self, attrs) }
}



/// Defines a wrapper around a [`SpanningImpl`] that uses its information to implement
/// `Spanning::get_span()` on a single struct or enum variant.
pub struct SpanningImplInfoGetSpan<'s, 'i, 'a>(&'s SpanningImplInfo<'i, 'a>);
impl<'s, 'i, 'a> ToTokens for SpanningImplInfoGetSpan<'s, 'i, 'a> {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let Self(SpanningImplInfo(info, attrs)) = self;
        let ast_toolkit_span = &attrs.ast_toolkit_span;

        // Return the proper innards
        let span = info.span_ident();
        let ty = info.span_ty();
        let s = &attrs.s;
        if !info.is_joining() {
            tokens.extend(quote! { <#ty as #ast_toolkit_span::Spanning<#s>>::get_span(#span) })
        } else {
            tokens.extend(
                quote! { let __span: ::std::borrow::Cow<#ast_toolkit_span::Span<#s>> = <#ty as #ast_toolkit_span::Spanning<#s>>::get_span(#span)?; },
            );
            for (field, ty) in info.joining() {
                tokens.extend(quote! { let __span: #ast_toolkit_span::Span<#s> = <#ast_toolkit_span::Span<#s>>::join(&__span, <#ty as #ast_toolkit_span::Spanning<#s>>::get_span(#field)?.as_ref())?; });
            }
            tokens.extend(quote! { ::std::option::Option::Some(::std::borrow::Cow::Owned(__span)) });
        }
    }
}
impl<'i, 'a> SpanningImplInfo<'i, 'a> {
    /// Returns a child struct of this SpanningImplInfo that generates the body of
    /// `Spanning::get_span()` for a single struct or variant.
    ///
    /// # Returns
    /// A [`SpanningImplInfoGetSpan`] that implements [`ToTokens`].
    #[inline]
    pub const fn get_span<'s>(&'s self) -> SpanningImplInfoGetSpan<'s, 'i, 'a> { SpanningImplInfoGetSpan(self) }
}



/// Defines a wrapper around a [`SpanningImplInfo`] that uses its information to implement
/// `Spanning::take_span()` on a single struct or enum variant.
pub struct SpanningImplInfoTakeSpan<'s, 'i, 'a>(&'s SpanningImplInfo<'i, 'a>);
impl<'s, 'i, 'a> ToTokens for SpanningImplInfoTakeSpan<'s, 'i, 'a> {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let Self(SpanningImplInfo(info, attrs)) = self;
        let ast_toolkit_span = &attrs.ast_toolkit_span;

        // Return the proper innards
        let span = info.span_ident();
        let ty = info.span_ty();
        let s = &attrs.s;
        if !info.is_joining() {
            tokens.extend(quote! { <#ty as #ast_toolkit_span::Spanning<#s>>::take_span(#span) })
        } else {
            tokens.extend(quote! { let __span: #ast_toolkit_span::Span<#s> = <#ty as #ast_toolkit_span::Spanning<#s>>::take_span(#span)?; });
            for (field, ty) in info.joining() {
                tokens.extend(quote! { let __span: #ast_toolkit_span::Span<#s> = <#ast_toolkit_span::Span<#s>>::join(&__span, &<#ty as #ast_toolkit_span::Spanning<#s>>::take_span(#field)?)?; });
            }
            tokens.extend(quote! { ::std::option::Option::Some(__span) });
        }
    }
}
impl<'i, 'a> SpanningImplInfo<'i, 'a> {
    /// Returns a child struct of this SpanningImplInfo that generates the body of
    /// `Spanning::take_span()` for a single struct or variant.
    ///
    /// # Returns
    /// A [`SpanningImplInfoTakeSpan`] that implements [`ToTokens`].
    #[inline]
    pub const fn take_span(&self) -> SpanningImplInfoTakeSpan { SpanningImplInfoTakeSpan(self) }
}



/// Defines a wrapper around a [`SpanningImplInfo`] that uses its information to implement
/// `Spanning` for some _struct_ type.
pub struct SpanningImplInfoStructImpl<'s, 'i, 'a, 'n, 'g>(&'s SpanningImplInfo<'i, 'a>, &'n Ident, &'g Generics);
impl<'s, 'i, 'a, 'n, 'g> ToTokens for SpanningImplInfoStructImpl<'s, 'i, 'a, 'n, 'g> {
    #[inline]
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let Self(sinfo @ SpanningImplInfo(info, attrs), ident, gens) = self;
        let ast_toolkit_span = &attrs.ast_toolkit_span;
        let get_span_impl = sinfo.get_span();
        let take_span_impl = sinfo.take_span();

        let (ig, ty_gen, _) = gens.split_for_impl();
        let impl_gen: TokenStream2 = match &attrs.impl_gen {
            Some(impl_gen) => quote! { < #impl_gen > },
            None => ig.to_token_stream(),
        };

        // Serialize to tokens
        let s = &attrs.s;
        let where_preds_old = gens.where_clause.as_ref().map(|wc| WhereClauseWithoutWhereWithComma(wc));
        let where_preds_new = info.where_preds(attrs, "Spanning");
        let ty_pat = info.ty_pat();
        tokens.extend(quote! {
            impl #impl_gen #ast_toolkit_span::Spanning<#s> for #ident #ty_gen where #where_preds_old #where_preds_new {
                #[inline]
                fn get_span(&self) -> ::std::option::Option<::std::borrow::Cow<#ast_toolkit_span::Span<#s>>> {
                    let Self #ty_pat = self;
                    #get_span_impl
                }

                #[inline]
                fn take_span(self) -> ::std::option::Option<#ast_toolkit_span::Span<#s>> {
                    let Self #ty_pat = self;
                    #take_span_impl
                }
            }
        });
    }
}
impl<'i, 'a> SpanningImplInfo<'i, 'a> {
    /// Returns a child struct of this SpanningImplInfo that generates the body of
    /// a `Spanning` impl on a struct.
    ///
    /// # Returns
    /// A [`SpanningImplInfoStructImpl`] that implements [`ToTokens`].
    #[inline]
    pub const fn struct_impl<'s, 'n, 'g>(&'s self, ident: &'n Ident, gens: &'g Generics) -> SpanningImplInfoStructImpl<'s, 'i, 'a, 'n, 'g> {
        SpanningImplInfoStructImpl(self, ident, gens)
    }
}



/// Defines a wrapper around [`SpanningImplInfoEnum`] that encodes the finishing state (i.e., we
/// have enough info to generate the full impl).
pub struct SpanningImplInfoEnumImplFinish<'a, 'g, 'i>(GenericEnumImplInfo, &'a ToplevelAttrs, &'g Generics, &'i Ident);
impl<'a, 'g, 'i> ToTokens for SpanningImplInfoEnumImplFinish<'a, 'g, 'i> {
    #[inline]
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let Self(GenericEnumImplInfo(where_preds_new, get_tokens, take_tokens), attrs, gens, ident) = self;
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
            impl #impl_gen #ast_toolkit_span::Spanning<#s> for #ident #ty_gen where #where_preds_old #where_preds_new {
                #[inline]
                fn get_span(&self) -> ::std::option::Option<::std::borrow::Cow<#ast_toolkit_span::Span<#s>>> {
                    match self {
                        #get_tokens
                    }
                }

                #[inline]
                fn take_span(self) -> ::std::option::Option<#ast_toolkit_span::Span<#s>> {
                    match self {
                        #take_tokens
                    }
                }
            }
        });
    }
}
impl<'a, 'g, 'i> From<(GenericEnumImplInfo, &'a ToplevelAttrs, &'g Generics, &'i Ident)> for SpanningImplInfoEnumImplFinish<'a, 'g, 'i> {
    #[inline]
    fn from((info, where_preds, func1_tokens, func2_tokens): (GenericEnumImplInfo, &'a ToplevelAttrs, &'g Generics, &'i Ident)) -> Self {
        Self(info, where_preds, func1_tokens, func2_tokens)
    }
}
