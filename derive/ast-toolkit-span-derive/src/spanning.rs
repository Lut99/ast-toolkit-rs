//  SPANNING.rs
//    by Lut99
//
//  Description:
//!   Implements structs that will generate `Spanning` impls.
//

use proc_macro2::{Punct, Spacing, Span, TokenStream as TokenStream2};
use quote::{ToTokens, TokenStreamExt as _, quote};
use syn::{Generics, Ident};

use crate::analyze::{GenericImplInfo, StaircaseImplInfo, ToplevelAttrs};
use crate::generate::{EnumImplInfo, WhereClauseWithoutWhereWithComma};


/***** GENERIC INFO *****/
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
pub struct SpanningImplInfoEnumImplFinish<'a, 'g, 'i>(EnumImplInfo, &'a ToplevelAttrs, &'g Generics, &'i Ident);
impl<'a, 'g, 'i> ToTokens for SpanningImplInfoEnumImplFinish<'a, 'g, 'i> {
    #[inline]
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let Self(EnumImplInfo(where_preds_new, get_tokens, take_tokens), attrs, gens, ident) = self;
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
impl<'a, 'g, 'i> From<(EnumImplInfo, &'a ToplevelAttrs, &'g Generics, &'i Ident)> for SpanningImplInfoEnumImplFinish<'a, 'g, 'i> {
    #[inline]
    fn from((info, where_preds, func1_tokens, func2_tokens): (EnumImplInfo, &'a ToplevelAttrs, &'g Generics, &'i Ident)) -> Self {
        Self(info, where_preds, func1_tokens, func2_tokens)
    }
}





/***** STAIRCASE INFO *****/
/// Specializes [`StaircaseImplInfo`] for the [`Spanning`]-trait.
pub struct SpanningStaircaseImplInfo<'i, 'a>(&'i StaircaseImplInfo, &'a ToplevelAttrs);
impl StaircaseImplInfo {
    /// Returns a wrapper struct that specializes this StaircaseImplInfo to the `Spanning`-trait.
    ///
    /// # Arguments
    /// - `attrs`: Some [`ToplevelAttrs`] that carry user-specific configuration for the
    ///   generation.
    ///
    /// # Returns
    /// A [`SpanningImplInfo`] that can generate parts of the implementation.
    pub const fn spanning<'i, 'a>(&'i self, attrs: &'a ToplevelAttrs) -> SpanningStaircaseImplInfo<'i, 'a> { SpanningStaircaseImplInfo(self, attrs) }
}



/// Defines a wrapper around a [`SpanningImpl`] that uses its information to implement
/// `Spanning::get_span()` on a single struct or enum variant.
pub struct SpanningStaircaseImplInfoGetSpan<'s, 'i, 'a>(&'s SpanningStaircaseImplInfo<'i, 'a>);
impl<'s, 'i, 'a> ToTokens for SpanningStaircaseImplInfoGetSpan<'s, 'i, 'a> {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        // Generate the fall-through for the left struct.
        let Self(SpanningStaircaseImplInfo(info, attrs)) = self;
        let ast_toolkit_span = &attrs.ast_toolkit_span;
        let s = &attrs.s;

        /* LEFT SPAN */
        let mut spans = info.spans();
        if let Some((is_infallible, ident, ty)) = spans.next() {
            // Write the first span
            tokens.append(Ident::new("let", ident.span()));
            tokens.append(Ident::new("__left_span", ident.span()));
            tokens.append(Punct::new('=', Spacing::Alone));
            if is_infallible {
                // It's easy; we get the span and we're done (not an option)
                tokens.extend(quote! {<#ty as #ast_toolkit_span::SpanningInf<#s>>::span(#ident)});
                tokens.append(Punct::new(';', Spacing::Alone));
            } else {
                // We write this token with something that return `Option`, then write subsequent
                // tokens to defeat it.
                tokens.extend(quote! {<#ty as #ast_toolkit_span::Spanning<#s>>::get_span(#ident)});
                tokens.append(Punct::new(';', Spacing::Alone));
                let mut is_opt: bool = true;
                for (is_infallible, ident, ty) in spans {
                    tokens.append(Ident::new("let", ident.span()));
                    tokens.append(Ident::new("__left_span", ident.span()));
                    tokens.append(Punct::new('=', Spacing::Alone));
                    tokens.append(Ident::new("__left_span", ident.span()));
                    tokens.append(Punct::new('.', Spacing::Alone));
                    if is_infallible {
                        // It's easy; we get the span and we're done (not an option)
                        tokens.append(Ident::new("unwrap_or_else", ident.span()));
                        tokens.extend(quote! {(|| <#ty as #ast_toolkit_span::SpanningInf<#s>>::span(#ident))});
                        tokens.append(Punct::new(';', Spacing::Alone));
                        is_opt = false;
                        break;
                    } else {
                        // We write this token with something that return `Option`, then write subsequent
                        // tokens to defeat it.
                        tokens.append(Ident::new("or_else", ident.span()));
                        tokens.extend(quote! {(|| <#ty as #ast_toolkit_span::Spanning<#s>>::get_span(#ident))});
                        tokens.append(Punct::new(';', Spacing::Alone));
                    }
                }
                if is_opt {
                    // Unwrap the final result
                    tokens.append(Ident::new("let", ident.span()));
                    tokens.append(Ident::new("__left_span", ident.span()));
                    tokens.append(Punct::new('=', Spacing::Alone));
                    tokens.append(Ident::new("__left_span", ident.span()));
                    tokens.append(Punct::new('?', Spacing::Alone));
                    tokens.append(Punct::new(';', Spacing::Alone));
                }
            }
        } else {
            // `return ::std::option::Option::None;`
            tokens.append(Ident::new("return", Span::mixed_site()));
            tokens.append_all(&[proc_macro2::Punct::new(':', Spacing::Joint), proc_macro2::Punct::new(':', Spacing::Alone)]);
            tokens.append(Ident::new("std", Span::mixed_site()));
            tokens.append_all(&[proc_macro2::Punct::new(':', Spacing::Joint), proc_macro2::Punct::new(':', Spacing::Alone)]);
            tokens.append(Ident::new("option", Span::mixed_site()));
            tokens.append_all(&[proc_macro2::Punct::new(':', Spacing::Joint), proc_macro2::Punct::new(':', Spacing::Alone)]);
            tokens.append(Ident::new("Option", Span::mixed_site()));
            tokens.append_all(&[proc_macro2::Punct::new(':', Spacing::Joint), proc_macro2::Punct::new(':', Spacing::Alone)]);
            tokens.append(Ident::new("None", Span::mixed_site()));
            tokens.append(Punct::new(';', Spacing::Alone));
            return;
        };

        /* RIGHT SPAN */
        let mut spans = info.spans().rev();
        let Some((is_infallible, ident, ty)) = spans.next() else { panic!("Empty spans even though it wasn't empty literally just now") };

        // Write the first span
        tokens.append(Ident::new("let", ident.span()));
        tokens.append(Ident::new("__right_span", ident.span()));
        tokens.append(Punct::new('=', Spacing::Alone));
        if is_infallible {
            // It's easy; we get the span and we're done (not an option)
            tokens.extend(quote! {<#ty as #ast_toolkit_span::SpanningInf<#s>>::span(#ident)});
            tokens.append(Punct::new(';', Spacing::Alone));
        } else {
            // We write this token with something that return `Option`, then write subsequent
            // tokens to defeat it.
            tokens.extend(quote! {<#ty as #ast_toolkit_span::Spanning<#s>>::get_span(#ident)});
            tokens.append(Punct::new(';', Spacing::Alone));
            let mut is_opt: bool = true;
            for (is_infallible, ident, ty) in spans {
                tokens.append(Ident::new("let", ident.span()));
                tokens.append(Ident::new("__right_span", ident.span()));
                tokens.append(Punct::new('=', Spacing::Alone));
                tokens.append(Ident::new("__right_span", ident.span()));
                tokens.append(Punct::new('.', Spacing::Alone));
                if is_infallible {
                    // It's easy; we get the span and we're done (not an option)
                    tokens.append(Ident::new("unwrap_or_else", ident.span()));
                    tokens.extend(quote! {(|| <#ty as #ast_toolkit_span::SpanningInf<#s>>::span(#ident))});
                    tokens.append(Punct::new(';', Spacing::Alone));
                    is_opt = false;
                    break;
                } else {
                    // We write this token with something that return `Option`, then write subsequent
                    // tokens to defeat it.
                    tokens.append(Ident::new("or_else", ident.span()));
                    tokens.extend(quote! {(|| <#ty as #ast_toolkit_span::Spanning<#s>>::get_span(#ident))});
                    tokens.append(Punct::new(';', Spacing::Alone));
                }
            }
            if is_opt {
                // Unwrap the final result
                tokens.append(Ident::new("let", ident.span()));
                tokens.append(Ident::new("__right_span", ident.span()));
                tokens.append(Punct::new('=', Spacing::Alone));
                tokens.append(Ident::new("__right_span", ident.span()));
                tokens.append(Punct::new('?', Spacing::Alone));
                tokens.append(Punct::new(';', Spacing::Alone));
            }
        }



        /* JOINING */
        // Now we hit 'em with the join
        tokens.extend(quote! { <std::option::Option<_>>::map(<#ast_toolkit_span::Span<#s>>::join(<::std::borrow::Cow<_>>::as_ref(&__left_span), <::std::borrow::Cow<_>>::as_ref(&__right_span)), ::std::borrow::Cow::Owned) });
    }
}
impl<'i, 'a> SpanningStaircaseImplInfo<'i, 'a> {
    /// Returns a child struct of this SpanningStaircaseImplInfo that generates the body of
    /// `Spanning::get_span()` for a single struct or variant.
    ///
    /// # Returns
    /// A [`SpanningStaircaseImplInfoGetSpan`] that implements [`ToTokens`].
    #[inline]
    pub const fn get_span<'s>(&'s self) -> SpanningStaircaseImplInfoGetSpan<'s, 'i, 'a> { SpanningStaircaseImplInfoGetSpan(self) }
}



/// Defines a wrapper around a [`SpanningImplInfo`] that uses its information to implement
/// `Spanning` for some _struct_ type.
pub struct SpanningStaircaseImplInfoStructImpl<'s, 'i, 'a, 'n, 'g>(&'s SpanningStaircaseImplInfo<'i, 'a>, &'n Ident, &'g Generics);
impl<'s, 'i, 'a, 'n, 'g> ToTokens for SpanningStaircaseImplInfoStructImpl<'s, 'i, 'a, 'n, 'g> {
    #[inline]
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let Self(sinfo @ SpanningStaircaseImplInfo(info, attrs), ident, gens) = self;
        let ast_toolkit_span = &attrs.ast_toolkit_span;
        let get_span_impl = sinfo.get_span();

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
                    <::std::option::Option<_>>::map(<Self as #ast_toolkit_span::Spanning<#s>>::get_span(&self), ::std::borrow::Cow::into_owned)
                }
            }
        });
    }
}
impl<'i, 'a> SpanningStaircaseImplInfo<'i, 'a> {
    /// Returns a child struct of this SpanningStaircaseImplInfo that generates the body of
    /// a `Spanning` impl on a struct.
    ///
    /// # Returns
    /// A [`SpanningStaircaseImplInfoStructImpl`] that implements [`ToTokens`].
    #[inline]
    pub const fn struct_impl<'s, 'n, 'g>(&'s self, ident: &'n Ident, gens: &'g Generics) -> SpanningStaircaseImplInfoStructImpl<'s, 'i, 'a, 'n, 'g> {
        SpanningStaircaseImplInfoStructImpl(self, ident, gens)
    }
}
