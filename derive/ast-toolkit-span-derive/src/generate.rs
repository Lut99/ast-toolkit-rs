//  GENERATE.rs
//    by Lut99
//
//  Description:
//!   Defines trait-agnostic generation functions.
//

use proc_macro2::{Delimiter, Group, Punct, Spacing, Span, TokenStream as TokenStream2};
use quote::{ToTokens, TokenStreamExt as _};
use syn::punctuated::Punctuated;
use syn::{
    AngleBracketedGenericArguments, GenericArgument, GenericParam, Generics, Ident, Lifetime, LifetimeParam, Path, PathArguments, PathSegment,
    PredicateType, Token, TraitBound, TraitBoundModifier, Type, TypeParam, TypeParamBound, TypePath, WhereClause, WherePredicate,
};

use crate::analyze::{GenericImplInfo, ToplevelAttrs};


/***** HELPER FUNCTIONS *****/
/// Inserts a generic parameter in the given [`Generics`] if not already there.
///
/// # Arguments
/// - `ident`: The [`Ident`]ifier of the new generic parameter.
/// - `gens`: The list of [`GenericParam`]s to potentially add them to.
fn insert_ty_in_gens(ident: &Ident, gens: &mut Punctuated<GenericParam, Token![,]>) {
    let mut found = false;
    for param in &*gens {
        if let GenericParam::Type(param) = param {
            if ident == &param.ident {
                found = true;
                break;
            }
        }
    }
    if !found {
        gens.push(GenericParam::Type(TypeParam {
            attrs: Vec::new(),
            ident: ident.clone(),
            colon_token: None,
            bounds: Punctuated::new(),
            eq_token: None,
            default: None,
        }));
    }
}

/// Inserts a lifetime parameter in the given [`Generics`] if not already there.
///
/// # Arguments
/// - `lt`: The [`Lifetime`] of the new generic parameter.
/// - `gens`: The list of [`GenericParam`]s to potentially add them to. Note we do a little
///   funkiness with turning the `None` into a `Some` if we had to add it.
fn insert_lt_in_gens(lt: &Lifetime, gens: &mut Option<Punctuated<GenericParam, Token![,]>>) {
    let mut found = false;
    for param in gens.iter().flat_map(Punctuated::iter) {
        if let GenericParam::Lifetime(param) = param {
            if lt == &param.lifetime {
                found = true;
                break;
            }
        }
    }
    if !found {
        if gens.is_none() {
            *gens = Some(Punctuated::new());
        }
        gens.as_mut().unwrap().push(GenericParam::Lifetime(LifetimeParam {
            attrs: Vec::new(),
            lifetime: lt.clone(),
            colon_token: None,
            bounds: Punctuated::new(),
        }));
    }
}

/// Generates a [`WherePredicate`] with the required bounds for `S`.
///
/// # Arguments
/// - `attrs`: The [`TopleverAttrs`] carrying user config, like the name for `S` or the path to
///   `ast_toolkit_span`.
/// - `lt`: If given, adds a `Spannable` bound with the given lifetime.
fn gen_s_where_pred(attrs: &ToplevelAttrs, lt: Option<&Lifetime>) -> WherePredicate {
    WherePredicate::Type(PredicateType {
        lifetimes:   None,
        bounded_ty:  Type::Path(TypePath {
            qself: None,
            path:  Path {
                leading_colon: None,
                segments:      {
                    let mut segs = Punctuated::new();
                    segs.push(PathSegment { ident: attrs.s.clone(), arguments: PathArguments::None });
                    segs
                },
            },
        }),
        colon_token: Default::default(),
        bounds:      {
            let mut bounds = Punctuated::new();
            bounds.push(TypeParamBound::Trait(TraitBound {
                paren_token: None,
                modifier: TraitBoundModifier::None,
                lifetimes: None,
                path: Path {
                    leading_colon: Some(Default::default()),
                    segments:      {
                        let mut segs = Punctuated::new();
                        segs.push(PathSegment { ident: Ident::new("std", attrs.s.span()), arguments: PathArguments::None });
                        segs.push(PathSegment { ident: Ident::new("clone", attrs.s.span()), arguments: PathArguments::None });
                        segs.push(PathSegment { ident: Ident::new("Clone", attrs.s.span()), arguments: PathArguments::None });
                        segs
                    },
                },
            }));
            if let Some(lt) = lt {
                // Add the `S: Spannable<'s>`-bound, too, because we'll need to join
                bounds.push(TypeParamBound::Trait(TraitBound {
                    paren_token: None,
                    modifier: TraitBoundModifier::None,
                    lifetimes: None,
                    path: {
                        let mut path = attrs.ast_toolkit_span.clone();
                        path.segments.push(PathSegment {
                            ident:     Ident::new("Spannable", lt.span()),
                            arguments: PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                                colon2_token: None,
                                lt_token: Default::default(),
                                args: {
                                    let mut args = Punctuated::new();
                                    args.push(GenericArgument::Lifetime(lt.clone()));
                                    args
                                },
                                gt_token: Default::default(),
                            }),
                        });
                        path
                    },
                }));
            }
            bounds
        },
    })
}





/***** GENERATOR FUNCTIONS *****/
/// Injects the required bounds for implementing any of the traits in the given [`Generics`].
///
/// # Arguments
/// - `info`: The [`GenericImplInfo`] that we use to discover for which types to inject.
/// - `attrs`: The [`ToplevelAttrs`] that describe toplevel information needed to properly
///   construct paths and all that. And where we update bounds in.
/// - `gens`: The [`Generics`] to update.
pub fn inject_attrs_and_gens(info: &GenericImplInfo, attrs: &mut ToplevelAttrs, gens: &mut Generics) {
    // Inject the generic if not already there
    insert_ty_in_gens(&attrs.s, &mut gens.params);
    // Inject the lifetime if we're going to join
    if info.is_joining() {
        let was_empty: bool = attrs.impl_gen.is_none();
        insert_lt_in_gens(&attrs.lifetime, &mut attrs.impl_gen);
        if let (true, Some(impl_gen)) = (was_empty, &mut attrs.impl_gen) {
            impl_gen.extend(gens.params.clone());
        }
    }

    // Add bounds to `S`
    gens.make_where_clause().predicates.push(gen_s_where_pred(attrs, if info.is_joining() { Some(&attrs.lifetime) } else { None }));
}





/***** GENERIC INFO GENERATORS *****/
/// Wraps a [`GenericImplInfo`] in order to generate a list of [`WherePredicate`]s for a specific
/// trait impl.
pub struct GenericImplInfoWherePreds<'i, 'a>(&'i GenericImplInfo, &'a ToplevelAttrs, &'static str);
impl<'i, 'a> ToTokens for GenericImplInfoWherePreds<'i, 'a> {
    #[inline]
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        /// Appends `#ty: #crate::$trt<#s>,` to the given [`TokenStream2`].
        ///
        /// # Arguments
        /// - `attrs`: Some [`ToplevelAttrs`] to use for some settings (e.g., generic parameter and
        ///   `ast_toolkit_span`-path).
        /// - `ty`: Some type to put a bound for.
        /// - `trt`: The trait which bounds.
        /// - `tokens`: The [`TokenStream2`] to append to.
        fn append_ty_bound_with_s(attrs: &ToplevelAttrs, ty: &Type, trt: &'static str, tokens: &mut TokenStream2) {
            use proc_macro2::{Ident, Punct};
            ty.to_tokens(tokens);
            tokens.append(Punct::new(':', Spacing::Alone));
            attrs.ast_toolkit_span.to_tokens(tokens);
            tokens.append(Punct::new(':', Spacing::Joint));
            tokens.append(Punct::new(':', Spacing::Alone));
            tokens.append(Ident::new(trt, Span::mixed_site()));
            tokens.append(Punct::new('<', Spacing::Alone));
            attrs.s.to_tokens(tokens);
            tokens.append(Punct::new('>', Spacing::Alone));
            tokens.append(Punct::new(',', Spacing::Alone));
        }


        let Self(info, attrs, trt) = self;

        // Simply generate everything straightforwardly
        append_ty_bound_with_s(attrs, info.span_ty(), trt, tokens);
        for (_, ty) in info.joining() {
            append_ty_bound_with_s(attrs, ty, trt, tokens);
        }
    }
}
impl GenericImplInfo {
    /// Returns a wrapper type that will generate the where predicates needed to implement the
    /// required trait-bounds for that trait.
    ///
    /// # Arguments
    /// - `attrs`: Some [`ToplevelAttrs`] to use during generation.
    /// - `trt`: The name of the trait for which we're generating the where-clauses.
    ///
    /// # Returns
    /// A [`GenericImplInfoWherePreds`] that implements [`ToTokens`].
    #[inline]
    pub const fn where_preds<'i, 'a>(&'i self, attrs: &'a ToplevelAttrs, trt: &'static str) -> GenericImplInfoWherePreds<'i, 'a> {
        GenericImplInfoWherePreds(self, attrs, trt)
    }
}



/// Generator for populating the pattern for the parent type.
pub struct GenericImplInfoTyPat<'i>(&'i GenericImplInfo);
impl<'i> ToTokens for GenericImplInfoTyPat<'i> {
    #[inline]
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        use proc_macro2::{Delimiter, Group};

        let Self(info) = self;
        match info {
            GenericImplInfo::Struct { span, joining, .. } => {
                let mut inner = span.into_token_stream();
                for (ident, _) in joining {
                    inner.append(Punct::new(',', Spacing::Alone));
                    ident.to_tokens(&mut inner);
                }
                inner.append(Punct::new(',', Spacing::Alone));
                inner.append(Punct::new('.', Spacing::Joint));
                inner.append(Punct::new('.', Spacing::Alone));
                tokens.append(Group::new(Delimiter::Brace, inner));
            },
            GenericImplInfo::Tuple { idents, .. } => {
                tokens.append(Group::new(Delimiter::Parenthesis, idents.to_token_stream()));
            },
        }
    }
}
impl GenericImplInfo {
    /// Returns a wrapper that will generate the body of a pattern matching the struct/variant for
    /// which this GenericImplInfo was created.
    ///
    /// # Returns
    /// A [`GenericImplInfoTyPat`] which implements [`ToTokens`].
    #[inline]
    pub const fn ty_pat(&self) -> GenericImplInfoTyPat { GenericImplInfoTyPat(self) }
}



/// Defines a generic wrapper around multiple [`GenericImplInfo`]s, one for each variant of an
/// enum.
pub struct GenericEnumImplInfo(pub TokenStream2, pub TokenStream2, pub TokenStream2);
impl GenericEnumImplInfo {
    /// Creates a new GenericEnumImplInfo.
    ///
    /// Call [`GenericEnumImplInfo::add_variant()`] repeatedly to add enum variants. Then, when all
    /// are added, call [`GenericEnumImplInfo::finish()`] to get an implementator specific for your
    /// trait.
    ///
    /// # Returns
    /// A new GenericEnumImplInfo that can be loaded with variants.
    #[inline]
    pub fn new() -> Self { Self(TokenStream2::new(), TokenStream2::new(), TokenStream2::new()) }

    /// Adds a new variant to this enum implementation generator.
    ///
    /// # Arguments
    /// - `ident`: The name of the variant to add.
    /// - `ty_pat_impl`: Something generating the type pattern of your variant.
    /// - `func1_impl`: Something generating the implementation for a single variant for your
    ///   trait's first function.
    /// - `func2_impl`: Something generating the implementation for a single variant for your
    ///   trait's second function.
    /// - `where_preds_impl`: Something generating the appropriate where predicates for your trait.
    ///
    /// # Returns
    /// Self for chaining.
    pub fn add_variant(
        &mut self,
        ident: Ident,
        ty_pat_impl: impl ToTokens,
        func1_impl: impl ToTokens,
        func2_impl: impl ToTokens,
        where_preds_impl: impl ToTokens,
    ) -> &mut Self {
        use proc_macro2::Ident;
        let Self(where_preds, func1_tokens, func2_tokens) = self;

        // Append the where predicates
        where_preds_impl.to_tokens(where_preds);

        // Do the `get_span()` first
        func1_tokens.append(Ident::new("Self", ident.span()));
        func1_tokens.append(Punct::new(':', Spacing::Joint));
        func1_tokens.append(Punct::new(':', Spacing::Alone));
        ident.to_tokens(func1_tokens);
        ty_pat_impl.to_tokens(func1_tokens);
        func1_tokens.append(Punct::new('=', Spacing::Joint));
        func1_tokens.append(Punct::new('>', Spacing::Alone));
        func1_tokens.append(Group::new(Delimiter::Brace, func1_impl.into_token_stream()));

        // Then the `take_span()`
        func2_tokens.append(Ident::new("Self", ident.span()));
        func2_tokens.append(Punct::new(':', Spacing::Joint));
        func2_tokens.append(Punct::new(':', Spacing::Alone));
        ident.to_tokens(func2_tokens);
        ty_pat_impl.to_tokens(func2_tokens);
        func2_tokens.append(Punct::new('=', Spacing::Joint));
        func2_tokens.append(Punct::new('>', Spacing::Alone));
        func2_tokens.append(Group::new(Delimiter::Brace, func2_impl.into_token_stream()));

        // Done
        self
    }

    /// Adds a new variant to this enum implementation generator for traits that have only one
    /// function.
    ///
    /// # Arguments
    /// - `ident`: The name of the variant to add.
    /// - `ty_pat_impl`: Something generating the type pattern of your variant.
    /// - `func_impl`: Something generating the implementation for a single variant for your
    ///   trait's first function.
    /// - `where_preds_impl`: Something generating the appropriate where predicates for your trait.
    ///
    /// # Returns
    /// Self for chaining.
    pub fn add_variant_one(
        &mut self,
        ident: Ident,
        ty_pat_impl: impl ToTokens,
        func_impl: impl ToTokens,
        where_preds_impl: impl ToTokens,
    ) -> &mut Self {
        use proc_macro2::Ident;
        let Self(where_preds, func_tokens, _) = self;

        // Append the where predicates
        where_preds_impl.to_tokens(where_preds);

        // Do the `get_span()` first
        func_tokens.append(Ident::new("Self", ident.span()));
        func_tokens.append(Punct::new(':', Spacing::Joint));
        func_tokens.append(Punct::new(':', Spacing::Alone));
        ident.to_tokens(func_tokens);
        ty_pat_impl.to_tokens(func_tokens);
        func_tokens.append(Punct::new('=', Spacing::Joint));
        func_tokens.append(Punct::new('>', Spacing::Alone));
        func_tokens.append(Group::new(Delimiter::Brace, func_impl.into_token_stream()));

        // Done
        self
    }

    /// Finishes this predicate, returning a custom type that will do the actual implementation
    /// with the given token streams.
    ///
    /// # Generics
    /// - `T`: Some generator (something implementing [`ToTokens`]) that will generate the final
    ///   impl. It is supposed to be able to constructed ([`From`]) this GenericEnumImplInfo and
    ///   some other things that are contextually useful.
    ///
    /// # Arguments
    /// - `attrs`: A [`ToplevelAttrs`] to pass to `T`.
    /// - `gens`: A [`Generics`] to pass to `T`.
    /// - `ident`: The name of the whole enum to pass to `T`.
    ///
    /// # Returns
    /// A new instance of `T` that can generate things.
    pub fn finish<'a, 'g, 'i, T: From<(Self, &'a ToplevelAttrs, &'g Generics, &'i Ident)>>(
        self,
        attrs: &'a ToplevelAttrs,
        gens: &'g Generics,
        ident: &'i Ident,
    ) -> T {
        T::from((self, attrs, gens, ident))
    }
}



/// Wraps a [`WhereClause`] to generate it in the way we like.
pub struct WhereClauseWithoutWhereWithComma<'w>(pub &'w WhereClause);
impl<'w> ToTokens for WhereClauseWithoutWhereWithComma<'w> {
    #[inline]
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        self.0.predicates.to_tokens(tokens);
        if !self.0.predicates.empty_or_trailing() {
            tokens.append(Punct::new(',', Spacing::Alone));
        }
    }
}
