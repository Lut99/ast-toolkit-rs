//  ANALYZE.rs
//    by Lut99
//
//  Description:
//!   Contains macro code for understanding the to-be-derived struct/enum, which is the same across
//!   the different macros.
//

use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::ToTokens;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::token::Underscore;
use syn::{
    Attribute, Error, Fields, GenericParam, Ident, Lifetime, Meta, Path, PathArguments, PathSegment, Token, Type, WherePredicate, parenthesized,
};


/***** PARSING *****/
/// Defines super toplevel attributes.
pub struct ToplevelAttrs {
    /// The path to the `ast_toolkit::span`-crate.
    pub ast_toolkit_span: Path,
    /// The generic type `S`.
    pub s: Ident,
    /// The lifetime to use when generating `Spannable` bounds.
    pub lifetime: Lifetime,
    /// Any diverging impl generics.
    pub impl_gen: Option<Punctuated<GenericParam, Token![,]>>,
    /// Any bounds to put at the end of the file.
    pub bounds: Option<Punctuated<WherePredicate, Token![,]>>,
}
impl Default for ToplevelAttrs {
    #[inline]
    fn default() -> Self {
        Self {
            ast_toolkit_span: Path {
                leading_colon: Some(Default::default()),
                segments:      {
                    let mut segs = Punctuated::new();
                    segs.push(PathSegment { ident: Ident::new("ast_toolkit", Span::mixed_site()), arguments: PathArguments::None });
                    segs.push(PathSegment { ident: Ident::new("span", Span::mixed_site()), arguments: PathArguments::None });
                    segs
                },
            },
            s: Ident::new("S", Span::mixed_site()),
            lifetime: Lifetime { apostrophe: Span::mixed_site(), ident: Ident::new("s", Span::mixed_site()) },
            impl_gen: None,
            bounds: None,
        }
    }
}
impl ToplevelAttrs {
    /// Parses the ToplevelAttrs from a given list of attributes.
    ///
    /// # Arguments
    /// - `attrs`: The list of [`Attribute`]s to parse.
    ///
    /// # Returns
    /// A ToplevelAttrs representing the `attrs`' information.
    ///
    /// # Errors
    /// This function can error if we recognized any attribute as our own, but failed to parse its
    /// contents.
    pub fn parse(attrs: impl IntoIterator<Item = Attribute>) -> Result<Self, Error> {
        /// Defines a single ToplevelAttr and how to parse it.
        enum ToplevelAttr {
            Crate(Path),
            Generic(Ident),
            Lifetime(Lifetime),
            ImplGen(Punctuated<GenericParam, Token![,]>),
            Bounds(Punctuated<WherePredicate, Token![,]>),
        }
        impl Parse for ToplevelAttr {
            #[inline]
            fn parse(input: ParseStream) -> syn::Result<Self> {
                let path: Path = input.parse()?;
                if path.is_ident("crate") {
                    // `crate = ast_toolkit::span`
                    input.parse::<Token![=]>()?;
                    Ok(Self::Crate(input.parse()?))
                } else if path.is_ident("gen") || path.is_ident("generic") {
                    // `generic = S`
                    input.parse::<Token![=]>()?;
                    Ok(Self::Generic(input.parse()?))
                } else if path.is_ident("impl_gen") {
                    // `generic = S`
                    input.parse::<Token![=]>()?;
                    input.parse::<Token![<]>()?;
                    let generics = Punctuated::parse_terminated(input)?;
                    input.parse::<Token![>]>()?;
                    Ok(Self::ImplGen(generics))
                } else if path.is_ident("lifetime") {
                    // `lifetime = 's`
                    input.parse::<Token![=]>()?;
                    Ok(Self::Lifetime(input.parse()?))
                } else if path.is_ident("bound") || path.is_ident("bounds") {
                    // `bound = (S: Spannable<'s>)`
                    input.parse::<Token![=]>()?;
                    let content;
                    parenthesized!(content in input);
                    Ok(Self::Bounds(Punctuated::parse_terminated(&content)?))
                } else {
                    Err(Error::new(path.span(), "Unknown attribute for `spanning`"))
                }
            }
        }


        // Iterate over all attributes
        let mut res = Self::default();
        for attr in attrs {
            match attr.meta {
                Meta::List(l) if l.path.is_ident("spanning") => {
                    // Parse the arguments to the list as specialized metas
                    let attrs: Punctuated<ToplevelAttr, Token![,]> = l.parse_args_with(Punctuated::parse_terminated)?;
                    for attr in attrs {
                        match attr {
                            ToplevelAttr::Crate(p) => res.ast_toolkit_span = p,
                            ToplevelAttr::Generic(i) => res.s = i,
                            ToplevelAttr::Lifetime(l) => res.lifetime = l,
                            ToplevelAttr::ImplGen(ig) => res.impl_gen = Some(ig),
                            ToplevelAttr::Bounds(b) => res.bounds = Some(b),
                        }
                    }
                },

                Meta::Path(p) if p.is_ident("spanning") => {
                    return Err(Error::new(p.span(), "`spanning`-attribute must be followed by a list of name/value pairs"));
                },
                Meta::NameValue(nv) if nv.path.is_ident("spanning") => {
                    return Err(Error::new(nv.path.span(), "`spanning`-attribute cannot be used with name/value syntax"));
                },

                // Ignore anything else
                _ => continue,
            }
        }
        Ok(res)
    }
}



/// Defines what we will learn from field-level attributes.
pub struct FieldAttrs {
    /// Whether this field is marked as The Span:tm:
    pub is_span:    bool,
    /// Whether the span is joined with the main one.
    pub is_joining: bool,
}
impl Default for FieldAttrs {
    #[inline]
    fn default() -> Self { Self { is_span: false, is_joining: false } }
}
impl FieldAttrs {
    /// Parses the FieldAttrs from a given list of attributes.
    ///
    /// # Arguments
    /// - `attrs`: The list of [`Attribute`]s to parse.
    ///
    /// # Returns
    /// A FieldsAttr representing the `attrs`' information.
    ///
    /// # Errors
    /// This function can error if we recognized any attribute as our own, but failed to parse its
    /// contents.
    pub fn parse(attrs: impl IntoIterator<Item = Attribute>) -> Result<Self, Error> {
        enum FieldAttr {
            Joining,
        }
        impl Parse for FieldAttr {
            #[inline]
            fn parse(input: ParseStream) -> syn::Result<Self> {
                // Match on the initial identifier
                let path: Path = input.parse()?;
                if path.is_ident("join") || path.is_ident("joining") {
                    Ok(Self::Joining)
                } else {
                    Err(Error::new(path.span(), "Unknown attribute for `span`"))
                }
            }
        }


        // Go thru all of the attributes
        let mut res = Self::default();
        for attr in attrs {
            match attr.meta {
                Meta::Path(p) if p.is_ident("span") => {
                    // That's gooood, it's just the blank `span`-identifier.
                    res.is_span = true;
                },

                // Name values we don't care about
                Meta::NameValue(nv) if nv.path.is_ident("span") => {
                    return Err(Error::new(nv.path.span(), "`span`-attribute cannot be used with name/value syntax"));
                },

                // List we dive deeper at
                Meta::List(l) if l.path.is_ident("span") => {
                    // Parse the arguments to the list as specialized metas
                    let attrs: Punctuated<FieldAttr, Token![,]> = l.parse_args_with(Punctuated::parse_terminated)?;
                    for attr in attrs {
                        match attr {
                            FieldAttr::Joining => res.is_joining = true,
                        }
                    }
                },

                // Anything else, we ignore
                _ => continue,
            }
        }
        Ok(res)
    }
}





/***** AUXILLARY *****/
/// Defines everything we need to know to generate an implementation for a set of fields.
pub enum GenericImplInfo {
    /// We're implementing for a struct struct/variant.
    Struct {
        /// The span's identifier.
        span:    Ident,
        /// The span's type.
        ty:      Type,
        /// A list of ident/type pairs of every joined field.
        joining: Vec<(Ident, Type)>,
    },
    /// We're implementing for a tuple struct/variant.
    Tuple {
        /// The list of field identifiers. Those unused are compiled as wildcards.
        idents:  Punctuated<IdentOrWildcard, Token![,]>,
        /// The index of the item that is going to be the span.
        span:    usize,
        /// The type of the item that is going to be the span.
        ty:      Type,
        /// A list of index/type pairs of every joined field.
        joining: Vec<(usize, Type)>,
    },
}
impl GenericImplInfo {
    /// Returns the span's identifier.
    ///
    /// # Returns
    /// An [`Ident`] naming the span's identifier.
    pub fn span_ident(&self) -> &Ident {
        match self {
            Self::Struct { span, .. } => span,
            Self::Tuple { idents, span, .. } => match &idents[*span] {
                IdentOrWildcard::Ident(ident) => ident,
                IdentOrWildcard::Wildcard(_) => panic!("`span` points to a wildcard instead of an ident"),
            },
        }
    }

    /// Return the span's type.
    ///
    /// # Returns
    /// A [`Type`] naming the span's identifier.
    pub fn span_ty(&self) -> &Type {
        match self {
            Self::Struct { ty, .. } => ty,
            Self::Tuple { ty, .. } => ty,
        }
    }

    /// Returns an iterator over the joining spans.
    ///
    /// # Returns
    /// An [`Iterator`] yielding `(Ident, Type)` pairs.
    pub fn joining<'s>(&'s self) -> Box<dyn 's + Iterator<Item = (&'s Ident, &'s Type)>> {
        match self {
            Self::Struct { joining, .. } => Box::new(joining.iter().map(|(ident, ty)| (ident, ty))),
            Self::Tuple { idents, joining, .. } => Box::new(joining.iter().map(|(i, ty)| {
                (
                    match &idents[*i] {
                        IdentOrWildcard::Ident(ident) => ident,
                        IdentOrWildcard::Wildcard(_) => panic!("`span` points to a wildcard instead of an ident"),
                    },
                    ty,
                )
            })),
        }
    }

    /// Checks if there is any joining spans.
    ///
    /// # Returns
    /// True if a `join()` is necessary, false otherwise.
    #[inline]
    pub fn is_joining(&self) -> bool {
        match self {
            Self::Struct { joining, .. } => !joining.is_empty(),
            Self::Tuple { joining, .. } => !joining.is_empty(),
        }
    }
}



/// Defines the possible ways in which we generate fields.
pub enum IdentOrWildcard {
    Ident(Ident),
    Wildcard(Span),
}
impl ToTokens for IdentOrWildcard {
    #[inline]
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match self {
            Self::Ident(i) => i.to_tokens(tokens),
            Self::Wildcard(span) => Underscore { spans: [*span] }.to_tokens(tokens),
        }
    }
}





/***** LIBRARY *****/
/// Given a set of fields, returns the information needed to create an impl for this one.
///
/// # Arguments
/// - `fields`: Some [`Fields`] to analyze for what we need to know to implement some `Spanning`.
///
/// # Returns
/// A tuple with loose information that calling functions can turn into something public.
pub fn analyze_fields(fields: Fields) -> Result<GenericImplInfo, Error> {
    // Split on the style of fields
    match fields {
        Fields::Named(fields) => {
            // Find the field marked as span
            let mut span: Option<(Ident, Type)> = None;
            let mut joining: Vec<(Ident, Type)> = Vec::new();
            for field in fields.named {
                // SAFETY: We can do this because we're sure the field is named
                let ident: Ident = field.ident.unwrap();
                let attrs = FieldAttrs::parse(field.attrs)?;
                if attrs.is_span || ident == "span" {
                    // Just overwrite, I can't be bothered
                    span = Some((ident, field.ty));
                } else if attrs.is_joining {
                    joining.push((ident, field.ty));
                }
            }
            if let Some((span, ty)) = span {
                Ok(GenericImplInfo::Struct { span, ty, joining })
            } else {
                Err(Error::new(Span::mixed_site(), "No 'span'-field found; either name one of the `span`, or mark it with `#[span]`"))
            }
        },
        Fields::Unnamed(fields) => {
            // Find the field marked as span, generating idents as we go
            let mut span: Option<(usize, Type)> = None;
            let mut joining: Vec<(usize, Type)> = Vec::new();
            let idents: Punctuated<IdentOrWildcard, Token![,]> = fields
                .unnamed
                .into_iter()
                .enumerate()
                .map(|(i, field)| {
                    // Parse the attributes
                    let attrs = FieldAttrs::parse(field.attrs)?;
                    if attrs.is_span {
                        // Just overwrite, I can't be bothered
                        let ty_span: Span = field.ty.span();
                        span = Some((i, field.ty));
                        Ok(IdentOrWildcard::Ident(Ident::new(&format!("field{i}"), ty_span)))
                    } else if attrs.is_joining {
                        let ty_span: Span = field.ty.span();
                        joining.push((i, field.ty));
                        Ok(IdentOrWildcard::Ident(Ident::new(&format!("field{i}"), ty_span)))
                    } else {
                        Ok(IdentOrWildcard::Wildcard(field.ty.span()))
                    }
                })
                .collect::<Result<Punctuated<_, _>, Error>>()?;
            if let Some((span, ty)) = span {
                Ok(GenericImplInfo::Tuple { idents, span, ty, joining })
            } else {
                Err(Error::new(Span::mixed_site(), "No 'span'-field found; mark one with `#[span]` to indicate it holds the span for this type"))
            }
        },
        Fields::Unit => Err(Error::new(Span::mixed_site(), "No 'span'-found; add fields and name one 'span' or mark it with `#[span]`")),
    }
}
