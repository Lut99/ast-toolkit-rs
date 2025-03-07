//  COMBINATOR.rs
//    by Lut99
//
//  Created:
//    06 Aug 2024, 15:23:00
//  Last edited:
//    07 Mar 2025, 18:41:36
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements an attribute for transforming functions into combinator
//!   factories.
//

use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{ToTokens, quote};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned as _;
use syn::token::{And, Comma, Gt, Lt, Mut, PathSep, RArrow, SelfValue};
use syn::{
    AngleBracketedGenericArguments, Attribute, Block, Error, Expr, FnArg, GenericArgument, GenericParam, Ident, Item, ItemFn, LitStr, Path,
    PathArguments, PathSegment, Receiver, ReturnType, Signature, Token, Type, TypePath, TypeReference, VisRestricted, Visibility, WhereClause,
    parenthesized, parse2,
};


/***** HELPER FUNCTIONS *****/
/// Generates the default return type.
///
/// # Arguments
/// - `attrs`: The [`CombinatorAttributes`] parsed from the main attribute.
///
/// # Returns
/// A [`Type`] representing the target return type.
#[inline]
fn default_return_type(attrs: &CombinatorAttributes) -> Type {
    // Build the generic arguments
    let mut args: Punctuated<GenericArgument, Comma> = Punctuated::new();
    args.push(GenericArgument::Type(attrs.output.clone()));
    args.push(GenericArgument::Type(attrs.recoverable.clone()));
    args.push(GenericArgument::Type(attrs.fatal.clone()));
    {
        let mut segs: Punctuated<PathSegment, PathSep> = Punctuated::new();
        segs.push(PathSegment { ident: Ident::new("F", Span::mixed_site()), arguments: PathArguments::None });
        args.push(GenericArgument::Type(Type::Path(TypePath { qself: None, path: Path { leading_colon: None, segments: segs } })));
    }
    {
        let mut segs: Punctuated<PathSegment, PathSep> = Punctuated::new();
        segs.push(PathSegment { ident: Ident::new("S", Span::mixed_site()), arguments: PathArguments::None });
        args.push(GenericArgument::Type(Type::Path(TypePath { qself: None, path: Path { leading_colon: None, segments: segs } })));
    }

    // Build the type path itself
    let mut path = attrs.prefix.clone();
    path.segments.push(PathSegment { ident: Ident::new("result", Span::mixed_site()), arguments: PathArguments::None });
    path.segments.push(PathSegment {
        ident:     Ident::new("Result", Span::mixed_site()),
        arguments: PathArguments::AngleBracketed(AngleBracketedGenericArguments {
            colon2_token: None,
            lt_token: Lt::default(),
            args,
            gt_token: Gt::default(),
        }),
    });

    // OK, return the type
    Type::Path(TypePath { qself: None, path })
}

/// Returns a clone of the given visibility, but with one additional layer added to it.
///
/// The use-case is: we generate a module but want to copy the visibility of the function defined
/// above it. For absolute paths, this is not a worry (child modules inherit that of parent
/// modules). However, for relative paths up, we need to bump it by one to account for the
/// additional module.
///
/// # Arguments
/// - `vis`: Some [`Visibility`] to copy and optionally bump.
///
/// # Returns
/// A new [`Visibility`] that has an additional level up if it's relative.
fn get_vis_plus_one(vis: &Visibility) -> Visibility {
    match vis {
        Visibility::Inherited => Visibility::Restricted(VisRestricted {
            pub_token: Default::default(),
            paren_token: Default::default(),
            in_token: None,
            path: Box::new(Path {
                leading_colon: None,
                segments:      {
                    let mut segments = Punctuated::new();
                    segments.push(PathSegment { ident: Ident::new("super", Span::mixed_site()), arguments: PathArguments::None });
                    segments
                },
            }),
        }),
        Visibility::Public(_) => vis.clone(),
        Visibility::Restricted(VisRestricted { pub_token, paren_token, in_token, path }) => {
            let mut new_path = path.clone();
            for seg in &path.segments {
                // If it starts with super, prepend one to account for the additional module
                if seg.ident == "super" {
                    new_path.segments.push(PathSegment { ident: seg.ident.clone(), arguments: PathArguments::None });
                } else if seg.ident == "self" {
                    // Basically the `.` of paths
                    continue;
                } else {
                    // It's absolute, no need to preprend.
                    break;
                }
            }
            Visibility::Restricted(VisRestricted { pub_token: *pub_token, paren_token: *paren_token, in_token: *in_token, path: new_path })
        },
    }
}



/// Generates the expected formatter.
///
/// # Arguments
/// - `attrs`: The [`CombinatorAttributes`] parsed from the main attribute.
/// - `func`: The main combinator function parsed from the input.
///
/// # Returns
/// A [`TokenStream2`] with the generated `ExpectsFormatter` implementation.
fn generate_formatter(attrs: &CombinatorAttributes, func: &CombinatorFunc) -> TokenStream2 {
    let prefix: &Path = &attrs.prefix;
    let vis: Visibility = get_vis_plus_one(&func.vis);
    let sname: String = func.sig.ident.to_string();
    let fname: &Ident = &attrs.fmt;
    quote! {
        #[doc = ::std::concat!("Expects strings formatter for the [`", #sname, "()`]-combinator.")]
        #[automatically_derived]
        #[derive(::std::clone::Clone, ::std::marker::Copy, ::std::fmt::Debug, ::std::cmp::Eq, ::std::cmp::PartialEq)]
        #vis struct #fname;
        #[automatically_derived]
        impl ::std::fmt::Display for #fname {
            #[inline]
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                write!(f, "Expected ")?;
                <Self as #prefix::ExpectsFormatter>::expects_fmt(self, f, 0)
            }
        }
    }
}

/// Generates the expected formatter implementation.
///
/// # Arguments
/// - `attrs`: The [`CombinatorAttributes`] parsed from the main attribute.
/// - `func`: The main combinator function parsed from the input.
///
/// # Returns
/// A [`TokenStream2`] with the generated `ExpectsFormatter` implementation.
fn generate_formatter_impl(attrs: &CombinatorAttributes) -> TokenStream2 {
    let prefix: &Path = &attrs.prefix;
    let module: &Ident = attrs.module.as_ref().unwrap();
    let fname: &Ident = &attrs.fmt;
    let exps: &ExpectedString = &attrs.expected;
    quote! {
        #[automatically_derived]
        impl #prefix::ExpectsFormatter for #module::#fname {
            #[inline]
            fn expects_fmt(&self, f: &mut ::std::fmt::Formatter, _ident: usize) -> ::std::fmt::Result { ::std::write!(f, #exps) }
        }
    }
}

/// Generates the main combinator implementation.
///
/// # Arguments
/// - `attrs`: The [`CombinatorAttributes`] parsed from the main attribute.
/// - `func`: The main combinator function parsed from the input.
///
/// # Returns
/// A [`TokenStream2`] with the generated `ExpectsFormatter` implementation.
fn generate_combinator(attrs: &CombinatorAttributes, func: &CombinatorFunc) -> TokenStream2 {
    let vis: Visibility = get_vis_plus_one(&func.vis);
    let sname: String = func.sig.ident.to_string();
    let cname: &Ident = &attrs.comb;
    quote! {
        #[doc = ::std::concat!("Combinator returned by the [`", #sname, "()`]-combinator.")]
        #[automatically_derived]
        #[derive(::std::clone::Clone, ::std::marker::Copy, ::std::fmt::Debug)]
        #vis struct #cname<F, S> {
            pub(super) _f: ::std::marker::PhantomData<F>,
            pub(super) _s: ::std::marker::PhantomData<S>,
        }
    }
}

/// Generates the combinator implementation.
///
/// # Arguments
/// - `attrs`: The [`CombinatorAttributes`] parsed from the main attribute.
/// - `func`: The main combinator function parsed from the input.
///
/// # Returns
/// A [`TokenStream2`] with the generated `Combinator` implementation.
fn generate_combinator_impl(attrs: &CombinatorAttributes, func: &CombinatorFunc) -> TokenStream2 {
    // Mod the signature to be correct
    let (mut prm, mut whr): (Punctuated<GenericParam, Token![,]>, Option<WhereClause>) = (Punctuated::new(), None);
    let mut sig: Signature = func.sig.clone();
    sig.ident = Ident::new("parse", sig.ident.span());
    if !sig.inputs.is_empty() || matches!(sig.inputs[0], FnArg::Receiver(_)) {
        let mut segs: Punctuated<PathSegment, PathSep> = Punctuated::new();
        segs.push(PathSegment { ident: Ident::new("Self", Span::mixed_site()), arguments: PathArguments::None });
        sig.inputs.insert(
            0,
            FnArg::Receiver(Receiver {
                attrs: vec![],
                reference: Some((And::default(), None)),
                mutability: Some(Mut::default()),
                self_token: SelfValue::default(),
                colon_token: None,
                ty: Box::new(Type::Reference(TypeReference {
                    and_token: And::default(),
                    lifetime: None,
                    mutability: Some(Mut::default()),
                    elem: Box::new(Type::Path(TypePath { qself: None, path: Path { leading_colon: None, segments: segs } })),
                })),
            }),
        );
    }
    std::mem::swap(&mut prm, &mut sig.generics.params);
    std::mem::swap(&mut whr, &mut sig.generics.where_clause);

    // Write the impl
    let module: &Ident = attrs.module.as_ref().unwrap();
    let prefix: &Path = &attrs.prefix;
    let fname: &Ident = &attrs.fmt;
    let cname: &Ident = &attrs.comb;
    let (out, recoverable, fatal): (&Type, &Type, &Type) = (&attrs.output, &attrs.recoverable, &attrs.fatal);
    let body: &Block = &func.body;
    quote! {
        #[automatically_derived]
        impl<#prm> #prefix::Combinator<'static, F, S> for #module::#cname<F, S> #whr {
            type ExpectsFormatter = #module::#fname;
            type Output = #out;
            type Recoverable = #recoverable;
            type Fatal = #fatal;

            #[inline]
            fn expects(&self) -> Self::ExpectsFormatter { #module::#fname }

            #sig #body
        }
    }
}

/// Generates the final combinator.
///
/// # Arguments
/// - `attrs`: The [`CombinatorAttributes`] parsed from the main attribute.
/// - `func`: The main combinator function parsed from the input.
///
/// # Returns
/// A [`TokenStream2`] with the generated factory method implementation.
fn generate_factory(attrs: &CombinatorAttributes, func: &CombinatorFunc) -> TokenStream2 {
    let module: &Ident = attrs.module.as_ref().unwrap();
    let ars: &[Attribute] = &func.attrs;
    let vis: &Visibility = &func.vis;
    let name: &Ident = &func.sig.ident;
    let cname: &Ident = &attrs.comb;
    let (impl_gen, ty_gen, where_gen) = func.sig.generics.split_for_impl();
    quote! {
        #(#ars)*
        #[inline]
        #vis const fn #name #impl_gen() -> #module::#cname #ty_gen #where_gen {
            #module::#cname {
                _f: ::std::marker::PhantomData,
                _s: ::std::marker::PhantomData,
            }
        }
    }
}





/***** HELPERS *****/
/// Represents the parsed information from the attribute.
#[derive(Clone, Debug)]
struct CombinatorAttributes {
    /// The prefix path to prefix before all modules. Will default to `::ast_toolkit::snack`.
    prefix: Path,

    /// The name of the module to generate. If omitted, equals the camelcase version of the function name.
    module: Option<Ident>,
    /// The name of the combinator to generate within the module. If omitted, equals `Combinator`.
    comb:   Ident,
    /// The name of the expects formatter to generate within the module. If omitted, equals `ExpectsFormatter`.
    fmt:    Ident,

    /// The string describing what to expect.
    expected: ExpectedString,
    /// Any output type. Defaults to `()`.
    output: Type,
    /// Any (recoverable) error type. Defaults to `::std::convert::Infallible`.
    recoverable: Type,
    /// Any (fatal) error type. Defaults to `::std::convert::Infallible`.
    fatal: Type,
}
impl Parse for CombinatorAttributes {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let total: Span = input.span();

        // Parse the input as a list of metas
        let mut prefix: Option<Path> = None;
        let mut module: Option<Ident> = None;
        let mut comb: Option<Ident> = None;
        let mut fmt: Option<Ident> = None;
        let mut expected: Option<ExpectedString> = None;
        let mut output: Option<Type> = None;
        let mut recoverable: Option<Type> = None;
        let mut fatal: Option<Type> = None;
        let attrs: Punctuated<AttrNameValue, Token![,]> = Punctuated::parse_terminated(input)?;
        for attr in attrs {
            match attr {
                AttrNameValue::Prefix(v) => {
                    prefix = Some(v);
                },
                AttrNameValue::Module(v) => {
                    module = Some(v);
                },
                AttrNameValue::Combinator(v) => {
                    comb = Some(v);
                },
                AttrNameValue::Formatter(v) => {
                    fmt = Some(v);
                },
                AttrNameValue::Expected(v) => {
                    expected = Some(v);
                },
                AttrNameValue::Output(v) => {
                    output = Some(v);
                },
                AttrNameValue::Recoverable(v) => {
                    recoverable = Some(v);
                },
                AttrNameValue::Fatal(v) => {
                    fatal = Some(v);
                },
            }
        }

        // Unwrap the options
        let prefix: Path = match prefix {
            Some(ident) => ident,
            None => Path {
                leading_colon: Some(Default::default()),
                segments:      {
                    let mut segments = Punctuated::new();
                    segments.push(PathSegment { ident: Ident::new("ast_toolkit", Span::mixed_site()), arguments: PathArguments::None });
                    segments.push(PathSegment { ident: Ident::new("snack", Span::mixed_site()), arguments: PathArguments::None });
                    segments
                },
            },
        };
        let comb: Ident = match comb {
            Some(ident) => ident,
            None => Ident::new("Combinator", Span::mixed_site()),
        };
        let fmt: Ident = match fmt {
            Some(ident) => ident,
            None => Ident::new("ExpectsFormatter", Span::mixed_site()),
        };
        let expected: ExpectedString = match expected {
            Some(exp) => exp,
            None => return Err(Error::new(total, "Missing 'expected = \"...\"' attribute")),
        };
        let output: Type = match output {
            Some(out) => out,
            None => {
                let mut path: Path = prefix.clone();
                path.segments.push(PathSegment {
                    ident:     Ident::new("Span", Span::mixed_site()),
                    arguments: PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                        colon2_token: None,
                        lt_token: Default::default(),
                        gt_token: Default::default(),
                        args: {
                            let mut args = Punctuated::new();
                            args.push(GenericArgument::Type(Type::Path(TypePath {
                                qself: None,
                                path:  Path {
                                    leading_colon: None,
                                    segments:      {
                                        let mut segments = Punctuated::new();
                                        segments.push(PathSegment { ident: Ident::new("F", Span::mixed_site()), arguments: PathArguments::None });
                                        segments
                                    },
                                },
                            })));
                            args.push(GenericArgument::Type(Type::Path(TypePath {
                                qself: None,
                                path:  Path {
                                    leading_colon: None,
                                    segments:      {
                                        let mut segments = Punctuated::new();
                                        segments.push(PathSegment { ident: Ident::new("S", Span::mixed_site()), arguments: PathArguments::None });
                                        segments
                                    },
                                },
                            })));
                            args
                        },
                    }),
                });
                Type::Path(TypePath { qself: None, path })
            },
        };
        let recoverable: Type = match recoverable {
            Some(out) => out,
            None => {
                let mut segs: Punctuated<PathSegment, PathSep> = Punctuated::new();
                segs.push(PathSegment { ident: Ident::new("std", Span::mixed_site()), arguments: PathArguments::None });
                segs.push(PathSegment { ident: Ident::new("convert", Span::mixed_site()), arguments: PathArguments::None });
                segs.push(PathSegment { ident: Ident::new("Infallible", Span::mixed_site()), arguments: PathArguments::None });
                Type::Path(TypePath { qself: None, path: Path { leading_colon: Some(PathSep::default()), segments: segs } })
            },
        };
        let fatal: Type = match fatal {
            Some(out) => out,
            None => {
                let mut segs: Punctuated<PathSegment, PathSep> = Punctuated::new();
                segs.push(PathSegment { ident: Ident::new("std", Span::mixed_site()), arguments: PathArguments::None });
                segs.push(PathSegment { ident: Ident::new("convert", Span::mixed_site()), arguments: PathArguments::None });
                segs.push(PathSegment { ident: Ident::new("Infallible", Span::mixed_site()), arguments: PathArguments::None });
                Type::Path(TypePath { qself: None, path: Path { leading_colon: Some(PathSep::default()), segments: segs } })
            },
        };

        // OK, construct self
        Ok(Self { prefix, module, comb, fmt, expected, output, recoverable, fatal })
    }
}

/// Represents a name/value pair for the attributes.
#[derive(Clone, Debug)]
enum AttrNameValue {
    /// It's the start of a path to find the snack stuff in.
    Prefix(Path),

    /// It's the name of the generated module.
    Module(Ident),
    /// It's the name of the generated combinator.
    Combinator(Ident),
    /// It's the name of the generated expects formatter.
    Formatter(Ident),

    /// It's the expected string.
    Expected(ExpectedString),
    /// It's the output type.
    Output(Type),
    /// It's the recoverable error type.
    Recoverable(Type),
    /// It's the fatal error type.
    Fatal(Type),
}
impl Parse for AttrNameValue {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        // Parse the identifier first, and  then always expected an equals
        let ident: Ident = input.parse()?;
        input.parse::<Token![=]>()?;
        if ident == "prefix" || ident == "snack" {
            Ok(Self::Prefix(input.parse::<Path>()?))
        } else if ident == "mod" || ident == "module" || ident == "Mod" || ident == "Module" {
            Ok(Self::Module(input.parse::<Ident>()?))
        } else if ident == "comb" || ident == "combinator" || ident == "Comb" || ident == "Combinator" {
            Ok(Self::Combinator(input.parse::<Ident>()?))
        } else if ident == "fmt" || ident == "formatter" || ident == "Fmt" || ident == "Formatter" {
            Ok(Self::Formatter(input.parse::<Ident>()?))
        } else if ident == "expected" || ident == "Expected" {
            Ok(Self::Expected(input.parse::<ExpectedString>()?))
        } else if ident == "output" || ident == "Output" {
            Ok(Self::Output(input.parse::<Type>()?))
        } else if ident == "recoverable" || ident == "Recoverable" {
            Ok(Self::Recoverable(input.parse::<Type>()?))
        } else if ident == "fatal" || ident == "Fatal" {
            Ok(Self::Fatal(input.parse::<Type>()?))
        } else {
            Err(Error::new(ident.span(), format!("Unknown attribute '{ident}'")))
        }
    }
}

/// Represents one of two types of expected strings.
#[derive(Clone, Debug)]
enum ExpectedString {
    /// It's a direct literal
    Lit(LitStr),
    /// It's a formatter string.
    Fmt(LitStr, Punctuated<Expr, Token![,]>),
}
impl Parse for ExpectedString {
    #[inline]
    fn parse(input: ParseStream) -> syn::Result<Self> {
        // Attempt to parse a string literal, first
        if let Ok(lit) = input.parse::<LitStr>() {
            return Ok(ExpectedString::Lit(lit));
        }

        // Else, check if there are parenthesis.
        let content;
        parenthesized!(content in input);
        let fmt: LitStr = content.parse()?;
        let args: Punctuated<Expr, Token![,]> =
            if content.parse::<Token![,]>().is_ok() { Punctuated::parse_terminated(&content)? } else { Punctuated::new() };
        Ok(ExpectedString::Fmt(fmt, args))
    }
}
impl ToTokens for ExpectedString {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match self {
            Self::Lit(lit) => lit.to_tokens(tokens),
            Self::Fmt(fmt, args) => {
                fmt.to_tokens(tokens);
                if !args.is_empty() {
                    Comma::default().to_tokens(tokens);
                    args.to_tokens(tokens);
                }
            },
        }
    }
}

/// Represents the wrapper combinator function.
#[derive(Clone, Debug)]
struct CombinatorFunc {
    /// The attributes of the function.
    attrs: Vec<Attribute>,
    /// The visibility of the function.
    vis:   Visibility,
    /// The signature of the function itself.
    sig:   Signature,
    /// The body of the combinator.
    body:  Block,
}
impl Parse for CombinatorFunc {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        // Parse it as a Rust function first
        let item: Item = input.parse()?;
        let func: ItemFn = if let Item::Fn(func) = item { func } else { return Err(Error::new(item.span(), "Expected a function")) };

        // OK, construct self
        Ok(Self { attrs: func.attrs, vis: func.vis, sig: func.sig, body: *func.block })
    }
}





/***** LIBRARY *****/
/// Calls the macro.
///
/// # Arguments
/// - `attrs`: A [`TokenStream2`] that encodes the contents of the main `#[combinator(...)]` attribute.
/// - `input`: A [`TokenStream2`] encoding the input (function).
///
/// # Returns
/// A [`TokenStream2`] with the generated combinator.
///
/// # Errors
/// If anything fails during the above process, an appropriate [`Diagnostic`] is generated.
pub fn call(attrs: TokenStream2, input: TokenStream2) -> Result<TokenStream2, Error> {
    // Start by parsing the streams
    let mut attrs: CombinatorAttributes = parse2(attrs)?;
    let mut func: CombinatorFunc = parse2(input)?;

    // Populate defaults for the attributes
    if attrs.module.is_none() {
        attrs.module = Some(func.sig.ident.clone());
    }

    // And for the function
    if let ReturnType::Type(_, ty) = &mut func.sig.output {
        if matches!(**ty, Type::Infer(_)) {
            // Generate a default type instead
            *ty = Box::new(default_return_type(&attrs));
        }
    } else {
        // Also generate the default
        func.sig.output = ReturnType::Type(RArrow::default(), Box::new(default_return_type(&attrs)));
    }

    // Generate the parts of the combinator implementation
    let fmt: TokenStream2 = generate_formatter(&attrs, &func);
    let fmt_impl: TokenStream2 = generate_formatter_impl(&attrs);
    let comb: TokenStream2 = generate_combinator(&attrs, &func);
    let comb_impl: TokenStream2 = generate_combinator_impl(&attrs, &func);
    let factory: TokenStream2 = generate_factory(&attrs, &func);

    // OK, return that
    let module: &Ident = attrs.module.as_ref().unwrap();
    let vis = func.vis;
    Ok(quote! {
        #vis mod #module {
            use super::*;
            #fmt
            #comb
        }
        #fmt_impl
        #comb_impl
        #factory
    })
}
