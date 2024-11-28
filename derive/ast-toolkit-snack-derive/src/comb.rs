//  COMBINATOR.rs
//    by Lut99
//
//  Created:
//    06 Aug 2024, 15:23:00
//  Last edited:
//    28 Nov 2024, 14:37:56
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements an attribute for transforming functions into combinator
//!   factories.
//

use std::fmt::{Display, Formatter, Result as FResult};

use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{quote, ToTokens};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned as _;
use syn::token::{And, Comma, Gt, Lt, Mut, Paren, PathSep, RArrow, SelfValue};
use syn::{
    parse2, AngleBracketedGenericArguments, Attribute, Block, Error, Expr, ExprLit, ExprTuple, FnArg, GenericArgument, GenericParam, Ident, Item,
    ItemFn, Lifetime, Lit, LitStr, Path, PathArguments, PathSegment, Receiver, ReturnType, Signature, Token, Type, TypePath, TypeReference,
    TypeTuple, Visibility, WhereClause,
};
use unicode_segmentation::UnicodeSegmentation;


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
    args.push(GenericArgument::Lifetime(Lifetime::new("'static", Span::mixed_site())));
    args.push(GenericArgument::Type(attrs.output.clone()));
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
    args.push(GenericArgument::Type(attrs.error.clone()));

    // Build the type path itself
    let mut segs: Punctuated<PathSegment, PathSep> = Punctuated::new();
    segs.push(PathSegment { ident: Ident::new("ast_toolkit_snack", Span::mixed_site()), arguments: PathArguments::None });
    segs.push(PathSegment {
        ident:     Ident::new("Result", Span::mixed_site()),
        arguments: PathArguments::AngleBracketed(AngleBracketedGenericArguments {
            colon2_token: None,
            lt_token: Lt::default(),
            args,
            gt_token: Gt::default(),
        }),
    });

    // OK, return the type
    Type::Path(TypePath { qself: None, path: Path { leading_colon: Some(PathSep::default()), segments: segs } })
}



/// Generates the expected formatter implementation.
///
/// # Arguments
/// - `attrs`: The [`CombinatorAttributes`] parsed from the main attribute.
/// - `func`: The main combinator function parsed from the input.
///
/// # Returns
/// A [`TokenStream2`] with the generated `ExpectsFormatter` implementation.
fn generate_formatter(attrs: &CombinatorAttributes, func: &CombinatorFunc) -> TokenStream2 {
    let mut exp_fmt: Path = attrs.prefix.clone();
    exp_fmt.segments.push(PathSegment { ident: Ident::new("ExpectsFormatter", Span::call_site()), arguments: PathArguments::None });
    let sname: String = func.sig.ident.to_string();
    let vis: &Visibility = &func.vis;
    let fname: &Ident = attrs.fmt.as_ref().unwrap();
    let exps: &ExpectedString = &attrs.expected;
    quote! {
        #[doc = ::std::concat!("Expects strings formatter for the [`", #sname, "()`]-combinator.")]
        #[automatically_derived]
        #[derive(::std::clone::Clone, ::std::marker::Copy, ::std::fmt::Debug)]
        #vis struct #fname;
        #[automatically_derived]
        impl ::std::fmt::Display for #fname {
            #[inline]
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                write!(f, "Expected ")?;
                <Self as #exp_fmt>::expects_fmt(self, f, 0)
            }
        }
        #[automatically_derived]
        impl #exp_fmt for #fname {
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
    let sname: String = func.sig.ident.to_string();
    let vis: &Visibility = &func.vis;
    let cname: &Ident = attrs.comb.as_ref().unwrap();
    quote! {
        #[doc = ::std::concat!("Combinator returned by the [`", #sname, "()`]-combinator.")]
        #[automatically_derived]
        #[derive(::std::clone::Clone, ::std::marker::Copy, ::std::fmt::Debug)]
        #vis struct #cname<F, S> {
            _f: ::std::marker::PhantomData<F>,
            _s: ::std::marker::PhantomData<S>,
        }
    }
}

/// Generates the expects implementation.
///
/// # Arguments
/// - `attrs`: The [`CombinatorAttributes`] parsed from the main attribute.
///
/// # Returns
/// A [`TokenStream2`] with the generated `Expects` implementation.
fn generate_expects_impl(attrs: &CombinatorAttributes) -> TokenStream2 {
    // Prepare the trait path
    let mut exp: Path = attrs.prefix.clone();
    exp.segments.push(PathSegment { ident: Ident::new("Expects", Span::call_site()), arguments: PathArguments::None });

    // Generate the impl
    let fname: &Ident = attrs.fmt.as_ref().unwrap();
    let cname: &Ident = attrs.comb.as_ref().unwrap();
    quote! {
        #[automatically_derived]
        impl<F, S> #exp<'static> for #cname<F, S> {
            type Formatter = #fname;

            #[inline]
            fn expects(&self) -> Self::Formatter { #fname }
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

    // Prepare the combinator path
    let mut comb = attrs.prefix.clone();
    comb.segments.push(PathSegment { ident: Ident::new("Combinator", Span::call_site()), arguments: PathArguments::None });

    // Write the impl
    let cname: &Ident = attrs.comb.as_ref().unwrap();
    let (out, err): (&Type, &Type) = (&attrs.output, &attrs.error);
    let body: &Block = &func.body;
    quote! {
        #[automatically_derived]
        impl<#prm> #comb<'static, F, S> for #cname<F, S> #whr {
            type Output = #out;
            type Error = #err;

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
    let ars: &[Attribute] = &func.attrs;
    let vis: &Visibility = &func.vis;
    let name: &Ident = &func.sig.ident;
    let cname: &Ident = attrs.comb.as_ref().unwrap();
    let (impl_gen, ty_gen, where_gen) = func.sig.generics.split_for_impl();
    quote! {
        #(#ars)*
        #[inline]
        #vis const fn #name #impl_gen() -> #cname #ty_gen #where_gen {
            #cname {
                _f: ::std::marker::PhantomData,
                _s: ::std::marker::PhantomData,
            }
        }
    }
}





/***** HELPERS *****/
/// [`Display`]s a string but with a capital first letter.
struct CamelCaseifyer<S>(S);
impl<S: AsRef<str>> Display for CamelCaseifyer<S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        let mut capital: bool = true;
        for c in self.0.as_ref().graphemes(true) {
            // Consider snake cases
            if c == "_" {
                capital = true;
                continue;
            }

            // Capitalize as necessary
            if capital {
                write!(f, "{}", c.to_uppercase())?;
                capital = false;
            } else {
                write!(f, "{c}")?;
            }
        }
        Ok(())
    }
}



/// Represents the parsed information from the attribute.
#[derive(Clone, Debug)]
struct CombinatorAttributes {
    /// The path to prefix to paths referring to the `ast_toolkit_snack` crate.
    prefix: Path,
    /// The name of the combinator to generate. If omitted, equals the camelcase version of the function name.
    comb:   Option<Ident>,
    /// The name of the formatter to generate. If omitted, equals the combinator's name + `ExpectsFormatter`.
    fmt:    Option<Ident>,

    /// The string describing what to expect.
    expected: ExpectedString,
    /// Any output type. Defaults to `()`.
    output:   Type,
    /// Any error type. Defaults to `::std::convert::Infallible`.
    error:    Type,
}
impl Parse for CombinatorAttributes {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let total: Span = input.span();

        // Parse the input as a list of metas
        let mut prefix = {
            let mut segments = Punctuated::new();
            segments.push(PathSegment { ident: Ident::new("ast_toolkit_snack", Span::call_site()), arguments: PathArguments::None });
            Path { leading_colon: Some(PathSep::default()), segments }
        };
        let mut comb: Option<Ident> = None;
        let mut fmt: Option<Ident> = None;
        let mut expected: Option<ExpectedString> = None;
        let mut output: Option<Type> = None;
        let mut error: Option<Type> = None;
        let attrs: Punctuated<AttrNameValue, Token![,]> = Punctuated::parse_terminated(input)?;
        for attr in attrs {
            match attr {
                AttrNameValue::Snack(v) => {
                    prefix = v;
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
                AttrNameValue::Error(v) => {
                    error = Some(v);
                },
            }
        }

        // Unwrap the options
        let expected: ExpectedString = match expected {
            Some(exp) => exp,
            None => return Err(Error::new(total, "Missing 'expected = \"...\"' attribute")),
        };
        let output: Type = match output {
            Some(out) => out,
            None => Type::Tuple(TypeTuple { paren_token: Paren::default(), elems: Punctuated::new() }),
        };
        let error: Type = match error {
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
        Ok(Self { prefix, comb, fmt, expected, output, error })
    }
}

/// Represents a name/value pair for the attributes.
#[derive(Clone, Debug)]
enum AttrNameValue {
    /// The root path prefix.
    Snack(Path),
    /// It's the name of the combinator class.
    Combinator(Ident),
    /// It's the name of the formatter class.
    Formatter(Ident),

    /// It's the expected string.
    Expected(ExpectedString),
    /// It's the output type.
    Output(Type),
    /// It's the error type.
    Error(Type),
}
impl Parse for AttrNameValue {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        // Parse the identifier first, and  then always expected an equals
        let ident: Ident = input.parse()?;
        input.parse::<Token![=]>()?;
        if ident == "snack" || ident == "crate" || ident == "prefix" {
            Ok(Self::Snack(input.parse::<Path>()?))
        } else if ident == "comb" || ident == "combinator" || ident == "Combinator" {
            Ok(Self::Combinator(input.parse::<Ident>()?))
        } else if ident == "fmt" || ident == "formatter" || ident == "Formatter" {
            Ok(Self::Formatter(input.parse::<Ident>()?))
        } else if ident == "expected" || ident == "Expected" {
            Ok(Self::Expected(input.parse::<ExpectedString>()?))
        } else if ident == "output" || ident == "Output" {
            Ok(Self::Output(input.parse::<Type>()?))
        } else if ident == "error" || ident == "Error" {
            Ok(Self::Error(input.parse::<Type>()?))
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

        // Otherwise, parse as a formatter string tuple
        match input.parse::<ExprTuple>() {
            Ok(ExprTuple { elems, .. }) => {
                // The first element should be a literal, always
                let mut iter = elems.into_pairs();
                if let Some(fmt) = iter.next() {
                    // Extract the string literal
                    let fmt: Expr = fmt.into_value();
                    if let Expr::Lit(ExprLit { lit: Lit::Str(fmt), .. }) = fmt {
                        // The rest are arguments to the formatter
                        Ok(ExpectedString::Fmt(fmt, iter.collect()))
                    } else {
                        Err(Error::new(fmt.span(), "Expected a string literal"))
                    }
                } else {
                    Err(input.error("Expected at least a formatter string literal"))
                }
            },
            Err(err) => Err(Error::new(err.span(), "Expected a string literal or a tuple with format string arguments")),
        }
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
    if attrs.comb.is_none() {
        attrs.comb = Some(Ident::new(&CamelCaseifyer(&func.sig.ident.to_string()).to_string(), func.sig.ident.span()));
    }
    if attrs.fmt.is_none() {
        attrs.fmt = Some(Ident::new(&format!("{}ExpectsFormatter", attrs.comb.as_ref().unwrap()), func.sig.ident.span()));
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
    let comb: TokenStream2 = generate_combinator(&attrs, &func);
    let exp_impl: TokenStream2 = generate_expects_impl(&attrs);
    let comb_impl: TokenStream2 = generate_combinator_impl(&attrs, &func);
    let factory: TokenStream2 = generate_factory(&attrs, &func);

    // OK, return that
    Ok(quote! {
        #fmt
        #comb
        #exp_impl
        #comb_impl
        #factory
    })
}
