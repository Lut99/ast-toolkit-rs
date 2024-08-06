//  COMBINATOR.rs
//    by Lut99
//
//  Created:
//    06 Aug 2024, 15:23:00
//  Last edited:
//    06 Aug 2024, 16:59:26
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
use syn::{
    parse2, Block, Error, Expr, ExprLit, FnArg, GenericArgument, Ident, Item, ItemFn, Lit, LitStr, Meta, PathArguments, ReturnType, Signature, Token,
    Type, TypeGroup, TypeParen,
};
use unicode_segmentation::UnicodeSegmentation;


/***** HELPER FUNCTIONS *****/
/// Generates the expected formatter implementation.
///
/// # Arguments
/// - `attrs`: The [`CombinatorAttributes`] parsed from the main attribute.
/// - `func`: The main combinator function parsed from the input.
///
/// # Returns
/// A [`TokenStream2`] with the generated `ExpectsFormatter` implementation and an [`Ident`] with the name of the newly generated formatter.
fn generate_formatter(attrs: &CombinatorAttributes, func: &CombinatorFunc) -> (TokenStream2, Ident) {
    let sname: String = func.name.to_string();
    let fname: Ident = Ident::new(&format!("{}ExpectsFormatter", CamelCaseifyer(&sname)), func.name.span());
    let exps: &LitStr = &attrs.expected;
    (
        quote! {
            #[doc = ::std::concat!("Expects strings formatter for the [`", #sname, "()`]-combinator.")]
            #[automatically_derived]
            #[derive(::std::clone::Clone, ::std::marker::Copy, ::std::fmt::Debug)]
            pub struct #fname;
            #[automatically_derived]
            impl ::std::fmt::Display for #fname {
                #[inline]
                fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                    write!(f, "Expected ")?;
                    self.expects_fmt(f, 0)
                }
            }
            #[automatically_derived]
            impl ::ast_toolkit_snack::ExpectsFormatter for #fname {
                #[inline]
                fn expects_fmt(&self, f: &mut ::std::fmt::Formatter, _ident: usize) -> ::std::fmt::Result { ::std::write!(f, #exps) }
            }
        },
        fname,
    )
}

/// Generates the main combinator implementation.
///
/// # Arguments
/// - `func`: The main combinator function parsed from the input.
///
/// # Returns
/// A [`TokenStream2`] with the generated `ExpectsFormatter` implementation and an [`Ident`] with the name of the newly generated formatter.
fn generate_combinator(func: &CombinatorFunc) -> (TokenStream2, Ident) {
    let sname: String = func.name.to_string();
    let cname: Ident = Ident::new(&format!("{}", CamelCaseifyer(&sname)), func.name.span());
    (
        quote! {
            #[doc = ::std::concat!("Combinator returned by the [`", #sname, "()`]-combinator.")]
            #[automatically_derived]
            #[derive(::std::clone::Clone, ::std::marker::Copy, ::std::fmt::Debug)]
            pub struct #cname<F, S> {
                _f: ::std::marker::PhantomData<F>,
                _s: ::std::marker::PhantomData<S>,
            }
        },
        cname,
    )
}

/// Generates the expects implementation.
///
/// # Arguments
/// - `cname`: The name of the combinator to implement `Expects` for.
/// - `fname`: The name of the formatter to use when generating the expects-string.
///
/// # Returns
/// A [`TokenStream2`] with the generated `Expects` implementation.
fn generate_expects_impl(cname: &Ident, fname: &Ident) -> TokenStream2 {
    quote! {
        #[automatically_derived]
        impl<F, S> ::ast_toolkit_snack::Expects<'static> for #cname<F, S> {
            type Formatter = #fname;

            #[inline]
            fn expects(&self) -> Self::Formatter { #fname }
        }
    }
}

/// Generates the combinator implementation.
///
/// # Arguments
/// - `func`: The main combinator function parsed from the input.
/// - `cname`: The name of the combinator to implement `Expects` for.
///
/// # Returns
/// A [`TokenStream2`] with the generated `Combinator` implementation.
fn generate_combinator_impl(func: &CombinatorFunc, cname: &Ident) -> TokenStream2 {
    let (out, err): (&Type, &Type) = (&func.output.0, &func.output.1);
    let mut sig: Signature = func.sig.clone();
    sig.ident = Ident::new("parse", sig.ident.span());
    let body: &Block = &func.body;
    quote! {
        #[automatically_derived]
        impl<F, S> ::ast_toolkit_snack::Combinator<'static, F, S> for #cname<F, S> {
            type Output = #out;
            type Error = #err;

            #sig #body
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
    /// The string describing what to expect.
    expected: LitStr,
}
impl Parse for CombinatorAttributes {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let total: Span = input.span();

        // Parse the input as a list of metas
        let mut expected: Option<LitStr> = None;
        let attrs: Punctuated<Meta, Token![,]> = Punctuated::parse_terminated(input)?;
        for attr in attrs {
            let span: Span = attr.span();
            match attr {
                Meta::Path(p) => return Err(Error::new(span, format!("Unknown attribute '{}'", p.to_token_stream()))),
                Meta::NameValue(nv) => {
                    if nv.path.is_ident("expected") {
                        // Parse the value as a string literal
                        if let Expr::Lit(ExprLit { lit: Lit::Str(lit), .. }) = nv.value {
                            expected = Some(lit);
                        } else {
                            return Err(Error::new(nv.value.span(), "Expected string literal"));
                        }
                    } else {
                        return Err(Error::new(span, format!("Unknown attribute '{}'", nv.path.to_token_stream())));
                    }
                },
                Meta::List(l) => return Err(Error::new(span, format!("Unknown attribute '{}'", l.path.to_token_stream()))),
            }
        }

        // Unwrap the options
        let expected: LitStr = match expected {
            Some(exp) => exp,
            None => return Err(Error::new(total, "Missing 'expected = \"...\"' attribute")),
        };

        // OK, construct self
        Ok(Self { expected })
    }
}

/// Represents the wrapper combinator function.
#[derive(Clone, Debug)]
struct CombinatorFunc {
    /// The name of the combinator function.
    name:   Ident,
    /// The output/error types returned by the function
    output: (Type, Type),
    /// The signature of the function itself.
    sig:    Signature,
    /// The body of the combinator.
    body:   Block,
}
impl Parse for CombinatorFunc {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        // Parse it as a Rust function first
        let item: Item = input.parse()?;
        let func: ItemFn = if let Item::Fn(func) = item { func } else { return Err(Error::new(item.span(), "Expected a function")) };

        // Next, extract the name & output types of the function
        let name: Ident = func.sig.ident.clone();
        let sig: Signature = func.sig;
        let body: Block = *func.block;

        // Extract the output & error types from it
        let output: (Type, Type) = match &sig.output {
            ReturnType::Type(_, ty) => {
                fn unwrap_ty(ty: &Type) -> Result<(Type, Type), Error> {
                    match ty {
                        // The actual type
                        Type::Path(path) => {
                            // Check if it ends in 'Result'
                            if let Some(last) = path.path.segments.last() {
                                if last.ident == "Result" {
                                    // Now expected the two options
                                    match &last.arguments {
                                        PathArguments::AngleBracketed(args) => {
                                            if args.args.len() == 2 {
                                                match (&args.args[0], &args.args[1]) {
                                                    (GenericArgument::Type(out), GenericArgument::Type(err)) => Ok((out.clone(), err.clone())),
                                                    (GenericArgument::Type(_), other) => {
                                                        return Err(Error::new(other.span(), "Expected a 'Result<OUTPUT, ERROR>' return type"));
                                                    },
                                                    (other, _) => {
                                                        return Err(Error::new(other.span(), "Expected a 'Result<OUTPUT, ERROR>' return type"));
                                                    },
                                                }
                                            } else {
                                                return Err(Error::new(last.span(), "Expected a 'Result<OUTPUT, ERROR>' return type"));
                                            }
                                        },
                                        other => return Err(Error::new(other.span(), "Expected a 'Result<OUTPUT, ERROR>' return type")),
                                    }
                                } else {
                                    return Err(Error::new(last.span(), "Expected a 'Result<OUTPUT, ERROR>' return type"));
                                }
                            } else {
                                return Err(Error::new(path.span(), "Expected a 'Result<OUTPUT, ERROR>' return type"));
                            }
                        },

                        // Simple recursion
                        Type::Paren(TypeParen { elem, .. }) => unwrap_ty(elem),
                        Type::Group(TypeGroup { elem, .. }) => unwrap_ty(elem),

                        // Anything else is wack
                        other => return Err(Error::new(other.span(), "Expected a 'Result<OUTPUT, ERROR>' return type")),
                    }
                }
                unwrap_ty(ty)?
            },
            ReturnType::Default => return Err(Error::new(sig.output.span(), "Expected a 'Result<OUTPUT, ERROR>' return type")),
        };

        // OK, construct self
        Ok(Self { name, output, sig, body })
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
    // Start by parsing the attributes
    let attrs: CombinatorAttributes = parse2(attrs)?;
    // Then parse the body as a function
    let func: CombinatorFunc = parse2(input)?;

    // Generate the parts of the combinator implementation
    let (fmt, fname): (TokenStream2, Ident) = generate_formatter(&attrs, &func);
    let (comb, cname): (TokenStream2, Ident) = generate_combinator(&func);
    let exp_impl: TokenStream2 = generate_expects_impl(&cname, &fname);
    let comb_impl: TokenStream2 = generate_combinator_impl(&func, &cname);

    // OK, return that
    Ok(quote! {
        #fmt
        #comb
        #exp_impl
        #comb_impl
    })
}
