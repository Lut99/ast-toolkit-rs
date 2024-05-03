//  COMB.rs
//    by Lut99
//
//  Created:
//    02 May 2024, 15:37:18
//  Last edited:
//    03 May 2024, 13:23:55
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the `comb!()`-macro.
//

use std::hash::{DefaultHasher, Hash as _, Hasher as _};

use proc_macro2::{LineColumn, Span, TokenStream as TokenStream2};
use proc_macro_error::{Diagnostic, Level};
use quote::quote;
use syn::parse::{Parse, ParseStream};
use syn::spanned::Spanned as _;
use syn::token::{Move, Underscore};
use syn::{Block, Error, Expr, ExprLit, ExprTuple, Ident, Lifetime, Lit, LitStr, Token, Type, TypeInfer};


/***** HELPER FUNCTIONS *****/
/// Generates the closure that implements the expects string.
///
/// # Arguments
/// - `exp`: The [`RawExpects`] that encodes what the user gave us.
///
/// # Returns
/// A new [`TokenStream2`] with the instance generation.
fn generate_exp_impl(exp: &RawExpects) -> TokenStream2 {
    match exp {
        RawExpects::Lit(lit) => quote! {
            || -> String { format!(#lit) }
        },
        RawExpects::Tuple(lit, args) => quote! {
            || -> String { format!(#lit, #(#args),*) }
        },
        RawExpects::Closure(mv, closure) => quote! {
            #(#mv)? || -> String #closure
        },
    }
}

/// Generates the closure that implements the actual combinator.
///
/// # Arguments
/// - `comb`: The [`RawCombinator`] that encodes what the user gave us.
///
/// # Returns
/// A new [`TokenStream2`] with the instance generation.
fn generate_comb_impl(comb: &RawCombinator) -> TokenStream2 {
    let comb_ret_lifetime: &Lifetime = &comb.lifetime;
    let comb_ret_output: &Type = &comb.output;
    let comb_body: &Block = &comb.body;
    quote! {
        |input: ::ast_toolkit_span::Span<F, S>| -> ::ast_toolkit_snack::Result<#comb_ret_lifetime, #comb_ret_output, F, S> #comb_body
    }
}





/***** HELPERS *****/
/// Defines the input as a whole.
struct RawInput {
    /// The expects-part.
    exp:  RawExpects,
    /// The combinator-part.
    comb: RawCombinator,
}
impl Parse for RawInput {
    #[inline]
    fn parse(input: ParseStream) -> syn::Result<Self> {
        // Attempt to parse either `expects` or `combinator` keywords
        let mut exp: Option<RawExpects> = None;
        let mut comb: Option<RawCombinator> = None;
        while !input.is_empty() {
            // Match either of the keywords
            let ident: Ident = input.parse()?;
            if ident == "expects" || ident == "expected" {
                if let Ok(rexp) = input.parse::<RawExpects>() {
                    if exp.is_some() {
                        return Err(Error::new(ident.span(), "Only one 'expects' can be given"));
                    }
                    exp = Some(rexp);
                    continue;
                }
            } else if ident == "combinator" {
                if let Ok(rcomb) = input.parse::<RawCombinator>() {
                    if comb.is_some() {
                        return Err(Error::new(ident.span(), "Only one 'combinator' can be given"));
                    }
                    comb = Some(rcomb);
                    continue;
                }
            } else {
                return Err(input.error("Only 'expects' or 'combinator' are recognized section keywords"));
            }
        }

        // Ensure they are both given
        let exp: RawExpects = match exp {
            Some(exp) => exp,
            None => return Err(input.error("Did not specify 'expects'")),
        };
        let comb: RawCombinator = match comb {
            Some(comb) => comb,
            None => return Err(input.error("Did not specify 'combinator'")),
        };

        // Done
        Ok(Self { exp, comb })
    }
}

/// Defines the possible input to the `Expects`-string.
enum RawExpects {
    /// It's a literal, possible with embedded format string.
    Lit(LitStr),
    /// It's a tuple format string.
    Tuple(LitStr, Vec<Expr>),
    /// It's a full-blown closure.
    Closure(Option<Move>, Block),
}
impl Parse for RawExpects {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        // Parse either a literal, tuple or block
        let res: RawExpects = if let Ok(lit) = input.parse::<LitStr>() {
            RawExpects::Lit(lit)
        } else if let Ok(tuple) = input.parse::<ExprTuple>() {
            let tuple_span: Span = tuple.span();
            let mut exprs: Vec<Expr> = tuple.elems.into_iter().collect();
            if exprs.is_empty() {
                return Err(Error::new(tuple_span, "Expected at least a format string literal"));
            }

            // Ensure the first element is a string literal
            let lit: Expr = exprs.remove(0);
            let lit: LitStr = if let Expr::Lit(ExprLit { attrs: _, lit: Lit::Str(lit) }) = lit {
                lit
            } else {
                return Err(Error::new(lit.span(), "Expected first tuple element to be a format string literal"));
            };

            // Return it
            RawExpects::Tuple(lit, exprs)
        } else if let Ok(mv) = input.parse::<Move>() {
            // Also parse the block
            let closure: Block = match input.parse::<Block>() {
                Ok(block) => block,
                Err(_) => return Err(input.error("Expected code block after 'move'")),
            };
            RawExpects::Closure(Some(mv), closure)
        } else if let Ok(closure) = input.parse::<Block>() {
            RawExpects::Closure(None, closure)
        } else {
            return Err(input.error("Expected either a string literal, tuple with formatter input or a closure block"));
        };

        // Parse the semicolon
        input.parse::<Token![;]>()?;

        // OK, done!
        Ok(res)
    }
}

/// Defines the possible input to the `Combinator`-string.
struct RawCombinator {
    /// The _optional_ lifetime specifier, or else `'static`.
    lifetime: Lifetime,
    /// The _optional_ output specifier, or else `_`.
    output:   Type,
    /// The body of the closure.
    body:     Block,
}
impl Parse for RawCombinator {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        // Parse the optional return annotation
        let (lifetime, output): (Option<Lifetime>, Option<Type>) = if input.parse::<Token![->]>().is_ok() {
            // Parse a lifetime, optionally
            if let Ok(lifetime) = input.parse::<Lifetime>() {
                // See if there's also a type
                if input.parse::<Token![,]>().is_ok() {
                    if let Ok(ty) = input.parse::<Type>() {
                        (Some(lifetime), Some(ty))
                    } else {
                        return Err(input.error("Expected a returned type after a returned lifetime"));
                    }
                } else {
                    (Some(lifetime), None)
                }
            } else if let Ok(ty) = input.parse::<Type>() {
                (None, Some(ty))
            } else {
                return Err(input.error("Expected either a returned lifetime or a returned type"));
            }
        } else {
            (None, None)
        };

        // Parse the codeblock
        let body: Block = match input.parse::<Block>() {
            Ok(block) => block,
            Err(_) => return Err(input.error("Expected either a lifetime, a comma and a type or a codeblock")),
        };

        // Parse the semicolon
        input.parse::<Token![;]>()?;

        // OK, done!
        Ok(Self {
            lifetime: lifetime.unwrap_or_else(|| Lifetime::new("'static", Span::call_site())),
            output: output.unwrap_or_else(|| Type::Infer(TypeInfer { underscore_token: Underscore { spans: [Span::call_site()] } })),
            body,
        })
    }
}





/***** LIBRARY *****/
/// Calls the macro.
///
/// # Arguments
/// - `input`: The [`TokenStream2`] encoding the Rust tokens as input.
///
/// # Returns
/// A [`TokenStream2`] with the generated combinator.
///
/// # Errors
/// If anything fails during the above process, an appropriate [`Diagnostic`] is generated.
pub fn call(input: TokenStream2) -> Result<TokenStream2, Diagnostic> {
    // Split the stream first
    let input: RawInput = match syn::parse2(input) {
        Ok(input) => input,
        Err(err) => return Err(Diagnostic::spanned(err.span(), Level::Error, err.to_string())),
    };

    // Create a unique identifier by hasing the input
    let hash: u64 = {
        let loc: LineColumn = Span::call_site().start();
        let mut hasher: DefaultHasher = DefaultHasher::new();
        loc.hash(&mut hasher);
        hasher.finish()
    };
    let cmb_name: Ident = Ident::new(&format!("OneTimeComb{hash}"), Span::call_site());
    let fmt_name: Ident = Ident::new(&format!("OneTimeCombFmt{hash}"), Span::call_site());

    // Generate the combinator impl
    let comb: TokenStream2 = generate_comb_impl(&input.comb);
    // Generate the expects impl
    let exp: TokenStream2 = generate_exp_impl(&input.exp);

    // Construct the main combinator type
    let comb_ret_lifetime: &Lifetime = &input.comb.lifetime;
    let res: TokenStream2 = quote! {
        /// Formats the expected string for this closure.
        #[derive(::std::fmt::Debug)]
        struct #fmt_name {
            /// The already formatted string.
            exp_str: ::std::string::String,
        }
        impl ::std::fmt::Display for #fmt_name {
            #[inline]
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                ::std::write!(f, "Expected {}", self.exp_str)
            }
        }
        impl ::ast_toolkit_snack::ExpectsFormatter for #fmt_name {
            #[inline]
            fn expects_fmt(&self, f: &mut ::std::fmt::Formatter<'_>, _indent: ::std::primitive::usize) -> ::std::fmt::Result {
                ::std::write!(f, "Expected {}", self.exp_str)
            }
        }

        /// One-time combinator for the closure we're wrapping.
        struct #cmb_name<E, C> {
            /// Some closure producing the expects-string.
            exp: E,
            /// The closure that we wrap.
            comb: C,
        }
        impl<E: ::std::ops::Fn() -> ::std::string::String, C> ::ast_toolkit_snack::Expects<'static> for #cmb_name<E, C> {
            type Formatter = #fmt_name;

            #[inline]
            fn expects(&self) -> Self::Formatter {
                #fmt_name { exp_str: (self.exp)() }
            }
        }
        impl<F, S, R, E: ::std::ops::Fn() -> ::std::string::String, C: ::std::ops::FnMut(::ast_toolkit_span::Span<F, S>) -> ::ast_toolkit_snack::Result<#comb_ret_lifetime, R, F, S>> ::ast_toolkit_snack::Combinator<#comb_ret_lifetime, F, S> for #cmb_name<E, C> {
            type Output = R;

            #[inline]
            fn parse(&mut self, input: ::ast_toolkit_span::Span<F, S>) -> ::ast_toolkit_snack::Result<#comb_ret_lifetime, R, F, S> {
                (self.comb)(input)
            }
        }

        // Now create one of them
        #cmb_name {
            exp: #exp,
            comb: #comb,
        }
    };

    // Done
    Ok(res)
}
