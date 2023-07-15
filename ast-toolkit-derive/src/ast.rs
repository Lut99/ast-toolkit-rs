//  AST.rs
//    by Lut99
// 
//  Created:
//    13 Jul 2023, 12:24:09
//  Last edited:
//    15 Jul 2023, 13:01:56
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines the
//!   [`abstract_syntax_tree!{}`](super::abstract_syntax_tree)'s inner
//!   workings.
// 

use proc_macro::TokenStream;
use proc_macro_error::{Diagnostic, Level};
use quote::quote;
use syn::{parse, Data};
use syn::parse::Parse;


/***** DEFINITIONS *****/
/// Defines the input layout of the macro.
struct AstInput {
    
}

impl Parse for AstInput {
    fn parse(input: parse::ParseStream) -> syn::Result<Self> {
        Ok(AstInput {  })
    }
}





/***** LIBRARY *****/
/// Essentially the body of the macro.
/// 
/// # Arguments
/// - `input`: The input [`TokenStream`] to start parsing.
/// 
/// # Returns
/// A new [`TokenStream`] that should be written instead.
/// 
/// # Errors
/// This function may error if the input tokenstream was not valid for the macro.
pub fn handle(input: TokenStream) -> Result<TokenStream, Diagnostic> {
    // Let us parse the input
    let input: AstInput = match parse::<AstInput>(input) {
        Ok(input) => input,
        Err(err)  => { return Ok(TokenStream::from(err.to_compile_error())); }
    };

    // The quote to run
    Ok(quote!{}.into())
}
