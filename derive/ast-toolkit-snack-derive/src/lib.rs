//  LIB.rs
//    by Lut99
//
//  Created:
//    02 May 2024, 15:24:48
//  Last edited:
//    06 Aug 2024, 15:34:30
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the derive macros for the `ast-toolkit-snack`-crate.
//

// Declare modules
mod comb;
mod combinator;

// Imports
use proc_macro::TokenStream;
use proc_macro_error::proc_macro_error;


/***** LIBRARY *****/
/// See the auto-generated documentation at `ast_toolkit_snack::procedural::comb` for more information.
#[proc_macro_error]
#[proc_macro]
pub fn comb(input: TokenStream) -> TokenStream {
    // Pass to the main function
    match comb::call(input.into()) {
        Ok(stream) => stream.into(),
        Err(err) => {
            err.abort();
        },
    }
}



/// See `ast_toolkit_snack::procedural::combinator` in the auto-generated documentation for more information.
#[proc_macro_attribute]
pub fn combinator(attr: TokenStream, input: TokenStream) -> TokenStream {
    // Pass to the main function
    match combinator::call(attr.into(), input.into()) {
        Ok(stream) => stream.into(),
        Err(err) => err.into_compile_error().into(),
    }
}
