//  LIB.rs
//    by Lut99
//
//  Created:
//    02 May 2024, 15:24:48
//  Last edited:
//    07 Aug 2024, 22:06:09
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the derive macros for the `ast-toolkit-snack`-crate.
//

// Declare modules
mod comb;

// Imports
use proc_macro::TokenStream;


/***** LIBRARY *****/
/// See `ast_toolkit_snack::procedural::comb` in the auto-generated documentation for more information.
#[proc_macro_attribute]
pub fn comb(attr: TokenStream, input: TokenStream) -> TokenStream {
    // Pass to the main function
    match comb::call(attr.into(), input.into()) {
        Ok(stream) => stream.into(),
        Err(err) => err.into_compile_error().into(),
    }
}
