//  LIB.rs
//    by Lut99
// 
//  Created:
//    05 Jul 2023, 15:47:40
//  Last edited:
//    05 Jul 2023, 18:19:44
//  Auto updated?
//    Yes
// 
//  Description:
//!   Crate that implements various `#[derive(...)]` macros for the
//!   `ast-toolkit` crate.
// 

// Declare submodules
mod diagnostic;



/***** LIBRARY *****/
/// Derives the [`directories::Directory`] trait automagically.
#[proc_macro_error::proc_macro_error]
#[proc_macro_derive(Diagnostic, attributes(diagnostic, diag))]
pub fn derive_directory(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    use syn::{parse_macro_input, DeriveInput};


    // Parse the thing we've gotten
    let DeriveInput{ ident, data, attrs, generics, vis } = parse_macro_input!(input);

    // Pass to the main function
    match diagnostic::derive(ident, data, attrs, generics, vis) {
        Ok(stream) => stream,
        Err(err)   => { err.abort(); },
    }
}