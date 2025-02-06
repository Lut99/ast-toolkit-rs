//  LIB.rs
//    by Lut99
//
//  Created:
//    05 Jul 2023, 15:47:40
//  Last edited:
//    06 Feb 2025, 09:51:29
//  Auto updated?
//    Yes
//
//  Description:
//!   Crate that implements various `#[derive(...)]` macros for the
//!   `ast-toolkit` crate.
//

// Declare submodules
mod railroad;


/***** LIBRARY *****/
/// See the auto-generated documentation at `ast_toolkit_railroad::procedural::ToNode` for more information.
#[proc_macro_error::proc_macro_error]
#[proc_macro_derive(ToNode, attributes(railroad))]
pub fn derive_to_node(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    use syn::{DeriveInput, parse_macro_input};


    // Parse the thing we've gotten
    let DeriveInput { ident, data, attrs, generics, vis } = parse_macro_input!(input);

    // Pass to the main function
    match railroad::derive_to_node(ident, data, attrs, generics, vis) {
        Ok(stream) => stream,
        Err(err) => {
            err.abort();
        },
    }
}

/// See the auto-generated documentation at `ast_toolkit_railroad::procedural::ToNonTerm` for more information.
#[proc_macro_error::proc_macro_error]
#[proc_macro_derive(ToNonTerm, attributes(railroad))]
pub fn derive_to_nonterm(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    use syn::{DeriveInput, parse_macro_input};


    // Parse the thing we've gotten
    let DeriveInput { ident, data, attrs, generics, vis } = parse_macro_input!(input);

    // Pass to the main function
    match railroad::derive_to_non_term(ident, data, attrs, generics, vis) {
        Ok(stream) => stream,
        Err(err) => {
            err.abort();
        },
    }
}

/// See the auto-generated documentation at `ast_toolkit_railroad::procedural::ToDelimNode` for more information.
#[proc_macro_error::proc_macro_error]
#[proc_macro_derive(ToDelimNode, attributes(railroad))]
pub fn derive_to_delim_node(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    use syn::{DeriveInput, parse_macro_input};


    // Parse the thing we've gotten
    let DeriveInput { ident, data, attrs, generics, vis } = parse_macro_input!(input);

    // Pass to the main function
    match railroad::derive_to_delim_node(ident, data, attrs, generics, vis) {
        Ok(stream) => stream,
        Err(err) => {
            err.abort();
        },
    }
}
