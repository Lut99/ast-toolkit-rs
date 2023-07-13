//  LIB.rs
//    by Lut99
// 
//  Created:
//    05 Jul 2023, 15:47:40
//  Last edited:
//    13 Jul 2023, 12:26:08
//  Auto updated?
//    Yes
// 
//  Description:
//!   Crate that implements various `#[derive(...)]` macros for the
//!   `ast-toolkit` crate.
// 

// Declare submodules
mod ast;
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



/// Procedural macro for easily constructing ASTs with branching elements.
/// 
/// # Input
/// You define your structs as normal, except that you can postfix field types with `?`:
/// ```rust
/// abstract_syntax_tree! {
///     // Defines the branches defined below we are ultimately interested in
///     parsed = Toplevel[],
///     typed = Toplevel[data_type],
///     =>
/// 
///     // Here, we define structs as normal
///     /// Some toplevel struct
///     struct Toplevel {
///         /// A field as you know it
///         pub span : ast_toolkit::Span,
/// 
///         /// The magic: using '?' introduces a branch!
///         pub data_type : DataType?,
/// 
///         /// Here, we depend on a nested tree node that branches
///         pub branching : Nested,
///         /// You can also define fixed branches
///         pub nested : Nested[data_type],
///     }
/// 
///     /// Only exists to demonstrate nested branching
///     struct Nested {
///         pub data_type : DataType?,
///     }
/// }
/// ```
#[proc_macro_error::proc_macro_error]
#[proc_macro]
#[inline]
pub fn abstract_syntax_tree(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    // Just call the main function
    match ast::handle(input) {
        Ok(stream) => stream,
        Err(err)   => { err.abort(); },
    }
}
