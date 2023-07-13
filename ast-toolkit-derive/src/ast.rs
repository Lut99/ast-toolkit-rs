//  AST.rs
//    by Lut99
// 
//  Created:
//    13 Jul 2023, 12:24:09
//  Last edited:
//    13 Jul 2023, 12:25:50
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines the
//!   [`abstract_syntax_tree!{}`](super::abstract_syntax_tree)'s inner
//!   workings.
// 

use proc_macro::TokenStream;
use proc_macro_error::Diagnostic;


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
    Ok(input)
}
