//  LIB.rs
//    by Lut99
// 
//  Created:
//    02 Jul 2023, 16:40:06
//  Last edited:
//    15 Jul 2023, 12:38:35
//  Auto updated?
//    Yes
// 
//  Description:
//!   A collection of structs and interfaces extremely useful when working
//!   with compilers that parse text into ASTs.
// 

// Define the submodules
pub mod span;
pub mod diagnostic;


// Pull the relevant stuff into the global namespace
pub use span::{Position, Span};
pub use diagnostic::{Diagnostic, DiagnosticKind};
#[cfg(feature = "derive")]
pub use ast_toolkit_derive::{abstract_syntax_tree, Diagnostic};
