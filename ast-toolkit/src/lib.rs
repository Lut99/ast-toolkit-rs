//  LIB.rs
//    by Lut99
// 
//  Created:
//    02 Jul 2023, 16:40:06
//  Last edited:
//    07 Jul 2023, 09:48:07
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
