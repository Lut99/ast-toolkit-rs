//  LIB.rs
//    by Lut99
//
//  Created:
//    25 Feb 2024, 11:17:34
//  Last edited:
//    28 Nov 2024, 12:58:07
//  Auto updated?
//    Yes
//
//  Description:
//!   Provides various tools that are useful when working with ASTs as
//!   used in compilers.
//

// Use some deps
#[cfg(feature = "diagnostic")]
pub use ast_toolkit_diagnostic as diagnostic;
#[cfg(any(feature = "punctuated-normal", feature = "punctuated-trailing"))]
pub use ast_toolkit_punctuated as punctuated;
#[cfg(feature = "railroad")]
pub use ast_toolkit_railroad as railroad;
#[cfg(feature = "snack")]
pub use ast_toolkit_snack as snack;
#[cfg(feature = "span")]
pub use ast_toolkit_span as span;
#[cfg(feature = "tokens")]
pub use ast_toolkit_tokens as tokens;
