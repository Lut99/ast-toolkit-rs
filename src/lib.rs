//  LIB.rs
//    by Lut99
//
//  Created:
//    25 Feb 2024, 11:17:34
//  Last edited:
//    11 Sep 2024, 16:45:23
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
#[cfg(feature = "span")]
pub use ast_toolkit_span as span;
