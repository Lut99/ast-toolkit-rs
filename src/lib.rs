//  LIB.rs
//    by Lut99
//
//  Created:
//    25 Feb 2024, 11:17:34
//  Last edited:
//    25 Feb 2024, 11:18:14
//  Auto updated?
//    Yes
//
//  Description:
//!   Provides various tools that are useful when working with ASTs as
//!   used in compilers.
//

// Use some deps
#[cfg(feature = "diagnostic")]
pub use ast_toolkit_diagnostic;
#[cfg(feature = "railroad")]
pub use ast_toolkit_railroad;
#[cfg(feature = "span")]
pub use ast_toolkit_span;
