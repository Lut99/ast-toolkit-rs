//  MOD.rs
//    by Lut99
//
//  Created:
//    16 Dec 2023, 12:01:01
//  Last edited:
//    16 Dec 2023, 12:43:55
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines everything necessary for working with [`Diagnostic`]s
//!   (except the procedural macro).
//

// Declare modules
#[cfg(feature = "diagnostic-located-span")]
pub mod located_span;
mod span;
mod style;

// Flatten them all in this namespace
pub use span::*;
pub use style::*;
