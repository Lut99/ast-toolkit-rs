//  MOD.rs
//    by Lut99
//
//  Created:
//    16 Dec 2023, 12:01:01
//  Last edited:
//    17 Feb 2024, 12:43:10
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines everything necessary for working with [`Diagnostic`]s
//!   (except the procedural macro).
//

// Declare modules
mod diagnostic;
mod formatter;
mod formatting;
mod span;
mod style;

// Flatten them all in this namespace
pub use diagnostic::*;
pub use formatter::*;
pub use formatting::*;
pub use span::*;
pub use style::*;
