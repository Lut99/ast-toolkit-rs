//  MOD.rs
//    by Lut99
//
//  Created:
//    16 Dec 2023, 12:01:01
//  Last edited:
//    15 Feb 2024, 22:18:28
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
mod span;
mod style;

// Flatten them all in this namespace
pub use diagnostic::*;
pub use formatter::*;
pub use span::*;
pub use style::*;
