//  LIB.rs
//    by Lut99
//
//  Created:
//    24 May 2024, 17:08:52
//  Last edited:
//    25 Aug 2024, 17:51:11
//  Auto updated?
//    Yes
//
//  Description:
//!   Provides an error-like type called a `Diagnostic` that is used to
//!   build informative error messages for compiler users.
//

// Declare modules
// pub mod annotations;
pub mod diagnostic;
pub mod render;
pub mod span;
pub mod style;

// Bring some of it onto the crate-level.
pub use annotations::Annotation;
pub use diagnostic::{Diagnostic, Level};
