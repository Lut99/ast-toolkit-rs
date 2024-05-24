//  LIB.rs
//    by Lut99
//
//  Created:
//    24 May 2024, 17:08:52
//  Last edited:
//    24 May 2024, 18:12:11
//  Auto updated?
//    Yes
//
//  Description:
//!   Provides an error-like type called a `Diagnostic` that is used to
//!   build informative error messages for compiler users.
//

// Declare modules
pub mod annotations;
pub mod diagnostic;
pub mod style;

// Bring some of it onto the crate-level.
pub use annotations::Annotation;
pub use diagnostic::{Diagnostic, Level};


/***** LIBRARY *****/
/// Trait that allows things (e.g., typical [`Error`](std::error::Error)s) to be translated into [`Diagnostic`]s.
///
/// # Generics
/// - `F`: The "from"-string type of any [`Span`](ast_toolkit_span::Span) passed to the resulting diagnostic.
/// - `S`: The "source"-string type of any [`Span`](ast_toolkit_span::Span) passed to the resulting diagnostic.
pub trait IntoDiagnostic<F, S> {
    /// Returns a [`Diagnostic`] out of `self`.
    ///
    /// # Returns
    /// A [`Diagnostic`] made out of `self`.
    fn into_diagnostic(self) -> Diagnostic<F, S>;
}
