//  DIAGNOSTIC.rs
//    by Lut99
//
//  Created:
//    24 May 2024, 17:22:17
//  Last edited:
//    12 Sep 2024, 17:11:26
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the main [`Diagnostic`]-interface itself.
//

use std::fmt::{Formatter, Result as FResult};

use crate::Severity;
use crate::annotations::Annotation;
use crate::style::Style;


/***** AUXILLARY *****/
/// Trait that allows things (e.g., typical [`Error`](std::error::Error)s) to be translated into [`Diagnostic`]s.
///
/// # Generics
/// - `F`: The "from"-string type of any [`Span`](ast_toolkit_span::Span) passed to the resulting diagnostic.
/// - `S`: The "source"-string type of any [`Span`](ast_toolkit_span::Span) passed to the resulting diagnostic.
pub trait IntoDiagnostic {
    /// Returns a [`Diagnostic`] out of `self`.
    ///
    /// # Returns
    /// A [`Diagnostic`] made out of `self`.
    fn into_diagnostic(self) -> Diagnostic;
}





/***** LIBRARY *****/
/// Provides an interface for building complex diagnostic messages.
///
/// Most notably, the Diagnostic allows for typical compiler-style error messages on several
/// sevirity levels (errors and warnings).
pub struct Diagnostic {
    /// The style to apply while rendering.
    _style: Box<dyn Style>,

    /// The main severity of the Diagnostic.
    _sev:  Severity,
    /// Any code to display with the main message.
    _code: Option<String>,
    /// The main message of this Diagnostic.
    _msg:  String,

    /// The annotations slapped onto this Diagnostic.
    _annots: Vec<Annotation>,
}

// Constructors
impl Diagnostic {}

// Factory methods
impl Diagnostic {}
impl Diagnostic {}

// Rendering
impl Diagnostic {
    /// Renders this Diagnostic by drawing it the given [`Formatter`].
    ///
    /// # Arguments
    /// - `f`: The [`Formatter`] to render to.
    ///
    /// # Errors
    /// This function may error if we failed to write to `f`.
    pub fn render(&self, _f: &mut Formatter) -> FResult { todo!() }
}

// From
impl IntoDiagnostic for Diagnostic {
    #[inline]
    fn into_diagnostic(self) -> Diagnostic { self }
}
