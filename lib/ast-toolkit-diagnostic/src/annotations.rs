//  ANNOTATIONS.rs
//    by Lut99
//
//  Created:
//    24 May 2024, 17:38:35
//  Last edited:
//    12 Sep 2024, 17:07:12
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines the annotations that flavour
//!   [`Diagnostic`](crate::Diagnostic)s.
//

use std::fmt::Debug;

use ast_toolkit_span::{Span, Spanning};

use crate::span::Highlight;


/***** AUXILLARY *****/
/// The severity levels supported by the [`Diagnostic`].
///
/// This essentially determines some prompt and accent colouration.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Severity {
    /// Fatal errors.
    Error,
    /// Some other information auxillary to earlier diagnostics.
    Help,
    /// Non-fatal warnings.
    Warning,
}





/***** LIBRARY *****/
/// Defines annotations that can be given in a snippet.
#[derive(Debug)]
pub enum Annotation {
    /// Highlights a text, potentially with a message.
    Highlight(AnnotationHighlight),
    /// Renders a piece of source text replaced with another.
    Suggestion(AnnotationSuggestion),
}

/// Defines annotations that highlight a particular source text with a message.
#[derive(Debug)]
pub struct AnnotationHighlight {
    /// The severity level to use for this highlight.
    pub severity:  Severity,
    /// Any message to show, if any.
    pub message:   Option<String>,
    /// The [`Highlight`] that represents the highlighted area. If omitted, will translate into a
    /// message at the end of the thing instead.
    pub highlight: Box<dyn Highlight>,
}

// Constructors
impl AnnotationHighlight {}

// Factory methods
impl AnnotationHighlight {}

// Convertions
impl From<AnnotationHighlight> for Annotation {
    #[inline]
    fn from(value: AnnotationHighlight) -> Self { Self::Highlight(value) }
}

/// Defines annotations that suggest a replacement (or insert) in the source text.
#[derive(Debug)]
pub struct AnnotationSuggestion {
    /// The replacement to insert.
    pub replacement: String,
    /// Any message to show, if any.
    pub message:     Option<String>,
    /// The [`Highlight`] that represents the highlighted area. If omitted, will translate into a
    /// message at the end of the thing instead.
    pub highlight:   Box<dyn Highlight>,
}
impl From<AnnotationSuggestion> for Annotation {
    #[inline]
    fn from(value: AnnotationSuggestion) -> Self { Self::Suggestion(value) }
}
