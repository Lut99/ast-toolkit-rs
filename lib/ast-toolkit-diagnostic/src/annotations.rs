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

use ast_toolkit_span::Span;
use better_derive::{Clone, Copy, Debug};


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
#[derive(Clone, Debug)]
pub struct Annotation<S> {
    /// Defines any annotation-specific fields.
    pub inner: AnnotationInner,
    /// Defines the place in the source describing what this annotation highlights.
    pub span:  Span<S>,
}

/// Defines variations of the base [`Annotation`].
#[derive(Clone, Debug)]
pub enum AnnotationInner {
    /// It's a highlight, i.e., marking an area in the text with potentially a message.
    Highlight(AnnotationInnerHighlight),
    /// It's a suggestion, i.e., replacing the same source text with new info.
    Suggestion(AnnotationInnerSuggestion),
}



/// Defines annotations that highlight a particular source text, optionally with a message.
#[derive(Clone, Debug)]
pub struct AnnotationInnerHighlight {
    /// The severity level to use for this highlight.
    pub severity: Severity,
    /// Any message to show, if any.
    pub message:  Option<String>,
}

// Convertions
impl From<AnnotationInnerHighlight> for AnnotationInner {
    #[inline]
    fn from(value: AnnotationInnerHighlight) -> Self { Self::Highlight(value) }
}



/// Defines annotations that suggest a replacement (or insert) in the source text.
#[derive(Clone, Debug)]
pub struct AnnotationInnerSuggestion {
    /// The replacement to insert.
    pub replacement: String,
    /// Any message to show, if any.
    pub message:     Option<String>,
}

// Convertions
impl From<AnnotationInnerSuggestion> for AnnotationInner {
    #[inline]
    fn from(value: AnnotationInnerSuggestion) -> Self { Self::Suggestion(value) }
}
