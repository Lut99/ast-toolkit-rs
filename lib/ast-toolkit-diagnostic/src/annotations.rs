//  ANNOTATIONS.rs
//    by Lut99
//
//  Created:
//    24 May 2024, 17:38:35
//  Last edited:
//    09 Sep 2024, 14:34:49
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines the annotations that flavour
//!   [`Diagnostic`](crate::Diagnostic)s.
//

use std::fmt::Debug;

use ast_toolkit_span::{Span, Spanning};


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
pub enum Annotation<F, S> {
    /// Highlights a text, potentially with a message.
    Highlight(AnnotationHighlight<F, S>),
    /// Renders a piece of source text replaced with another.
    Suggestion(AnnotationSuggestion<F, S>),
}

/// Defines annotations that highlight a particular source text with a message.
#[derive(Clone, Debug)]
pub struct AnnotationHighlight<F, S> {
    /// The [`Span`] that represents the highlighted area.
    pub span:     Span<F, S>,
    /// The severity level to use for this highlight.
    pub severity: Severity,
    /// Some error code that users might recognize elsewhere.
    pub code:     Option<String>,
    /// Any message to show, if any.
    pub message:  Option<String>,
    /// Any notes to attach to the message.
    pub notes:    Vec<String>,
}

// Constructors
impl<F, S> AnnotationHighlight<F, S> {
    /// Constructor for the AnnotationHighlight that creates a highlight with a message.
    ///
    /// # Arguments
    /// - `span`: The [`Span`] describing which part of the source text to highlight.
    /// - `severity`: The [`Severity`] of the problem that we're highlighting.
    /// - `message`: Some message to show accompanying the highlight.
    ///
    /// # Returns
    /// A new AnnotationHighlight with the given `message`.
    #[inline]
    pub fn new(span: Span<F, S>, severity: Severity, message: impl Into<String>) -> Self {
        Self { span, severity, code: None, message: Some(message.into()), notes: Vec::new() }
    }

    /// Constructor for the AnnotationHighlight that creates a highlight _without_ a message.
    ///
    /// # Arguments
    /// - `span`: The [`Span`] describing which part of the source text to highlight.
    /// - `severity`: The [`Severity`] of the problem that we're highlighting.
    ///
    /// # Returns
    /// A new AnnotationHighlight that only highlights.
    #[inline]
    pub fn plain(span: Span<F, S>, severity: Severity) -> Self { Self { span, severity, code: None, message: None, notes: Vec::new() } }
}

// Factory methods
impl<F, S> AnnotationHighlight<F, S> {
    ///
}

// Convertions
impl<F, S> From<AnnotationHighlight<F, S>> for Annotation<F, S> {
    #[inline]
    fn from(value: AnnotationHighlight<F, S>) -> Self { Self::Highlight(value) }
}

/// Defines annotations that suggest a replacement (or insert) in the source text.
#[derive(Clone, Debug)]
pub struct AnnotationSuggestion<F, S> {
    /// The [`Span`] that represents the highlighted area.
    pub span: Span<F, S>,
    /// The replacement to insert.
    pub replacement: String,
    /// Any message to show, if any.
    pub message: Option<String>,
}
impl<F, S> From<AnnotationSuggestion<F, S>> for Annotation<F, S> {
    #[inline]
    fn from(value: AnnotationSuggestion<F, S>) -> Self { Self::Suggestion(value) }
}
