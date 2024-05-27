//  ANNOTATIONS.rs
//    by Lut99
//
//  Created:
//    24 May 2024, 17:38:35
//  Last edited:
//    27 May 2024, 10:48:14
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines the annotations that flavour
//!   [`Diagnostic`](crate::Diagnostic)s.
//

use std::fmt::Display;

use ast_toolkit_span::Span;


/***** AUXILLARY *****/
/// Implements translations to [`Annotation`]s.
pub trait IntoAnnotation<F, S> {
    /// Returns an [`Annotation`] out of `self`.
    ///
    /// # Returns
    /// An [`Annotation`] made out of `self`.
    fn into_annotation(self) -> Annotation<F, S>;
}





/***** LIBRARY *****/
/// Defines an annotation to a [`Diagnostic`].
///
/// This is stuff like a note, a replacement, etc. Depending on where it relates to the main span,
/// this may either be integrated in the main render or produce separate renderings.
#[derive(Clone, Debug)]
pub enum Annotation<F, S> {
    // Highlighting
    /// A highlighted part of source text.
    ///
    /// This is typically used to show where things are defined or something that is otherwise related to the error.
    Highlight(AnnotationHighlight<F, S>),
    /// A suggested replacement of a part of source text.
    ///
    /// This is used to suggest to users what to do next.
    Suggestion(AnnotationSuggestion<F, S>),

    // Miscellaneous
    /// A note attached to the end of a source snippet.
    ///
    /// This is conventionally used to communicate "P.S.-like" information, such as how to disable a warning or where to find more information.
    Note(AnnotationNote),
}
impl<F, S> IntoAnnotation<F, S> for Annotation<F, S> {
    #[inline]
    fn into_annotation(self) -> Annotation<F, S> { self }
}
impl<F, S> From<AnnotationHighlight<F, S>> for Annotation<F, S> {
    #[inline]
    fn from(value: AnnotationHighlight<F, S>) -> Self { value.into_annotation() }
}
impl<F, S> From<AnnotationSuggestion<F, S>> for Annotation<F, S> {
    #[inline]
    fn from(value: AnnotationSuggestion<F, S>) -> Self { value.into_annotation() }
}
impl<F, S> From<AnnotationNote> for Annotation<F, S> {
    #[inline]
    fn from(value: AnnotationNote) -> Self { value.into_annotation() }
}



/// Defines an annotation that highlights some source text.
#[derive(Clone, Debug)]
pub struct AnnotationHighlight<F, S> {
    /// Some message to show with the highlight.
    pub msg:  String,
    /// The span that refers to the highlighted part.
    pub span: Span<F, S>,
}
impl<F, S> AnnotationHighlight<F, S> {
    /// Constructor for the highlight-annotation.
    ///
    /// # Arguments
    /// - `msg`: Some message to show next to the highlighted source text.
    /// - `span`: Some [`Span`] that is used to find the highlighted source text.
    ///
    /// # Returns
    /// A new AnnotationHighlight.
    #[inline]
    pub fn new(msg: impl Display, span: Span<F, S>) -> Self { Self { msg: msg.to_string(), span } }
}
impl<F, S> AnnotationHighlight<F, S> {
    /// Changes the message of the highlight.
    ///
    /// # Arguments
    /// - `msg`: The message to display next to the highlighted part of the source text.
    ///
    /// # Returns
    /// Self with the given message.
    #[inline]
    pub fn message_mut(&mut self, msg: impl Display) -> &mut Self {
        self.msg = msg.to_string();
        self
    }

    /// Changes the span of the highlight.
    ///
    /// # Arguments
    /// - `span`: The [`Span`]ned source text to highlight.
    ///
    /// # Returns
    /// Self with the given `span`.
    pub fn span_mut(&mut self, span: Span<F, S>) -> &mut Self {
        self.span = span;
        self
    }
}
impl<F, S> AnnotationHighlight<F, S> {
    /// Changes the message of the highlight.
    ///
    /// # Arguments
    /// - `msg`: The message to display next to the highlighted part of the source text.
    ///
    /// # Returns
    /// Self with the given message.
    #[inline]
    pub fn message(mut self, msg: impl Display) -> Self {
        self.msg = msg.to_string();
        self
    }

    /// Changes the span of the highlight.
    ///
    /// # Arguments
    /// - `span`: The [`Span`]ned source text to highlight.
    ///
    /// # Returns
    /// Self with the given `span`.
    pub fn span(mut self, span: Span<F, S>) -> Self {
        self.span = span;
        self
    }
}
impl<F, S> IntoAnnotation<F, S> for AnnotationHighlight<F, S> {
    #[inline]
    fn into_annotation(self) -> Annotation<F, S> { Annotation::Highlight(self) }
}

/// Defines an annotation that suggests a replacement of some source text.
#[derive(Clone, Debug)]
pub struct AnnotationSuggestion<F, S> {
    /// Some message to show with the highlight.
    pub msg: String,
    /// Something to replace the highlighted, `span`ned source text with.
    pub replacement: String,
    /// The span that refers to the highlighted part.
    pub span: Span<F, S>,
}
impl<F, S> AnnotationSuggestion<F, S> {
    /// Constructor for the suggestion-annotation.
    ///
    /// # Arguments
    /// - `msg`: Some message to show next to the highlighted source text.
    /// - `replacement`: Some replacement of the highlighted source text to show instead.
    /// - `span`: Some [`Span`] that is used to find the highlighted source text.
    ///
    /// # Returns
    /// A new AnnotationSuggestion.
    #[inline]
    pub fn new(msg: impl Display, replacement: impl Display, span: Span<F, S>) -> Self {
        Self { msg: msg.to_string(), replacement: replacement.to_string(), span }
    }
}
impl<F, S> AnnotationSuggestion<F, S> {
    /// Changes the message of the suggestion.
    ///
    /// # Arguments
    /// - `msg`: The message to display next to the suggested part of the source text.
    ///
    /// # Returns
    /// Self with the given message.
    #[inline]
    pub fn message_mut(&mut self, msg: impl Display) -> &mut Self {
        self.msg = msg.to_string();
        self
    }

    /// Changes the replacement of the suggestion.
    ///
    /// # Arguments
    /// - `replacement`: Some replacement of the highlighted source text to show instead.
    ///
    /// # Returns
    /// Self with the given replacement.
    #[inline]
    pub fn replacement_mut(&mut self, replacement: impl Display) -> &mut Self {
        self.replacement = replacement.to_string();
        self
    }

    /// Changes the span of the suggestion.
    ///
    /// # Arguments
    /// - `span`: The [`Span`]ned source text to suggestion.
    ///
    /// # Returns
    /// Self with the given `span`.
    pub fn span_mut(&mut self, span: Span<F, S>) -> &mut Self {
        self.span = span;
        self
    }
}
impl<F, S> AnnotationSuggestion<F, S> {
    /// Changes the message of the suggestion.
    ///
    /// # Arguments
    /// - `msg`: The message to display next to the suggested part of the source text.
    ///
    /// # Returns
    /// Self with the given message.
    #[inline]
    pub fn message(mut self, msg: impl Display) -> Self {
        self.msg = msg.to_string();
        self
    }

    /// Changes the replacement of the suggestion.
    ///
    /// # Arguments
    /// - `replacement`: Some replacement of the highlighted source text to show instead.
    ///
    /// # Returns
    /// Self with the given replacement.
    #[inline]
    pub fn replacement(mut self, replacement: impl Display) -> Self {
        self.replacement = replacement.to_string();
        self
    }

    /// Changes the span of the suggestion.
    ///
    /// # Arguments
    /// - `span`: The [`Span`]ned source text to suggestion.
    ///
    /// # Returns
    /// Self with the given `span`.
    pub fn span(mut self, span: Span<F, S>) -> Self {
        self.span = span;
        self
    }
}
impl<F, S> IntoAnnotation<F, S> for AnnotationSuggestion<F, S> {
    #[inline]
    fn into_annotation(self) -> Annotation<F, S> { Annotation::Suggestion(self) }
}



/// Defines an annotation that doesn't highlight source text, but adds some text to the end of the source snippet.
#[derive(Clone, Debug)]
pub struct AnnotationNote {
    /// The message to note.
    pub msg: String,
}
impl AnnotationNote {
    /// Constructor for the note-annotation.
    ///
    /// # Arguments
    /// - `msg`: Some message to show at the bottom of a source snippet.
    ///
    /// # Returns
    /// A new AnnotationNote.
    #[inline]
    pub fn new(msg: impl Display) -> Self { Self { msg: msg.to_string() } }
}
impl AnnotationNote {
    /// Changes the message of the note.
    ///
    /// # Arguments
    /// - `msg`: The message to display at the bottom of a source snippet.
    ///
    /// # Returns
    /// Self with the given message.
    #[inline]
    pub fn message_mut(&mut self, msg: impl Display) -> &mut Self {
        self.msg = msg.to_string();
        self
    }
}
impl AnnotationNote {
    /// Changes the message of the note.
    ///
    /// # Arguments
    /// - `msg`: The message to display at the bottom of a source snippet.
    ///
    /// # Returns
    /// Self with the given message.
    #[inline]
    pub fn message(mut self, msg: impl Display) -> Self {
        self.msg = msg.to_string();
        self
    }
}
impl<F, S> IntoAnnotation<F, S> for AnnotationNote {
    #[inline]
    fn into_annotation(self) -> Annotation<F, S> { Annotation::Note(self) }
}
