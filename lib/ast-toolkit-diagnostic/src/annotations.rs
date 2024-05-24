//  ANNOTATIONS.rs
//    by Lut99
//
//  Created:
//    24 May 2024, 17:38:35
//  Last edited:
//    24 May 2024, 17:46:09
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines the annotations that flavour
//!   [`Diagnostic`](crate::Diagnostic)s.
//

use ast_toolkit_span::Span;


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



/// Defines an annotation that highlights some source text.
#[derive(Clone, Debug)]
pub struct AnnotationHighlight<F, S> {
    /// Some message to show with the highlight.
    pub msg:  String,
    /// The span that refers to the highlighted part.
    pub span: Span<F, S>,
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



/// Defines an annotation that doesn't highlight source text, but adds some text to the end of the source snippet.
#[derive(Clone, Debug)]
pub struct AnnotationNote {
    /// The message to note.
    pub msg: String,
}
