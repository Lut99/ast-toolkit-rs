//  RENDER.rs
//    by Lut99
//
//  Created:
//    27 May 2024, 10:49:06
//  Last edited:
//    24 Aug 2024, 18:40:20
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the functions to render [`Diagnostic`]s.
//

use std::borrow::Cow;
use std::fmt::Display;

use ast_toolkit_span::{Span, Spannable, SpannableLines, SpannableLocate, Spanning as _};
use stackvec::StackVec;
use unicode_segmentation::UnicodeSegmentation;

use crate::annotations::{AnnotationHighlight, IntoAnnotation};
use crate::diagnostic::Diagnostic;
use crate::span::AsUtf8;
use crate::Annotation;


/***** CONSTANTS *****/
/// Dictates the maximum number of snippets per [`Diagnostic`].
pub const MAX_SNIPPETS: usize = 8;
/// Dictates the maximum number of annotations per snippet.
pub const MAX_ANNOTS_PER_SNIPPET: usize = 8;

/// Dictates the width of [`SnippetBuffer`]s.
pub const SNIPPET_WIDTH: usize = 100;

/// Dictates how many lines are consider "close enough" before the three dots appear to truncate the source text.
///
/// Inclusive on both ends, i.e., a distance of 3 implies only one line in between highlighted areas.
pub const MAX_SOURCE_LINES_DISTANCE: usize = 3;





/***** HELPER FUNCTIONS *****/
/// Groups all the [`Annotation`]s in a [`Diagnostic`] such that every group represents annotation
/// that can be rendered in one snippet.
///
/// # Arguments
/// - `diag`: The [`Diagnostic`] to group the annotations of.
///
/// # Returns
/// A list of lists that represent the groups.
fn group_annotations<F, S>(diag: &Diagnostic<F, S>) -> StackVec<MAX_SNIPPETS, StackVec<MAX_ANNOTS_PER_SNIPPET, &dyn Annotation<F, S>>>
where
    S: SpannableLocate,
{
    // The main one is always the first group
    let mut groups: StackVec<MAX_SNIPPETS, StackVec<MAX_ANNOTS_PER_SNIPPET, &dyn Annotation<F, S>>> = StackVec::from([StackVec::from([&diag.main])]);

    // Add the secondary annotations
    for annot in &diag.annots {
        // See with whom it can be grouped
        for group in &mut groups {
            // We can group if:
            // - There is no overlap with existing annotations; and
            // - The smallest number of lines between the new and an existing annotation is smaller than MAX_SOURCE_LINES_DISTANCE.

            // Consider the annotations in the target group
            for annot2 in group {
                //
            }
        }

        // Otherwise, add it to a new group
        groups.push(StackVec::from([annot]));
    }

    todo!()
}





/***** FRAMES *****/
/// Some representation of whatever [`Diagnostic`]s render to.
///
/// Note that every frame may have multiple [`SnippetBufferUtf8`]s.
pub struct SnippetFrameUtf8 {}

/// A representation of a single snippet within a [`SnippetFrameUtf8`].
struct SnippetBufferUtf8 {
    /// The from-string describing where this snippet is from.
    from:   String,
    /// The message to show for this snippet.
    msg:    String,
    /// The physical characters to render
    buffer: Vec<SnippetLineUtf8>,
}
impl SnippetBufferUtf8 {
    /// Constructor for the SnippetBufferUtf8.
    ///
    /// # Arguments
    /// - `main`: The main message- and source text to display, wrapped in an [`AnnotationHighlight`].
    ///
    /// # Returns
    /// A new SnippetBufferUtf8 ready to render to.
    #[inline]
    #[track_caller]
    fn new<'m, F, S>(main: &'m AnnotationHighlight<F, S>) -> Self
    where
        F: Display,
        S: SpannableLines + SpannableLocate,
        S::Slice<'m>: AsUtf8,
    {
        // Get the lines of the main source annotation
        let mut lines: Vec<SnippetLineUtf8> = Vec::new();
        if let Some(l) = main.span.line() {
            for (i, line) in main.span.spanned_lines().enumerate() {
                lines.push(SnippetLineUtf8::new(l + i, line))
            }
        }

        // Apply the highlighted area in it
        todo!()
    }
}

/// A representation of a line of snippet.
struct SnippetLineUtf8 {
    /// The line number for this line
    l:    usize,
    /// The chunks of the raw line.
    line: Vec<String>,
}
impl SnippetLineUtf8 {
    /// Constructor for the SnippetLine that creates it from a spanned slice.
    ///
    /// # Arguments
    /// - `l`: This line's line number.
    /// - `line`: The raw source line to wrap.
    ///
    /// # Returns
    /// A new SnippetLineUtf8.
    #[inline]
    #[track_caller]
    pub fn new<S: AsUtf8>(l: usize, line: S) -> Self {
        Self {
            l,
            line: vec![line.as_utf8().unwrap_or_else(|err| panic!("Can only render Diagnostics from valid UTF-8 spans (error: {err})")).into()],
        }
    }
}





/***** LIBRARY *****/
// Implements `Diagnostic::render()`.
impl<F, S> Diagnostic<F, S> {
    /// Renders this diagnostic to the given [`SnippetFrame`].
    ///
    /// You probably don't have to call this function yourself. Instead, see [`Diagnostic::emit()`]
    /// or [`Diagnostic::abort()`] (and similar functions).
    ///
    /// # Arguments
    /// - `frame`: The [`SnippetFrameUtf8`] to render to.
    #[track_caller]
    pub fn render_utf8<'s>(&'s self, frame: &mut SnippetFrameUtf8)
    where
        F: Display,
        S: SpannableLines + SpannableLocate,
        S::Slice<'s>: AsUtf8,
    {
        // Render the main snippet first
        let mut main = SnippetBufferUtf8::new(&self.main);
    }
}
