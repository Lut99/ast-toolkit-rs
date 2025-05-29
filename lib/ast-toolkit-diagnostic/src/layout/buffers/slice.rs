//  SLICE.rs
//    by Lut99
//
//  Description:
//!   Defines the so-called [`SliceBuffer`], which is a bunch of spans that
//!   represent a source snippet and its highlighted areas.
//!
//!   In the pipeline as described [here](super::super), this is the first
//!   step.
//

use ast_toolkit_span::{Span, Spannable};

use crate::annotations::{Annotation, Severity};


/***** AUXILLARY *****/
/// Defines the colouring options for a [`Slice`].
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum SliceColor {
    Severity(Severity),
    Suggestion,
    Plain,
}



/// Defines a source snippet annotation with something.
#[derive(Clone, Debug)]
pub struct Slice<'s, S>
where
    S: Spannable<'s>,
{
    /// The span that is being represented here.
    span:   Span<S>,
    /// What to colour this span as.
    color:  SliceColor,
    /// A list of annotations that are sticked on top of this area.
    annots: Vec<SliceAnnot<S::Slice>>,
}

/// Defines possible annotations attached to a [`Slice`].
#[derive(Clone, Debug)]
pub enum SliceAnnot<E> {
    /// It's a message.
    Highlight(String),
    /// It's a suggestion.
    Suggestion(Vec<E>),
}





/***** LIBRARY *****/
/// Defines a buffer for holding a continious slice of source snippet with
/// annotations applied (but not yet rendered).
pub struct SliceBuffer<'s, S>
where
    S: Spannable<'s>,
{
    /// The list of slices.
    slices: Vec<Slice<'s, S>>,
}

// Constructors
impl<'s, S: Spannable<'s>> SliceBuffer<'s, S> {
    /// Creates a new SliceBuffer around the given spanned area.
    ///
    /// # Arguments
    /// - `span`: Some main Span to create in the buffer.
    ///
    /// # Returns
    /// A new SliceBuffer ready to write annotations to.
    #[inline]
    pub fn new(span: Span<S>) -> Self { Self { slices: vec![Slice { span, color: SliceColor::Plain, annots: Vec::new() }] } }
}

// Annotations
impl<'s, S: Spannable<'s>> SliceBuffer<'s, S> {
    /// Writes a new annotation to the slice buffer.
    ///
    /// # Arguments
    /// - `annot`: Some [`Annotation`] to apply to the internal source text.
    ///
    /// # Returns
    /// This function will return `true` if the annotation was applied, or `false` if it was either
    /// from a different `S`ource or not in range of the internal main span.
    #[inline]
    pub fn annotate(&mut self, Annotation { span: mut annot_span, inner: annot_inner }: Annotation<'s, S>) -> bool {
        // Assert it is from the same source
        if self.slices[0].span.source_id() != annot_span.source_id() {
            return false;
        }

        // Get the ranges from the annotation span
        let annot_source_len: usize = annot_span.source().len();
        let annot_start: usize = annot_span.range().start_resolved(annot_source_len).unwrap_or(0);
        let annot_end: usize = annot_span.range().end_resolved(annot_source_len).unwrap_or(0);

        // Search to apply it
        let slices_len: usize = self.slices.len();
        for i in 0..slices_len {
            if annot_span.is_empty() {
                break;
            }

            // Get the ranges of the slice's span
            let slice: &Slice<S> = &self.slices[i];
            let slice_source_len: usize = slice.span.source().len();
            let slice_start: usize = slice.span.range().start_resolved(slice_source_len).unwrap_or(0);
            let slice_end: usize = slice.span.range().end_resolved(slice_source_len).unwrap_or(0);

            // If they overlap, cut the
        }

        todo!()
    }
}
