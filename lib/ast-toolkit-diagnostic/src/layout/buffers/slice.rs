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

use crate::annotations::{Annotation, AnnotationInner, AnnotationInnerHighlight, AnnotationInnerSuggestion, Severity};


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
    ///
    /// Note that the latest colour is always applied.
    color:  SliceColor,
    /// A list of annotations that are sticked on top of this area.
    annots: Vec<SliceAnnot<S::Elem>>,
}

/// Defines possible annotations attached to a [`Slice`].
#[derive(Clone, Debug)]
pub struct SliceAnnot<E> {
    /// The message to write.
    pub message:    Option<String>,
    /// Any replacement if this is a suggestion.
    pub suggestion: Option<Vec<E>>,
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
impl<'s, S: Spannable<'s>> SliceBuffer<'s, S>
where
    S::Elem: Clone,
{
    /// Writes a new annotation to the slice buffer.
    ///
    /// # Arguments
    /// - `annot`: Some [`Annotation`] to apply to the internal source text.
    ///
    /// # Returns
    /// This function will return `true` if the annotation was applied, or `false` if it was either
    /// from a different `S`ource or not in range of the internal main span.
    #[inline]
    pub fn annotate(&mut self, annot: Annotation<'s, S>) -> bool {
        let Annotation { inner, span } = annot;

        // Assert it is from the same source
        if self.slices[0].span.source_id() != span.source_id() {
            return false;
        }

        // Extract some o' t' details
        let (color, message, suggestion): (SliceColor, Option<String>, Option<Vec<S::Elem>>) = match inner {
            AnnotationInner::Highlight(AnnotationInnerHighlight { severity, message }) => (SliceColor::Severity(severity), message, None),
            AnnotationInner::Suggestion(AnnotationInnerSuggestion { replacement, message }) => (SliceColor::Suggestion, message, Some(replacement)),
        };

        // Get the ranges from the annotation span
        let annot_source_len: usize = span.source().len();
        let annot_start: usize = span.range().start_resolved(annot_source_len).unwrap_or(0);
        let annot_end: usize = span.range().end_resolved(annot_source_len).unwrap_or(0);

        // Search to apply it
        let slices_len: usize = self.slices.len();
        for i in 0..slices_len {
            if span.is_empty() {
                break;
            }

            // Get the ranges of the slice's span
            let slice: &Slice<S> = &self.slices[i];
            let slice_source_len: usize = slice.span.source().len();
            let slice_start: usize = slice.span.range().start_resolved(slice_source_len).unwrap_or(0);
            let slice_end: usize = slice.span.range().end_resolved(slice_source_len).unwrap_or(0);

            // There's multiple ways in which they overlap
            if annot_start == slice_start && annot_end >= slice_end {
                // Perfect replace
                self.slices[i].color = color;
                // self.slices[i].annots.push(SliceAnnot { message, suggestion });

                // Now either quit, or refresh and try for the next one
                if annot_end > slice_end {
                } else {
                    return true;
                }
            } else if annot_start == slice_start && annot_end < slice_end {
                // Cut the start
                // NOTE: Yes there's an unguarded subtraction here. Should be fine though, I'm
                // sure, since `annot_start` == `slice_start` and all that.
                self.slices[i].span.shrink(annot_end - slice_start..);
                let mut annots: Vec<SliceAnnot<S::Elem>> = self.slices[i].annots.clone();
                // annots.push(SliceAnnot { message, suggestion });
                self.slices.insert(i, Slice { span, color, annots });
                return true;
            } else if annot_start > slice_start && annot_start < slice_end && annot_end >= slice_end {
                // Cut the end
            } else if annot_start > slice_start && annot_start < slice_end && annot_end < slice_end {
                // Middle replace
            }
        }

        // It wasn't within range of any slice
        false
    }
}
