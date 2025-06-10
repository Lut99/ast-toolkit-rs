//  STEP 2: APPLYING REPLACEMENTS.rs
//    by Lut99
//
//  Description:
//!   This module implements the third step of the layouting algorithm
//!   described in the [`layout`](super)-module.
//!
//!   Given an [`AnnotGroup`], it will wraps its [`Span`] in a [`VirtualSpan`]
//!   and interleave slices of the first with replacements embedded in the
//!   [`AnnotGroup`]. This allows the rest of the pipeline to abstract over
//!   suggestions.
//

use ast_toolkit_span::{Span, Spannable};

use super::s1_grouping::{AnnotGroup, Range};
use crate::annotations::Severity;
use crate::layout::s1_grouping::Annot;


/***** DATA STRUCTURES *****/
/// Abstracts over a [`Span`] with sliced replaced values.
///
/// You can obtain one by calling [`apply_replace_annots()`] on an [`AnnotGroup`].
pub(super) struct VirtualSpan<'s, S: Spannable<'s>> {
    /// A list of the slices that make up this span.
    slices: Vec<VirtualSlice<'s, S>>,
    /// The annotations that we haven't processed yet.
    annots: Vec<VirtualAnnot>,
}



/// Abstracts over either an original `S`ource slice, or a replacement.
pub(super) enum VirtualSlice<'s, S: Spannable<'s>> {
    /// It's original source slice.
    Source(Span<S>),
    /// It's a replacement array.
    Replace(Vec<S::Elem>),
}



/// Defines a processed version of an [`Annot`] that has no more replacement.
#[derive(Clone, Debug)]
pub(super) struct VirtualAnnot {
    /// Any message to give with this annotation.
    pub message:  Option<String>,
    /// The severity of this annotation.
    pub severity: Severity,
    /// The range describing which part of the main span this annotation concerns.
    pub range:    Range,
}

// Conversion
impl<E> From<Annot<E>> for VirtualAnnot {
    #[inline]
    fn from(value: Annot<E>) -> Self {
        let Annot { replacement: _, message, severity, range } = value;
        Self { message, severity, range }
    }
}





/***** LIBRARY *****/
/// Given an [`AnnotGroup`], it will wraps its [`Span`] in a [`VirtualSpan`] and interleave slices
/// of the first with replacements embedded in the [`AnnotGroup`].
///
/// This implements the third step of the layouting algorithm detailled in [`layout`](super).
///
/// # Arguments
/// - `group`: An [`AnnotGroup`] to turn into a [`VirtualSpan`].
///
/// # Returns
/// A [`VirtualSpan`]. Note that it does not embed [`Annot`]s, but rather [`VirtualAnnot`]s who do
/// not carry replacements with them anymore.
pub fn apply_replace_annots<'s, S: Clone + Spannable<'s>>(group: AnnotGroup<'s, S>) -> VirtualSpan<'s, S> {
    let AnnotGroup { mut span, mut annots } = group;

    // First: order the annotations by starting range. Since we've guaranteed ourselves in step 2
    // that any annotation with a replace is unique, this will allow us to scan and slice the span
    // quickly.
    annots.sort_by_key(|a| a.range.start);

    // Then: loop through the replacing annotations and slice
    let mut slices: Vec<VirtualSlice<'s, S>> = Vec::with_capacity(2 * annots.len());
    let mut vannots: Vec<VirtualAnnot> = Vec::with_capacity(annots.len());
    let mut running_doffset: isize = 0;
    for annot in annots {
        let Annot { replacement, message, severity, range } = annot;

        // If it's a replacement, then we start slicing
        let mut new_running_doffset: isize = running_doffset;
        if let Some(replacement) = replacement {
            // Add any source up to the start of the replacement
            // NOTE: We don't slice because we're not working with Span-relative indices, but
            // source-relative
            let source: &S = span.source();
            let pre_span: Span<S> = Span::ranged(source.clone(), span.range().start_resolved(source.len()).unwrap_or(0)..range.start);
            if !pre_span.is_empty() {
                slices.push(VirtualSlice::Source(pre_span));
            }
            span = Span::ranged(span.into_source(), range.end..);

            // Add the replacement
            // SAFETY: We can unwrap because `replace_annots` only contains replacements that are
            // `Some`
            let replacement_len: usize = replacement.len();
            slices.push(VirtualSlice::Replace(replacement));

            // Keep tally of how much offset to apply to any following ranges
            new_running_doffset = running_doffset + (replacement_len as isize - range.len() as isize);
        }

        // Add the annotation with updated offset
        // Note that, for _this_ one, the first still has to use the OLD offset (it isn't
        // influenced by its own, potential replace). The second value IS influenced.
        vannots.push(VirtualAnnot {
            message,
            severity,
            range: Range { start: (range.start as isize + running_doffset) as usize, end: (range.end as isize + new_running_doffset) as usize },
        });
        running_doffset = new_running_doffset;
    }

    // Add the final span, if any
    if !span.is_empty() {
        slices.push(VirtualSlice::Source(span));
    }

    // OK, done!
    VirtualSpan { slices, annots: vannots }
}





/***** TESTS *****/
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_apply_replace_annots() {
        let span = Span::new("Hello, world!");
        let annot = Annot { replacement: Some("Goodbye".into()), message: None, severity: Severity::Suggestion, range: Range { start: 0, end: 5 } };
        let vspan = apply_replace_annots(AnnotGroup { span, annots: vec![annot] });
        let mut bytes: Vec<u8> = Vec::new();
        for slice in vspan.slices {
            match slice {
                VirtualSlice::Source(s) => bytes.extend(s.as_bytes()),
                VirtualSlice::Replace(v) => bytes.extend(v.as_slice()),
            }
        }
        assert_eq!(bytes, b"Goodbye, world!");
        assert_eq!(vspan.annots.len(), 1);
        assert_eq!(vspan.annots[0].range, Range { start: 0, end: 7 });

        let annot1 = Annot { replacement: Some(":".into()), message: None, severity: Severity::Suggestion, range: Range { start: 5, end: 6 } };
        let annot2 = Annot { replacement: Some("?".into()), message: None, severity: Severity::Suggestion, range: Range { start: 12, end: 13 } };
        let vspan = apply_replace_annots(AnnotGroup { span, annots: vec![annot1, annot2] });
        let mut bytes: Vec<u8> = Vec::new();
        for slice in vspan.slices {
            match slice {
                VirtualSlice::Source(s) => bytes.extend(s.as_bytes()),
                VirtualSlice::Replace(v) => bytes.extend(v.as_slice()),
            }
        }
        assert_eq!(bytes, b"Hello: world?");
        assert_eq!(vspan.annots.len(), 2);
        assert_eq!(vspan.annots[0].range, Range { start: 5, end: 6 });
        assert_eq!(vspan.annots[1].range, Range { start: 12, end: 13 });

        let annot1 = Annot { replacement: Some("Goodbye".into()), message: None, severity: Severity::Suggestion, range: Range { start: 0, end: 5 } };
        let annot2 = Annot { replacement: Some("!!".into()), message: None, severity: Severity::Suggestion, range: Range { start: 12, end: 13 } };
        let vspan = apply_replace_annots(AnnotGroup { span, annots: vec![annot1, annot2] });
        let mut bytes: Vec<u8> = Vec::new();
        for slice in vspan.slices {
            match slice {
                VirtualSlice::Source(s) => bytes.extend(s.as_bytes()),
                VirtualSlice::Replace(v) => bytes.extend(v.as_slice()),
            }
        }
        assert_eq!(bytes, b"Goodbye, world!!");
        assert_eq!(vspan.annots.len(), 2);
        assert_eq!(vspan.annots[0].range, Range { start: 0, end: 7 });
        assert_eq!(vspan.annots[1].range, Range { start: 14, end: 16 });
    }
}
