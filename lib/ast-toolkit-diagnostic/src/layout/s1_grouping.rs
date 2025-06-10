//  STEP 1: GROUPING.rs
//    by Lut99
//
//  Description:
//!   This module implements the first step of the layouting algorithm
//!   described in the [`layout`](super)-module.
//!
//!   Given a list of [`Annotation`]s, it will split them into several
//!   [`AnnotGroup`]s who are unified by the same source. Accordingly, each is
//!   represented by a new data structure that doesn't consider [`Span`]s but
//!   [`Range`]s instead.
//

use std::collections::HashMap;

use ast_toolkit_span::{Span, Spannable};

use crate::annotations::{Annotation, Severity};


/***** DATA STRUCTURES *****/
/// Represents a bundle of [`Annot`]ations with their `S`ources externalized here.
#[derive(Clone, Debug)]
pub(super) struct AnnotGroup<'s, S: Spannable<'s>> {
    /// The main span bundling the source itself.
    pub span:   Span<S>,
    /// The bundle of annotations that are grouped in this source.
    pub annots: Vec<Annot<S::Elem>>,
}



/// Represents a simplified [`Annotation`] where its [`Span`]'s the original
/// source is externalized.
#[derive(Clone, Debug)]
pub(super) struct Annot<E> {
    /// Any replacement to do for this annotation.
    pub replacement: Option<Vec<E>>,
    /// Any message to give with this annotation.
    pub message: Option<String>,
    /// The severity of this annotation.
    pub severity: Severity,
    /// The range describing which part of the main span this annotation concerns.
    pub range: Range,
}

// Conversion
impl<'s, S: Spannable<'s>> From<Annotation<'s, S>> for Annot<S::Elem> {
    #[inline]
    fn from(value: Annotation<'s, S>) -> Self {
        let Annotation { replacement, message, severity, span } = value;

        // Resolve the span range
        let source: &S = span.source();
        let source_len: usize = source.len();
        let start: usize = span.range().start_resolved(source_len).unwrap_or(0);
        let end: usize = span.range().end_resolved(source_len).unwrap_or(0);

        // Build self
        Self { replacement, message, severity, range: Range { start, end } }
    }
}



/// Defines a concrete range over an array.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(super) struct Range {
    /// The start index (inclusive).
    pub start: usize,
    /// The stop index (exclusive).
    pub end:   usize,
}

// Range
impl Range {
    /// Check if this range overlaps with another one.
    ///
    /// # Arguments
    /// - `other`: Some other [`Range`] to check for overlap with.
    ///
    /// # Returns
    /// True if they overlap, or false otherwise.
    #[inline]
    pub fn overlaps_with(&self, other: &Self) -> bool {
        // Based on https://stackoverflow.com/a/3269471
        self.start < other.end && other.start < self.end
    }



    /// Returns the length of this Range.
    ///
    /// # Returns
    /// A [`usize`] counting how many elements are spanned.
    #[inline]
    pub fn len(&self) -> usize { if self.start <= self.end { self.end - self.start } else { 0 } }
}





/***** LIBRARY *****/
/// Given a list of [`Annotation`]s, groups them by source and returns them as [`Annot`]ations that
/// are agnostic to the source they apply in.
///
/// This implements the first step of the layouting algorithm detailled in [`layout`](super).
///
/// # Arguments
/// - `annots`: Something yielding [`Annotation`]s that we will group.
///
/// # Returns
/// A set of [`AnnotGroup`]s that group [`Annot`]s by `S`ource.
#[inline]
pub(super) fn group_annots<'s, S>(annots: impl IntoIterator<Item = Annotation<'s, S>>) -> HashMap<S::SourceId, AnnotGroup<'s, S>>
where
    S: Spannable<'s>,
{
    let iter = annots.into_iter();
    let size_hint: (usize, Option<usize>) = iter.size_hint();
    let mut res: HashMap<S::SourceId, AnnotGroup<'s, S>> = HashMap::with_capacity(size_hint.1.unwrap_or(size_hint.0));
    for annot in iter {
        // Check if we've seen annotations of this source before
        let source_id: S::SourceId = annot.span.source_id();
        if let Some(group) = res.get_mut(&source_id) {
            group.span.extend(&annot.span);
            group.annots.push(annot.into());
        } else {
            let Annotation { replacement, message, severity, span } = annot;

            // Resolve the span range
            let source: &S = span.source();
            let source_len: usize = source.len();
            let start: usize = span.range().start_resolved(source_len).unwrap_or(0);
            let end: usize = span.range().end_resolved(source_len).unwrap_or(0);

            // Insert the annotation
            res.insert(source_id, AnnotGroup { span, annots: vec![Annot { replacement, message, severity, range: Range { start, end } }] });
        }
    }
    res
}
