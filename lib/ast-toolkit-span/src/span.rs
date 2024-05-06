//  SPAN.rs
//    by Lut99
//
//  Created:
//    15 Dec 2023, 19:05:00
//  Last edited:
//    06 May 2024, 16:46:11
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements a [`Span`], which abstracts over some input to track a particular location in it.
//

use std::convert::Infallible;
use std::fmt::{Display, Formatter, Result as FResult};
use std::hash::{Hash, Hasher};
use std::ops::{Bound, RangeBounds};

use crate::eq::SpannableEq;
use crate::hash::SpannableHash;
use crate::range::SpanRange;
use crate::spannable::Spannable;
use crate::SpannableDisplay;


/***** AUXILLARY *****/
/// A helper trait formalizing things that return [`Span`]s.
pub trait Spanning<F, S> {
    /// Returns some [`Span`] representing self's relation to the source text it was parsed from.
    ///
    /// # Returns
    /// A new [`Span`] that represents the spanned area.
    fn span(&self) -> Span<F, S>;
}

// Default impls
impl<F, S> Spanning<F, S> for Infallible {
    /// Non-sensible implementation of [`Spanning`] for [`Infallible`].
    ///
    /// # Panics
    /// This function _always_ panics. It is not meant to be used by itself, but rather to be given as a sensible default when something [`Spanning`] is not used.
    #[inline]
    fn span(&self) -> Span<F, S> { unimplemented!() }
}





/***** LIBRARY *****/
/// Defines a wrapper around some input source text that allows us to track parts of it.
///
/// Built to be [`nom`](https://github.com/rust-bakery/nom)-compatible; see the `nom`-feature.
///
/// # Example
/// ```rust
/// todo!() 
/// ````
#[derive(Clone, Copy, Debug)]
pub struct Span<F, S> {
    /// Something describing the input (e.g., filename).
    from:   F,
    /// An input of \*something\* spannable.
    source: S,
    /// The [`SpanRange`] of this Span over the `source`.
    range:  SpanRange,
}

impl<F, S> Span<F, S> {
    /// Constructor for the `Span` that initializes it from a source text, but spanning nothing.
    ///
    /// # Arguments
    /// - `from`: Something describing the input (e.g., filename).
    /// - `source`: The input source (text) to wrap.
    ///
    /// # Returns
    /// A new `Span` that spans none of the given `source`.
    #[inline]
    pub fn empty(from: F, source: S) -> Self { Self { from, source, range: SpanRange::Empty } }

    /// Constructor for the `Span` that initializes it to Span the entire given range.
    ///
    /// # Arguments
    /// - `source`: The input source (text) to wrap.
    ///
    /// # Returns
    /// A new `Span` that spans the entire given `source`.
    #[inline]
    pub fn new(from: F, source: S) -> Self { Self { from, source, range: SpanRange::Open } }

    /// Provides access to the internal `from`-string.
    ///
    /// If `F` implements [`Copy`], you might prefer [`Span::from()`] instead to avoid the lifetime to `self`.
    ///
    /// # Returns
    /// A reference to the internal `from`-string.
    #[inline]
    pub fn from_ref(&self) -> &F { &self.from }

    /// Provides access to the internal `source`-string.
    ///
    /// If `S` implements [`Copy`], you might prefer [`Span::source()`] instead to avoid the lifetime to `self`.
    ///
    /// # Returns
    /// A reference to the internal `source`-string.
    #[inline]
    pub fn source_ref(&self) -> &S { &self.source }

    /// Returns the inner range over the source text.
    ///
    /// # Returns
    /// A [`SpanRange`] describing the spanned area.
    #[inline]
    pub fn range(&self) -> SpanRange { self.range }
}
impl<F, S: Spannable> Span<F, S> {
    /// Extends this Span to also cover the other Span.
    ///
    /// This is like [`Span::join()`], except that no cloning of the source is performed.
    ///
    /// # Arguments
    /// - `other`: The other [`Span`] to join with.
    ///
    /// # Returns
    /// Returns `true` if the join was successful, or `false` if the `other` Span pointed to another source than we did (i.e., unjoinable).
    #[inline]
    #[must_use]
    pub fn join_mut(&mut self, other: &Self) -> bool {
        if !self.source.is_same(&other.source) {
            return false;
        }
        self.range.join_mut(&other.range);
        true
    }

    /// Provides access to the internal `source`-string, but only the spanned area.
    ///
    /// # Returns
    /// A reference to the internal `source`-string.
    #[inline]
    pub fn value<'s>(&'s self) -> S::Slice<'s> { self.source.slice(self.range) }

    /// Returns if the spanned area is empty.
    ///
    /// # Returns
    /// True if this Span does not cover a substantial range, or false otherwise.
    pub fn is_empty(&self) -> bool {
        // If the source is empty, so are we
        let source_len: usize = self.source.byte_len();
        if source_len == 0 {
            return true;
        }

        // Then, match on the range type
        match self.range {
            SpanRange::Closed(s, e) => s >= e,
            SpanRange::ClosedOpen(s) => s >= source_len,
            SpanRange::OpenClosed(e) => e == 0,
            SpanRange::Open => source_len == 0,
            SpanRange::Empty => true,
        }
    }
}
impl<F: Copy, S> Span<F, S> {
    /// Provides access to the internal `from`-string.
    ///
    /// If `F` does not implement [`Copy`], you might prefer [`Span::from_ref()`] instead.
    ///
    /// # Returns
    /// The internal `from`-string.
    #[inline]
    pub fn from(&self) -> F { self.from }
}
impl<F, S: Copy> Span<F, S> {
    /// Provides access to the internal `input`-string.
    ///
    /// If `S` does not implement [`Copy`], you might prefer [`Span::source_ref()`] instead.
    ///
    /// # Returns
    /// The internal `input`-string.
    #[inline]
    pub fn source(&self) -> S { self.source }
}
impl<F: Clone, S: Clone> Span<F, S> {
    /// Returns a new [`Span`] that represents a sub-span of this one.
    ///
    /// # Arguments
    /// - `range`: The range to slice.
    ///
    /// # Returns
    /// A new [`Span`] that is a sub-span of this one.
    ///
    /// # Panics
    /// This function panics if:
    /// - `range.end` - `range.start` is a negative number (after adjusting for inclusive/exclusive); or
    /// - `range.start` or `range.end` are out-of-bounds for this
    #[inline]
    pub fn slice(&self, range: impl RangeBounds<usize>) -> Self {
        // Examine the bounds to find new start & stop
        let range: SpanRange = match (range.start_bound(), range.end_bound()) {
            (Bound::Included(s), Bound::Included(e)) => SpanRange::Closed(*s, *e + 1),
            (Bound::Included(s), Bound::Excluded(e)) => SpanRange::Closed(*s, *e),
            (Bound::Included(s), Bound::Unbounded) => SpanRange::ClosedOpen(*s),
            (Bound::Excluded(s), Bound::Included(e)) => SpanRange::Closed(*s + 1, *e + 1),
            (Bound::Excluded(s), Bound::Excluded(e)) => SpanRange::Closed(*s + 1, *e),
            (Bound::Excluded(s), Bound::Unbounded) => SpanRange::ClosedOpen(*s + 1),
            (Bound::Unbounded, Bound::Included(e)) => SpanRange::OpenClosed(*e + 1),
            (Bound::Unbounded, Bound::Excluded(e)) => SpanRange::OpenClosed(*e),
            (Bound::Unbounded, Bound::Unbounded) => SpanRange::Open,
        };

        // Merge the found range to apply to self instead of the full one
        let range: SpanRange = self.range.span(&range);

        // Build self
        Self { from: self.from.clone(), source: self.source.clone(), range }
    }

    /// Returns a new Span that is equal to this one, but with an open end bound.
    ///
    /// This is useful during parsing to indicate a "from this moment onwards" kind of span.
    ///
    /// # Returns
    /// A new Span that has the same start bound as `self` but and open end bound.
    #[inline]
    pub fn start_onwards(&self) -> Self {
        let range: SpanRange = match self.range {
            SpanRange::Closed(s, _) => SpanRange::ClosedOpen(s),
            SpanRange::ClosedOpen(s) => SpanRange::ClosedOpen(s),
            SpanRange::OpenClosed(_) => SpanRange::Open,
            SpanRange::Open => SpanRange::Open,
            SpanRange::Empty => SpanRange::Empty,
        };
        Self { from: self.from.clone(), source: self.source.clone(), range }
    }
}
impl<F: Clone, S: Clone + Spannable> Span<F, S> {
    /// Combines this span with another Span to span both areas.
    ///
    /// Specifically, given a span `s1..e1` and `s2..e2`, produces a new span `s1..e2`.
    ///
    /// # Arguments
    /// - `other`: The other [`Span`] to join with.
    ///
    /// # Returns
    /// A new [`Span`] that is the combination of both spans, unless the Spans span different sources (then [`None`] is returned).
    #[inline]
    pub fn join(&self, other: &Self) -> Option<Self> {
        if self.source.is_same(&other.source) {
            Some(Self { from: self.from.clone(), source: self.source.clone(), range: self.range.join(&other.range) })
        } else {
            None
        }
    }
}

impl<F, S: SpannableDisplay> Display for Span<F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult { self.source.slice_fmt(self.range, f) }
}
impl<F, S: SpannableEq> Eq for Span<F, S> {}
impl<F, S: SpannableHash> Hash for Span<F, S> {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) { self.source.slice_hash(self.range, state) }
}
impl<F, S: SpannableEq> PartialEq for Span<F, S> {
    #[inline]
    fn eq(&self, other: &Self) -> bool { self.source.slice_eq(self.range, &other.source, other.range) }
}

impl<F: Clone, S: Clone> Spanning<F, S> for Span<F, S> {
    #[inline]
    fn span(&self) -> Span<F, S> { self.clone() }
}
