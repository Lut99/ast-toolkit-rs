//  SPAN.rs
//    by Lut99
//
//  Created:
//    15 Dec 2023, 19:05:00
//  Last edited:
//    15 Mar 2025, 16:31:11
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements a [`Span`], which abstracts over some input to track a particular location in it.
//

use std::cmp::Ordering;
use std::convert::Infallible;
use std::fmt::{Debug, Display, Formatter, Result as FResult};
use std::hash::{Hash, Hasher};
use std::ops::{Bound, RangeBounds};
use std::rc::Rc;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate::as_str::{SpannableAsStr, SpannableTryAsStr};
use crate::display::SpannableDisplay;
use crate::eq::SpannableEq;
use crate::hash::SpannableHash;
use crate::lines::SpannableLines;
use crate::locate::SpannableLocate;
use crate::range::SpanRange;
use crate::spannable::Spannable;
use crate::{SpannableAsBytes, SpannableOrd};


/***** AUXILLARY *****/
/// A helper trait formalizing things that return [`Span`]s.
pub trait Spanning<F, S> {
    /// Returns some [`Span`] representing self's relation to the source text it was parsed from.
    ///
    /// # Returns
    /// Some [`Span`] that represents the spanned area.
    ///
    /// Note that, because this function takes `self` by reference, the given span may be a copy.
    /// You can prefer to use [`IntoSpanning::into_span()`] when possible to get it by ownership
    /// instead.
    fn span(&self) -> Span<F, S>;

    /// Returns some [`Span`] representing self's relation to the source text it was parsed from.
    ///
    /// # Returns
    /// Some [`Span`] that represents the spanned area.
    fn into_span(self) -> Span<F, S>
    where
        Self: Sized,
    {
        self.span()
    }
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
/// ```ignore
/// todo!()
/// ````
#[derive(Clone, Copy)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct Span<F, S> {
    /// Something describing the input (e.g., filename).
    from:   F,
    /// An input of \*something\* spannable.
    source: S,
    /// The [`SpanRange`] of this Span over the `source`.
    range:  SpanRange,
}

// Constructors
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
    /// - `from`: Something describing the input (e.g., filename).
    /// - `source`: The input source (text) to wrap.
    ///
    /// # Returns
    /// A new `Span` that spans the entire given `source`.
    #[inline]
    pub fn new(from: F, source: S) -> Self { Self { from, source, range: SpanRange::Open } }

    /// Constructor for the `Span` that initializes it with a custom range.
    ///
    /// # Arguments
    /// - `from`: Something describing the input (e.g., filename).
    /// - `source`: The input source (text) to wrap.
    /// - `range`: Something akin to a [`SpanRange`] that describes the range to write.
    ///
    /// # Returns
    /// A new `Span` that spans the given `range` of `source`.
    #[inline]
    pub fn ranged(from: F, source: S, range: impl RangeBounds<usize>) -> Self {
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

        // OK, build self
        Self { from, source, range }
    }
}

// Formatters
impl<F, S> Debug for Span<F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        let mut fmt = f.debug_struct(&format!("Span<{}, {}>", std::any::type_name::<F>(), std::any::type_name::<S>()));
        fmt.field("from", &..);
        fmt.field("source", &..);
        fmt.field("range", &self.range);
        fmt.finish()
    }
}

// Joining
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

// Spanning
impl<F, S: Spannable> Span<F, S> {
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
impl<F, S: SpannableLocate> Span<F, S> {
    /// Finds the column/line pair of the given position in the underlying source text.
    ///
    /// The position is in _logical coordinates_, i.e., whatever are logical chunks for the spanned
    /// object instead of bytes. For example, for strings, this would be graphemes.
    ///
    /// This function assumes that the start of the underlying source text is (0, 0). I.e., it
    /// ignores the range.
    ///
    /// # Arguments
    /// - `idx`: Some offset in the spanned object's logical space (e.g., graphemes).
    ///
    /// # Returns
    /// A pair of column and line numbers, respectively. Both are zero-indexed.
    ///
    /// If the given `idx` is out-of-bounds, then [`None`] is returned.
    #[inline]
    pub fn coords_of(&self, idx: usize) -> Option<(usize, usize)> { self.source.coords_of(idx) }



    /// Returns the line/column pair of the start of this span.
    ///
    /// # Returns
    /// A pair of [`usize`]s encoding the zero-indexed line number and column number for the start of this span, respectively. Returns [`None`] if the span is empty.
    #[inline]
    pub fn start(&self) -> Option<(usize, usize)> {
        match self.range {
            SpanRange::Closed(s, _) => Some(
                self.source
                    .coords_of(s)
                    .unwrap_or_else(|| panic!("Internal start bound {} is out-of-bounds for source string of length {}", s, self.source.byte_len())),
            ),
            SpanRange::ClosedOpen(s) => Some(
                self.source
                    .coords_of(s)
                    .unwrap_or_else(|| panic!("Internal start bound {} is out-of-bounds for source string of length {}", s, self.source.byte_len())),
            ),
            SpanRange::OpenClosed(_) => self.source.coords_of(0),
            SpanRange::Open => self.source.coords_of(0),
            SpanRange::Empty => None,
        }
    }

    /// Returns the line number of the start of this span.
    ///
    /// # Returns
    /// A [`usize`]s encoding the zero-indexed line number for the start of this span. Returns [`None`] if the span is empty.
    #[inline]
    pub fn line(&self) -> Option<usize> { self.start().map(|(l, _)| l) }

    /// Returns the column number of the start of this span.
    ///
    /// # Returns
    /// A [`usize`]s encoding the zero-indexed column number for the start of this span. Returns [`None`] if the span is empty.
    #[inline]
    pub fn col(&self) -> Option<usize> { self.start().map(|(_, c)| c) }



    /// Returns the line/column pair of the end of this span.
    ///
    /// # Returns
    /// A pair of [`usize`]s encoding the zero-indexed line number and column number for the end of this span, respectively. Returns [`None`] if the span is empty.
    #[inline]
    pub fn end(&self) -> Option<(usize, usize)> {
        match self.range {
            SpanRange::Closed(_, e) => Some(
                self.source
                    .coords_of(e)
                    .unwrap_or_else(|| panic!("Internal end bound {} is out-of-bounds for source string of length {}", e, self.source.byte_len())),
            ),
            SpanRange::ClosedOpen(_) => self.source.coords_of(self.source.byte_len() - 1),
            SpanRange::OpenClosed(e) => Some(
                self.source
                    .coords_of(e)
                    .unwrap_or_else(|| panic!("Internal end bound {} is out-of-bounds for source string of length {}", e, self.source.byte_len())),
            ),
            SpanRange::Open => self.source.coords_of(self.source.byte_len() - 1),
            SpanRange::Empty => None,
        }
    }

    /// Returns the line number of the end of this span.
    ///
    /// # Returns
    /// A [`usize`]s encoding the zero-indexed line number for the end of this span. Returns [`None`] if the span is empty.
    #[inline]
    pub fn end_line(&self) -> Option<usize> { self.end().map(|(l, _)| l) }

    /// Returns the column number of the end of this span.
    ///
    /// # Returns
    /// A [`usize`]s encoding the zero-indexed column number for the end of this span. Returns [`None`] if the span is empty.
    #[inline]
    pub fn end_col(&self) -> Option<usize> { self.end().map(|(_, c)| c) }
}
impl<F, S: SpannableLines> Span<F, S> {
    /// Returns a slice of the source text matching the internal range, but then the full lines of
    /// the highlighted text.
    ///
    /// # Returns
    /// An [`S::Slice`] that captures the highlighted area plus the start- and ends of its lines.
    #[inline]
    pub fn spanned_lines<'s>(&'s self) -> S::Lines<'s> { self.source.slice_lines(self.range) }
}

// Inherited spanning
impl<F, S: SpannableAsBytes> Span<F, S> {
    /// Returns the spanned area as raw bytes.
    ///
    /// Note that this has to be a _reference_ to a _continious_ byte range.
    ///
    /// # Returns
    /// A [`&[u8]`](u8) that represents (some) raw byte value of the spanned area.
    #[inline]
    pub fn as_bytes(&self) -> &[u8] { self.source.as_bytes(self.range) }
}
impl<F, S: SpannableTryAsStr> Span<F, S> {
    /// Attempts to return the spanned area as a string.
    ///
    /// This may fail if the spanned area is _not_ a string. This can theoretically happen for e.g.
    /// byte slices if they do not span valid UTF-8.
    ///
    /// # Returns
    /// A [`&str`](str) that represents the string value of the spanned area.
    ///
    /// # Errors
    /// This function may error if the spanned area is not somehow a string.
    #[inline]
    pub fn try_as_str(&self) -> Result<&str, S::Error> { self.source.try_as_str(self.range) }
}
impl<F, S: SpannableAsStr> Span<F, S> {
    /// Returns the spanned area as a string.
    ///
    /// # Returns
    /// A [`&str`](str) that represents the string value of the spanned area.
    #[inline]
    pub fn as_str(&self) -> &str { self.source.as_str(self.range) }
}
impl<F, S: SpannableDisplay> Display for Span<F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult { self.source.slice_fmt(self.range, f) }
}
impl<F: Eq, S: SpannableEq> Eq for Span<F, S> {}
impl<F: Hash, S: SpannableHash> Hash for Span<F, S> {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.from.hash(state);
        self.source.slice_hash(self.range, state)
    }
}
impl<F: Eq, S: SpannableEq + SpannableOrd> Ord for Span<F, S> {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        // SAFETY: This is OK because [`Span::partial_cmp()`] always returns `Some` (see below)
        unsafe { self.partial_cmp(other).unwrap_unchecked() }
    }
}
impl<F: PartialEq, S: SpannableEq> PartialEq for Span<F, S> {
    #[inline]
    fn eq(&self, other: &Self) -> bool { self.from == other.from && self.source.slice_eq(self.range, &other.source, other.range) }
}
impl<F: PartialEq, S: SpannableEq + SpannableOrd> PartialOrd for Span<F, S> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> { Some(self.source.slice_ord(self.range, &other.source, other.range)) }
}
impl<F: Clone, S: Clone> Spanning<F, S> for Span<F, S> {
    #[inline]
    fn span(&self) -> Span<F, S> { self.clone() }
}

// Accessors
impl<F, S> Span<F, S> {
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

// Transformers
impl<F, S> Span<F, S> {
    /// Casts the underlying `from`- and `source`-strings in this Span to some owned counterparts.
    ///
    /// The owned versions are wrapped in reference-counted pointers in order to allow sharing the `from`- and `source`-strings between Spans.
    ///
    /// This is therefore only really useful when converting errors into ones that do not depend on the final AST anymore.
    ///
    /// # Generics
    /// - `FO`: The chosen owned counterpart to `F`.
    /// - `SO`: The chosen owned counterpart to `S`.
    ///
    /// # Returns
    /// A span with a clone of the original `from`- and `source`-texts.
    #[inline]
    pub fn into_owned<FO: From<F>, SO: From<S>>(self) -> Span<Rc<FO>, Rc<SO>> {
        Span { from: Rc::new(self.from.into()), source: Rc::new(self.source.into()), range: self.range }
    }
}
