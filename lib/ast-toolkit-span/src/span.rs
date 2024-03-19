//  SPAN.rs
//    by Lut99
//
//  Created:
//    15 Dec 2023, 19:05:00
//  Last edited:
//    19 Mar 2024, 14:37:45
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements a [`Span`], which abstracts over some input to track a particular location in it.
//

use std::borrow::Cow;
use std::hash::{Hash, Hasher};
use std::ops::{Bound, RangeBounds};


/***** HELPER MACROS *****/
/// Converts from a [`RangeBound`] to a concrete bound.
macro_rules! index_range_bound {
    ($slice:expr, $range:expr) => {{
        let slice = $slice;
        let range = $range;
        match (range.start_bound(), range.end_bound()) {
            (Bound::Included(s), Bound::Included(e)) => &slice[*s..=*e],
            (Bound::Included(s), Bound::Excluded(e)) => &slice[*s..*e],
            (Bound::Included(s), Bound::Unbounded) => &slice[*s..],
            (Bound::Excluded(_), _) => unimplemented!(),
            (Bound::Unbounded, Bound::Included(e)) => &slice[..=*e],
            (Bound::Unbounded, Bound::Excluded(e)) => &slice[..*e],
            (Bound::Unbounded, Bound::Unbounded) => &slice[..],
        }
    }};
}





/***** AUXILLARY *****/
/// A helper trait for the [`Span`] that can be implemented for anything used as input.
pub trait Spannable {
    type Slice<'s>: 's
    where
        Self: 's;

    /// Checks if this Spannable is the same (for all intends and purposes) as another Spannable of the same type.
    ///
    /// While it suffices to show that two [`Span`]s are the same semantically (e.g., using a simple byte-wise compare), it's also allowed to compare by pointer equality if possible for performance. Implementations for [`&[u8]`] and [`&str`] do this, for example.
    ///
    /// # Arguments
    /// - `other`: Some other Spannable of type Self to check with.
    ///
    /// # Returns
    /// True if these Spannables are the same, or false otherwise.
    fn is_same(&self, other: &Self) -> bool;

    /// Slices this Spannable by raw index.
    ///
    /// # Arguments
    /// - `range`: The range that slices this Spannable. Can be anything implementing [`RangeBounds`].
    ///
    /// # Returns
    /// A new instance of type `Self::Slice`, that is self but sliced.
    ///
    /// # Panics
    /// This function panics if out-of-bounds.
    fn slice<'s>(&'s self, range: impl RangeBounds<usize>) -> Self::Slice<'s>;
    /// Checks if the slices range of this Spannable is the same for the given sliced range of the same type.
    ///
    /// # Arguments
    /// - `range`: The range that slices this Spannable. Can be anything implementing [`RangeBounds`].
    /// - `other`: Some other Spannable of type Self to check with.
    ///
    /// # Returns
    /// True if they are equal, or false otherwise.
    ///
    /// # Panics
    /// This function panics if out-of-bounds.
    fn slice_eq(&self, range: impl RangeBounds<usize>, other: &Self, other_range: impl RangeBounds<usize>) -> bool;
    /// Computes a hash for the given range of this Spannable.
    ///
    /// # Arguments
    /// - `range`: The range that slices this Spannable. Can be anything implementing [`RangeBounds`].
    /// - `state`: Some [`Hasher`] that does the tough work.
    ///
    /// # Panics
    /// This function panics if out-of-bounds.
    fn slice_hash<H: Hasher>(&self, range: impl RangeBounds<usize>, state: &mut H);

    /// Returns the number of currently spanned "raw" items (e.g., bytes).
    ///
    /// # Returns
    /// A [`usize`] with the total number of bytes or other elementary items as is stored on-disk.
    fn byte_len(&self) -> usize;
}

// Default binary impls for [`Spannable`]
impl<'b> Spannable for &'b [u8] {
    type Slice<'s> = &'s [u8] where Self: 's;

    #[inline]
    fn is_same(&self, other: &Self) -> bool {
        let ptr_eq: bool = std::ptr::eq(self, other);
        #[cfg(debug_assertions)]
        {
            if !ptr_eq && self == other {
                eprintln!(
                    "DEBUG ASSERTION WARNING: Two byte arrays do not share the same pointer but are semantically equal. The &[u8]-implementation \
                     for Spannable assumes comparing them by pointer equality is sufficient."
                );
            }
        }
        ptr_eq
    }

    #[inline]
    #[track_caller]
    fn slice<'s2>(&'s2 self, range: impl RangeBounds<usize>) -> Self::Slice<'s2> { index_range_bound!(self, range) }

    #[inline]
    #[track_caller]
    fn slice_eq(&self, range: impl RangeBounds<usize>, other: &Self, other_range: impl RangeBounds<usize>) -> bool {
        index_range_bound!(self, range) == index_range_bound!(other, other_range)
    }

    #[inline]
    #[track_caller]
    fn slice_hash<H: Hasher>(&self, range: impl RangeBounds<usize>, state: &mut H) { index_range_bound!(self, range).hash(state) }

    #[inline]
    fn byte_len(&self) -> usize { self.len() }
}
impl<'b> Spannable for Cow<'b, [u8]> {
    type Slice<'s> = Cow<'s, [u8]> where Self: 's;

    #[inline]
    fn is_same(&self, other: &Self) -> bool {
        match (self, other) {
            // Compare by pointer if possible
            (Self::Borrowed(b1), Self::Borrowed(b2)) => {
                let ptr_eq: bool = std::ptr::eq(*b1, *b2);
                #[cfg(debug_assertions)]
                {
                    if !ptr_eq && self == other {
                        eprintln!(
                            "DEBUG ASSERTION WARNING: Two byte arrays do not share the same pointer but are semantically equal. The \
                             Cow<u8>-implementation for Spannable assumes comparing them by pointer equality if they're both borrowed is sufficient."
                        );
                    }
                }
                ptr_eq
            },
            // Otherwise, fall back to equality testing
            (o1, o2) => o1 == o2,
        }
    }

    #[inline]
    #[track_caller]
    fn slice<'s2>(&'s2 self, range: impl RangeBounds<usize>) -> Self::Slice<'s2> { Cow::Borrowed(index_range_bound!(self, range)) }

    #[inline]
    #[track_caller]
    fn slice_eq(&self, range: impl RangeBounds<usize>, other: &Self, other_range: impl RangeBounds<usize>) -> bool {
        index_range_bound!(self, range) == index_range_bound!(other, other_range)
    }

    #[inline]
    #[track_caller]
    fn slice_hash<H: Hasher>(&self, range: impl RangeBounds<usize>, state: &mut H) { index_range_bound!(self, range).hash(state) }

    #[inline]
    fn byte_len(&self) -> usize { self.len() }
}
impl Spannable for Vec<u8> {
    type Slice<'s> = Vec<u8> where Self: 's;

    #[inline]
    fn is_same(&self, other: &Self) -> bool { self == other }

    #[inline]
    #[track_caller]
    fn slice<'s2>(&'s2 self, range: impl RangeBounds<usize>) -> Self::Slice<'s2> { index_range_bound!(self, range).to_vec() }

    #[inline]
    #[track_caller]
    fn slice_eq(&self, range: impl RangeBounds<usize>, other: &Self, other_range: impl RangeBounds<usize>) -> bool {
        index_range_bound!(self, range) == index_range_bound!(other, other_range)
    }

    #[inline]
    #[track_caller]
    fn slice_hash<H: Hasher>(&self, range: impl RangeBounds<usize>, state: &mut H) { index_range_bound!(self, range).hash(state) }

    #[inline]
    fn byte_len(&self) -> usize { self.len() }
}

// Default string impls for [`Spannable`]
impl<'s> Spannable for &'s str {
    type Slice<'s2> = &'s2 str where Self: 's2;

    #[inline]
    fn is_same(&self, other: &Self) -> bool {
        let ptr_eq: bool = std::ptr::eq(self, other);
        #[cfg(debug_assertions)]
        {
            if !ptr_eq && self == other {
                eprintln!(
                    "DEBUG ASSERTION WARNING: Two string slices do not share the same pointer but are semantically equal. The &str-implementation \
                     for Spannable assumes comparing them by pointer equality is sufficient."
                );
            }
        }
        ptr_eq
    }

    #[inline]
    #[track_caller]
    fn slice<'s2>(&'s2 self, range: impl RangeBounds<usize>) -> Self::Slice<'s2> { index_range_bound!(self, range) }

    #[inline]
    #[track_caller]
    fn slice_eq(&self, range: impl RangeBounds<usize>, other: &Self, other_range: impl RangeBounds<usize>) -> bool {
        index_range_bound!(self, range) == index_range_bound!(other, other_range)
    }

    #[inline]
    #[track_caller]
    fn slice_hash<H: Hasher>(&self, range: impl RangeBounds<usize>, state: &mut H) { index_range_bound!(self, range).hash(state) }

    #[inline]
    fn byte_len(&self) -> usize { self.len() }
}
impl<'s> Spannable for Cow<'s, str> {
    type Slice<'s2> = Cow<'s2, str> where Self: 's2;

    #[inline]
    fn is_same(&self, other: &Self) -> bool {
        match (self, other) {
            // Compare by pointer if possible
            (Self::Borrowed(b1), Self::Borrowed(b2)) => {
                let ptr_eq: bool = std::ptr::eq(*b1, *b2);
                #[cfg(debug_assertions)]
                {
                    if !ptr_eq && self == other {
                        eprintln!(
                            "DEBUG ASSERTION WARNING: Two string slices do not share the same pointer but are semantically equal. The \
                             Cow<str>-implementation for Spannable assumes comparing them by pointer equality if they're both borrowed is \
                             sufficient."
                        );
                    }
                }
                ptr_eq
            },
            // Otherwise, fall back to equality testing
            (o1, o2) => o1 == o2,
        }
    }

    #[inline]
    #[track_caller]
    fn slice<'s2>(&'s2 self, range: impl RangeBounds<usize>) -> Self::Slice<'s2> { Cow::Borrowed(index_range_bound!(self, range)) }

    #[inline]
    #[track_caller]
    fn slice_eq(&self, range: impl RangeBounds<usize>, other: &Self, other_range: impl RangeBounds<usize>) -> bool {
        index_range_bound!(self, range) == index_range_bound!(other, other_range)
    }

    #[inline]
    #[track_caller]
    fn slice_hash<H: Hasher>(&self, range: impl RangeBounds<usize>, state: &mut H) { index_range_bound!(self, range).hash(state) }

    #[inline]
    fn byte_len(&self) -> usize { self.len() }
}
impl Spannable for String {
    type Slice<'s2> = String where Self: 's2;

    #[inline]
    fn is_same(&self, other: &Self) -> bool { self == other }

    #[inline]
    #[track_caller]
    fn slice<'s2>(&'s2 self, range: impl RangeBounds<usize>) -> Self::Slice<'s2> { index_range_bound!(self, range).to_string() }

    #[inline]
    #[track_caller]
    fn slice_eq(&self, range: impl RangeBounds<usize>, other: &Self, other_range: impl RangeBounds<usize>) -> bool {
        index_range_bound!(self, range) == index_range_bound!(other, other_range)
    }

    #[inline]
    #[track_caller]
    fn slice_hash<H: Hasher>(&self, range: impl RangeBounds<usize>, state: &mut H) { index_range_bound!(self, range).hash(state) }

    #[inline]
    fn byte_len(&self) -> usize { self.len() }
}



/// Extends a [`Spannable`] with the power to be match-prefix'ed by a byte-like object.
pub trait MatchBytes {
    /// Returns the position up to which the given bytes are a match.
    ///
    /// # Arguments
    /// - `bytes`: The byte pattern to match.
    ///
    /// # Returns
    /// A `usize` that indicates the first "wrong" character. Some notes:
    /// - If result is the length of `self`, the entire source was matched (but `bytes` may be longer!)
    /// - If result is 0, then none of `self` could be matched (i.e., first characters are wrong ...or `self` is empty!)
    fn match_bytes(&self, bytes: &[u8]) -> usize;
}

// Default binary impls for [`MatchBytes`]
impl<'b> MatchBytes for &'b [u8] {
    fn match_bytes(&self, bytes: &[u8]) -> usize {
        // Match the prefixes
        let mut i: usize = 0;
        for (b1, b2) in self.iter().zip(bytes.iter()) {
            if b1 != b2 {
                return i;
            }
            i += 1;
        }
        i
    }
}
impl<'b> MatchBytes for Cow<'b, [u8]> {
    #[inline]
    fn match_bytes(&self, bytes: &[u8]) -> usize { <&[u8]>::match_bytes(&&**self, bytes) }
}
impl MatchBytes for Vec<u8> {
    #[inline]
    fn match_bytes(&self, bytes: &[u8]) -> usize { <&[u8]>::match_bytes(&self.as_slice(), bytes) }
}

// Default string impls for [`Spannable`]
impl<'s> MatchBytes for &'s str {
    fn match_bytes(&self, bytes: &[u8]) -> usize {
        // Match the prefixes
        let mut i: usize = 0;
        for (b1, b2) in self.as_bytes().iter().zip(bytes.iter()) {
            if b1 != b2 {
                return i;
            }
            i += 1;
        }
        i
    }
}
impl<'s> MatchBytes for Cow<'s, str> {
    #[inline]
    fn match_bytes(&self, bytes: &[u8]) -> usize { <&str>::match_bytes(&&**self, bytes) }
}
impl MatchBytes for String {
    #[inline]
    fn match_bytes(&self, bytes: &[u8]) -> usize { <&str>::match_bytes(&self.as_str(), bytes) }
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
    /// A start position in this input as it is on the disk (e.g., bytes). Inclusive.
    start:  usize,
    /// An end position in this input as it is on the disk (e.g., bytes). Exclusive.
    end:    usize,
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
    pub fn empty(from: F, source: S) -> Self { Self { from, source, start: 0, end: 0 } }

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
}
impl<F, S: Spannable> Span<F, S> {
    /// Constructor for the `Span` that initializes it to Span the entire given range.
    ///
    /// # Arguments
    /// - `source`: The input source (text) to wrap.
    ///
    /// # Returns
    /// A new `Span` that spans the entire given `source`.
    #[inline]
    pub fn new(from: F, source: S) -> Self {
        let len: usize = source.byte_len();
        Self { from, source, start: 0, end: len }
    }

    /// Provides access to the internal `source`-string, but only the spanned area.
    ///
    /// # Returns
    /// A reference to the internal `source`-string.
    #[inline]
    pub fn value<'s>(&'s self) -> S::Slice<'s> { self.source.slice(self.start..self.end) }

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
        self.start = std::cmp::min(self.start, other.start);
        self.end = std::cmp::max(self.end, other.end);
        true
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
            let start: usize = std::cmp::min(self.start, other.start);
            let end: usize = std::cmp::max(self.end, other.end);
            Some(Self { from: self.from.clone(), source: self.source.clone(), start, end })
        } else {
            None
        }
    }

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
        let (start, end): (usize, usize) = match (range.start_bound(), range.end_bound()) {
            (Bound::Included(s), Bound::Included(e)) => (*s, *e + 1),
            (Bound::Included(s), Bound::Excluded(e)) => (*s, *e),
            (Bound::Included(s), Bound::Unbounded) => (*s, self.source.byte_len()),
            (Bound::Excluded(s), Bound::Included(e)) => (*s + 1, *e + 1),
            (Bound::Excluded(s), Bound::Excluded(e)) => (*s + 1, *e),
            (Bound::Excluded(s), Bound::Unbounded) => (*s + 1, self.source.byte_len()),
            (Bound::Unbounded, Bound::Included(e)) => (0, *e + 1),
            (Bound::Unbounded, Bound::Excluded(e)) => (0, *e),
            (Bound::Unbounded, Bound::Unbounded) => (0, self.source.byte_len()),
        };

        // Build self
        Self { from: self.from.clone(), source: self.source.clone(), start, end }
    }
}
impl<F: Clone, S: Clone + Spannable + MatchBytes> Span<F, S> {}
impl<F, S: Spannable> Eq for Span<F, S> {}
impl<F, S: Spannable> Hash for Span<F, S> {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) { self.source.slice_hash(self.start..self.end, state) }
}
impl<F, S: Spannable> PartialEq for Span<F, S> {
    #[inline]
    fn eq(&self, other: &Self) -> bool { self.source.slice_eq(self.start..self.end, &other.source, other.start..other.end) }
}
