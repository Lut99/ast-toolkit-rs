//  SPAN.rs
//    by Lut99
//
//  Created:
//    15 Dec 2023, 19:05:00
//  Last edited:
//    02 May 2024, 12:04:55
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
        match $range {
            SpanRange::Closed(s, e) => &slice[s..e],
            SpanRange::ClosedOpen(s) => &slice[s..],
            SpanRange::OpenClosed(e) => &slice[..e],
            SpanRange::Open => &slice[..],
            SpanRange::Empty => &slice[0..0],
        }
    }};
}





/***** AUXILLARY *****/
/// Defines a range in some [`Span`]ned area.
///
/// This range can either be open- or closed on either end. 'Open' means 'everything on that side', i.e., if the left is Open, it refers to the start of the source; and if the right is Open, it refers to the end.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum SpanRange {
    /// Closed on both ends.
    ///
    /// Given as: inclusive start, exclusive end.
    Closed(usize, usize),
    /// Closed on the left, open on the right.
    ///
    /// Left is inclusive.
    ClosedOpen(usize),
    /// Open on the left, closed on the right.
    ///
    /// Right is exclusive.
    OpenClosed(usize),
    /// Fully open on both ends.
    ///
    /// Spans the entire source.
    Open,
    /// Special case of a range that doesn't span anything.
    Empty,
}
impl SpanRange {
    /// Creates a new SpanRange that is self further spanned by the given range.
    ///
    /// This means that the given range is assumed to be relative to this range, e.g.,
    /// ```plain
    /// (0..10).span(2..4) => 2..4
    /// (5..10).span(2..4) => 7..9
    /// (5..10).span(2..) => 7..10
    /// ```
    ///
    /// # Arguments
    /// - `span`: The other SpanRange that spans ourselves.
    ///
    /// # Returns
    /// A new SpanRange that is `self` but spanned by `span`.
    #[inline]
    pub fn span(&self, span: &Self) -> Self {
        // Get the simplified range of self
        let lhs: (Option<usize>, Option<usize>) = match self {
            Self::Closed(s, e) => (Some(*s), Some(*e)),
            Self::ClosedOpen(s) => (Some(*s), None),
            Self::OpenClosed(e) => (None, Some(*e)),
            Self::Open => (None, None),
            // If we're empty, not a lot to span
            Self::Empty => return Self::Empty,
        };

        // Get the simplified range of other
        let rhs: (Option<usize>, Option<usize>) = match span {
            Self::Closed(s, e) => (Some(*s), Some(*e)),
            Self::ClosedOpen(s) => (Some(*s), None),
            Self::OpenClosed(e) => (None, Some(*e)),
            Self::Open => (None, None),
            // If the other is empty, the span is quite empty too
            Self::Empty => return Self::Empty,
        };

        // Attempt to combine the start & end separately
        let start: Option<usize> = match (lhs.0, rhs.0) {
            (Some(s1), Some(s2)) => Some(s1 + s2),
            (Some(s1), None) => Some(s1),
            (None, Some(s2)) => Some(s2),
            (None, None) => None,
        };
        let end: Option<usize> = match (lhs.0, lhs.1, rhs.1) {
            (Some(s), Some(_), Some(e2)) => Some(s + e2),
            (Some(_), Some(e1), None) => Some(e1),
            (Some(s), None, Some(e2)) => Some(s + e2),
            (Some(_), None, None) => None,
            (None, Some(_), Some(e2)) => Some(e2),
            (None, Some(e1), None) => Some(e1),
            (None, None, Some(e2)) => Some(e2),
            (None, None, None) => None,
        };

        // Now create the self
        match (start, end) {
            (Some(s), Some(e)) => {
                // Double-check the range isn't empty
                if s < e {
                    // Double-check that both s and e are before the old one
                    if let Some(oe) = lhs.1 {
                        if s < oe { if e <= oe { Self::Closed(s, e) } else { Self::Closed(s, oe) } } else { Self::Empty }
                    } else {
                        Self::Closed(s, e)
                    }
                } else {
                    Self::Empty
                }
            },
            (Some(s), None) => {
                if let Some(oe) = lhs.1 {
                    if s < oe { Self::ClosedOpen(s) } else { Self::Empty }
                } else {
                    Self::ClosedOpen(s)
                }
            },
            (None, Some(e)) => {
                if let Some(oe) = lhs.1 {
                    if e <= oe { Self::OpenClosed(e) } else { Self::OpenClosed(oe) }
                } else {
                    Self::OpenClosed(e)
                }
            },
            (None, None) => Self::Open,
        }
    }

    /// Creates a new SpanRange that is the merger of both self and the given one.
    ///
    /// # Arguments
    /// - `other`: The other SpanRange to join with this one.
    ///
    /// # Returns
    /// A new SpanRange that covers both ranges and everything in between, e.g.,
    /// ```plain
    /// self           other
    /// <------>       <------------->
    ///
    /// result
    /// <---------------------------->
    /// ```
    #[inline]
    pub fn join(&self, other: &Self) -> Self {
        let mut res: Self = *self;
        res.join_mut(other);
        res
    }

    /// Extends this SpanRange to also cover another.
    ///
    /// To illustrate:
    /// ```plain
    /// self           other
    /// <------>       <------------->
    ///
    /// self.join_mut(other)
    /// <---------------------------->
    /// ```
    ///
    /// # Arguments
    /// - `other`: The other SpanRange to join with this one.
    #[inline]
    pub fn join_mut(&mut self, other: &Self) {
        // Get the simplified range of self
        let lhs: (Option<usize>, Option<usize>) = match self {
            Self::Closed(s, e) => (Some(*s), Some(*e)),
            Self::ClosedOpen(s) => (Some(*s), None),
            Self::OpenClosed(e) => (None, Some(*e)),
            Self::Open => (None, None),
            // If we're empty, the other determines everything
            Self::Empty => {
                *self = *other;
                return;
            },
        };

        // Get the simplified range of other
        let rhs: (Option<usize>, Option<usize>) = match other {
            Self::Closed(s, e) => (Some(*s), Some(*e)),
            Self::ClosedOpen(s) => (Some(*s), None),
            Self::OpenClosed(e) => (None, Some(*e)),
            Self::Open => (None, None),
            // If the other is empty, nothing needs changing
            Self::Empty => return,
        };

        // Now compare those ones to make a proper start & end
        let start: Option<usize> = match (lhs, rhs) {
            // If they're both closed, we need the leftmost;
            ((Some(s1), _), (Some(s2), _)) => Some(std::cmp::min(s1, s2)),
            // Else, one of the lefties is open, so then so are we.
            _ => None,
        };
        let end: Option<usize> = match (lhs, rhs) {
            // If they're both closed, we need the rightmost;
            ((_, Some(e1)), (_, Some(e2))) => Some(std::cmp::max(e1, e2)),
            // Else, one of the righties is open, so then so are we.
            _ => None,
        };

        // OK, finally build a new self
        *self = match (start, end) {
            (Some(s), Some(e)) => Self::Closed(s, e),
            (Some(s), None) => Self::ClosedOpen(s),
            (None, Some(e)) => Self::OpenClosed(e),
            (None, None) => Self::Open,
        };
    }

    /// Applies this SpanRange to a given slice.
    ///
    /// This slice can be anything, as long as it's a slice.
    ///
    /// # Arguments
    /// - `slice`: The slice (of type [`&[T]`]) to apply this range to.
    ///
    /// # Returns
    /// A new slice of type `[T]` that is the sliced counterpart.
    #[inline]
    pub fn apply_to<'s, T>(&self, slice: &'s [T]) -> &'s [T] { index_range_bound!(slice, *self) }

    /// Applies this SpanRange to a given [`str`].
    ///
    /// This is the overload of [`Self::apply_to`](SpanRange::apply_to) but then to [`str`]s.
    ///
    /// # Arguments
    /// - `string`: The string (of type [`&str`]) to apply this range to.
    ///
    /// # Returns
    /// A new slice of type `str` that is the sliced counterpart.
    #[inline]
    pub fn apply_to_str<'s>(&self, string: &'s str) -> &'s str { index_range_bound!(string, *self) }
}



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
    fn slice<'s>(&'s self, range: SpanRange) -> Self::Slice<'s>;
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
    fn slice_eq(&self, range: SpanRange, other: &Self, other_range: SpanRange) -> bool;
    /// Computes a hash for the given range of this Spannable.
    ///
    /// # Arguments
    /// - `range`: The range that slices this Spannable. Can be anything implementing [`RangeBounds`].
    /// - `state`: Some [`Hasher`] that does the tough work.
    ///
    /// # Panics
    /// This function panics if out-of-bounds.
    fn slice_hash<H: Hasher>(&self, range: SpanRange, state: &mut H);

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
    fn slice<'s2>(&'s2 self, range: SpanRange) -> Self::Slice<'s2> { index_range_bound!(self, range) }

    #[inline]
    #[track_caller]
    fn slice_eq(&self, range: SpanRange, other: &Self, other_range: SpanRange) -> bool {
        index_range_bound!(self, range) == index_range_bound!(other, other_range)
    }

    #[inline]
    #[track_caller]
    fn slice_hash<H: Hasher>(&self, range: SpanRange, state: &mut H) { index_range_bound!(self, range).hash(state) }

    #[inline]
    fn byte_len(&self) -> usize { self.len() }
}
impl<'b, const LEN: usize> Spannable for &'b [u8; LEN] {
    type Slice<'s> = &'s [u8] where Self: 's;

    #[inline]
    fn is_same(&self, other: &Self) -> bool {
        let ptr_eq: bool = std::ptr::eq(self, other);
        #[cfg(debug_assertions)]
        {
            if !ptr_eq && self == other {
                eprintln!(
                    "DEBUG ASSERTION WARNING: Two byte arrays do not share the same pointer but are semantically equal. The &[u8; \
                     LEN]-implementation for Spannable assumes comparing them by pointer equality is sufficient."
                );
            }
        }
        ptr_eq
    }

    #[inline]
    #[track_caller]
    fn slice<'s2>(&'s2 self, range: SpanRange) -> Self::Slice<'s2> { index_range_bound!(self, range) }

    #[inline]
    #[track_caller]
    fn slice_eq(&self, range: SpanRange, other: &Self, other_range: SpanRange) -> bool {
        index_range_bound!(self, range) == index_range_bound!(other, other_range)
    }

    #[inline]
    #[track_caller]
    fn slice_hash<H: Hasher>(&self, range: SpanRange, state: &mut H) { index_range_bound!(self, range).hash(state) }

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
    fn slice<'s2>(&'s2 self, range: SpanRange) -> Self::Slice<'s2> { Cow::Borrowed(index_range_bound!(self, range)) }

    #[inline]
    #[track_caller]
    fn slice_eq(&self, range: SpanRange, other: &Self, other_range: SpanRange) -> bool {
        index_range_bound!(self, range) == index_range_bound!(other, other_range)
    }

    #[inline]
    #[track_caller]
    fn slice_hash<H: Hasher>(&self, range: SpanRange, state: &mut H) { index_range_bound!(self, range).hash(state) }

    #[inline]
    fn byte_len(&self) -> usize { self.len() }
}
impl Spannable for Vec<u8> {
    type Slice<'s> = Vec<u8> where Self: 's;

    #[inline]
    fn is_same(&self, other: &Self) -> bool { self == other }

    #[inline]
    #[track_caller]
    fn slice<'s2>(&'s2 self, range: SpanRange) -> Self::Slice<'s2> { index_range_bound!(self, range).to_vec() }

    #[inline]
    #[track_caller]
    fn slice_eq(&self, range: SpanRange, other: &Self, other_range: SpanRange) -> bool {
        index_range_bound!(self, range) == index_range_bound!(other, other_range)
    }

    #[inline]
    #[track_caller]
    fn slice_hash<H: Hasher>(&self, range: SpanRange, state: &mut H) { index_range_bound!(self, range).hash(state) }

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
    fn slice<'s2>(&'s2 self, range: SpanRange) -> Self::Slice<'s2> { index_range_bound!(self, range) }

    #[inline]
    #[track_caller]
    fn slice_eq(&self, range: SpanRange, other: &Self, other_range: SpanRange) -> bool {
        index_range_bound!(self, range) == index_range_bound!(other, other_range)
    }

    #[inline]
    #[track_caller]
    fn slice_hash<H: Hasher>(&self, range: SpanRange, state: &mut H) { index_range_bound!(self, range).hash(state) }

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
    fn slice<'s2>(&'s2 self, range: SpanRange) -> Self::Slice<'s2> { Cow::Borrowed(index_range_bound!(self, range)) }

    #[inline]
    #[track_caller]
    fn slice_eq(&self, range: SpanRange, other: &Self, other_range: SpanRange) -> bool {
        index_range_bound!(self, range) == index_range_bound!(other, other_range)
    }

    #[inline]
    #[track_caller]
    fn slice_hash<H: Hasher>(&self, range: SpanRange, state: &mut H) { index_range_bound!(self, range).hash(state) }

    #[inline]
    fn byte_len(&self) -> usize { self.len() }
}
impl Spannable for String {
    type Slice<'s2> = String where Self: 's2;

    #[inline]
    fn is_same(&self, other: &Self) -> bool { self == other }

    #[inline]
    #[track_caller]
    fn slice<'s2>(&'s2 self, range: SpanRange) -> Self::Slice<'s2> { index_range_bound!(self, range).to_string() }

    #[inline]
    #[track_caller]
    fn slice_eq(&self, range: SpanRange, other: &Self, other_range: SpanRange) -> bool {
        index_range_bound!(self, range) == index_range_bound!(other, other_range)
    }

    #[inline]
    #[track_caller]
    fn slice_hash<H: Hasher>(&self, range: SpanRange, state: &mut H) { index_range_bound!(self, range).hash(state) }

    #[inline]
    fn byte_len(&self) -> usize { self.len() }
}



/// A helper trait formalizing things that return [`Span`]s.
pub trait Spanning<F, S> {
    /// Returns some [`Span`] representing self's relation to the source text it was parsed from.
    ///
    /// # Returns
    /// A new [`Span`] that represents the spanned area.
    fn span(&self) -> Span<F, S>;
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

impl<F, S: Spannable> Eq for Span<F, S> {}
impl<F, S: Spannable> Hash for Span<F, S> {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) { self.source.slice_hash(self.range, state) }
}
impl<F, S: Spannable> PartialEq for Span<F, S> {
    #[inline]
    fn eq(&self, other: &Self) -> bool { self.source.slice_eq(self.range, &other.source, other.range) }
}

impl<F: Clone, S: Clone> Spanning<F, S> for Span<F, S> {
    #[inline]
    fn span(&self) -> Span<F, S> { self.clone() }
}
