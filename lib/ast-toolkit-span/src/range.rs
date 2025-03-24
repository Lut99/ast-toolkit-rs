//  RANGE 2.rs
//    by Lut99
//
//  Created:
//    14 Mar 2025, 16:58:17
//  Last edited:
//    24 Mar 2025, 11:42:13
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements [`Range`], an abstraction of a slice of an array.
//

use std::fmt::{Debug, Display, Formatter, Result as FResult};


/***** HELPERS *****/
/// Actual implementation of the [`Range`].
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum RangeInner {
    // Fully bounded
    /// Captures a specific slice of the source text.
    ///
    /// Given as `[inclusive, exclusive)`.
    Bounded(usize, usize),

    // Partially bounded
    /// Captures everything of a source text from a specific point onwards.
    ///
    /// Given as `[inclusive, ...`.
    Onwards(usize),
    /// Captures everything of a source text up to a specific point.
    ///
    /// Given as `..., exclusive)`.
    Until(usize),

    // Special cases
    /// Captures the WHOLE source (i.e., open on both ends).
    Full,
    /// Captures NO source.
    Empty,
}





/***** LIBRARY *****/
/// Implements an abstraction of a slice of an array.
///
/// This differs from [`std::ops::Range`] in the following ways:
/// - It implements [`Copy`].
/// - It can encode special states of the range, e.g. [`Empty`](Range::Empty) or open-ended ranges.
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct Range {
    /// The actual range type, but hidden to avoid manual construction (and preserve properties).
    inner: RangeInner,
}

// Constructors
impl Default for Range {
    #[inline]
    fn default() -> Self { Self { inner: RangeInner::Full } }
}
impl Range {
    /// Constructor for a Range that is bounded on both sides.
    ///
    /// # Arguments
    /// - `start`: The start point of the range in the larger slice.
    /// - `end`: The end point of the range in the larger slice.
    ///
    /// # Returns
    /// A new Range, which is either bounded by `start` and `end` or, when `start > end`, empty.
    #[inline]
    pub const fn bounded(start: usize, end: usize) -> Self {
        Self { inner: if start < end { RangeInner::Bounded(start, end) } else { RangeInner::Empty } }
    }

    /// Constructor for a range that is only bounded on the left side.
    ///
    /// # Arguments
    /// - `start`: The start point of the range in the larger slice.
    ///
    /// # Returns
    /// A new Range, which is onwards from the given `start` point.
    #[inline]
    pub const fn onwards(start: usize) -> Self { Self { inner: RangeInner::Onwards(start) } }

    /// Constructor for a range that is only bounded on the right side.
    ///
    /// Note that, if `end` is 0, this will default to [`Range::empty()`] instead.
    ///
    /// # Arguments
    /// - `end`: The end point of the range in the larger slice.
    ///
    /// # Returns
    /// A new Range, which is until the given `end` point.
    #[inline]
    pub const fn until(end: usize) -> Self { Self { inner: if end > 0 { RangeInner::Until(end) } else { RangeInner::Empty } } }

    /// Constructor for a range that covers the whole parent slice.
    ///
    /// # Returns
    /// A new Range, which spans the full larger slice.
    #[inline]
    pub const fn full() -> Self { Self { inner: RangeInner::Full } }

    /// Constructor for a range that covers none of the parent slice.
    ///
    /// # Returns
    /// A new Range, which spans nothing.
    #[inline]
    pub const fn empty() -> Self { Self { inner: RangeInner::Empty } }
}

// Ops
impl Debug for Range {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        match self.inner {
            RangeInner::Bounded(start, end) => write!(f, "{start}..{end}"),
            RangeInner::Onwards(start) => write!(f, "{start}.."),
            RangeInner::Until(end) => write!(f, "..{end}"),
            RangeInner::Full => write!(f, ".."),
            RangeInner::Empty => write!(f, "!"),
        }
    }
}
impl Display for Range {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult { <Self as Debug>::fmt(self, f) }
}
impl PartialEq<std::ops::Range<usize>> for Range {
    #[inline]
    fn eq(&self, other: &std::ops::Range<usize>) -> bool {
        match self.inner {
            RangeInner::Bounded(start, end) => start == other.start && end == other.end,
            _ => false,
        }
    }
}
impl PartialEq<std::ops::RangeFrom<usize>> for Range {
    #[inline]
    fn eq(&self, other: &std::ops::RangeFrom<usize>) -> bool {
        match self.inner {
            RangeInner::Onwards(start) => start == other.start,
            _ => false,
        }
    }
}
impl PartialEq<std::ops::RangeTo<usize>> for Range {
    #[inline]
    fn eq(&self, other: &std::ops::RangeTo<usize>) -> bool {
        match self.inner {
            RangeInner::Until(end) => end == other.end,
            _ => false,
        }
    }
}
impl PartialEq<std::ops::RangeFull> for Range {
    #[inline]
    fn eq(&self, _other: &std::ops::RangeFull) -> bool {
        match self.inner {
            RangeInner::Full => true,
            _ => false,
        }
    }
}
impl PartialEq<()> for Range {
    #[inline]
    fn eq(&self, _other: &()) -> bool {
        match self.inner {
            RangeInner::Empty => true,
            _ => false,
        }
    }
}

// Range
impl Range {
    /// Returns a new Range which is a slice of this one.
    ///
    /// # Arguments
    /// - `range`: Some other range to slice ourselves with.
    ///
    /// # Returns
    /// A new Range that is the given `range` contextualized within this one.
    #[inline]
    pub fn slice(&self, range: impl Into<Self>) -> Self {
        let range: Self = range.into();
        let res = match (self.inner, range.inner) {
            // Bounded cases
            (RangeInner::Onwards(lstart), RangeInner::Onwards(rstart)) => Range { inner: RangeInner::Onwards(lstart + rstart) },
            (RangeInner::Onwards(lstart), RangeInner::Bounded(rstart, end)) => {
                let start: usize = lstart + rstart;
                let end: usize = lstart + end;
                Range { inner: if start < end { RangeInner::Bounded(start, end) } else { RangeInner::Empty } }
            },
            (RangeInner::Onwards(start), RangeInner::Until(end)) => {
                let end: usize = start + end;
                Range { inner: if start < end { RangeInner::Bounded(start, end) } else { RangeInner::Empty } }
            },
            (RangeInner::Bounded(lstart, end), RangeInner::Onwards(rstart)) => {
                let start: usize = lstart + rstart;
                Range { inner: if start < end { RangeInner::Bounded(start, end) } else { RangeInner::Empty } }
            },
            (RangeInner::Bounded(lstart, lend), RangeInner::Bounded(rstart, rend)) => {
                let start: usize = lstart + rstart;
                let end: usize = std::cmp::min(lend, lstart + rend);
                Range { inner: if start < end { RangeInner::Bounded(start, end) } else { RangeInner::Empty } }
            },
            (RangeInner::Bounded(start, lend), RangeInner::Until(rend)) => {
                let end: usize = std::cmp::min(lend, start + rend);
                Range { inner: if start < end { RangeInner::Bounded(start, end) } else { RangeInner::Empty } }
            },
            (RangeInner::Until(end), RangeInner::Onwards(start)) => {
                Range { inner: if start < end { RangeInner::Bounded(start, end) } else { RangeInner::Empty } }
            },
            (RangeInner::Until(lend), RangeInner::Bounded(start, rend)) => {
                let end: usize = std::cmp::min(lend, rend);
                Range { inner: if start < end { RangeInner::Bounded(start, end) } else { RangeInner::Empty } }
            },
            (RangeInner::Until(lend), RangeInner::Until(rend)) => Range { inner: RangeInner::Until(std::cmp::min(lend, rend)) },

            // The full cases
            (RangeInner::Full, inner) | (inner, RangeInner::Full) => Self { inner },
            // Any empty resolves to empty
            (RangeInner::Empty, _) | (_, RangeInner::Empty) => Self { inner: RangeInner::Empty },
        };
        res
    }

    /// Returns a Range which represents the part of this Range until another Range.
    ///
    /// This sounds very abstract. Imagine that there are two ranges, overlapping like so:
    /// ```plain
    /// < 1 ............ >
    ///             < 2 .......... >
    /// ^^^^^^^^^^^^
    /// ```
    ///
    /// This function will simply return the underlined part.
    ///
    /// However, it returns [`None`] in the following cases:
    /// ```plain
    /// < 1 ............ >
    ///                          < 2 .......... >
    /// ```
    /// ```plain
    ///                        < 1 ............ >
    /// < 2 .......... >
    /// ```
    /// ```plain
    ///          < 1 ............ >
    /// < 2 .......... >
    /// ```
    ///
    /// # Arguments
    /// - `other`: Some other Range that represents the second, overlapping range.
    ///
    /// # Returns
    /// A new Range that represents the relative complement of `self` with respect to `other`.
    ///
    /// If the two ranges are not overlapping at all, or the start of 2 precedes 1, [`None`] is returned.
    #[inline]
    pub fn relative_complement(&self, other: &Range) -> Option<Range> {
        // Let's just match all cases for clarity's sake.
        match (self.inner, other.inner) {
            (RangeInner::Onwards(lstart), RangeInner::Onwards(rstart) | RangeInner::Bounded(rstart, _)) => {
                if lstart <= rstart {
                    Some(Range::bounded(lstart, rstart))
                } else {
                    // `self` is after `other`
                    None
                }
            },
            (RangeInner::Onwards(start), RangeInner::Until(_) | RangeInner::Full) => {
                // Return empty is the starts overlap (starts at 0). Otherwise, `self` is always
                // after `other`
                if start == 0 { Some(Range::empty()) } else { None }
            },

            (RangeInner::Bounded(lstart, end), RangeInner::Onwards(rstart) | RangeInner::Bounded(rstart, _)) => {
                if lstart <= rstart && rstart < end {
                    Some(Range::bounded(lstart, rstart))
                } else {
                    // `self` is after `other`, or `other` is beyond `self`'s end
                    None
                }
            },
            (RangeInner::Bounded(start, _), RangeInner::Until(_) | RangeInner::Full) => {
                // Return empty is the starts overlap (starts at 0). Otherwise, `self` is always
                // after `other`
                if start == 0 { Some(Range::empty()) } else { None }
            },

            (RangeInner::Until(end), RangeInner::Onwards(rstart) | RangeInner::Bounded(rstart, _)) => {
                if rstart < end {
                    Some(Range::until(rstart))
                } else {
                    // `other` is beyond `self`'s end
                    None
                }
            },
            (RangeInner::Until(_), RangeInner::Until(_) | RangeInner::Full) => {
                // Always empty, because their starts collide
                Some(Range::empty())
            },

            (RangeInner::Full, RangeInner::Onwards(rstart) | RangeInner::Bounded(rstart, _)) => {
                // Cannot go out of `self`'s end; always something!
                Some(Range::until(rstart))
            },
            (RangeInner::Full, RangeInner::Until(_) | RangeInner::Full) => {
                // They have the same start; always empty
                Some(Range::empty())
            },

            // Empty catch-alls
            (RangeInner::Empty, _) | (_, RangeInner::Empty) => None,
        }
    }

    /// Joins two Ranges.
    ///
    /// The result will encompass both spans; e.g.,
    /// ```plain
    /// < 1 ............ >
    ///                          < 2 .......... >
    /// ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    /// ```
    /// (The underlined is what is returned)
    ///
    /// # Arguments
    /// - `other`: Some other Range to join this one with.
    ///
    /// # Returns
    /// A new Range that is the union of the given two ranges.
    #[inline]
    pub fn join(&self, other: &Range) -> Range {
        match (self.inner, other.inner) {
            (RangeInner::Onwards(lstart), RangeInner::Onwards(rstart)) => Range::onwards(std::cmp::min(lstart, rstart)),
            (RangeInner::Onwards(lstart), RangeInner::Bounded(rstart, _)) => Range::onwards(std::cmp::min(lstart, rstart)),
            (RangeInner::Onwards(_), RangeInner::Until(_)) => Range::full(),
            (RangeInner::Bounded(lstart, _), RangeInner::Onwards(rstart)) => Range::onwards(std::cmp::min(lstart, rstart)),
            (RangeInner::Bounded(lstart, lend), RangeInner::Bounded(rstart, rend)) => {
                Range::bounded(std::cmp::min(lstart, rstart), std::cmp::max(lend, rend))
            },
            (RangeInner::Bounded(_, lend), RangeInner::Until(rend)) => Range::until(std::cmp::max(lend, rend)),
            (RangeInner::Until(_), RangeInner::Onwards(_)) => Range::full(),
            (RangeInner::Until(lend), RangeInner::Bounded(_, rend)) => Range::until(std::cmp::max(lend, rend)),
            (RangeInner::Until(lend), RangeInner::Until(rend)) => Range::until(std::cmp::max(lend, rend)),

            // Full- and empty catch-alls
            (RangeInner::Full, _) | (_, RangeInner::Full) => Range::full(),
            (RangeInner::Empty, _) => *other,
            (_, RangeInner::Empty) => *self,
        }
    }



    /// Gets the lefthand-side of the range.
    ///
    /// # Returns
    /// The start index (inclusive), or [`None`] if the range is unbounded on this side (or empty).
    #[inline]
    pub fn start(&self) -> Option<usize> {
        match self.inner {
            RangeInner::Bounded(start, _) | RangeInner::Onwards(start) => Some(start),
            RangeInner::Until(_) | RangeInner::Full | RangeInner::Empty => None,
        }
    }
    /// Gets the lefthand-side of the range, resolved to a slice starting at the given index.
    ///
    /// # Arguments
    /// - `len`: The total size of the parent slice.
    ///
    /// # Returns
    /// The start index (inclusive). Note that this is capped to `len` (i.e., it will never be
    /// larger than that).
    ///
    /// Will still be [`None`] if this range is empty OR `len` is 0 (but only then).
    #[inline]
    pub fn start_resolved(&self, len: usize) -> Option<usize> {
        match self.inner {
            RangeInner::Bounded(start, _) | RangeInner::Onwards(start) => {
                if len > 0 {
                    Some(std::cmp::min(start, len))
                } else {
                    None
                }
            },
            RangeInner::Until(_) | RangeInner::Full => {
                if len > 0 {
                    Some(0)
                } else {
                    None
                }
            },
            RangeInner::Empty => None,
        }
    }

    /// Gets the righthand-side of the range.
    ///
    /// # Returns
    /// The end index (exclusive), or [`None`] if the range is unbounded on this side (or empty).
    #[inline]
    pub fn end(&self) -> Option<usize> {
        match self.inner {
            RangeInner::Bounded(_, end) | RangeInner::Until(end) => Some(end),
            RangeInner::Onwards(_) | RangeInner::Full | RangeInner::Empty => None,
        }
    }
    /// Gets the righthand-side of the range, resolved to a slice with the given endpoint.
    ///
    /// # Arguments
    /// - `len`: The total size of the parent slice.
    ///
    /// # Returns
    /// The end index (exclusive). Note that this is capped to `len` (i.e., it will never be
    /// larger than that).
    ///
    /// Will still be [`None`] if this range is empty OR `len` is 0 (but only then).
    #[inline]
    pub fn end_resolved(&self, len: usize) -> Option<usize> {
        match self.inner {
            RangeInner::Bounded(_, end) | RangeInner::Until(end) => {
                if len > 0 {
                    Some(std::cmp::min(end, len))
                } else {
                    None
                }
            },
            RangeInner::Onwards(_) | RangeInner::Full => {
                if len > 0 {
                    Some(len)
                } else {
                    None
                }
            },
            RangeInner::Empty => None,
        }
    }

    /// Returns the length of this slice.
    ///
    /// The length is always compared to the total length of the given parent slice.
    ///
    /// # Arguments
    /// - `len`: The total length of the parent slice.
    ///
    /// # Returns
    /// A [`usize`] describing how many elements are encompassed in this Range.
    #[inline]
    pub fn resolved_len(&self, len: usize) -> usize {
        match self.inner {
            RangeInner::Bounded(start, end) => {
                let start: usize = if len > 0 { std::cmp::min(start, len - 1) } else { 0 };
                let end: usize = std::cmp::min(end, len);
                if start <= end { end - start } else { 0 }
            },
            RangeInner::Onwards(start) => len.saturating_sub(start),
            RangeInner::Until(end) => std::cmp::min(end, len),
            RangeInner::Full => len,
            RangeInner::Empty => 0,
        }
    }
}

// Conversion
impl From<std::ops::Range<usize>> for Range {
    #[inline]
    fn from(value: std::ops::Range<usize>) -> Self { Self::bounded(value.start, value.end) }
}
impl From<std::ops::RangeFrom<usize>> for Range {
    #[inline]
    fn from(value: std::ops::RangeFrom<usize>) -> Self { Self { inner: RangeInner::Onwards(value.start) } }
}
impl From<std::ops::RangeTo<usize>> for Range {
    #[inline]
    fn from(value: std::ops::RangeTo<usize>) -> Self { Self { inner: RangeInner::Until(value.end) } }
}
impl From<std::ops::RangeFull> for Range {
    #[inline]
    fn from(_: std::ops::RangeFull) -> Self { Self { inner: RangeInner::Full } }
}
impl From<()> for Range {
    #[inline]
    fn from(_: ()) -> Self { Self { inner: RangeInner::Empty } }
}





/***** TESTS *****/
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_slice() {
        // Some testcases; extend when more are known!
        assert_eq!(Range::from(0..10).slice(0..5), 0..5);
        assert_eq!(Range::from(..10).slice(0..5), 0..5);
        assert_eq!(Range::from(..10).slice(..5), ..5);
        assert_eq!(Range::from(5..10).slice(..5), 5..10);
        assert_eq!(Range::from(10..5), ());
        assert_eq!(Range::from(5..).slice(..5), 5..10);
        assert_eq!(Range::from(5..).slice(..10), 5..15);
        assert_eq!(Range::from(..).slice(5..10), 5..10);
        assert_eq!(Range::from(()).slice(1..10), ());

        // Remember, we're slicing _in_ the left slice
        assert_eq!(Range::from(1..).slice(1..), 2..);
        assert_eq!(Range::from(1..).slice(1..3), 2..4);
        assert_eq!(Range::from(1..).slice(1..1), ());
        assert_eq!(Range::from(1..3).slice(1..), 2..3);
        assert_eq!(Range::from(1..4).slice(1..2), 2..3);
        assert_eq!(Range::from(1..4).slice(1..7), 2..4);
        assert_eq!(Range::from(1..8).slice(1..2), 2..3);
        assert_eq!(Range::from(1..4).slice(..2), 1..3);

        assert_eq!(Range::from(2..).slice(..1), 2..3);
    }
}
