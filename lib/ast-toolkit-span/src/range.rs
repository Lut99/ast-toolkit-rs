//  RANGE.rs
//    by Lut99
//
//  Created:
//    06 May 2024, 16:17:54
//  Last edited:
//    27 May 2024, 11:57:37
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements a low-level range abstraction for Spans.
//

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
pub(crate) use index_range_bound;

/// Converts from a [`SpanRange`] to two indices.
macro_rules! resolve_range {
    ($range:expr, $len:expr) => {{
        let len: usize = $len;
        match $range {
            SpanRange::Closed(s, e) => {
                if s >= len || e > len {
                    panic!("Internal SpanRange::Closed({s}, {e}) is out-of-bounds for source string of {len} bytes");
                }
                if s < e { Some((s, e)) } else { None }
            },
            SpanRange::ClosedOpen(s) => {
                if len > 0 && s >= len {
                    panic!("Internal SpanRange::ClosedOpen({s}) is out-of-bounds for source string of {len} bytes");
                }
                if s < len { Some((s, len)) } else { None }
            },
            SpanRange::OpenClosed(e) => {
                if len > 0 && e > len {
                    panic!("Internal SpanRange::OpenClosed({e}) is out-of-bounds for source string of {len} bytes");
                }
                if e > 0 { Some((0, e)) } else { None }
            },
            SpanRange::Open => {
                if len > 0 {
                    Some((0, len))
                } else {
                    None
                }
            },
            SpanRange::Empty => None,
        }
    }};
}
pub(crate) use resolve_range;





/***** LIBRARY *****/
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
impl RangeBounds<usize> for SpanRange {
    #[inline]
    fn start_bound(&self) -> Bound<&usize> {
        match self {
            Self::Closed(s, _) => Bound::Included(s),
            Self::ClosedOpen(s) => Bound::Included(s),
            Self::OpenClosed(_) => Bound::Unbounded,
            Self::Open => Bound::Unbounded,
            Self::Empty => Bound::Excluded(&0),
        }
    }

    #[inline]
    fn end_bound(&self) -> std::ops::Bound<&usize> {
        match self {
            Self::Closed(_, e) => Bound::Excluded(e),
            Self::ClosedOpen(_) => Bound::Unbounded,
            Self::OpenClosed(e) => Bound::Excluded(e),
            Self::Open => Bound::Unbounded,
            Self::Empty => Bound::Excluded(&0),
        }
    }
}
