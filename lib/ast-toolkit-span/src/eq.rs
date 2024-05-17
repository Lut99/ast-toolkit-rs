//  EQ.rs
//    by Lut99
//
//  Created:
//    06 May 2024, 16:23:07
//  Last edited:
//    17 May 2024, 16:27:58
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements an [`Eq`](std::cmp::Eq)-like implementation over [`Spannable`]s that
//!   does not require one to name the [`Spannable::Slice`], avoiding some
//!   gnarly lifetime problems.
//

use std::borrow::Cow;
use std::rc::Rc;
use std::sync::Arc;

use crate::range::{index_range_bound, SpanRange};
use crate::Spannable;


/***** LIBRARY *****/
/// An extension for [`Spannable`]s that make them [`Eq`](std::cmp::Eq)able.
pub trait SpannableEq: Spannable {
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
}

// Default binary impls for [`Spannable`]
impl<'b> SpannableEq for &'b [u8] {
    #[inline]
    #[track_caller]
    fn slice_eq(&self, range: SpanRange, other: &Self, other_range: SpanRange) -> bool {
        index_range_bound!(self, range) == index_range_bound!(other, other_range)
    }
}
impl<'b, const LEN: usize> SpannableEq for &'b [u8; LEN] {
    #[inline]
    #[track_caller]
    fn slice_eq(&self, range: SpanRange, other: &Self, other_range: SpanRange) -> bool {
        <&[u8] as SpannableEq>::slice_eq(&self.as_slice(), range, &other.as_slice(), other_range)
    }
}
impl<'b> SpannableEq for Cow<'b, [u8]> {
    #[inline]
    #[track_caller]
    fn slice_eq(&self, range: SpanRange, other: &Self, other_range: SpanRange) -> bool {
        <&[u8] as SpannableEq>::slice_eq(&self.as_ref(), range, &other.as_ref(), other_range)
    }
}
impl SpannableEq for Vec<u8> {
    #[inline]
    #[track_caller]
    fn slice_eq(&self, range: SpanRange, other: &Self, other_range: SpanRange) -> bool {
        <&[u8] as SpannableEq>::slice_eq(&self.as_slice(), range, &other.as_slice(), other_range)
    }
}
impl SpannableEq for Rc<[u8]> {
    #[inline]
    fn slice_eq(&self, range: SpanRange, other: &Self, other_range: SpanRange) -> bool {
        <&[u8] as SpannableEq>::slice_eq(&self.as_ref(), range, &other.as_ref(), other_range)
    }
}
impl SpannableEq for Arc<[u8]> {
    #[inline]
    fn slice_eq(&self, range: SpanRange, other: &Self, other_range: SpanRange) -> bool {
        <&[u8] as SpannableEq>::slice_eq(&self.as_ref(), range, &other.as_ref(), other_range)
    }
}

// Default string impls for [`Spannable`]
impl<'s> SpannableEq for &'s str {
    #[inline]
    #[track_caller]
    fn slice_eq(&self, range: SpanRange, other: &Self, other_range: SpanRange) -> bool {
        index_range_bound!(self, range) == index_range_bound!(other, other_range)
    }
}
impl<'s> SpannableEq for Cow<'s, str> {
    #[inline]
    #[track_caller]
    fn slice_eq(&self, range: SpanRange, other: &Self, other_range: SpanRange) -> bool {
        <&str as SpannableEq>::slice_eq(&self.as_ref(), range, &other.as_ref(), other_range)
    }
}
impl SpannableEq for String {
    #[inline]
    #[track_caller]
    fn slice_eq(&self, range: SpanRange, other: &Self, other_range: SpanRange) -> bool {
        <&str as SpannableEq>::slice_eq(&self.as_str(), range, &other.as_str(), other_range)
    }
}
impl SpannableEq for Rc<str> {
    #[inline]
    fn slice_eq(&self, range: SpanRange, other: &Self, other_range: SpanRange) -> bool {
        <&str as SpannableEq>::slice_eq(&self.as_ref(), range, &other.as_ref(), other_range)
    }
}
impl SpannableEq for Arc<str> {
    #[inline]
    fn slice_eq(&self, range: SpanRange, other: &Self, other_range: SpanRange) -> bool {
        <&str as SpannableEq>::slice_eq(&self.as_ref(), range, &other.as_ref(), other_range)
    }
}
