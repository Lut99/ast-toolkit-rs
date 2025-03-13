//  ORD.rs
//    by Lut99
//
//  Created:
//    13 Feb 2025, 10:59:34
//  Last edited:
//    13 Feb 2025, 11:05:09
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements an [`Ord`](std::cmp::Ord)-like implementation over [`Spannable`]s that
//!   does not require one to name the [`Spannable::Slice`], avoiding some
//!   gnarly lifetime problems.
//

use std::borrow::Cow;
use std::cmp::Ordering;
use std::rc::Rc;
use std::sync::Arc;

use crate::range::{SpanRange, index_range_bound};
use crate::spannable::Spannable;


/***** LIBRARY *****/
/// An extension for [`Spannable`]s that make them [`Eq`](std::cmp::Eq)able.
pub trait SpannableOrd: Spannable {
    /// Computes how the spanned area compares to that of another span.
    ///
    /// # Arguments
    /// - `range`: The range that slices this Spannable. Can be anything implementing [`RangeBounds`].
    /// - `other`: Some other Spannable of type Self to check with.
    ///
    /// # Returns
    /// An [`Ordering`] describing how they are ordered.
    ///
    /// # Panics
    /// This function panics if out-of-bounds.
    fn slice_ord(&self, range: SpanRange, other: &Self, other_range: SpanRange) -> Ordering;
}

// Default binary impls for [`Spannable`]
impl SpannableOrd for [u8] {
    #[inline]
    #[track_caller]
    fn slice_ord(&self, range: SpanRange, other: &Self, other_range: SpanRange) -> Ordering {
        index_range_bound!(self, range).cmp(index_range_bound!(other, other_range))
    }
}
impl<const LEN: usize> SpannableOrd for [u8; LEN] {
    #[inline]
    #[track_caller]
    fn slice_ord(&self, range: SpanRange, other: &Self, other_range: SpanRange) -> Ordering {
        <[u8] as SpannableOrd>::slice_ord(&self.as_slice(), range, &other.as_slice(), other_range)
    }
}
impl<'b> SpannableOrd for Cow<'b, [u8]> {
    #[inline]
    #[track_caller]
    fn slice_ord(&self, range: SpanRange, other: &Self, other_range: SpanRange) -> Ordering {
        <[u8] as SpannableOrd>::slice_ord(&self.as_ref(), range, &other.as_ref(), other_range)
    }
}
impl SpannableOrd for Vec<u8> {
    #[inline]
    #[track_caller]
    fn slice_ord(&self, range: SpanRange, other: &Self, other_range: SpanRange) -> Ordering {
        <[u8] as SpannableOrd>::slice_ord(&self.as_slice(), range, &other.as_slice(), other_range)
    }
}
impl SpannableOrd for Rc<[u8]> {
    #[inline]
    fn slice_ord(&self, range: SpanRange, other: &Self, other_range: SpanRange) -> Ordering {
        <[u8] as SpannableOrd>::slice_ord(&self.as_ref(), range, &other.as_ref(), other_range)
    }
}
impl SpannableOrd for Arc<[u8]> {
    #[inline]
    fn slice_ord(&self, range: SpanRange, other: &Self, other_range: SpanRange) -> Ordering {
        <[u8] as SpannableOrd>::slice_ord(&self.as_ref(), range, &other.as_ref(), other_range)
    }
}

// Default string impls for [`Spannable`]
impl SpannableOrd for str {
    #[inline]
    #[track_caller]
    fn slice_ord(&self, range: SpanRange, other: &Self, other_range: SpanRange) -> Ordering {
        index_range_bound!(self, range).cmp(index_range_bound!(other, other_range))
    }
}
impl<'s> SpannableOrd for Cow<'s, str> {
    #[inline]
    #[track_caller]
    fn slice_ord(&self, range: SpanRange, other: &Self, other_range: SpanRange) -> Ordering {
        <str as SpannableOrd>::slice_ord(&self.as_ref(), range, &other.as_ref(), other_range)
    }
}
impl SpannableOrd for String {
    #[inline]
    #[track_caller]
    fn slice_ord(&self, range: SpanRange, other: &Self, other_range: SpanRange) -> Ordering {
        <str as SpannableOrd>::slice_ord(&self.as_str(), range, &other.as_str(), other_range)
    }
}
impl SpannableOrd for Rc<str> {
    #[inline]
    fn slice_ord(&self, range: SpanRange, other: &Self, other_range: SpanRange) -> Ordering {
        <str as SpannableOrd>::slice_ord(&self.as_ref(), range, &other.as_ref(), other_range)
    }
}
impl SpannableOrd for Arc<str> {
    #[inline]
    fn slice_ord(&self, range: SpanRange, other: &Self, other_range: SpanRange) -> Ordering {
        <str as SpannableOrd>::slice_ord(&self.as_ref(), range, &other.as_ref(), other_range)
    }
}

// Default pointer-like impls
impl<'t, T: ?Sized + SpannableOrd> SpannableOrd for &'t T {
    #[inline]
    #[track_caller]
    fn slice_ord(&self, range: SpanRange, other: &Self, other_range: SpanRange) -> Ordering { T::slice_ord(self, range, other, other_range) }
}
