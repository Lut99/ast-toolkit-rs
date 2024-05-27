//  HASH.rs
//    by Lut99
//
//  Created:
//    06 May 2024, 16:21:34
//  Last edited:
//    27 May 2024, 13:24:12
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements a [`Hash`](std::hash::Hash)-like implementation over [`Spannable`]s that
//!   does not require one to name the [`Spannable::Slice`], avoiding some
//!   gnarly lifetime problems.
//

use std::borrow::Cow;
use std::hash::{Hash as _, Hasher};
use std::rc::Rc;
use std::sync::Arc;

use crate::range::{index_range_bound, SpanRange};
use crate::spannable::Spannable;


/***** LIBRARY *****/
/// An extension for [`Spannable`]s that make them [`Hash`](std::hash::Hash)able.
pub trait SpannableHash: Spannable {
    /// Computes a hash for the given range of this Spannable.
    ///
    /// # Arguments
    /// - `range`: The range that slices this Spannable. Can be anything implementing [`RangeBounds`].
    /// - `state`: Some [`Hasher`] that does the tough work.
    ///
    /// # Panics
    /// This function panics if out-of-bounds.
    fn slice_hash<H: Hasher>(&self, range: SpanRange, state: &mut H);
}

// Default binary impls for [`Spannable`]
impl SpannableHash for [u8] {
    #[inline]
    #[track_caller]
    fn slice_hash<H: Hasher>(&self, range: SpanRange, state: &mut H) { index_range_bound!(self, range).hash(state) }
}
impl<const LEN: usize> SpannableHash for [u8; LEN] {
    #[inline]
    #[track_caller]
    fn slice_hash<H: Hasher>(&self, range: SpanRange, state: &mut H) { <[u8] as SpannableHash>::slice_hash(&self.as_slice(), range, state) }
}
impl<'b> SpannableHash for Cow<'b, [u8]> {
    #[inline]
    #[track_caller]
    fn slice_hash<H: Hasher>(&self, range: SpanRange, state: &mut H) { <[u8] as SpannableHash>::slice_hash(&self.as_ref(), range, state) }
}
impl SpannableHash for Vec<u8> {
    #[inline]
    #[track_caller]
    fn slice_hash<H: Hasher>(&self, range: SpanRange, state: &mut H) { <[u8] as SpannableHash>::slice_hash(&self.as_slice(), range, state) }
}
impl SpannableHash for Rc<[u8]> {
    #[inline]
    fn slice_hash<H: Hasher>(&self, range: SpanRange, state: &mut H) { <[u8] as SpannableHash>::slice_hash(&self.as_ref(), range, state) }
}
impl SpannableHash for Arc<[u8]> {
    #[inline]
    fn slice_hash<H: Hasher>(&self, range: SpanRange, state: &mut H) { <[u8] as SpannableHash>::slice_hash(&self.as_ref(), range, state) }
}

// Default string impls for [`Spannable`]
impl SpannableHash for str {
    #[inline]
    #[track_caller]
    fn slice_hash<H: Hasher>(&self, range: SpanRange, state: &mut H) { index_range_bound!(self, range).hash(state) }
}
impl<'s> SpannableHash for Cow<'s, str> {
    #[inline]
    #[track_caller]
    fn slice_hash<H: Hasher>(&self, range: SpanRange, state: &mut H) { <str as SpannableHash>::slice_hash(&self.as_ref(), range, state) }
}
impl SpannableHash for String {
    #[inline]
    #[track_caller]
    fn slice_hash<H: Hasher>(&self, range: SpanRange, state: &mut H) { <str as SpannableHash>::slice_hash(&self.as_str(), range, state) }
}
impl SpannableHash for Rc<str> {
    #[inline]
    fn slice_hash<H: Hasher>(&self, range: SpanRange, state: &mut H) { <str as SpannableHash>::slice_hash(&self.as_ref(), range, state) }
}
impl SpannableHash for Arc<str> {
    #[inline]
    fn slice_hash<H: Hasher>(&self, range: SpanRange, state: &mut H) { <str as SpannableHash>::slice_hash(&self.as_ref(), range, state) }
}

// Default pointer-like impls
impl<'t, T: ?Sized + SpannableHash> SpannableHash for &'t T {
    #[inline]
    #[track_caller]
    fn slice_hash<H: Hasher>(&self, range: SpanRange, state: &mut H) { T::slice_hash(self, range, state) }
}
