//  DISPLAY.rs
//    by Lut99
//
//  Created:
//    06 May 2024, 16:37:03
//  Last edited:
//    27 May 2024, 13:24:20
//  Auto updated?
//    Yes
//
//  Description:
//!   Adds a [`Display`]-like trait for [`Spannable`]s
//!   that displays them without having to name [`Spannable::Slice`],
//!   avoiding some gnarly lifetime problems.
//

use std::borrow::Cow;
use std::fmt::{Display, Formatter, Result as FResult};
use std::rc::Rc;
use std::sync::Arc;

use crate::range::{index_range_bound, SpanRange};
use crate::spannable::Spannable;


/***** LIBRARY *****/
/// An extension for [`Spannable`]s that make them [`Display`]able.
pub trait SpannableDisplay: Spannable {
    /// Displays the spanned slice of this Spannable.
    ///
    /// # Arguments
    /// - `range`: The range that slices this Spannable. Can be anything implementing [`RangeBounds`].
    /// - `f`: The [`Formatter`] to write to.
    ///
    /// # Errors
    /// This function errors if it failed to write to the given `f`ormatter.
    ///
    /// # Panics
    /// This function panics if out-of-bounds.
    fn slice_fmt(&self, range: SpanRange, f: &mut Formatter<'_>) -> FResult;
}

// Default binary impls for [`SpannableDisplay`]
impl SpannableDisplay for [u8] {
    #[inline]
    #[track_caller]
    fn slice_fmt(&self, range: SpanRange, f: &mut Formatter<'_>) -> FResult {
        for (i, b) in index_range_bound!(self, range).iter().enumerate() {
            if i > 0 {
                write!(f, " ")?;
            }
            write!(f, "{b:02X}")?;
        }
        Ok(())
    }
}
impl<const LEN: usize> SpannableDisplay for [u8; LEN] {
    #[inline]
    #[track_caller]
    fn slice_fmt(&self, range: SpanRange, f: &mut Formatter<'_>) -> FResult { <[u8] as SpannableDisplay>::slice_fmt(&self.as_slice(), range, f) }
}
impl<'b> SpannableDisplay for Cow<'b, [u8]> {
    #[inline]
    #[track_caller]
    fn slice_fmt(&self, range: SpanRange, f: &mut Formatter<'_>) -> FResult { <[u8] as SpannableDisplay>::slice_fmt(&self.as_ref(), range, f) }
}
impl SpannableDisplay for Vec<u8> {
    #[inline]
    #[track_caller]
    fn slice_fmt(&self, range: SpanRange, f: &mut Formatter<'_>) -> FResult { <[u8] as SpannableDisplay>::slice_fmt(&self.as_slice(), range, f) }
}
impl SpannableDisplay for Rc<[u8]> {
    #[inline]
    fn slice_fmt(&self, range: SpanRange, f: &mut Formatter<'_>) -> FResult { <[u8] as SpannableDisplay>::slice_fmt(&self.as_ref(), range, f) }
}
impl SpannableDisplay for Arc<[u8]> {
    #[inline]
    fn slice_fmt(&self, range: SpanRange, f: &mut Formatter<'_>) -> FResult { <[u8] as SpannableDisplay>::slice_fmt(&self.as_ref(), range, f) }
}

// Default string impls for [`Spannable`]
impl SpannableDisplay for str {
    #[inline]
    #[track_caller]
    fn slice_fmt(&self, range: SpanRange, f: &mut Formatter<'_>) -> FResult { <str as Display>::fmt(index_range_bound!(self, range), f) }
}
impl<'s> SpannableDisplay for Cow<'s, str> {
    #[inline]
    #[track_caller]
    fn slice_fmt(&self, range: SpanRange, f: &mut Formatter<'_>) -> FResult { <str as SpannableDisplay>::slice_fmt(&self.as_ref(), range, f) }
}
impl SpannableDisplay for String {
    #[inline]
    #[track_caller]
    fn slice_fmt(&self, range: SpanRange, f: &mut Formatter<'_>) -> FResult { <str as SpannableDisplay>::slice_fmt(&self.as_str(), range, f) }
}
impl SpannableDisplay for Rc<str> {
    #[inline]
    fn slice_fmt(&self, range: SpanRange, f: &mut Formatter<'_>) -> FResult { <str as SpannableDisplay>::slice_fmt(&self.as_ref(), range, f) }
}
impl SpannableDisplay for Arc<str> {
    #[inline]
    fn slice_fmt(&self, range: SpanRange, f: &mut Formatter<'_>) -> FResult { <str as SpannableDisplay>::slice_fmt(&self.as_ref(), range, f) }
}

// Default pointer-like impls
impl<'t, T: ?Sized + SpannableDisplay> SpannableDisplay for &'t T {
    #[inline]
    #[track_caller]
    fn slice_fmt(&self, range: SpanRange, f: &mut Formatter<'_>) -> FResult { T::slice_fmt(self, range, f) }
}
