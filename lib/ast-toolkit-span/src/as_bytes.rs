//  AS BYTES.rs
//    by Lut99
//
//  Created:
//    02 Jul 2024, 11:24:44
//  Last edited:
//    02 Jul 2024, 11:27:20
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines an extension to [`Spannable`]s that will return their slice
//!   as a byte array reference.
//

use std::borrow::Cow;
use std::rc::Rc;
use std::sync::Arc;

use crate::range::{index_range_bound, SpanRange};
use crate::spannable::Spannable;


/***** LIBRARY *****/
/// An extension for [`Spannable`]s that allows to get the spanned area as some string.
pub trait SpannableAsBytes: Spannable {
    /// Returns the spanned area as a string of bytes.
    ///
    /// # Arguments
    /// - `range`: A [`SpanRange`] that determines which are of the underlying object is spanned.
    ///
    /// # Returns
    /// A [`&[u8]`](u8) that encodes (some) raw value of the spanned area.
    fn as_bytes(&self, range: SpanRange) -> &[u8];
}

// Default binary impls for [`SpannableAsBytes`]
impl SpannableAsBytes for [u8] {
    #[inline]
    fn as_bytes(&self, range: SpanRange) -> &[u8] { index_range_bound!(self, range) }
}
impl<const LEN: usize> SpannableAsBytes for [u8; LEN] {
    #[inline]
    fn as_bytes(&self, range: SpanRange) -> &[u8] { <[u8] as SpannableAsBytes>::as_bytes(self, range) }
}
impl<'b> SpannableAsBytes for Cow<'b, [u8]> {
    #[inline]
    fn as_bytes(&self, range: SpanRange) -> &[u8] { <[u8] as SpannableAsBytes>::as_bytes(self, range) }
}
impl SpannableAsBytes for Vec<u8> {
    #[inline]
    fn as_bytes(&self, range: SpanRange) -> &[u8] { <[u8] as SpannableAsBytes>::as_bytes(self, range) }
}
impl SpannableAsBytes for Rc<[u8]> {
    #[inline]
    fn as_bytes(&self, range: SpanRange) -> &[u8] { <[u8] as SpannableAsBytes>::as_bytes(self, range) }
}
impl SpannableAsBytes for Arc<[u8]> {
    #[inline]
    fn as_bytes(&self, range: SpanRange) -> &[u8] { <[u8] as SpannableAsBytes>::as_bytes(self, range) }
}

// Default string impls for [`SpannableLocate`]
impl SpannableAsBytes for str {
    #[inline]
    fn as_bytes(&self, range: SpanRange) -> &[u8] { index_range_bound!(self.as_bytes(), range) }
}
impl<'s> SpannableAsBytes for Cow<'s, str> {
    #[inline]
    fn as_bytes(&self, range: SpanRange) -> &[u8] { <str as SpannableAsBytes>::as_bytes(self, range) }
}
impl SpannableAsBytes for String {
    #[inline]
    fn as_bytes(&self, range: SpanRange) -> &[u8] { <str as SpannableAsBytes>::as_bytes(self, range) }
}
impl SpannableAsBytes for Rc<str> {
    #[inline]
    fn as_bytes(&self, range: SpanRange) -> &[u8] { <str as SpannableAsBytes>::as_bytes(self, range) }
}
impl SpannableAsBytes for Arc<str> {
    #[inline]
    fn as_bytes(&self, range: SpanRange) -> &[u8] { <str as SpannableAsBytes>::as_bytes(self, range) }
}

// Default pointer-like impls for [`SpannableLocate`]
impl<'t, T: ?Sized + SpannableAsBytes> SpannableAsBytes for &'t T {
    #[inline]
    fn as_bytes(&self, range: SpanRange) -> &[u8] { <T as SpannableAsBytes>::as_bytes(self, range) }
}
