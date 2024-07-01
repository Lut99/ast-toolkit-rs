//  AS STR.rs
//    by Lut99
//
//  Created:
//    01 Jul 2024, 15:48:58
//  Last edited:
//    01 Jul 2024, 16:03:25
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines an extension to [`Spannable`]s that will return their slice
//!   as a string reference.
//

use std::borrow::Cow;
use std::convert::Infallible;
use std::error::Error;
use std::rc::Rc;
use std::sync::Arc;

use crate::range::{index_range_bound, SpanRange};
use crate::spannable::Spannable;


/***** LIBRARY *****/
/// Generalizes [`SpannableAsStr`] over also things that are potentially strings, i.e., bytes.
pub trait SpannableTryAsStr: Spannable {
    /// Describes why the spanned area is not a valid string.
    type Error: Error;


    /// Returns the spanned area as a string.
    ///
    /// # Arguments
    /// - `range`: A [`SpanRange`] that determines which are of the underlying object is spanned.
    ///
    /// # Returns
    /// A [`&str`](str) that encodes the string value of the spanned area.
    ///
    /// # Errors
    /// This function can error if the spanned area does not encode a string.
    fn try_as_str(&self, range: SpanRange) -> Result<&str, Self::Error>;
}

// Default binary impls for [`SpannableTryAsStr`]
impl SpannableTryAsStr for [u8] {
    type Error = std::str::Utf8Error;

    #[inline]
    fn try_as_str(&self, range: SpanRange) -> Result<&str, Self::Error> { std::str::from_utf8(index_range_bound!(self, range)) }
}
impl<const LEN: usize> SpannableTryAsStr for [u8; LEN] {
    type Error = <[u8] as SpannableTryAsStr>::Error;

    #[inline]
    fn try_as_str(&self, range: SpanRange) -> Result<&str, Self::Error> { <[u8] as SpannableTryAsStr>::try_as_str(self, range) }
}
impl<'b> SpannableTryAsStr for Cow<'b, [u8]> {
    type Error = <[u8] as SpannableTryAsStr>::Error;

    #[inline]
    fn try_as_str(&self, range: SpanRange) -> Result<&str, Self::Error> { <[u8] as SpannableTryAsStr>::try_as_str(self, range) }
}
impl SpannableTryAsStr for Vec<u8> {
    type Error = <[u8] as SpannableTryAsStr>::Error;

    #[inline]
    fn try_as_str(&self, range: SpanRange) -> Result<&str, Self::Error> { <[u8] as SpannableTryAsStr>::try_as_str(self, range) }
}
impl SpannableTryAsStr for Rc<[u8]> {
    type Error = <[u8] as SpannableTryAsStr>::Error;

    #[inline]
    fn try_as_str(&self, range: SpanRange) -> Result<&str, Self::Error> { <[u8] as SpannableTryAsStr>::try_as_str(self, range) }
}
impl SpannableTryAsStr for Arc<[u8]> {
    type Error = <[u8] as SpannableTryAsStr>::Error;

    #[inline]
    fn try_as_str(&self, range: SpanRange) -> Result<&str, Self::Error> { <[u8] as SpannableTryAsStr>::try_as_str(self, range) }
}

// Default string impls for [`SpannableLocate`]
impl SpannableTryAsStr for str {
    type Error = Infallible;

    #[inline]
    fn try_as_str(&self, range: SpanRange) -> Result<&str, Self::Error> { Ok(index_range_bound!(self, range)) }
}
impl<'s> SpannableTryAsStr for Cow<'s, str> {
    type Error = <str as SpannableTryAsStr>::Error;

    #[inline]
    fn try_as_str(&self, range: SpanRange) -> Result<&str, Self::Error> { <str as SpannableTryAsStr>::try_as_str(self, range) }
}
impl SpannableTryAsStr for String {
    type Error = <str as SpannableTryAsStr>::Error;

    #[inline]
    fn try_as_str(&self, range: SpanRange) -> Result<&str, Self::Error> { <str as SpannableTryAsStr>::try_as_str(self, range) }
}
impl SpannableTryAsStr for Rc<str> {
    type Error = <str as SpannableTryAsStr>::Error;

    #[inline]
    fn try_as_str(&self, range: SpanRange) -> Result<&str, Self::Error> { <str as SpannableTryAsStr>::try_as_str(self, range) }
}
impl SpannableTryAsStr for Arc<str> {
    type Error = <str as SpannableTryAsStr>::Error;

    #[inline]
    fn try_as_str(&self, range: SpanRange) -> Result<&str, Self::Error> { <str as SpannableTryAsStr>::try_as_str(self, range) }
}

// Default pointer-like impls for [`SpannableLocate`]
impl<'t, T: ?Sized + SpannableTryAsStr> SpannableTryAsStr for &'t T {
    type Error = <T as SpannableTryAsStr>::Error;

    #[inline]
    fn try_as_str(&self, range: SpanRange) -> Result<&str, Self::Error> { <T as SpannableTryAsStr>::try_as_str(self, range) }
}



/// An extension for [`Spannable`]s that allows to get the spanned area as some string.
pub trait SpannableAsStr: Spannable {
    /// Returns the spanned area as a string.
    ///
    /// # Arguments
    /// - `range`: A [`SpanRange`] that determines which are of the underlying object is spanned.
    ///
    /// # Returns
    /// A [`&str`](str) that encodes the string value of the spanned area.
    fn as_str(&self, range: SpanRange) -> &str;
}

// Default string impls for [`SpannableLocate`]
impl SpannableAsStr for str {
    #[inline]
    fn as_str(&self, range: SpanRange) -> &str { index_range_bound!(self, range) }
}
impl<'s> SpannableAsStr for Cow<'s, str> {
    #[inline]
    fn as_str(&self, range: SpanRange) -> &str { <str as SpannableAsStr>::as_str(self, range) }
}
impl SpannableAsStr for String {
    #[inline]
    fn as_str(&self, range: SpanRange) -> &str { <str as SpannableAsStr>::as_str(self, range) }
}
impl SpannableAsStr for Rc<str> {
    #[inline]
    fn as_str(&self, range: SpanRange) -> &str { <str as SpannableAsStr>::as_str(self, range) }
}
impl SpannableAsStr for Arc<str> {
    #[inline]
    fn as_str(&self, range: SpanRange) -> &str { <str as SpannableAsStr>::as_str(self, range) }
}

// Default pointer-like impls for [`SpannableLocate`]
impl<'t, T: ?Sized + SpannableAsStr> SpannableAsStr for &'t T {
    #[inline]
    fn as_str(&self, range: SpanRange) -> &str { <T as SpannableAsStr>::as_str(self, range) }
}
