//  SPAN.rs
//    by Lut99
//
//  Created:
//    27 May 2024, 13:29:47
//  Last edited:
//    27 May 2024, 14:32:53
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements some additional traits useful for abstracting over
//!   specific [`Span`]s.
//

use std::borrow::Cow;
use std::convert::Infallible;
use std::error::Error;
use std::rc::Rc;
use std::sync::Arc;


/***** LIBRARY *****/
/// Allows something to be converted to an UTF-8 string.
pub trait AsUtf8 {
    /// The error emitted when the conversion fails.
    type Error: Error;

    /// Splits the spanned area on a pattern of bytes.
    ///
    /// # Arguments
    /// - `b`: A byte to split on.
    ///
    /// # Returns
    /// An iterator over the byte patterns.
    fn as_utf8(&self) -> Result<&str, Self::Error>;
}

// Default binary impls
impl AsUtf8 for [u8] {
    type Error = std::str::Utf8Error;

    #[inline]
    fn as_utf8(&self) -> Result<&str, Self::Error> { std::str::from_utf8(self) }
}
impl<const LEN: usize> AsUtf8 for [u8; LEN] {
    type Error = <[u8] as AsUtf8>::Error;

    #[inline]
    fn as_utf8(&self) -> Result<&str, Self::Error> { <[u8]>::as_utf8(self) }
}
impl<'b> AsUtf8 for Cow<'b, [u8]> {
    type Error = <[u8] as AsUtf8>::Error;

    #[inline]
    fn as_utf8(&self) -> Result<&str, Self::Error> { <[u8]>::as_utf8(self) }
}
impl AsUtf8 for Vec<u8> {
    type Error = <[u8] as AsUtf8>::Error;

    #[inline]
    fn as_utf8(&self) -> Result<&str, Self::Error> { <[u8]>::as_utf8(self) }
}
impl AsUtf8 for Rc<[u8]> {
    type Error = <[u8] as AsUtf8>::Error;

    #[inline]
    fn as_utf8(&self) -> Result<&str, Self::Error> { <[u8]>::as_utf8(self) }
}
impl AsUtf8 for Arc<[u8]> {
    type Error = <[u8] as AsUtf8>::Error;

    #[inline]
    fn as_utf8(&self) -> Result<&str, Self::Error> { <[u8]>::as_utf8(self) }
}

// Default str impls
impl AsUtf8 for str {
    type Error = Infallible;

    #[inline]
    fn as_utf8(&self) -> Result<&str, Self::Error> { Ok(self) }
}
impl<'s> AsUtf8 for Cow<'s, str> {
    type Error = <str as AsUtf8>::Error;

    #[inline]
    fn as_utf8(&self) -> Result<&str, Self::Error> { <str>::as_utf8(self) }
}
impl AsUtf8 for String {
    type Error = <str as AsUtf8>::Error;

    #[inline]
    fn as_utf8(&self) -> Result<&str, Self::Error> { <str>::as_utf8(self) }
}
impl AsUtf8 for Rc<str> {
    type Error = <str as AsUtf8>::Error;

    #[inline]
    fn as_utf8(&self) -> Result<&str, Self::Error> { <str>::as_utf8(self) }
}
impl AsUtf8 for Arc<str> {
    type Error = <str as AsUtf8>::Error;

    #[inline]
    fn as_utf8(&self) -> Result<&str, Self::Error> { <str>::as_utf8(self) }
}

// Default pointer-like impls
impl<'t, T: ?Sized + AsUtf8> AsUtf8 for &'t T {
    type Error = T::Error;

    #[inline]
    #[track_caller]
    fn as_utf8(&self) -> Result<&str, Self::Error> { T::as_utf8(self) }
}
