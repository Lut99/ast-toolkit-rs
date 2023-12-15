//  DISPLAY SOURCE.rs
//    by Lut99
//
//  Created:
//    15 Dec 2023, 19:05:00
//  Last edited:
//    15 Dec 2023, 19:16:46
//  Auto updated?
//    Yes
//
//  Description:
//!   Provides the [`DisplaySource`] trait, and various types of
//!   [`Diagnostic`]s that implement it to show advanced source texts.
//

use std::fmt::{Display, Formatter, Result as FResult};


/***** FORMATTER *****/
/// Implements the pretty display for [`DisplaySpan`]-capable types.
pub struct DisplaySpanFormatter<'s, S: ?Sized, T> {
    /// The span to display
    span:  &'s S,
    /// The style to display it in.
    style: T,
}
impl<'s, S: DisplaySpanImpl, T: DisplaySpanStyle> Display for DisplaySpanFormatter<'s, S, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult { Ok(()) }
}





/***** LIBRARY *****/
/// Defines how to colour the formatting of a [`DisplaySpan`].
pub trait DisplaySpanStyle {}



/// Allows something that spans source text to be (prettily!) displayed.
pub trait DisplaySpan: DisplaySpanImpl {
    /// Returns a formatter that will show the source text highlighted by this span.
    ///
    /// # Arguments
    /// - `style`: A [`DisplaySpanStyle`] that is used to define the styling for accents in the formatted text.
    ///
    /// # Returns
    /// A [`DisplaySpanFormatter`] that implements [`Display`].
    #[inline]
    fn display_span<'s, T>(&'s self, style: T) -> DisplaySpanFormatter<'s, Self, T> { DisplaySpanFormatter { span: self, style } }
}
impl<T: DisplaySpanImpl> DisplaySpan for T {}

/// The half of the trait that is "private", and contains the child-specific implementation functions.
pub trait DisplaySpanImpl {}
