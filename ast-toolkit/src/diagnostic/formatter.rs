//  FORMATTER.rs
//    by Lut99
//
//  Created:
//    15 Feb 2024, 22:17:53
//  Last edited:
//    15 Feb 2024, 22:28:40
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements a formatter that takes a source text and one or multiple
//!   spans to show them to the user.
//

use std::borrow::Cow;
use std::fmt::Display;
use std::ops::{Deref, DerefMut};

use console::Style;

use super::style::DiagnosticStyle;


/***** HELPER *****/
/// Disambiguates a character index ([`usize`]) from grapheme indices.
struct GraphemeIndex(usize);
impl Deref for GraphemeIndex {
    type Target = usize;

    #[inline]
    fn deref(&self) -> &Self::Target { &self.0 }
}
impl DerefMut for GraphemeIndex {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target { &mut self.0 }
}





/***** AUXILLARY *****/
/// A helper trait that allows some things to be serialized as a UTF-8 source snippet.
pub trait SourceText {
    /// Returns a text representation of the whole object.
    ///
    /// Implementations can choose between owned or borrowed through the appropricate [`Cow`]-variant.
    ///
    /// # Returns
    /// A [`Cow`] that contains the binary reference.
    fn as_str(&self) -> Cow<str>;
}

// Default binary impls for [`SourceText`].
impl<'b, const LEN: usize> SourceText for &'b [u8; LEN] {
    #[inline]
    fn as_str(&self) -> Cow<str> { String::from_utf8_lossy(self.as_slice()) }
}
impl<'b> SourceText for &'b [u8] {
    #[inline]
    fn as_str(&self) -> Cow<str> { String::from_utf8_lossy(self) }
}
impl SourceText for Vec<u8> {
    #[inline]
    fn as_str(&self) -> Cow<str> { String::from_utf8_lossy(self.as_slice()) }
}

// Default string impls for [`SourceText`].
impl<'s> SourceText for &'s str {
    #[inline]
    fn as_str(&self) -> Cow<str> { Cow::Borrowed(self) }
}
impl SourceText for String {
    #[inline]
    fn as_str(&self) -> Cow<str> { Cow::Borrowed(<String>::as_str(self)) }
}



/// Represents a highlighted area, usually directly matching a span.
pub struct HighlightedRange {
    /// The start index of the range in the source text. Given as a grapheme index, inclusive.
    start: GraphemeIndex,
    /// The end index of the range in the source text. Given as a grapheme index, exclusive.
    end:   GraphemeIndex,
    /// The styling to apply to this area.
    style: Style,
}





/***** LIBRARY *****/
/// Defines a formatter that implements [`Display`] and that can format one or more Spans in a UTF-8 source text.
pub struct SourceTextFormatter<F, S> {
    /// The from-string to format.
    from: F,
    /// The source-string to format.
    source: S,
    /// A list of areas in the source text to highlight.
    ///
    /// NOTE: Must be sorted on start index (low to high) for the formatter to correctly figure out which source text range to display at all.
    highlights: Vec<HighlightedRange>,
    /// A style that dictates how everything non-highlighted looks.
    style: Box<dyn DiagnosticStyle>,
}
impl<F: Display, S: SourceText> Display for SourceTextFormatter<F, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self { from, source, highlights, style } = self;

        // First, we sort the highlights by

        //
        todo!()
    }
}
