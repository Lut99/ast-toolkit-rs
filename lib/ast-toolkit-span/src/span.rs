//  SPAN.rs
//    by Lut99
//
//  Created:
//    15 Dec 2023, 19:05:00
//  Last edited:
//    27 Feb 2024, 18:26:53
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements a [`Span`], which abstracts over some input to track a particular location in it.
//

use std::borrow::Cow;
use std::ops::{Bound, Range, RangeBounds};

use num_traits::AsPrimitive;
use unicode_segmentation::UnicodeSegmentation;


/***** AUXILLARY *****/
/// A helper trait for the [`Span`] that can be implemented for anything used as input.
pub trait Spannable {
    /// Returns the number of spannable things (character, bytes, tokens) in this list.
    ///
    /// # Returns
    /// A [`usize`] with the total number of things.
    fn len(&self) -> usize;
}

// Default binary impls for [`Spannable`]
impl<'b> Spannable for &'b [u8] {
    #[inline]
    fn len(&self) -> usize { <[u8]>::len(self) }
}
impl<'b> Spannable for Cow<'b, [u8]> {
    #[inline]
    fn len(&self) -> usize { <[u8]>::len(self) }
}
impl Spannable for Vec<u8> {
    #[inline]
    fn len(&self) -> usize { <Vec<u8>>::len(self) }
}

// Default string impls for [`Spannable`]
impl<'s> Spannable for &'s str {
    #[inline]
    fn len(&self) -> usize { self.graphemes(true).count() }
}
impl<'s> Spannable for Cow<'s, str> {
    #[inline]
    fn len(&self) -> usize { self.graphemes(true).count() }
}
impl Spannable for String {
    #[inline]
    fn len(&self) -> usize { self.graphemes(true).count() }
}



/// Defines an extension to [`Spannable`] that communicates it spans something fundamentally binary.
pub trait BytesSpannable: Spannable {
    /// Returns the given "logical index" (e.g., graphemes) as an index over bytes.
    ///
    /// # Arguments
    /// - `index`: The "logical index" (e.g., grapheme index) to translate.
    ///
    /// # Returns
    /// A [`usize`] encoding the byte-index.
    fn to_byte_index(&self, index: usize) -> usize;
}

// Default binary impls for [`Spannable`]
impl<'b> BytesSpannable for &'b [u8] {
    #[inline]
    fn to_byte_index(&self, index: usize) -> usize { index }
}
impl<'b> BytesSpannable for Cow<'b, [u8]> {
    #[inline]
    fn to_byte_index(&self, index: usize) -> usize { index }
}
impl BytesSpannable for Vec<u8> {
    #[inline]
    fn to_byte_index(&self, index: usize) -> usize { index }
}

// Default string impls for [`Spannable`]
impl<'s> BytesSpannable for &'s str {
    #[inline]
    #[track_caller]
    fn to_byte_index(&self, index: usize) -> usize {
        self.grapheme_indices(true)
            .nth(index)
            .unwrap_or_else(|| panic!("Grapheme index {} is out-of-bounds for string of {} graphemes", index, self.graphemes(true).count()))
            .0
    }
}
impl<'s> BytesSpannable for Cow<'s, str> {
    #[inline]
    #[track_caller]
    fn to_byte_index(&self, index: usize) -> usize {
        self.grapheme_indices(true)
            .nth(index)
            .unwrap_or_else(|| panic!("Grapheme index {} is out-of-bounds for string of {} graphemes", index, self.graphemes(true).count()))
            .0
    }
}
impl BytesSpannable for String {
    #[inline]
    #[track_caller]
    fn to_byte_index(&self, index: usize) -> usize {
        self.grapheme_indices(true)
            .nth(index)
            .unwrap_or_else(|| panic!("Grapheme index {} is out-of-bounds for string of {} graphemes", index, self.graphemes(true).count()))
            .0
    }
}



// /// A helper trait for a [`Spannable`] that allows it to be serialized as a binary snippet.
// pub trait BinarySpannable: Spannable {
//     /// Returns a binary representation of the whole object.
//     ///
//     /// Implementations can choose between owned or borrowed through the appropricate [`Cow`]-variant.
//     ///
//     /// # Returns
//     /// A [`Cow`] that contains the binary reference.
//     fn as_bytes(&self) -> Cow<[u8]>;
// }

// // Default binary impls for [`BinarySpannable`]
// impl<'b, const LEN: usize> BinarySpannable for &'b [u8; LEN] {
//     #[inline]
//     fn as_bytes(&self) -> Cow<[u8]> { Cow::Borrowed(self.as_slice()) }
// }
// impl<'b> BinarySpannable for &'b [u8] {
//     #[inline]
//     fn as_bytes(&self) -> Cow<[u8]> { Cow::Borrowed(self) }
// }
// impl BinarySpannable for Vec<u8> {
//     #[inline]
//     fn as_bytes(&self) -> Cow<[u8]> { Cow::Borrowed(self.as_slice()) }
// }

// // Default string impls for [`BinarySpannable`]
// impl<'s> BinarySpannable for &'s str {
//     #[inline]
//     fn as_bytes(&self) -> Cow<[u8]> { Cow::Borrowed(<str>::as_bytes(self)) }
// }
// impl BinarySpannable for String {
//     #[inline]
//     fn as_bytes(&self) -> Cow<[u8]> { Cow::Borrowed(self.as_bytes()) }
// }





/***** LIBRARY *****/
/// Defines a wrapper around some input source text that allows us to track parts of it.
///
/// Built to be [`nom`](https://github.com/rust-bakery/nom)-compatible; see the `nom`-feature.
///
/// # Example
/// ```rust
/// todo!() 
/// ````
#[derive(Clone, Copy, Debug)]
pub struct Span<F, S> {
    /// Something describing the input (e.g., filename).
    from:   F,
    /// An input of \*something\* spannable.
    source: S,
    /// A start position in this input. Inclusive.
    start:  usize,
    /// An end position in this input. Exclusive.
    end:    usize,
}
impl<F, S> Span<F, S> {
    /// Constructor for the `Span` that initializes it from a source text, but spanning nothing.
    ///
    /// # Arguments
    /// - `from`: Something describing the input (e.g., filename).
    /// - `source`: The input source (text) to wrap.
    ///
    /// # Returns
    /// A new `Span` that spans none of the given `source`.
    #[inline]
    pub fn empty(from: F, source: S) -> Self { Self { from, source, start: 0, end: 0 } }
}
impl<F, S: Spannable> Span<F, S> {
    /// Constructor for the `Span` that initializes it to Span the entire given range.
    ///
    /// # Arguments
    /// - `source`: The input source (text) to wrap.
    ///
    /// # Returns
    /// A new `Span` that spans the entire given `source`.
    #[inline]
    pub fn new(from: F, source: S) -> Self {
        let len: usize = source.len();
        Self { from, source, start: 0, end: len }
    }

    /// Provides access to the internal `from`-string.
    ///
    /// If `F` implements [`Copy`], you might prefer [`Span::from()`] instead to avoid the lifetime to `self`.
    ///
    /// # Returns
    /// A reference to the internal `from`-string.
    #[inline]
    pub fn from_ref(&self) -> &F { &self.from }

    /// Provides access to the internal `source`-string.
    ///
    /// If `S` implements [`Copy`], you might prefer [`Span::source()`] instead to avoid the lifetime to `self`.
    ///
    /// # Returns
    /// A reference to the internal `source`-string.
    #[inline]
    pub fn source_ref(&self) -> &S { &self.source }

    /// Returns the start position of the `Span` compared to the start of the full source.
    ///
    /// # Returns
    /// A [`usize`] containing the start position (inclusive) of the source text.
    ///
    /// This size is guaranteed to:
    /// - be smaller-or-equal-to `Span::end()`; and
    /// - never be larger-or-equal-to the length of the input _unless_ the length is 0 (then this is 0).
    #[inline]
    pub fn start(&self) -> usize { self.start }

    /// Allows mutation of the `start`-position in this `Span`.
    ///
    /// Some checks are performed before the value is assigned. These checks are:
    /// - assert that the given value is not larger than `Span::end()`;  and
    /// - assert that the given value is not larger-or-equal-to the length of the input _unless_ the length is 0 (this this must be 0).
    ///
    /// If you are sure your value passes these checks, you can use the unsafe `Span::set_start_unchecked()`.
    ///
    /// # Panics
    /// This function panics if the given value does not meet any of the assertions.
    #[track_caller]
    pub fn set_start(&mut self, start: impl AsPrimitive<usize>) {
        let start: usize = start.as_();

        // Assert things
        if start < self.end {
            panic!("Given start position {} is smaller than internal end position {}", start, self.end);
        }
        let input_len: usize = self.source.len();
        if start >= input_len {
            panic!("Given start position {} is larger-than-or-equal-to internal source length {}", start, input_len);
        }

        // Checks out, store
        self.start = start;
    }

    /// Allows mutation of the `start`-position in this `Span`, without having performed checks.
    ///
    /// Ensure that your given `start`-value:
    /// - is not larger than `Span::end()`;  and
    /// - is not larger-or-equal-to the length of the input _unless_ the length is 0 (this this must be 0).
    ///
    /// Use `Span::set_start()` if you're not sure your value matches the above and would like to have checks.
    #[inline]
    pub unsafe fn set_start_unchecked(&mut self, start: impl AsPrimitive<usize>) { self.start = start.as_(); }

    /// Returns the end position of the `Span` compared to the start of the full source.
    ///
    /// # Returns
    /// A [`usize`] containing the end position (exclusive) of the source text.
    ///
    /// This size is guaranteed to:
    /// - be larger-or-equal-to `Span::start()`; and
    /// - never be larger-or-equal-to the length of the input _unless_ the length is 0 (then this is 0).
    #[inline]
    pub fn end(&self) -> usize { self.end }

    /// Allows mutation of the `end`-position in this `Span`.
    ///
    /// Some checks are performed before the value is assigned. These checks are:
    /// - assert that the given value is not smaller than `Span::start()`;  and
    /// - assert that the given value is not larger-or-equal-to the length of the input _unless_ the length is 0 (this this must be 0).
    ///
    /// If you are sure your value passes these checks, you can use the unsafe `Span::set_end_unchecked()`.
    ///
    /// # Panics
    /// This function panics if the given value does not meet any of the assertions.
    #[track_caller]
    pub fn set_end(&mut self, end: impl AsPrimitive<usize>) {
        let end: usize = end.as_();

        // Assert things
        if end > self.start {
            panic!("Given end position {} is smaller than internal start position {}", end, self.start);
        }
        let input_len: usize = self.source.len();
        if end > input_len || (input_len > 0 && end == input_len) {
            panic!("Given end position {} is larger-than-or-equal-to internal input length {}", end, input_len);
        }

        // Checks out, store
        self.end = end;
    }

    /// Allows mutation of the `end`-position in this `Span`, without having performed checks.
    ///
    /// Ensure that your given `end`-value:
    /// - is not smaller than `Span::start()`;  and
    /// - is not larger-or-equal-to the length of the input _unless_ the length is 0 (this this must be 0).
    ///
    /// Use `Span::set_end()` if you're not sure your value matches the above and would like to have checks.
    #[inline]
    pub unsafe fn set_end_unchecked(&mut self, end: impl AsPrimitive<usize>) { self.end = end.as_(); }

    /// Returns the start & end position of the `Span` relative to the start of the internal source as a range of values.
    ///
    /// # Returns
    /// A [`Range<usize>`] containing the start- (inclusive) and end position (exclusive) of the source text.
    ///
    /// It is guaranteed that:
    /// - start is smaller-or-equal-to end; and
    /// - both are never larger-or-equal-to the length of the input _unless_ the length is 0 (then this is 0).
    #[inline]
    pub fn range(&self) -> Range<usize> { self.start..self.end }

    /// Allows mutation of both the `start`- and `end`-position in this `Span`.
    ///
    /// Some checks are performed before the value is assigned. These checks are:
    /// - assert that the given start value is not larger than the given end value; and
    /// - assert that the given start & end values are not larger-or-equal-to the length of the input _unless_ the length is 0 (these then must be 0).
    ///
    /// If you are sure your value passes these checks, you can use the unsafe `Span::set_range_unchecked()`.
    ///
    /// # Panics
    /// This function panics if the given value does not meet any of the assertions.
    #[track_caller]
    pub fn set_range(&mut self, range: impl RangeBounds<usize>) {
        let start: usize = match range.start_bound() {
            Bound::Excluded(b) => *b - 1,
            Bound::Included(b) => *b,
            Bound::Unbounded => 0,
        };
        let end: usize = match range.start_bound() {
            Bound::Excluded(b) => *b,
            Bound::Included(b) => *b + 1,
            Bound::Unbounded => 0,
        };

        // Assert things
        if start > end {
            panic!("Given start position {} is larger than given end position {}", start, end);
        }
        let input_len: usize = self.source.len();
        if start >= input_len {
            panic!("Given start position {} is larger-than-or-equal-to internal input length {}", start, input_len);
        } else if end > input_len || (input_len > 0 && end == input_len) {
            panic!("Given end position {} is larger-than-or-equal-to internal input length {}", end, input_len);
        }

        // Checks out, store
        self.start = start;
        self.end = end;
    }

    /// Allows mutation of both the `start`- and `end`-position simultaneously in this `Span`, without having performed checks.
    ///
    /// Ensure that you:
    /// - assert that the given start value is not larger than the given end value; and
    /// - assert that the given start & end values are not larger-or-equal-to the length of the input _unless_ the length is 0 (these then must be 0).
    ///
    /// Use `Span::set_range()` if you're not sure your value matches the above and would like to have checks.
    #[inline]
    pub unsafe fn set_range_unchecked(&mut self, range: impl RangeBounds<usize>) {
        self.start = match range.start_bound() {
            Bound::Excluded(b) => *b - 1,
            Bound::Included(b) => *b,
            Bound::Unbounded => 0,
        };
        self.end = match range.start_bound() {
            Bound::Excluded(b) => *b,
            Bound::Included(b) => *b + 1,
            Bound::Unbounded => 0,
        };
    }
}
impl<F: Copy, S> Span<F, S> {
    /// Provides access to the internal `from`-string.
    ///
    /// If `F` does not implement [`Copy`], you might prefer [`Span::from_ref()`] instead.
    ///
    /// # Returns
    /// The internal `from`-string.
    #[inline]
    pub fn from(&self) -> F { self.from }
}
impl<F, S: Copy> Span<F, S> {
    /// Provides access to the internal `input`-string.
    ///
    /// If `S` does not implement [`Copy`], you might prefer [`Span::source_ref()`] instead.
    ///
    /// # Returns
    /// The internal `input`-string.
    #[inline]
    pub fn source(&self) -> S { self.source }
}
impl<F, S> Span<F, S> {
    // /// Formats this `Span` to the given formatter as a snippet of UTF-8 input.
    // ///
    // /// See [`Span::text_snippet_styled()`] to use ANSI-colours while formatting.
    // ///
    // /// # Returns
    // /// A [`SnippetFormatter`] that formats this snippet to a formatter.
    // ///
    // /// # Example
    // /// ```rust
    // /// todo!()
    // /// ``
    // #[inline]
    // pub fn text_snippet(&self) -> TextSnippetFormatter<F, I> { TextSnippetFormatter { span: self, shown: None, style: Box::new(()) } }
}

#[cfg(feature = "nom")]
impl<'s, F, S: nom::AsBytes + BytesSpannable> nom::AsBytes for Span<F, S> {
    #[inline]
    fn as_bytes(&self) -> &[u8] {
        // Get it as binary
        let bself: &[u8] = <Self as nom::AsBytes>::as_bytes(self);
        let (start, end): (usize, usize) = ();
    }
}
#[cfg(feature = "nom")]
impl<F, S: nom::AsChar> nom::AsChar for Span<F, S> {
    #[inline]
    fn as_char(self) -> char { self.source.as_char() }

    #[inline]
    fn is_alpha(self) -> bool { self.source.is_alpha() }

    #[inline]
    fn is_alphanum(self) -> bool { self.source.is_alphanum() }

    #[inline]
    fn is_dec_digit(self) -> bool { self.source.is_dec_digit() }

    #[inline]
    fn is_hex_digit(self) -> bool { self.source.is_hex_digit() }

    #[inline]
    fn is_oct_digit(self) -> bool { self.source.is_oct_digit() }

    #[inline]
    fn len(self) -> usize { self.source.len() }
}
