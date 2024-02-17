//  SPAN.rs
//    by Lut99
//
//  Created:
//    15 Dec 2023, 19:05:00
//  Last edited:
//    17 Feb 2024, 12:42:47
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements a [`Span`], which abstracts over some input to track a particular location in it.
//

use std::borrow::Cow;

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
impl<'b, const LEN: usize> Spannable for &'b [u8; LEN] {
    #[inline]
    fn len(&self) -> usize { LEN }
}
impl<'b> Spannable for &'b [u8] {
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
    fn len(&self) -> usize { self.chars().count() }
}
impl Spannable for String {
    #[inline]
    fn len(&self) -> usize { self.graphemes(true).count() }
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
pub struct Span<F, I> {
    /// Something describing the input (e.g., filename).
    from:  F,
    /// An input of \*something\* spannable.
    input: I,
    /// A start position in this input. Inclusive.
    start: usize,
    /// An end position in this input. Exclusive.
    end:   usize,
}
impl<F, I> Span<F, I> {
    /// Constructor for the `Span` that initializes it from a source text, but spanning nothing.
    ///
    /// # Arguments
    /// - `from`: Something describing the input (e.g., filename).
    /// - `input`: The input source (text) to wrap.
    ///
    /// # Returns
    /// A new `Span` that spans none of the given `input`.
    #[inline]
    pub fn empty(from: F, input: I) -> Self { Self { from, input, start: 0, end: 0 } }
}
impl<F, I: Spannable> Span<F, I> {
    /// Constructor for the `Span` that initializes it to Span the entire given range.
    ///
    /// # Arguments
    /// - `input`: The input source (text) to wrap.
    ///
    /// # Returns
    /// A new `Span` that spans the entire given `input`.
    #[inline]
    pub fn new(from: F, input: I) -> Self {
        let len: usize = input.len();
        Self { from, input, start: 0, end: len }
    }

    /// Provides access to the internal `from`-string.
    ///
    /// If `F` implements [`Copy`], you might prefer [`Span::from()`] instead to avoid the lifetime to `self`.
    ///
    /// # Returns
    /// A reference to the internal `from`-string.
    #[inline]
    pub fn from_ref(&self) -> &F { &self.from }

    /// Provides access to the internal `input`-string.
    ///
    /// If `I` implements [`Copy`], you might prefer [`Span::input()`] instead to avoid the lifetime to `self`.
    ///
    /// # Returns
    /// A reference to the internal `input`-string.
    #[inline]
    pub fn input_ref(&self) -> &I { &self.input }

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
        let input_len: usize = self.input.len();
        if start > input_len || (input_len > 0 && start == input_len) {
            panic!("Given start position {} is larger-than-or-equal-to internal input length {}", start, input_len);
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
        let input_len: usize = self.input.len();
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
}
impl<F: Copy, I> Span<F, I> {
    /// Provides access to the internal `from`-string.
    ///
    /// If `F` does not implement [`Copy`], you might prefer [`Span::from_ref()`] instead.
    ///
    /// # Returns
    /// The internal `from`-string.
    #[inline]
    pub fn from(&self) -> F { self.from }
}
impl<F, I: Copy> Span<F, I> {
    /// Provides access to the internal `input`-string.
    ///
    /// If `F` does not implement [`Copy`], you might prefer [`Span::input_ref()`] instead.
    ///
    /// # Returns
    /// The internal `input`-string.
    #[inline]
    pub fn input(&self) -> I { self.input }
}
impl<F, I> Span<F, I> {
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
