//  SPAN.rs
//    by Lut99
//
//  Created:
//    15 Dec 2023, 19:05:00
//  Last edited:
//    28 Feb 2024, 15:51:15
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements a [`Span`], which abstracts over some input to track a particular location in it.
//

use std::borrow::Cow;
use std::ops::{Add, AddAssign, Bound, Deref, Range, RangeBounds, Sub, SubAssign};

use unicode_segmentation::UnicodeSegmentation;


/***** AUXILLARY *****/
/// Newtype wrapper that differentiates raw indices (e.g., byte indices) from [logic indices](LogicUsize) (e.g., grapheme indices).
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct RawUsize(usize);
impl RawUsize {
    /// Constructor for zero.
    ///
    /// # Returns
    /// A new RawUsize that encodes `0`.
    #[inline]
    pub const fn zero() -> Self { Self(0) }
}
impl Add<Self> for RawUsize {
    type Output = Self;

    #[inline]
    fn add(self, rhs: Self) -> Self::Output { Self(self.0 + rhs.0) }
}
impl AddAssign<Self> for RawUsize {
    #[inline]
    fn add_assign(&mut self, rhs: Self) { self.0 += rhs.0 }
}
impl Sub<Self> for RawUsize {
    type Output = Self;

    #[inline]
    fn sub(self, rhs: Self) -> Self::Output { Self(self.0 - rhs.0) }
}
impl SubAssign<Self> for RawUsize {
    #[inline]
    fn sub_assign(&mut self, rhs: Self) { self.0 -= rhs.0 }
}
impl Deref for RawUsize {
    type Target = usize;

    #[inline]
    fn deref(&self) -> &Self::Target { &self.0 }
}
impl From<usize> for RawUsize {
    #[inline]
    fn from(value: usize) -> Self { Self(value) }
}

/// Newtype wrapper that differentiates [raw indices](RawUsize) (e.g., byte indices) from logic indices (e.g., grapheme indices).
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct LogicUsize(usize);
impl LogicUsize {
    /// Constructor for zero.
    ///
    /// # Returns
    /// A new RawUsize that encodes `0`.
    #[inline]
    pub const fn zero() -> Self { Self(0) }
}
impl Add<Self> for LogicUsize {
    type Output = Self;

    #[inline]
    fn add(self, rhs: Self) -> Self::Output { Self(self.0 + rhs.0) }
}
impl AddAssign<Self> for LogicUsize {
    #[inline]
    fn add_assign(&mut self, rhs: Self) { self.0 += rhs.0 }
}
impl Sub<Self> for LogicUsize {
    type Output = Self;

    #[inline]
    fn sub(self, rhs: Self) -> Self::Output { Self(self.0 - rhs.0) }
}
impl SubAssign<Self> for LogicUsize {
    #[inline]
    fn sub_assign(&mut self, rhs: Self) { self.0 -= rhs.0 }
}
impl Deref for LogicUsize {
    type Target = usize;

    #[inline]
    fn deref(&self) -> &Self::Target { &self.0 }
}
impl From<usize> for LogicUsize {
    #[inline]
    fn from(value: usize) -> Self { Self(value) }
}



/// A helper trait for the [`Span`] that can be implemented for anything used as input.
pub trait Spannable {
    /// Returns the number of currently spanned "raw" items (e.g., bytes).
    ///
    /// # Returns
    /// A [`RawUsize`] with the total number of bytes or other elementary items as is stored on-disk.
    fn spanned_len(&self) -> RawUsize;
}

// Default binary impls for [`Spannable`]
impl<'b> Spannable for &'b [u8] {
    #[inline]
    fn spanned_len(&self) -> RawUsize { RawUsize(self.len()) }
}
impl<'b> Spannable for Cow<'b, [u8]> {
    #[inline]
    fn spanned_len(&self) -> RawUsize { RawUsize(self.len()) }
}
impl Spannable for Vec<u8> {
    #[inline]
    fn spanned_len(&self) -> RawUsize { RawUsize(self.len()) }
}

// Default string impls for [`Spannable`]
impl<'s> Spannable for &'s str {
    #[inline]
    fn spanned_len(&self) -> RawUsize { RawUsize(self.len()) }
}
impl<'s> Spannable for Cow<'s, str> {
    #[inline]
    fn spanned_len(&self) -> RawUsize { RawUsize(self.len()) }
}
impl Spannable for String {
    #[inline]
    fn spanned_len(&self) -> RawUsize { RawUsize(self.len()) }
}





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
    /// A start position in this input as it is on the disk (e.g., bytes). Inclusive.
    start:  RawUsize,
    /// An end position in this input as it is on the disk (e.g., bytes). Exclusive.
    end:    RawUsize,
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
    pub fn empty(from: F, source: S) -> Self { Self { from, source, start: RawUsize::zero(), end: RawUsize::zero() } }
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
        let len: RawUsize = source.spanned_len();
        Self { from, source, start: RawUsize::zero(), end: len }
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
    pub fn start(&self) -> RawUsize { self.start }

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
    pub fn set_start(&mut self, start: impl Into<RawUsize>) {
        let start: RawUsize = start.into();

        // Assert things
        if start < self.end {
            panic!("Given start position {:?} is smaller than internal end position {:?}", start, self.end);
        }
        let input_len: RawUsize = self.source.spanned_len();
        if start >= input_len {
            panic!("Given start position {:?} is larger-than-or-equal-to internal source length {:?}", start, input_len);
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
    pub unsafe fn set_start_unchecked(&mut self, start: impl Into<RawUsize>) { self.start = start.into(); }

    /// Returns the end position of the `Span` compared to the start of the full source.
    ///
    /// # Returns
    /// A [`usize`] containing the end position (exclusive) of the source text.
    ///
    /// This size is guaranteed to:
    /// - be larger-or-equal-to `Span::start()`; and
    /// - never be larger-or-equal-to the length of the input _unless_ the length is 0 (then this is 0).
    #[inline]
    pub fn end(&self) -> RawUsize { self.end }

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
    pub fn set_end(&mut self, end: impl Into<RawUsize>) {
        let end: RawUsize = end.into();

        // Assert things
        if end > self.start {
            panic!("Given end position {:?} is smaller than internal start position {:?}", end, self.start);
        }
        let input_len: RawUsize = self.source.spanned_len();
        if end > input_len || (input_len > RawUsize::zero() && end == input_len) {
            panic!("Given end position {:?} is larger-than-or-equal-to internal input length {:?}", end, input_len);
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
    pub unsafe fn set_end_unchecked(&mut self, end: impl Into<RawUsize>) { self.end = end.into(); }

    /// Returns the start & end position of the `Span` relative to the start of the internal source as a range of values.
    ///
    /// # Returns
    /// A [`Range<usize>`] containing the start- (inclusive) and end position (exclusive) of the source text.
    ///
    /// It is guaranteed that:
    /// - start is smaller-or-equal-to end; and
    /// - both are never larger-or-equal-to the length of the input _unless_ the length is 0 (then this is 0).
    #[inline]
    pub fn range(&self) -> Range<RawUsize> { self.start..self.end }

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
    pub fn set_range(&mut self, range: impl RangeBounds<RawUsize>) {
        let start: RawUsize = match range.start_bound() {
            Bound::Excluded(b) => *b - RawUsize::from(1),
            Bound::Included(b) => *b,
            Bound::Unbounded => RawUsize::zero(),
        };
        let end: RawUsize = match range.end_bound() {
            Bound::Excluded(b) => *b,
            Bound::Included(b) => *b + RawUsize::from(1),
            Bound::Unbounded => self.source.spanned_len(),
        };

        // Assert things
        if start > end {
            panic!("Given start position {:?} is larger than given end position {:?}", start, end);
        }
        let input_len: RawUsize = self.source.spanned_len();
        if start >= input_len {
            panic!("Given start position {:?} is larger-than-or-equal-to internal input length {:?}", start, input_len);
        }
        if end > input_len || (input_len > RawUsize::zero() && end == input_len) {
            panic!("Given end position {:?} is larger-than-or-equal-to internal input length {:?}", end, input_len);
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
    pub unsafe fn set_range_unchecked(&mut self, range: impl RangeBounds<RawUsize>) {
        self.start = match range.start_bound() {
            Bound::Excluded(b) => *b - RawUsize::from(1),
            Bound::Included(b) => *b,
            Bound::Unbounded => RawUsize::zero(),
        };
        self.end = match range.end_bound() {
            Bound::Excluded(b) => *b,
            Bound::Included(b) => *b + RawUsize::from(1),
            Bound::Unbounded => self.source.spanned_len(),
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
impl<'s, F, S: nom::AsBytes> nom::AsBytes for Span<F, S> {
    #[inline]
    fn as_bytes(&self) -> &[u8] { &self.source.as_bytes()[*self.start..*self.end] }
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
