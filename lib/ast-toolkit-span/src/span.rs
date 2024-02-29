//  SPAN.rs
//    by Lut99
//
//  Created:
//    15 Dec 2023, 19:05:00
//  Last edited:
//    29 Feb 2024, 14:48:56
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
    /// Maps a given raw index to a logic index.
    ///
    /// # Arguments
    /// - `raw`: The [`RawUsize`] to map.
    ///
    /// # Returns
    /// An equivalent [`LogicUsize`].
    ///
    /// # Panics
    /// This function should panic if `raw` is not on a logic boundary, or is out-of-range.
    fn raw_to_logic(&self, raw: RawUsize) -> LogicUsize;
    /// Maps a given logic index to a raw index.
    ///
    /// # Arguments
    /// - `logic`: The [`LogicUsize`] to map.
    ///
    /// # Returns
    /// An equivalent [`RawUsize`].
    ///
    /// # Panics
    /// This function should panic if `logic` is out-of-range.
    fn logic_to_raw(&self, logic: LogicUsize) -> RawUsize;

    /// Returns the number of currently spanned "raw" items (e.g., bytes).
    ///
    /// # Returns
    /// A [`RawUsize`] with the total number of bytes or other elementary items as is stored on-disk.
    fn raw_len(&self) -> RawUsize;
}

// Default binary impls for [`Spannable`]
impl<'b> Spannable for &'b [u8] {
    #[inline]
    fn raw_to_logic(&self, raw: RawUsize) -> LogicUsize {
        // Maps 1-to-1
        LogicUsize(raw.0)
    }

    #[inline]
    fn logic_to_raw(&self, logic: LogicUsize) -> RawUsize {
        // Maps 1-to-1
        RawUsize(logic.0)
    }

    #[inline]
    fn raw_len(&self) -> RawUsize { RawUsize(self.len()) }
}
impl<'b> Spannable for Cow<'b, [u8]> {
    #[inline]
    fn raw_to_logic(&self, raw: RawUsize) -> LogicUsize {
        // Map 1-to-1
        LogicUsize(raw.0)
    }

    #[inline]
    fn logic_to_raw(&self, logic: LogicUsize) -> RawUsize {
        // Maps 1-to-1
        RawUsize(logic.0)
    }

    #[inline]
    fn raw_len(&self) -> RawUsize { RawUsize(self.len()) }
}
impl Spannable for Vec<u8> {
    #[inline]
    fn raw_to_logic(&self, raw: RawUsize) -> LogicUsize {
        // Map 1-to-1
        LogicUsize(raw.0)
    }

    #[inline]
    fn logic_to_raw(&self, logic: LogicUsize) -> RawUsize {
        // Maps 1-to-1
        RawUsize(logic.0)
    }

    #[inline]
    fn raw_len(&self) -> RawUsize { RawUsize(self.len()) }
}

// Default string impls for [`Spannable`]
impl<'s> Spannable for &'s str {
    #[inline]
    fn raw_to_logic(&self, raw: RawUsize) -> LogicUsize {
        // Search graphemes to find it
        LogicUsize(
            self.grapheme_indices(true)
                .enumerate()
                .find_map(|(i, (b, _))| if RawUsize(b) == raw { Some(i) } else { None })
                .unwrap_or_else(|| panic!("Given raw index {raw:?} is either out-of-bounds or not at a grapheme boundary")),
        )
    }

    #[inline]
    fn logic_to_raw(&self, logic: LogicUsize) -> RawUsize {
        // Search graphemes to find it
        RawUsize(self.grapheme_indices(true).nth(*logic).map(|(i, _)| i).unwrap_or_else(|| panic!("Given logical index {logic:?} is out-of-bounds")))
    }

    #[inline]
    fn raw_len(&self) -> RawUsize { RawUsize(self.len()) }
}
impl<'s> Spannable for Cow<'s, str> {
    #[inline]
    fn raw_to_logic(&self, raw: RawUsize) -> LogicUsize {
        // Search graphemes to find it
        LogicUsize(
            self.grapheme_indices(true)
                .enumerate()
                .find_map(|(i, (b, _))| if RawUsize(b) == raw { Some(i) } else { None })
                .unwrap_or_else(|| panic!("Given raw index {raw:?} is either out-of-bounds or not at a grapheme boundary")),
        )
    }

    #[inline]
    fn logic_to_raw(&self, logic: LogicUsize) -> RawUsize {
        // Search graphemes to find it
        RawUsize(self.grapheme_indices(true).nth(*logic).map(|(i, _)| i).unwrap_or_else(|| panic!("Given logical index {logic:?} is out-of-bounds")))
    }

    #[inline]
    fn raw_len(&self) -> RawUsize { RawUsize(self.len()) }
}
impl Spannable for String {
    #[inline]
    fn raw_to_logic(&self, raw: RawUsize) -> LogicUsize {
        // Search graphemes to find it
        LogicUsize(
            self.grapheme_indices(true)
                .enumerate()
                .find_map(|(i, (b, _))| if RawUsize(b) == raw { Some(i) } else { None })
                .unwrap_or_else(|| panic!("Given raw index {raw:?} is either out-of-bounds or not at a grapheme boundary")),
        )
    }

    #[inline]
    fn logic_to_raw(&self, logic: LogicUsize) -> RawUsize {
        // Search graphemes to find it
        RawUsize(self.grapheme_indices(true).nth(*logic).map(|(i, _)| i).unwrap_or_else(|| panic!("Given logical index {logic:?} is out-of-bounds")))
    }

    #[inline]
    fn raw_len(&self) -> RawUsize { RawUsize(self.len()) }
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
    /// For strings, this would be the byte index. See [`start()`] to get the logical index of start (e.g., grapheme index for strings).
    ///
    /// # Returns
    /// A raw [`usize`] containing the start position (inclusive) of the source text.
    ///
    /// This size is guaranteed to:
    /// - point to a logic boundary (e.g., grapheme indices for strings);
    /// - be smaller-or-equal-to `Span::raw_end()`; and
    /// - never be larger-or-equal-to the length of the input _unless_ the length is 0 (then this is 0).
    #[inline]
    pub fn raw_start(&self) -> RawUsize { self.start }

    /// Returns the end position of the `Span` compared to the start of the full source.
    ///
    /// For strings, this would be the byte index. See [`end()`] to get the logical index of end (e.g., grapheme index for strings).
    ///
    /// # Returns
    /// A [`usize`] containing the end position (exclusive) of the source text.
    ///
    /// This size is guaranteed to:
    /// - point to a logic boundary (e.g., grapheme indices for strings);
    /// - be larger-or-equal-to `Span::raw_start()`; and
    /// - never be larger-or-equal-to the length of the input _unless_ the length is 0 (then this is 0).
    #[inline]
    pub fn raw_end(&self) -> RawUsize { self.end }

    /// Allows mutation of the `start`-position in this `Span`, without having performed checks.
    ///
    /// For strings, this would be the byte index.
    ///
    /// Ensure that your given `start`-value:
    /// - your index points to a logic boundary (e.g., grapheme boundary for strings);
    /// - is not larger than `Span::raw_end()`; and
    /// - is not larger-or-equal-to the length of the input _unless_ the length is 0 (this this must be 0).
    ///
    /// Use `Span::set_raw_start()` if you're not sure your value matches the above and would like to have checks.
    ///
    /// # Arguments
    /// - `start`: The index to which to set the internal start-pointer.
    #[inline]
    pub unsafe fn set_raw_start_unchecked(&mut self, start: impl Into<RawUsize>) { self.start = start.into(); }

    /// Allows mutation of the `end`-position in this `Span`, without having performed checks.
    ///
    /// For strings, this would be the byte index.
    ///
    /// Ensure that your given `end`-value:
    /// - your index points to a logic boundary (e.g., grapheme boundary for strings);
    /// - is not smaller than `Span::raw_start()`; and
    /// - is not larger-or-equal-to the length of the input _unless_ the length is 0 (this this must be 0).
    ///
    /// Use `Span::set_raw_end()` if you're not sure your value matches the above and would like to have checks.
    ///
    /// # Arguments
    /// - `end`: The index to which to set the internal end-pointer.
    #[inline]
    pub unsafe fn set_raw_end_unchecked(&mut self, end: impl Into<RawUsize>) { self.end = end.into(); }
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
        let len: RawUsize = source.raw_len();
        Self { from, source, start: RawUsize::zero(), end: len }
    }

    /// Returns the start position of the `Span` compared to the start of the full source.
    ///
    /// For strings, this would be the grapheme index. See [`raw_start()`] to get the raw index of start (e.g., byte index for strings).
    ///
    /// # Returns
    /// A logical [`usize`] containing the start position (inclusive) of the source text.
    ///
    /// This value is guaranteed to:
    /// - be smaller-or-equal-to `Span::end()`; and
    /// - never be larger-or-equal-to the length of the input _unless_ the length is 0 (then this is 0).
    #[inline]
    pub fn start(&self) -> LogicUsize { self.source.raw_to_logic(self.start) }

    /// Returns the end position of the `Span` compared to the start of the full source.
    ///
    /// For strings, this would be the grapheme index. See [`raw_end()`] to get the raw index of end (e.g., byte index for strings).
    ///
    /// # Returns
    /// A logical [`usize`] containing the end position (exclusive) of the source text.
    ///
    /// This value is guaranteed to:
    /// - be larger-or-equal-to `Span::start()`; and
    /// - never be larger-or-equal-to the length of the input _unless_ the length is 0 (then this is 0).
    #[inline]
    pub fn end(&self) -> LogicUsize { self.source.raw_to_logic(self.end) }

    /// Allows mutation of the `start`-position in this `Span`.
    ///
    /// For strings, this would be the byte index. See [`set_start()`] to set the logical index of start (e.g., grapheme index for strings).
    ///
    /// Some checks are performed before the value is assigned. These checks are:
    /// - asserting the given index falls on a grapheme boundary;
    /// - asserting that the given value is not larger than `Span::raw_end()`;  and
    /// - asserting that the given value is not larger-or-equal-to the length of the input _unless_ the length is 0 (this this must be 0).
    ///
    /// If you are sure your value passes these checks, you can use the unsafe `Span::set_raw_start_unchecked()`.
    ///
    /// # Arguments
    /// - `start`: The index to which to set the internal start-pointer.
    ///
    /// # Panics
    /// This function panics if the given value does not meet any of the assertions.
    #[inline]
    #[track_caller]
    pub fn set_raw_start(&mut self, start: impl Into<RawUsize>) {
        // Model as a range update with own end
        self.set_raw_range(start.into()..self.end)
    }

    /// Allows mutation of the `end`-position in this `Span`.
    ///
    /// For strings, this would be the byte index. See [`set_start()`] to set the logical index of start (e.g., grapheme index for strings).
    ///
    /// Some checks are performed before the value is assigned. These checks are:
    /// - asserting the given index falls on a grapheme boundary;
    /// - asserting that the given value is not smaller than `Span::raw_start()`; and
    /// - asserting that the given value is not larger-or-equal-to the length of the input _unless_ the length is 0 (this this must be 0).
    ///
    /// If you are sure your value passes these checks, you can use the unsafe `Span::set_raw_end_unchecked()`.
    ///
    /// # Arguments
    /// - `end`: The index to which to set the internal end-pointer.
    ///
    /// # Panics
    /// This function panics if the given value does not meet any of the assertions.
    #[inline]
    #[track_caller]
    pub fn set_raw_end(&mut self, end: impl Into<RawUsize>) {
        // Model as a range update with own start
        self.set_raw_range(self.start..end.into())
    }

    /// Allows mutation of both the `start`- and `end`-position in this `Span`.
    ///
    /// For strings, this would be the byte index. See [`set_range()`] to set the logical indices of start and end (e.g., grapheme index for strings).
    ///
    /// Some checks are performed before the value is assigned. These checks are:
    /// - asserting the given indices falls on a grapheme boundary;
    /// - asserting that the given start is not smaller than the given end; and
    /// - asserting that the given values are not larger-or-equal-to the length of the input _unless_ the length is 0 (then they must be 0).
    ///
    /// If you are sure your value passes these checks, you can use the unsafe `Span::set_raw_end_unchecked()`.
    ///
    /// # Arguments
    /// - `range`: The range of start and ends to set.
    ///
    /// # Panics
    /// This function panics if the given value does not meet any of the assertions.
    #[track_caller]
    pub fn set_raw_range(&mut self, range: impl RangeBounds<RawUsize>) {
        // Get the bounds
        let start: RawUsize = match range.start_bound() {
            Bound::Excluded(b) => *b - RawUsize::from(1),
            Bound::Included(b) => *b,
            Bound::Unbounded => RawUsize::zero(),
        };
        let end: RawUsize = match range.end_bound() {
            Bound::Excluded(b) => *b,
            Bound::Included(b) => *b + RawUsize::from(1),
            Bound::Unbounded => self.source.raw_len(),
        };

        // Assert things
        if start > end {
            panic!("Given start position {:?} is larger than given end position {:?}", start, end);
        }
        let input_len: RawUsize = self.source.raw_len();
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

    /// Mutates the start position of the `Span` compared to the start of the full source.
    ///
    /// For strings, this would be the grapheme index. See [`set_raw_start()`] to get the raw index of start (e.g., byte index for strings).
    ///
    /// The input value must:
    /// - be smaller-or-equal-to `Span::end()`; and
    /// - never be larger-or-equal-to the length of the input _unless_ the length is 0 (then this is 0).
    ///
    /// # Arguments
    /// - `start`: A logical [`usize`] defining the start position (inclusive) of the source text.
    ///
    /// # Panics
    /// This function may panic if the given index does not meet any of the above assertions.
    #[inline]
    #[track_caller]
    pub fn set_start(&mut self, start: impl Into<LogicUsize>) { self.set_range(start.into()..self.source.raw_to_logic(self.end)) }

    /// Mutates the end position of the `Span` compared to the start of the full source.
    ///
    /// For strings, this would be the grapheme index. See [`set_raw_start()`] to get the raw index of end (e.g., byte index for strings).
    ///
    /// The input value must:
    /// - be larger-or-equal-to `Span::start()`; and
    /// - never be larger-or-equal-to the length of the input _unless_ the length is 0 (then this is 0).
    ///
    /// # Arguments
    /// - `end`: A logical [`usize`] defining the end position (exclusive) of the source text.
    ///
    /// # Panics
    /// This function may panic if the given index does not meet any of the above assertions.
    #[inline]
    #[track_caller]
    pub fn set_end(&mut self, end: impl Into<LogicUsize>) { self.set_range(self.source.raw_to_logic(self.start)..end.into()) }

    /// Mutates the start- and end position of the `Span` compared to the start of the full source at the same time.
    ///
    /// For strings, this would be the grapheme index. See [`set_raw_start()`] to get the raw index of end (e.g., byte index for strings).
    ///
    /// It must be true that:
    /// - the given start is larger than the given end; and
    /// - both are never larger-or-equal-to the length of the input _unless_ the length is 0 (then they are 0).
    ///
    /// # Arguments
    /// - `range`: A logical [`RangeBounds<LogicUsize>`] defining the start- (inclusive) and end positions (exclusive) in the source text.
    ///
    /// # Panics
    /// This function may panic if any of the given indices does not meet any of the above assertions.
    #[inline]
    #[track_caller]
    pub fn set_range(&mut self, range: impl RangeBounds<LogicUsize>) {
        // Get the bounds
        let start: RawUsize = match range.start_bound() {
            Bound::Excluded(b) => self.source.logic_to_raw(*b - LogicUsize::from(1)),
            Bound::Included(b) => self.source.logic_to_raw(*b),
            Bound::Unbounded => RawUsize::zero(),
        };
        let end: RawUsize = match range.end_bound() {
            Bound::Excluded(b) => self.source.logic_to_raw(*b),
            Bound::Included(b) => self.source.logic_to_raw(*b + LogicUsize::from(1)),
            Bound::Unbounded => self.source.raw_len(),
        };

        // Run as raw set
        self.set_raw_range(start..end)
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
