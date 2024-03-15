//  SPAN.rs
//    by Lut99
//
//  Created:
//    15 Dec 2023, 19:05:00
//  Last edited:
//    15 Mar 2024, 10:45:24
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements a [`Span`], which abstracts over some input to track a particular location in it.
//

use std::borrow::Cow;
use std::error::Error;
use std::fmt::{Display, Formatter, Result as FResult};
use std::hash::{Hash, Hasher};
use std::num::NonZeroUsize;
use std::ops::{Add, AddAssign, Bound, Deref, Range, RangeBounds, Sub, SubAssign};
use std::str::FromStr;

use unicode_segmentation::UnicodeSegmentation;


/***** ERRORS *****/
/// Defines a wrapper around some user-defined [`Error`] to also emit a "cannot convert to UTF-8" error.
#[derive(Debug)]
pub enum FromBytesError<E> {
    /// Could not convert the bytes to UTF-8.
    Utf8 { err: std::str::Utf8Error },
    /// Could not parse otherwise.
    Parse { err: E },
}
impl<E: Display> Display for FromBytesError<E> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        use FromBytesError::*;
        match self {
            Utf8 { .. } => write!(f, "Bytes not valid UTF-8"),
            Parse { err } => err.fmt(f),
        }
    }
}
impl<E: Error> Error for FromBytesError<E> {
    #[inline]
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        use FromBytesError::*;
        match self {
            Utf8 { err } => Some(err),
            Parse { err } => err.source(),
        }
    }
}





/***** AUXILLARY *****/
/// Newtype wrapper that differentiates raw indices (e.g., byte indices) from [logic indices](LogicUsize) (e.g., grapheme indices).
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct RawUsize(pub usize);
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
pub struct LogicUsize(pub usize);
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
    type Slice<'s>: 's
    where
        Self: 's;

    /// Computes a _really_ quick hash for this Spannable.
    ///
    /// Propably just hashes the pointer or something.
    ///
    /// # Arguments
    /// - `hasher`: The [`Hasher`] to hash in.
    fn hash(&self, hasher: &mut impl Hasher);
    /// Asserts this [`Spannable`] refers to the same one as some other [`Spannable`].
    ///
    /// Used to see if comparing two Spans makes sense or not.
    ///
    /// # Arguments
    /// - `other`: Some other `Self` that we want to compare.
    ///
    /// # Returns
    /// True if this is conceptually the same source, or false otherwise.
    fn is_same(&self, other: &Self) -> bool;

    /// Slices this Spannable by raw index.
    ///
    /// # Arguments
    /// - `range`: The range to slice as.
    ///
    /// # Returns
    /// A new instance of type `Self::Slice`, that is self but sliced.
    ///
    /// # Panics
    /// This function panics if out-of-bounds.
    fn slice<'s>(&'s self, range: Range<RawUsize>) -> Self::Slice<'s>;

    /// Maps a given raw index to a logic index.
    ///
    /// # Arguments
    /// - `raw`: The [`RawUsize`] to map.
    ///
    /// # Returns
    /// An equivalent [`LogicUsize`] if the value was in range, or [`None`] if it wasn't.
    ///
    /// # Panics
    /// This function should panic if `raw` is not on a logic boundary.
    fn raw_to_logic(&self, raw: RawUsize) -> Option<LogicUsize>;
    /// Maps a given logic index to a raw index.
    ///
    /// # Arguments
    /// - `logic`: The [`LogicUsize`] to map.
    ///
    /// # Returns
    /// An equivalent [`RawUsize`] if the value was in range. Else, an [`Option<RawUsize>`] is returned, indicating how many more bytes are necessary to bring the logic index within range (or [`None`] if this is unknowable, e.g., for graphemes due to each element being different sizes).
    fn logic_to_raw(&self, logic: LogicUsize) -> Result<RawUsize, Option<RawUsize>>;

    /// Returns the number of currently spanned "raw" items (e.g., bytes).
    ///
    /// # Returns
    /// A [`RawUsize`] with the total number of bytes or other elementary items as is stored on-disk.
    fn raw_len(&self) -> RawUsize;
}

// Default binary impls for [`Spannable`]
impl<'b> Spannable for &'b [u8] {
    type Slice<'s> = &'s [u8] where Self: 's;

    #[inline]
    fn hash(&self, hasher: &mut impl Hasher) { hasher.write_usize(self.as_ptr() as usize); }

    #[inline]
    fn is_same(&self, other: &Self) -> bool { std::ptr::eq(self, other) }

    #[inline]
    fn slice<'s>(&'s self, range: Range<RawUsize>) -> Self::Slice<'s> { &self[range.start.0..range.end.0] }

    #[inline]
    fn raw_to_logic(&self, raw: RawUsize) -> Option<LogicUsize> {
        // Maps 1-to-1 if within range
        if raw.0 < self.len() { Some(LogicUsize(raw.0)) } else { None }
    }

    #[inline]
    fn logic_to_raw(&self, logic: LogicUsize) -> Result<RawUsize, Option<RawUsize>> {
        // Maps 1-to-1 if within range
        if logic.0 < self.len() { Ok(RawUsize(logic.0)) } else { Err(Some(RawUsize(1 + self.len() - logic.0))) }
    }

    #[inline]
    fn raw_len(&self) -> RawUsize { RawUsize(self.len()) }
}
impl<'b> Spannable for Cow<'b, [u8]> {
    type Slice<'s> = Cow<'s, [u8]> where Self: 's;

    #[inline]
    fn hash(&self, hasher: &mut impl Hasher) {
        match self {
            Cow::Borrowed(b) => hasher.write_usize(b.as_ptr() as usize),
            Cow::Owned(o) => <Vec<u8> as Hash>::hash(o, hasher),
        }
    }

    #[inline]
    fn is_same(&self, other: &Self) -> bool { std::ptr::eq(self, other) }

    #[inline]
    fn slice<'s>(&'s self, range: Range<RawUsize>) -> Self::Slice<'s> { Cow::Borrowed(&self[range.start.0..range.end.0]) }

    #[inline]
    fn raw_to_logic(&self, raw: RawUsize) -> Option<LogicUsize> {
        // Maps 1-to-1 if within range
        if raw.0 < self.len() { Some(LogicUsize(raw.0)) } else { None }
    }

    #[inline]
    fn logic_to_raw(&self, logic: LogicUsize) -> Result<RawUsize, Option<RawUsize>> {
        // Maps 1-to-1 if within range
        if logic.0 < self.len() { Ok(RawUsize(logic.0)) } else { Err(Some(RawUsize(1 + self.len() - logic.0))) }
    }

    #[inline]
    fn raw_len(&self) -> RawUsize { RawUsize(self.len()) }
}
impl Spannable for Vec<u8> {
    type Slice<'s> = Vec<u8> where Self: 's;

    #[inline]
    fn hash(&self, hasher: &mut impl Hasher) { <Vec<u8> as Hash>::hash(self, hasher) }

    #[inline]
    fn is_same(&self, other: &Self) -> bool { std::ptr::eq(self, other) }

    #[inline]
    fn slice<'s>(&'s self, range: Range<RawUsize>) -> Self::Slice<'s> { self[range.start.0..range.end.0].to_vec() }

    #[inline]
    fn raw_to_logic(&self, raw: RawUsize) -> Option<LogicUsize> {
        // Maps 1-to-1 if within range
        if raw.0 < self.len() { Some(LogicUsize(raw.0)) } else { None }
    }

    #[inline]
    fn logic_to_raw(&self, logic: LogicUsize) -> Result<RawUsize, Option<RawUsize>> {
        // Maps 1-to-1 if within range
        if logic.0 < self.len() { Ok(RawUsize(logic.0)) } else { Err(Some(RawUsize(1 + self.len() - logic.0))) }
    }

    #[inline]
    fn raw_len(&self) -> RawUsize { RawUsize(self.len()) }
}

// Default string impls for [`Spannable`]
impl<'s> Spannable for &'s str {
    type Slice<'s2> = &'s2 str where Self: 's2;

    #[inline]
    fn hash(&self, hasher: &mut impl Hasher) { hasher.write_usize(self.as_ptr() as usize); }

    #[inline]
    fn is_same(&self, other: &Self) -> bool { std::ptr::eq(self, other) }

    #[inline]
    fn slice<'s2>(&'s2 self, range: Range<RawUsize>) -> Self::Slice<'s2> { &self[range.start.0..range.end.0] }

    #[inline]
    fn raw_to_logic(&self, raw: RawUsize) -> Option<LogicUsize> {
        if raw.0 >= self.len() {
            return None;
        }
        Some(LogicUsize(
            self.grapheme_indices(true)
                .enumerate()
                .find_map(|(i, (b, _))| if RawUsize(b) == raw { Some(i) } else { None })
                .unwrap_or_else(|| panic!("Given raw index {raw:?} is not at a grapheme boundary")),
        ))
    }

    #[inline]
    fn logic_to_raw(&self, logic: LogicUsize) -> Result<RawUsize, Option<RawUsize>> {
        // Search graphemes to find it
        self.grapheme_indices(true).enumerate().find_map(|(i, (b, _))| if LogicUsize(i) == logic { Some(RawUsize(b)) } else { None }).ok_or(None)
    }

    #[inline]
    fn raw_len(&self) -> RawUsize { RawUsize(self.len()) }
}
impl<'s> Spannable for Cow<'s, str> {
    type Slice<'s2> = Cow<'s2, str> where Self: 's2;

    #[inline]
    fn hash(&self, hasher: &mut impl Hasher) {
        match self {
            Cow::Borrowed(b) => hasher.write_usize(b.as_ptr() as usize),
            Cow::Owned(o) => <String as Hash>::hash(o, hasher),
        }
    }

    #[inline]
    fn is_same(&self, other: &Self) -> bool { std::ptr::eq(self, other) }

    #[inline]
    fn slice<'s2>(&'s2 self, range: Range<RawUsize>) -> Self::Slice<'s2> { Cow::Borrowed(&self[range.start.0..range.end.0]) }

    #[inline]
    fn raw_to_logic(&self, raw: RawUsize) -> Option<LogicUsize> {
        if raw.0 >= self.len() {
            return None;
        }
        Some(LogicUsize(
            self.grapheme_indices(true)
                .enumerate()
                .find_map(|(i, (b, _))| if RawUsize(b) == raw { Some(i) } else { None })
                .unwrap_or_else(|| panic!("Given raw index {raw:?} is not at a grapheme boundary")),
        ))
    }

    #[inline]
    fn logic_to_raw(&self, logic: LogicUsize) -> Result<RawUsize, Option<RawUsize>> {
        // Search graphemes to find it
        self.grapheme_indices(true).enumerate().find_map(|(i, (b, _))| if LogicUsize(i) == logic { Some(RawUsize(b)) } else { None }).ok_or(None)
    }

    #[inline]
    fn raw_len(&self) -> RawUsize { RawUsize(self.len()) }
}
impl Spannable for String {
    type Slice<'s2> = String where Self: 's2;

    #[inline]
    fn hash(&self, hasher: &mut impl Hasher) { <String as Hash>::hash(self, hasher) }

    #[inline]
    fn is_same(&self, other: &Self) -> bool { std::ptr::eq(self, other) }

    #[inline]
    fn slice<'s2>(&'s2 self, range: Range<RawUsize>) -> Self::Slice<'s2> { self[range.start.0..range.end.0].to_string() }

    #[inline]
    fn raw_to_logic(&self, raw: RawUsize) -> Option<LogicUsize> {
        if raw.0 >= self.len() {
            return None;
        }
        Some(LogicUsize(
            self.grapheme_indices(true)
                .enumerate()
                .find_map(|(i, (b, _))| if RawUsize(b) == raw { Some(i) } else { None })
                .unwrap_or_else(|| panic!("Given raw index {raw:?} is not at a grapheme boundary")),
        ))
    }

    #[inline]
    fn logic_to_raw(&self, logic: LogicUsize) -> Result<RawUsize, Option<RawUsize>> {
        // Search graphemes to find it
        self.grapheme_indices(true).enumerate().find_map(|(i, (b, _))| if LogicUsize(i) == logic { Some(RawUsize(b)) } else { None }).ok_or(None)
    }

    #[inline]
    fn raw_len(&self) -> RawUsize { RawUsize(self.len()) }
}



/// An abstraction over a [`Spannable`] that marks that it can be parsed as a string.
#[cfg(feature = "nom")]
pub trait ParsableSpannable<R>: Spannable {
    /// The error returned when parsing fails.
    type Error: Error;

    /// Attempts to parse this Spannable as an `R`.
    ///
    /// # Returns
    /// A parsed object of type `R`.
    ///
    /// # Errors
    /// This function errors if it failed to run the parse function.
    fn parse(&self) -> Result<R, Self::Error>;
}

// Default binary impls for [`ParsableSpannable`]
#[cfg(feature = "nom")]
impl<'b, R> ParsableSpannable<R> for &'b [u8]
where
    R: FromStr,
    R::Err: Error,
{
    type Error = FromBytesError<R::Err>;

    #[inline]
    fn parse(&self) -> Result<R, Self::Error> {
        // Assert the bytes are UTF-8
        let text: &str = std::str::from_utf8(self).map_err(|err| FromBytesError::<R::Err>::Utf8 { err })?;
        R::from_str(text).map_err(|err| FromBytesError::Parse { err })
    }
}
#[cfg(feature = "nom")]
impl<'b, R> ParsableSpannable<R> for Cow<'b, [u8]>
where
    R: FromStr,
    R::Err: Error,
{
    type Error = FromBytesError<R::Err>;

    #[inline]
    fn parse(&self) -> Result<R, Self::Error> {
        // Assert the bytes are UTF-8
        let text: &str = std::str::from_utf8(self).map_err(|err| FromBytesError::<R::Err>::Utf8 { err })?;
        R::from_str(text).map_err(|err| FromBytesError::Parse { err })
    }
}
#[cfg(feature = "nom")]
impl<R> ParsableSpannable<R> for Vec<u8>
where
    R: FromStr,
    R::Err: Error,
{
    type Error = FromBytesError<R::Err>;

    #[inline]
    fn parse(&self) -> Result<R, Self::Error> {
        // Assert the bytes are UTF-8
        let text: &str = std::str::from_utf8(self).map_err(|err| FromBytesError::<R::Err>::Utf8 { err })?;
        R::from_str(text).map_err(|err| FromBytesError::Parse { err })
    }
}

// Default string impls for [`ParsableSpannable`]
#[cfg(feature = "nom")]
impl<'s, R> ParsableSpannable<R> for &'s str
where
    R: FromStr,
    R::Err: Error,
{
    type Error = R::Err;

    #[inline]
    fn parse(&self) -> Result<R, Self::Error> { R::from_str(self) }
}
#[cfg(feature = "nom")]
impl<'s, R> ParsableSpannable<R> for Cow<'s, str>
where
    R: FromStr,
    R::Err: Error,
{
    type Error = R::Err;

    #[inline]
    fn parse(&self) -> Result<R, Self::Error> { R::from_str(self) }
}
#[cfg(feature = "nom")]
impl<R> ParsableSpannable<R> for String
where
    R: FromStr,
    R::Err: Error,
{
    type Error = R::Err;

    #[inline]
    fn parse(&self) -> Result<R, Self::Error> { R::from_str(self) }
}



/// An abstraction over a [`Spannable`] that also makes it iterable.
#[cfg(feature = "nom")]
pub trait SpannableLogicIter: Spannable {
    /// The logic item that is iterated over.
    type Item<'i>
    where
        Self: 'i;
    /// Iterator over logic items.
    type Iter<'i>: Iterator<Item = Self::Item<'i>>
    where
        Self: 'i;
    /// Iterator over logic items and their raw indices.
    type IterIndices<'i>: Iterator<Item = (RawUsize, Self::Item<'i>)>
    where
        Self: 'i;


    /// Returns an iterator over the logical elements in this [`Spannable`].
    ///
    /// # Arguments
    /// - `range`: A range that determines which span of ourselves to iterate over.
    ///
    /// # Returns
    /// An iterator of type [`Self::Iter`](SpannableLogicIter::Iter) that produces elements of type [`Self::Item`](SpannableLogicIter::Item).
    fn spanned_iter_elems<'s>(&'s self, range: Range<RawUsize>) -> Self::Iter<'s>;

    /// Returns an iterator over the logical elements with their raw offsets in this [`Spannable`].
    ///
    /// # Arguments
    /// - `range`: A range that determines which span of ourselves to iterate over.
    ///
    /// # Returns
    /// An iterator of type [`Self::Iter`](SpannableLogicIter::Iter) that produces elements of type ([`RawUsize`], [`Self::Item`](SpannableLogicIter::Item)).
    fn spanned_iter_indices<'s>(&'s self, range: Range<RawUsize>) -> Self::IterIndices<'s>;
}

// Default binary impls for [`SpannableLogicIter`]
#[cfg(feature = "nom")]
impl<'b> SpannableLogicIter for &'b [u8] {
    type Item<'i> = u8 where Self: 'i;
    type Iter<'i> = std::iter::Map<std::slice::Iter<'i, u8>, fn(&'i u8) -> u8> where Self: 'i;
    type IterIndices<'i> = std::iter::Map<std::iter::Enumerate<std::slice::Iter<'i, u8>>, fn((usize, &'i u8)) -> (RawUsize, u8)> where Self: 'i;

    #[inline]
    fn spanned_iter_elems<'s>(&'s self, range: Range<RawUsize>) -> Self::Iter<'s> { self[range.start.0..range.end.0].iter().map(|b| *b) }

    #[inline]
    fn spanned_iter_indices<'s>(&'s self, range: Range<RawUsize>) -> Self::IterIndices<'s> {
        self[range.start.0..range.end.0].iter().enumerate().map(|(i, b)| (RawUsize(i), *b))
    }
}
#[cfg(feature = "nom")]
impl<'b> SpannableLogicIter for Cow<'b, [u8]> {
    type Item<'i> = u8 where Self: 'i;
    type Iter<'i> = std::iter::Map<std::slice::Iter<'i, u8>, fn(&'i u8) -> u8> where Self: 'i;
    type IterIndices<'i> = std::iter::Map<std::iter::Enumerate<std::slice::Iter<'i, u8>>, fn((usize, &'i u8)) -> (RawUsize, u8)> where Self: 'i;

    #[inline]
    fn spanned_iter_elems<'s>(&'s self, range: Range<RawUsize>) -> Self::Iter<'s> { self[range.start.0..range.end.0].iter().map(|b| *b) }

    #[inline]
    fn spanned_iter_indices<'s>(&'s self, range: Range<RawUsize>) -> Self::IterIndices<'s> {
        self[range.start.0..range.end.0].iter().enumerate().map(|(i, b)| (RawUsize(i), *b))
    }
}
#[cfg(feature = "nom")]
impl SpannableLogicIter for Vec<u8> {
    type Item<'i> = u8 where Self: 'i;
    type Iter<'i> = std::iter::Map<std::slice::Iter<'i, u8>, fn(&'i u8) -> u8> where Self: 'i;
    type IterIndices<'i> = std::iter::Map<std::iter::Enumerate<std::slice::Iter<'i, u8>>, fn((usize, &'i u8)) -> (RawUsize, u8)> where Self: 'i;

    #[inline]
    fn spanned_iter_elems<'s>(&'s self, range: Range<RawUsize>) -> Self::Iter<'s> { self[range.start.0..range.end.0].iter().map(|b| *b) }

    #[inline]
    fn spanned_iter_indices<'s>(&'s self, range: Range<RawUsize>) -> Self::IterIndices<'s> {
        self[range.start.0..range.end.0].iter().enumerate().map(|(i, b)| (RawUsize(i), *b))
    }
}

// Default string impls for [`SpannableLogicIter`]
#[cfg(feature = "nom")]
impl<'s> SpannableLogicIter for &'s str {
    type Item<'i> = &'i str where Self: 'i;
    type Iter<'i> = unicode_segmentation::Graphemes<'i> where Self: 'i;
    type IterIndices<'i> = std::iter::Map<unicode_segmentation::GraphemeIndices<'i>, fn((usize, &'i str)) -> (RawUsize, &'i str)> where Self: 'i;

    #[inline]
    fn spanned_iter_elems<'s2>(&'s2 self, range: Range<RawUsize>) -> Self::Iter<'s2> { self[range.start.0..range.end.0].graphemes(true) }

    #[inline]
    fn spanned_iter_indices<'s2>(&'s2 self, range: Range<RawUsize>) -> Self::IterIndices<'s2> {
        self[range.start.0..range.end.0].grapheme_indices(true).map(|(i, b)| (RawUsize(i), b))
    }
}
#[cfg(feature = "nom")]
impl<'s> SpannableLogicIter for Cow<'s, str> {
    type Item<'i> = &'i str where Self: 'i;
    type Iter<'i> = unicode_segmentation::Graphemes<'i> where Self: 'i;
    type IterIndices<'i> = std::iter::Map<unicode_segmentation::GraphemeIndices<'i>, fn((usize, &'i str)) -> (RawUsize, &'i str)> where Self: 'i;

    #[inline]
    fn spanned_iter_elems<'s2>(&'s2 self, range: Range<RawUsize>) -> Self::Iter<'s2> { self[range.start.0..range.end.0].graphemes(true) }

    #[inline]
    fn spanned_iter_indices<'s2>(&'s2 self, range: Range<RawUsize>) -> Self::IterIndices<'s2> {
        self[range.start.0..range.end.0].grapheme_indices(true).map(|(i, b)| (RawUsize(i), b))
    }
}
#[cfg(feature = "nom")]
impl<'s> SpannableLogicIter for String {
    type Item<'i> = &'i str where Self: 'i;
    type Iter<'i> = unicode_segmentation::Graphemes<'i> where Self: 'i;
    type IterIndices<'i> = std::iter::Map<unicode_segmentation::GraphemeIndices<'i>, fn((usize, &'i str)) -> (RawUsize, &'i str)> where Self: 'i;

    #[inline]
    fn spanned_iter_elems<'s2>(&'s2 self, range: Range<RawUsize>) -> Self::Iter<'s2> { self[range.start.0..range.end.0].graphemes(true) }

    #[inline]
    fn spanned_iter_indices<'s2>(&'s2 self, range: Range<RawUsize>) -> Self::IterIndices<'s2> {
        self[range.start.0..range.end.0].grapheme_indices(true).map(|(i, b)| (RawUsize(i), b))
    }
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

    /// Constructor for the `Span` that initializes it with the given raw range (e.g., byte indices).
    ///
    /// # Arguments
    /// - `from`: Some description of the input wrapped (e.g., a filename).
    /// - `source`: The input source (text) to wrap.
    /// - `range`: A [`RangeBound`] that denotes the range to span.
    ///
    /// # Returns
    /// A new `Span` that spans the given `range` of the given `source`.
    ///
    /// # Panics
    /// This function panics if:
    /// - The range is invalid (start > end); or
    /// - Either side of the `range` is out-of-bounds for the `source`.
    #[inline]
    #[track_caller]
    pub fn ranged(from: F, source: S, range: impl RangeBounds<LogicUsize>) -> Self {
        let mut span: Self = Self::empty(from, source);
        span.set_range(range);
        span
    }

    /// Constructor for the `Span` that initializes it with the given logical range (e.g., character indices).
    ///
    /// If you are sure your range fits the conditions below, then you could consider using the unsafe `Self::raw_ranged_unchecked()`.
    ///
    /// # Arguments
    /// - `from`: Some description of the input wrapped (e.g., a filename).
    /// - `source`: The input source (text) to wrap.
    /// - `range`: A [`RangeBound`] that denotes the range to span.
    ///
    /// # Returns
    /// A new `Span` that spans the given `range` of the given `source`.
    ///
    /// # Panics
    /// This function panics if:
    /// - The range is invalid (start > end); or
    /// - Either side of the `range` is out-of-bounds for the `source`.
    #[inline]
    #[track_caller]
    pub fn raw_ranged(from: F, source: S, range: impl RangeBounds<RawUsize>) -> Self {
        let mut span: Self = Self::empty(from, source);
        span.set_raw_range(range);
        span
    }

    /// Provides access to the internal `source`-string, but only the spanned area.
    ///
    /// # Returns
    /// A reference to the internal `source`-string.
    #[inline]
    pub fn spanned<'s>(&'s self) -> S::Slice<'s> { self.source.slice(self.start..self.end) }

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
    pub fn start(&self) -> LogicUsize {
        self.source.raw_to_logic(self.start).unwrap_or_else(|| panic!("Internal byte index is not aligned on a logic boundary"))
    }

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
    pub fn end(&self) -> LogicUsize {
        self.source.raw_to_logic(self.end).unwrap_or_else(|| panic!("Internal byte index is not aligned on a logic boundary"))
    }

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
    pub fn set_start(&mut self, start: impl Into<LogicUsize>) {
        self.set_range(
            start.into()..self.source.raw_to_logic(self.end).unwrap_or_else(|| panic!("Internal byte index is not aligned on a logic boundary")),
        )
    }

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
    pub fn set_end(&mut self, end: impl Into<LogicUsize>) {
        self.set_range(
            self.source.raw_to_logic(self.start).unwrap_or_else(|| panic!("Internal byte index is not aligned on a logic boundary"))..end.into(),
        )
    }

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
            Bound::Excluded(b) => {
                self.source.logic_to_raw(*b - LogicUsize::from(1)).unwrap_or_else(|_| panic!("Given index is out-of-range for span"))
            },
            Bound::Included(b) => self.source.logic_to_raw(*b).unwrap_or_else(|_| panic!("Given index is out-of-range for span")),
            Bound::Unbounded => RawUsize::zero(),
        };
        let end: RawUsize = match range.end_bound() {
            Bound::Excluded(b) => self.source.logic_to_raw(*b).unwrap_or_else(|_| panic!("Given index is out-of-range for span")),
            Bound::Included(b) => {
                self.source.logic_to_raw(*b + LogicUsize::from(1)).unwrap_or_else(|_| panic!("Given index is out-of-range for span"))
            },
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
impl<F, S: Spannable> Eq for Span<F, S> {}
impl<F, S: Spannable> Hash for Span<F, S> {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.source.hash(state);
        self.start.hash(state);
        self.end.hash(state);
    }
}
impl<F, S: Spannable> PartialEq for Span<F, S> {
    #[inline]
    fn eq(&self, other: &Self) -> bool { self.source.is_same(&other.source) && self.start == other.start && self.end == other.end }
}

#[cfg(feature = "nom")]
impl<F, S: nom::AsBytes> nom::AsBytes for Span<F, S> {
    #[inline]
    fn as_bytes(&self) -> &[u8] { &self.source.as_bytes()[*self.start..*self.end] }
}
#[cfg(feature = "nom")]
impl<F, S1, S2> nom::Compare<S2> for Span<F, S1>
where
    S1: Spannable,
    for<'s> S1::Slice<'s>: nom::Compare<S2>,
{
    #[inline]
    fn compare(&self, t: S2) -> nom::CompareResult { self.spanned().compare(t) }

    #[inline]
    fn compare_no_case(&self, t: S2) -> nom::CompareResult { self.spanned().compare_no_case(t) }
}
#[cfg(feature = "nom")]
impl<F, S, E, I> nom::ExtendInto for Span<F, S>
where
    S: Spannable,
    for<'s> S::Slice<'s>: nom::ExtendInto<Extender = E, Item = I>,
{
    type Extender = E;
    type Item = I;

    #[inline]
    fn new_builder(&self) -> Self::Extender { self.spanned().new_builder() }

    #[inline]
    fn extend_into(&self, acc: &mut Self::Extender) { self.spanned().extend_into(acc) }
}
#[cfg(feature = "nom")]
impl<F, S1, S2> nom::FindSubstring<S2> for Span<F, S1>
where
    S1: Spannable,
    for<'s> S1::Slice<'s>: nom::FindSubstring<S2>,
{
    #[inline]
    fn find_substring(&self, substr: S2) -> Option<usize> { self.spanned().find_substring(substr) }
}
#[cfg(feature = "nom")]
impl<F, S, T> nom::FindToken<T> for Span<F, S>
where
    S: Spannable,
    for<'s> S::Slice<'s>: nom::FindToken<T>,
{
    #[inline]
    fn find_token(&self, token: T) -> bool { self.spanned().find_token(token) }
}
#[cfg(feature = "nom")]
impl<F, S, I1, I2, I3> nom::InputIter for Span<F, S>
where
    for<'s> S: 's + SpannableLogicIter<Item<'s> = I1, IterIndices<'s> = I2, Iter<'s> = I3>,
    for<'s> S::Slice<'s>: Spannable,
    I2: Iterator<Item = (usize, I1)>,
    I3: Iterator<Item = I1>,
{
    type Item = I1;
    type Iter = I2;
    type IterElem = I3;

    #[inline]
    fn iter_indices(&self) -> Self::Iter { self.source.spanned_iter_indices(self.start..self.end) }

    #[inline]
    fn iter_elements(&self) -> Self::IterElem { self.source.spanned_iter_elems(self.start..self.end) }

    #[inline]
    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.source.spanned_iter_indices(self.start..self.end).find_map(|(i, e)| if predicate(e) { Some(i) } else { None })
    }

    #[inline]
    fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
        self.spanned()
            .logic_to_raw(LogicUsize(count))
            .map(|i| *i)
            .map_err(|needed| if let Some(count) = needed { nom::Needed::Size(NonZeroUsize::new(count.0).unwrap()) } else { nom::Needed::Unknown })
    }
}
#[cfg(feature = "nom")]
impl<F, S> nom::InputLength for Span<F, S> {
    #[inline]
    fn input_len(&self) -> usize { self.end.0 - self.start.0 }
}
#[cfg(feature = "nom")]
impl<F, S> nom::InputTake for Span<F, S>
where
    F: Clone,
    S: Clone,
{
    fn take(&self, count: usize) -> Self {
        // Assert count is within range
        let count: RawUsize = count.into();
        if self.start + count >= self.end {
            panic!("Given count of {:?} is out-of-range for Span of {:?} bytes", count, self.end - self.start);
        }

        // Return the spanned area
        Self { from: self.from.clone(), source: self.source.clone(), start: self.start, end: self.start + count }
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        // Get the first split
        let first: Self = self.take(count);

        // Return it + the second split
        (first, Self { from: self.from.clone(), source: self.source.clone(), start: self.start + RawUsize(count), end: self.end })
    }
}
#[cfg(feature = "nom")]
impl<F, S, I> nom::InputTakeAtPosition for Span<F, S>
where
    F: Clone,
    for<'s> S: 's + Clone + SpannableLogicIter<Item<'s> = I>,
{
    type Item = I;

    fn split_at_position<P, E: nom::error::ParseError<Self>>(&self, predicate: P) -> nom::IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        // Find the index using the `SpannableLogicIter`-implementation.
        let split: RawUsize =
            match self.source.spanned_iter_indices(self.start..self.end).find_map(|(i, c)| if predicate(c) { Some(i) } else { None }) {
                Some(split) => split,
                None => return Err(nom::Err::Incomplete(nom::Needed::Unknown)),
            };

        // Split ourselves according to this
        Ok((
            // Remainder (i.e., second half)
            Self { from: self.from.clone(), source: self.source.clone(), start: self.start + split, end: self.end },
            // Parsed (i.e., first half)
            Self { from: self.from.clone(), source: self.source.clone(), start: self.start, end: self.start + split },
        ))
    }

    fn split_at_position1<P, E: nom::error::ParseError<Self>>(&self, predicate: P, e: nom::error::ErrorKind) -> nom::IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        // Find the index using the `SpannableLogicIter`-implementation.
        let split: RawUsize =
            match self.source.spanned_iter_indices(self.start..self.end).find_map(|(i, c)| if predicate(c) { Some(i) } else { None }) {
                // Change compared to `split_at_position`: no first one allowed!
                Some(RawUsize(0)) => return Err(nom::Err::Error(E::from_error_kind(self.clone(), e))),
                Some(split) => split,
                None => return Err(nom::Err::Incomplete(nom::Needed::Unknown)),
            };

        // Split ourselves according to this
        Ok((
            // Remainder (i.e., second half)
            Self { from: self.from.clone(), source: self.source.clone(), start: self.start + split, end: self.end },
            // Parsed (i.e., first half)
            Self { from: self.from.clone(), source: self.source.clone(), start: self.start, end: self.start + split },
        ))
    }

    fn split_at_position1_complete<P, E: nom::error::ParseError<Self>>(&self, predicate: P, e: nom::error::ErrorKind) -> nom::IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        // Find the index using the `SpannableLogicIter`-implementation.
        let split: RawUsize =
            match self.source.spanned_iter_indices(self.start..self.end).find_map(|(i, c)| if predicate(c) { Some(i) } else { None }) {
                // Change compared to `split_at_position_complete()`: no first one allowed!
                Some(RawUsize(0)) => return Err(nom::Err::Error(E::from_error_kind(self.clone(), e))),
                Some(split) => split,
                // If not found, we do not split at all
                None => {
                    return Ok((
                        // Remainder (i.e., nothing)
                        Self { from: self.from.clone(), source: self.source.clone(), start: RawUsize::zero(), end: RawUsize::zero() },
                        // Parsed (i.e., everything)
                        Self { from: self.from.clone(), source: self.source.clone(), start: self.start, end: self.end },
                    ));
                },
            };

        // Split ourselves according to this
        Ok((
            // Remainder (i.e., second half)
            Self { from: self.from.clone(), source: self.source.clone(), start: self.start + split, end: self.end },
            // Parsed (i.e., first half)
            Self { from: self.from.clone(), source: self.source.clone(), start: self.start, end: self.start + split },
        ))
    }

    fn split_at_position_complete<P, E: nom::error::ParseError<Self>>(&self, predicate: P) -> nom::IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        // Find the index using the `SpannableLogicIter`-implementation.
        let split: RawUsize =
            match self.source.spanned_iter_indices(self.start..self.end).find_map(|(i, c)| if predicate(c) { Some(i) } else { None }) {
                Some(split) => split,
                // If not found, we do not split at all
                None => {
                    return Ok((
                        // Remainder (i.e., nothing)
                        Self { from: self.from.clone(), source: self.source.clone(), start: RawUsize::zero(), end: RawUsize::zero() },
                        // Parsed (i.e., everything)
                        Self { from: self.from.clone(), source: self.source.clone(), start: self.start, end: self.end },
                    ));
                },
            };

        // Split ourselves according to this
        Ok((
            // Remainder (i.e., second half)
            Self { from: self.from.clone(), source: self.source.clone(), start: self.start + split, end: self.end },
            // Parsed (i.e., first half)
            Self { from: self.from.clone(), source: self.source.clone(), start: self.start, end: self.start + split },
        ))
    }
}
#[cfg(feature = "nom")]
impl<F, S: Spannable + nom::Offset> nom::Offset for Span<F, S> {
    #[inline]
    fn offset(&self, second: &Self) -> usize {
        // Assert they have the same source
        if !self.source.is_same(&second.source) {
            panic!("No point in computing offset of Spans with different sources");
        }

        // Then compute the offset
        self.source.offset(&second.source)
    }
}
#[cfg(feature = "nom")]
impl<F, S, R> nom::ParseTo<R> for Span<F, S>
where
    S: Spannable,
    for<'s> S::Slice<'s>: ParsableSpannable<R>,
{
    #[inline]
    fn parse_to(&self) -> Option<R> { self.spanned().parse().ok() }
}
#[cfg(feature = "nom")]
impl<F, S, R> nom::Slice<R> for Span<F, S>
where
    for<'s> S: 's + Spannable<Slice<'s> = Self>,
    R: RangeBounds<usize>,
{
    #[inline]
    fn slice(&self, range: R) -> Self {
        // Translate the range
        let span: Self = self.spanned();
        let start: RawUsize = match range.start_bound() {
            Bound::Excluded(b) => span.source.logic_to_raw(LogicUsize(*b - 1)).unwrap_or_else(|_| panic!("Given index is out-of-range for span")),
            Bound::Included(b) => span.source.logic_to_raw(LogicUsize(*b)).unwrap_or_else(|_| panic!("Given index is out-of-range for span")),
            Bound::Unbounded => RawUsize::zero(),
        };
        let end: RawUsize = match range.end_bound() {
            Bound::Excluded(b) => span.source.logic_to_raw(LogicUsize(*b)).unwrap_or_else(|_| panic!("Given index is out-of-range for span")),
            Bound::Included(b) => {
                span.source.logic_to_raw(LogicUsize::from(*b + 1)).unwrap_or_else(|_| panic!("Given index is out-of-range for span"))
            },
            Bound::Unbounded => span.source.raw_len(),
        };

        // Run
        span.source.slice(start..end)
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
