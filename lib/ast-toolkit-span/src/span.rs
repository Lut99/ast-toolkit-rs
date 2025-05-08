//  SPAN 2.rs
//    by Lut99
//
//  Created:
//    14 Mar 2025, 16:51:07
//  Last edited:
//    08 May 2025, 11:40:49
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the `ast-toolkit`'s [`Span`], which forms the basis of
//!   many other of the crates.
//

use std::cmp::Ordering;
use std::fmt::{Debug, Formatter, Result as FResult};
use std::hash::{Hash, Hasher};

use crate::range::Range;
use crate::spannable::{Spannable, SpannableUtf8};


/***** LIBRARY *****/
/// Defines an abstraction over a part of a source file.
///
/// A source file is abstracted as a vector of objects, where, for the Span to work, we care as
/// least as possible about what these objects are.
///
/// Examples of vectors are:
/// - Bytes arrays.
/// - Strings (text files).
/// - Vectors of tokens.
///
/// # Generics
/// - `S`: The type of the internal source array. Usually, you would want this type to be very
///   cheaply [`Clone`]able (e.g., a reference or something like [`Rc`](std::rc::Rc)).
///   
///   Optionally, you can also give a tuple of [some type](Spannable::SourceId) and a [`Spannable`]
///   object to separate the identifier of the source from the source itself. Note, though, that
///   the whole type should _still_ be cheaply clonable, _and_ that the concrete value of the
///   source MUST be unique of the source text (e.g., hash-like properties).
///
/// # Examples
/// ```rust
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("howdy");
/// assert_eq!(span1.value(), "howdy");
///
/// let span2 = Span::ranged(("<example>", "Hello, world!"), ..5);
/// assert_eq!(span2.value(), "Hello");
/// assert_eq!(span2.source(), &("<example>", "Hello, world!"));
/// assert_eq!(span2.source_id(), "<example>");
/// ```
#[derive(Clone, Copy)]
pub struct Span<S> {
    /// The source array itself.
    source: S,
    /// The current slice of that array.
    ///
    /// Give as a [`Range`] to 1) make it [`Copy`]able, and 2) encode different states (e.g.,
    /// explicitly empty).
    range:  Range,
}

// Constructors
impl<'s, S: Spannable<'s>> Span<S> {
    /// Constructor for the Span that initializes it to span the given array.
    ///
    /// # Arguments
    /// - `source`: The [`Spannable`] source array that will be spanned.
    ///
    /// # Returns
    /// A new Span that spans the entire `source`.
    #[inline]
    pub fn new(source: S) -> Self { Self::ranged(source, Range::full()) }

    /// Constructor for the Span that initializes to around the given source array, but spans it
    /// around a specific slice.
    ///
    /// # Arguments
    /// - `source`: The [`Spannable`] source array that will be spanned.
    /// - `range`: A [`Range`] that describes the slice to span.
    ///
    /// # Returns
    /// A new Span that spans the given `range` of the given `source`.
    #[inline]
    pub fn ranged(source: S, range: impl Into<Range>) -> Self { Self { source, range: range.into() } }

    /// Constructor for the Span that initializes to around the given source array, but spans it
    /// empty.
    ///
    /// # Arguments
    /// - `source`: The [`Spannable`] source array that will be spanned.
    ///
    /// # Returns
    /// A new Span that spans nothing of the given `source`.
    #[inline]
    pub fn empty(source: S) -> Self { Self::ranged(source, Range::empty()) }
}

// Parsing
impl<'s, S: Spannable<'s>> Span<S> {
    /// Returns the index up to where the elements at the head of the spanned area match the given
    /// predicate.
    ///
    /// # Arguments
    /// - `pred`: Some predicate to match the elements at the head of self with.
    ///
    /// # Returns
    /// The index of the first element that does not match the predicate. If all elements match it,
    /// then this equals the length of the spanned area.
    #[inline]
    pub fn match_while(&self, mut pred: impl for<'a> FnMut(&'a S::Elem) -> bool) -> usize {
        // We can sidestep requiring `Slice` to implement `Spannable` by manually ensuring we stay
        // within the range
        let mut i: usize = 0;
        let source_len: usize = self.source.len();
        let start: usize = self.range.start_resolved(source_len).unwrap_or(0);
        let end: usize = self.range.end_resolved(source_len).unwrap_or(source_len);
        self.source.match_while(|elem| {
            if i < start {
                // We skip until we find the start of the range
                i += 1;
                true
            } else if i < end {
                // Match as usual
                i += 1;
                pred(elem)
            } else {
                // When we go out-of-range, we stop
                false
            }
        }) - start
    }
}
impl<'s, S: SpannableUtf8<'s>> Span<S> {
    /// Interpreting the spanned area as unicode segments, will match up to where graphemes at the
    /// head of the area match the given predicate.
    ///
    /// Note that it is iterated by _extended_ unicode graphemes.
    ///
    /// # Arguments
    /// - `pred`: Some predicate to match the graphemes at the head of self with.
    ///
    /// # Returns
    /// The _byte_ index of the first grapheme that does not match the predicate. If all elements
    /// match it, then this equals the length of the array.
    #[inline]
    fn match_utf8_while(&self, mut pred: impl for<'a> FnMut(&'a str) -> bool) -> usize {
        // We can sidestep requiring `Slice` to implement `Spannable` by manually ensuring we stay
        // within the range
        let mut i: usize = 0;
        let source_len: usize = self.source.len();
        let start: usize = self.range.start_resolved(source_len).unwrap_or(0);
        let end: usize = self.range.end_resolved(source_len).unwrap_or(source_len);
        self.source.match_utf8_while(|elem| {
            if i < start {
                // We skip until we find the start of the range
                i += elem.len();
                true
            } else if i < end {
                // Match as usual
                i += elem.len();
                pred(elem)
            } else {
                // When we go out-of-range, we stop
                false
            }
        }) - start
    }
}

// Ops
impl<'s, S: Spannable<'s>> Debug for Span<S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter) -> FResult {
        let Self { source, range } = self;

        // Debug it with the source ID instead of the whole source text.
        let mut fmt = f.debug_struct(&format!("Span<{}>", std::any::type_name::<S>()));
        fmt.field("source", &source.source_id());
        fmt.field("range", range);
        fmt.finish()
    }
}
impl<'s, S: Spannable<'s>> Eq for Span<S> {}
impl<'s, S: Spannable<'s>> PartialEq for Span<S> {
    /// Note that equality on Spans requires that:
    /// - They are [from the same source](Span::source_id()); and
    /// - They span the same area.
    #[inline]
    fn eq(&self, other: &Self) -> bool { self.source_id() == other.source_id() && self.range == other.range }
}
impl<'s, S: Spannable<'s>> Hash for Span<S>
where
    S::SourceId: Hash,
{
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.source_id().hash(state);
        self.range.hash(state);
    }
}
impl<'s, S: Spannable<'s>> Ord for Span<S>
where
    S::Slice: Ord,
{
    /// # Panics
    /// This function simply wraps the [`PartialOrd::partial_cmp()`]-implementation for this
    /// [`Span`]. As such, it calls [`Option::unwrap()`] on the value returned by that function.
    /// If your implementation is somehow inconsistent and returns [`None`], the error will be
    /// visible here.
    #[inline]
    #[track_caller]
    fn cmp(&self, other: &Self) -> Ordering { self.partial_cmp(other).unwrap() }
}
impl<'s, S: Spannable<'s>> PartialOrd for Span<S>
where
    S::Slice: PartialOrd,
{
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> { self.value().partial_cmp(&other.value()) }
}

// Span
impl<'s, S: Clone + Spannable<'s>> Span<S> {
    /// Slices the spanned area.
    ///
    /// This is like [`Span::shrink()`], but not in-place.
    ///
    /// # Arguments
    /// - `range`: The [`Range`] that decides how to slice the source.
    ///
    /// # Returns
    /// A new Span that is the same but with the sliced `range`.
    #[inline]
    pub fn slice(&self, range: impl Into<Range>) -> Self {
        let mut slice = self.clone();
        slice.shrink(range);
        slice
    }

    /// Creates a Span that encompass this plus the given one.
    ///
    /// This is like [`Span::extend()`], but not in-place.
    ///
    /// # Arguments
    /// - `other`: The [`Span`] to encompass.
    ///
    /// # Returns
    /// A new Span that encompasses both `self` and `other`.
    ///
    /// If the `other` Span is not spanning the same source, this function returns [`None`].
    #[inline]
    pub fn join(&self, other: &Self) -> Option<Self> {
        let mut span = self.clone();
        span.extend(other)?;
        Some(span)
    }
}
impl<'s, S: Spannable<'s>> Span<S> {
    /// Shrinks this the spanned area by this span.
    ///
    /// This is like [`Span::slice()`], but in-place.
    ///
    /// # Arguments
    /// - `range`: The [`Range`] that decides how to slice the source.
    ///
    /// # Returns
    /// A mutable reference to Self for chaining.
    #[inline]
    pub fn shrink(&mut self, range: impl Into<Range>) -> &mut Self {
        self.range = self.range.slice(range);
        self
    }

    /// Extends this Span to encompass itself plus the given one.
    ///
    /// This is like [`Span::join()`], but in-place.
    ///
    /// # Arguments
    /// - `other`: The [`Span`] to encompass.
    ///
    /// # Returns
    /// A mutable reference to Self for chaining.
    ///
    /// If the `other` Span is not spanning the same source, this function returns [`None`].
    #[inline]
    pub fn extend(&mut self, other: &Self) -> Option<&mut Self> {
        if self.source_id() == other.source_id() {
            self.range = self.range.join(&other.range);
            Some(self)
        } else {
            None
        }
    }



    /// Returns the spanned part of the source behind this Spannable.
    ///
    /// To obtain the **whole** source, see [`Span::source()`].
    ///
    /// # Returns
    /// A slice of the internal source as spanned by this Span.
    #[inline]
    pub fn value(&self) -> S::Slice { self.source.slice(self.range) }

    /// Returns the source behind this Spannable.
    ///
    /// Note that this is always the **whole** source. For the **spanned** slice of the source,
    /// see [`Span::value()`].
    ///
    /// # Returns
    /// A reference to the internal source wrapped by this Span.
    #[inline]
    pub const fn source(&self) -> &S { &self.source }

    /// Returns the source behind this Spannable.
    ///
    /// Note that this is always the **whole** source. For the **spanned** slice of the source,
    /// see [`Span::value()`].
    ///
    /// # Returns
    /// The internal source wrapped by this Span.
    #[inline]
    pub fn into_source(self) -> S { self.source }



    /// Returns the source ID of the underyling source text.
    ///
    /// # Returns
    /// An [`S::SourceId`](Spannable::SourceId) that can be used to compare sources.
    #[inline]
    pub fn source_id(&self) -> S::SourceId { <S as Spannable>::source_id(&self.source) }

    /// Returns the range over the main source span that is embedded in this span.
    ///
    /// # Returns
    /// A [`Range`] object that represents the spanned area in the total [source array](Span::source()).
    #[inline]
    pub const fn range(&self) -> &Range { &self.range }

    /// Returns the number of elements spanned by this Span.
    ///
    /// # Returns
    /// A [`usize`] that encodes this number.
    #[inline]
    pub fn len(&self) -> usize { self.range.resolved_len(self.source.len()) }

    /// Convenience function for checking if [`Span::len() == 0`](Span::len()).
    ///
    /// # Returns
    /// True if nothing is contained in this Span, or false otherwise.
    #[inline]
    pub fn is_empty(&self) -> bool { self.len() == 0 }
}
impl<'s, S: Clone + Spannable<'s>> Spannable<'s> for Span<S> {
    type Elem = <S as Spannable<'s>>::Elem;
    type SourceId = <S as Spannable<'s>>::SourceId;
    type Slice = Span<S>;

    #[inline]
    fn source_id(&self) -> Self::SourceId { <Self>::source_id(self) }

    #[inline]
    fn match_while(&self, pred: impl for<'a> FnMut(&'a Self::Elem) -> bool) -> usize { <Self>::match_while(self, pred) }

    #[inline]
    fn slice(&self, range: Range) -> Self::Slice { <Self>::slice(self, range) }

    #[inline]
    fn len(&self) -> usize { <Self>::len(self) }
}
impl<'s, S: Clone + SpannableUtf8<'s>> SpannableUtf8<'s> for Span<S> {
    #[inline]
    fn match_utf8_while(&self, pred: impl for<'a> FnMut(&'a str) -> bool) -> usize { <Self>::match_utf8_while(self, pred) }
}





/***** TESTS *****/
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_span_slice() {
        let span = Span::ranged("5+5", 1..);
        let slice = span.slice(1..);
        assert_eq!(*slice.range(), 2..);
    }

    #[test]
    fn test_span_commute_lifetime() {
        fn slice_value<'s>(value: &Span<&'s str>, range: std::ops::RangeFrom<usize>) -> &'s str { value.slice(range).value() }
        assert_eq!(slice_value(&Span::new("hiya"), 1..), "iya");
    }
}
