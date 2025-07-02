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

use std::borrow::Cow;
use std::cmp::Ordering;
use std::fmt::{Debug, Formatter, Result as FResult};
use std::hash::{Hash, Hasher};

use crate::range::Range;
use crate::spannable::{Spannable, SpannableBytes};
use crate::{Spanning, SpanningInf, SpanningMut, SpanningRef};


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
/// assert_eq!(span1.value(), b"howdy");
///
/// let span2 = Span::ranged(("<example>", "Hello, world!"), ..5);
/// assert_eq!(span2.value(), b"Hello");
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
impl<'s, S> Span<S> {
    /// Constructor for the Span that initializes it to span the given array.
    ///
    /// # Arguments
    /// - `source`: The [`Spannable`] source array that will be spanned.
    ///
    /// # Returns
    /// A new Span that spans the entire `source`.
    #[inline]
    pub const fn new(source: S) -> Self { Self { source, range: Range::full() } }

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
    pub const fn empty(source: S) -> Self { Self { source, range: Range::empty() } }
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
impl<'s, S: Spannable<'s>> Hash for Span<S> {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.source_id().hash(state);
        self.range.hash(state);
    }
}
impl<'s, S: Spannable<'s>> Ord for Span<S>
where
    S::Elem: Ord,
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
    S::Elem: PartialOrd,
{
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> { self.value().partial_cmp(&other.value()) }
}

// Serde
#[cfg(feature = "serde")]
impl<'s, S> serde::Serialize for Span<S>
where
    S: Spannable<'s>,
    S::SourceId: serde::Serialize,
{
    fn serialize<SE>(&self, serializer: SE) -> Result<SE::Ok, SE::Error>
    where
        SE: serde::Serializer,
    {
        use serde::ser::SerializeStruct as _;

        let mut ser = serializer.serialize_struct("Span", 2)?;
        ser.serialize_field("source", &self.source.source_id())?;
        ser.serialize_field("range", &self.range)?;
        ser.end()
    }
}

// Span
impl<'s, S: Clone + Spannable<'s>> Span<S> {
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
impl<'s, S: Clone> Span<S> {
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
}
impl<'s, S: Spannable<'s>> Span<S> {
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
    /// At this point, just an alias for [`Span::as_slice()`].
    ///
    /// # Returns
    /// A slice of the internal source as spanned by this Span.
    #[inline]
    pub fn value(&self) -> &[S::Elem] { self.as_slice() }



    /// Returns the source ID of the underyling source text.
    ///
    /// # Returns
    /// An [`S::SourceId`](Spannable::SourceId) that can be used to compare sources.
    #[inline]
    pub fn source_id(&self) -> S::SourceId { <S as Spannable>::source_id(&self.source) }

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
impl<S> Span<S> {
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



    /// Returns the range over the main source span that is embedded in this span.
    ///
    /// # Returns
    /// A [`Range`] object that represents the spanned area in the total [source array](Span::source()).
    #[inline]
    pub const fn range(&self) -> &Range { &self.range }
}
impl<'s> Span<&'s str> {
    /// Gives access to the internal, sliced string without having to try to unwrap it first.
    ///
    /// # Returns
    /// A [`str`] slice representing the spanned area.
    #[inline]
    pub fn as_str(&self) -> &'s str {
        let source_len: usize = self.source.len();
        let start: usize = self.range.start_resolved(source_len).unwrap_or(0);
        let end: usize = self.range.end_resolved(source_len).unwrap_or(0);
        &self.source[start..end]
    }
}
impl<'s> Span<Cow<'s, str>> {
    /// Gives access to the internal, sliced string without having to try to unwrap it first.
    ///
    /// # Returns
    /// A [`str`] slice representing the spanned area.
    #[inline]
    pub fn as_str(&self) -> &str {
        let source_len: usize = self.source.len();
        let start: usize = self.range.start_resolved(source_len).unwrap_or(0);
        let end: usize = self.range.end_resolved(source_len).unwrap_or(0);
        &self.source[start..end]
    }
}
impl Span<String> {
    /// Gives access to the internal, sliced string without having to try to unwrap it first.
    ///
    /// # Returns
    /// A [`str`] slice representing the spanned area.
    #[inline]
    pub fn as_str(&self) -> &str {
        let source_len: usize = self.source.len();
        let start: usize = self.range.start_resolved(source_len).unwrap_or(0);
        let end: usize = self.range.end_resolved(source_len).unwrap_or(0);
        &self.source[start..end]
    }
}
impl<'s, S: Spannable<'s>> Span<S> {
    /// Returns slice version of the spanned area.
    ///
    /// This function imposes quite some requirements on the underlying memory, namely that it is
    /// a) present and b) continuous. However, it does align with the intended usage of a [`Span`],
    /// and offers for great convenience in interoperating with other libraries (i.e., get a byte
    /// slice of [`str`] as value of a [`Span`]).
    ///
    /// NOTE: The only difference between this function and [`Spannable::as_slice()`] (which is
    /// also implemented on [`Span`]) is that its bounds are slightly less restrictive ([`Clone`]
    /// is not necessary).
    ///
    /// # Returns
    /// A slice of internal elements that should be a counterpart of the spanned area.
    #[inline]
    pub fn as_slice(&self) -> &[S::Elem] {
        let source_len: usize = self.source.len();
        let start: usize = self.range.start_resolved(source_len).unwrap_or(0);
        let end: usize = self.range.end_resolved(source_len).unwrap_or(0);
        &self.source.as_slice()[start..end]
    }

    /// Gets the start position of the span in the source text.
    ///
    /// # Returns
    /// A [`usize`] encoding a zero-indexed position in the spanned array where this span begins.
    /// If the [`Span`] is empty, it returns [`None`].
    #[inline]
    pub fn start(&self) -> Option<usize> { self.range.start_resolved(self.source.len()) }
}
impl<'s, S: SpannableBytes<'s>> Span<S> {
    /// Alias for [`Span::as_slice()`] that has a more topical name.
    ///
    /// NOTE: The only difference between this function and [`SpannableBytes::as_bytes()`] (which
    /// is also implemented on [`Span`]) is that its bounds are slightly less restrictive
    /// ([`Clone`] is not necessary).
    ///
    /// # Returns
    /// A byte slice representing the spanned area.
    #[inline]
    pub fn as_bytes(&self) -> &[u8] { <Self>::as_slice(self) }

    /// Gets the start position of the span in the source text as a (line, column)-pair.
    ///
    /// # Returns
    /// A pair of [`usize`]s encoding two one-indexed line- and column position, respectively, in
    /// the spanned array where this span begins. If the [`Span`] is empty, it returns [`None`].
    #[inline]
    pub fn start_pos(&self) -> Option<(usize, usize)> {
        let start: usize = self.start()?;
        let mut line = 1;
        let mut col = 1;
        for b in &self.source.as_bytes()[..start] {
            if *b == b'\n' {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
        }
        Some((line, col))
    }
}
impl<'s, S: Clone + Spannable<'s>> Spannable<'s> for Span<S> {
    type Elem = <S as Spannable<'s>>::Elem;
    type SourceId = <S as Spannable<'s>>::SourceId;
    // type Slice = Span<S>;

    #[inline]
    fn source_id(&self) -> Self::SourceId { <Self>::source_id(self) }

    #[inline]
    fn as_slice(&self) -> &[Self::Elem] { <Self>::as_slice(self) }

    #[inline]
    fn len(&self) -> usize { <Self>::len(self) }
}
impl<S: Clone> Spanning<S> for Span<S> {
    #[inline]
    fn get_span(&self) -> Option<Cow<Span<S>>> { Some(Cow::Borrowed(self)) }

    #[inline]
    fn take_span(self) -> Option<Span<S>> { Some(self) }
}
impl<S: Clone> SpanningInf<S> for Span<S> {
    #[inline]
    fn span(&self) -> Cow<Span<S>> { Cow::Borrowed(self) }

    #[inline]
    fn into_span(self) -> Span<S> { self }
}
impl<S: Clone> SpanningRef<S> for Span<S> {
    #[inline]
    fn span_ref(&self) -> &Span<S> { self }
}
impl<S: Clone> SpanningMut<S> for Span<S> {
    #[inline]
    fn span_mut(&mut self) -> &mut Span<S> { self }
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
        fn slice_value<'s>(value: &Span<&'s str>, range: std::ops::RangeFrom<usize>) -> &'s str { value.slice(range).as_str() }
        assert_eq!(slice_value(&Span::new("hiya"), 1..), "iya");
    }
}
