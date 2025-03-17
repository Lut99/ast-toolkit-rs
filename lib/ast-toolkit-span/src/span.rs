//  SPAN 2.rs
//    by Lut99
//
//  Created:
//    14 Mar 2025, 16:51:07
//  Last edited:
//    17 Mar 2025, 14:00:23
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
use crate::spannable::Spannable;


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
/// assert_eq!(span2.source(), "Hello, world!");
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
impl<S: Spannable> Span<S> {
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

// Ops
impl<S> Debug for Span<S>
where
    S: Spannable,
{
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
impl<S> Eq for Span<S> where S: Spannable {}
impl<S> PartialEq for Span<S>
where
    S: Spannable,
{
    /// Note that equality on Spans requires that:
    /// - They are [from the same source](Span::source_id()); and
    /// - They span the same area.
    #[inline]
    fn eq(&self, other: &Self) -> bool { self.source_id() == other.source_id() && self.range == other.range }
}
impl<S> Hash for Span<S>
where
    S: Spannable,
    for<'a> S::SourceId<'a>: Hash,
{
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.source_id().hash(state);
        self.range.hash(state);
    }
}
impl<S> Ord for Span<S>
where
    S: Spannable,
    for<'a> S::Slice<'a>: Ord,
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
impl<S> PartialOrd for Span<S>
where
    S: Spannable,
    for<'a> S::Slice<'a>: PartialOrd,
{
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> { self.value().partial_cmp(&other.value()) }
}

// Span
impl<S: Clone + Spannable> Span<S> {
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
impl<S: Spannable> Span<S> {
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



    /// Returns the spanned part of the source behind this Spannable.
    ///
    /// To obtain the **whole** source, see [`Span::source()`].
    ///
    /// # Returns
    /// A slice of the internal source as spanned by this Span.
    #[inline]
    pub fn value<'s>(&'s self) -> S::Slice<'s> { self.source.slice(self.range) }

    /// Returns the source behind this Spannable.
    ///
    /// Note that this is always the **whole** source. For the **spanned** slice of the source,
    /// see [`Span::value()`].
    ///
    /// # Returns
    /// The internal source wrapped by this Span.
    #[inline]
    pub const fn source(&self) -> &S { &self.source }



    /// Returns the source ID of the underyling source text.
    ///
    /// # Returns
    /// An [`S::SourceId`](Spannable::SourceId) that can be used to compare sources.
    #[inline]
    pub fn source_id<'s>(&'s self) -> S::SourceId<'s> { <S as Spannable>::source_id(&self.source) }

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
impl<S: Clone + Spannable> Spannable for Span<S> {
    type SourceId<'s>
        = <S as Spannable>::SourceId<'s>
    where
        Self: 's;
    type Slice<'s>
        = Span<S>
    where
        Self: 's;

    #[inline]
    fn source_id<'s>(&'s self) -> Self::SourceId<'s> { <Self>::source_id(self) }

    #[inline]
    fn slice<'s>(&'s self, range: Range) -> Self::Slice<'s> { <Self>::slice(self, range) }

    #[inline]
    fn len(&self) -> usize { <Self>::len(self) }
}
