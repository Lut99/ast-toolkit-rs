//  SPAN 2.rs
//    by Lut99
//
//  Created:
//    14 Mar 2025, 16:51:07
//  Last edited:
//    15 Mar 2025, 17:51:48
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the `ast-toolkit`'s [`Span`], which forms the basis of
//!   many other of the crates.
//

use std::cell::{Ref, RefMut};
use std::fmt::Display;
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use std::sync::{Arc, MutexGuard, RwLockReadGuard, RwLockWriteGuard};

use crate::range2::Range;


/***** INTERFACES *****/
/// Defines things that can be [`Span`]ned.
///
/// Remember that this always concerns a array of sorts.
pub trait Spannable {
    /// Describes the sliced version of this Spannable.
    type Slice<'s>
    where
        Self: 's;
    /// Describes the ID returned by [`Spannable::source_id()`].
    type SourceId<'s>: Eq + PartialEq
    where
        Self: 's;


    /// Returns some identifier of a source that is used to acertain uniqueness.
    ///
    /// # Returns
    /// A formatter of type [`Spannable::SourceId`] that implements [`Display`].
    fn source_id<'s>(&'s self) -> Self::SourceId<'s>;

    /// Returns a sliced version of this string.
    ///
    /// # Arguments
    /// - `range`: A [`Range`] that determines how to slice the Spannable.
    ///
    /// # Returns
    /// A [`Spannable::Slice`] that representes the sliced part.
    fn slice<'s>(&'s self, range: Range) -> Self::Slice<'s>;

    /// Returns the total length of the thing.
    ///
    /// # Returns
    /// The total number of things in the spanned array.
    fn len(&self) -> usize;

    /// Checks whether there is anything in this Spannable.
    ///
    /// Convenience function for:
    /// ```ignore
    /// Spannable::len() == 0
    /// ```
    ///
    /// # Returns
    /// True if no elements are in this array.
    #[inline]
    fn is_empty(&self) -> bool { self.len() == 0 }
}

// Default impls
impl Spannable for str {
    type Slice<'s> = &'s str;
    type SourceId<'s>
        = usize
    where
        Self: 's;


    #[inline]
    fn source_id<'s>(&'s self) -> Self::SourceId<'s> { (self as *const str).addr() }

    #[inline]
    fn slice<'s>(&'s self, range: Range) -> Self::Slice<'s> {
        let start: usize = match range.start_resolved(self.len()) {
            Some(pos) => pos,
            None => return &"",
        };
        let end: usize = match range.end_resolved(self.len()) {
            Some(pos) => pos,
            None => return &"",
        };
        &self[start..end]
    }

    #[inline]
    fn len(&self) -> usize { <str>::len(self) }
}
impl<T> Spannable for [T] {
    type Slice<'s>
        = &'s [T]
    where
        Self: 's;
    type SourceId<'s>
        = usize
    where
        Self: 's;


    #[inline]
    fn source_id<'s>(&'s self) -> Self::SourceId<'s> { (self as *const [T]).addr() }

    #[inline]
    fn slice<'s>(&'s self, range: Range) -> Self::Slice<'s> {
        let start: usize = match range.start_resolved(self.len()) {
            Some(pos) => pos,
            None => return &[],
        };
        let end: usize = match range.end_resolved(self.len()) {
            Some(pos) => pos,
            None => return &[],
        };
        &self[start..end]
    }

    #[inline]
    fn len(&self) -> usize { <[T]>::len(self) }
}
impl<T: Eq + PartialEq, U: Spannable> Spannable for (T, U) {
    type Slice<'s>
        = <U as Spannable>::Slice<'s>
    where
        Self: 's;
    type SourceId<'s>
        = &'s T
    where
        Self: 's;


    #[inline]
    fn source_id<'s>(&'s self) -> Self::SourceId<'s> { &self.0 }

    #[inline]
    fn slice<'s>(&'s self, range: Range) -> Self::Slice<'s> { <U as Spannable>::slice(&self.1, range) }

    #[inline]
    fn len(&self) -> usize { self.1.len() }
}

// Span impl
impl<S: Clone + Spannable> Spannable for Span<S> {
    type Slice<'s>
        = Span<S>
    where
        Self: 's;
    type SourceId<'s>
        = <S as Spannable>::SourceId<'s>
    where
        Self: 's;


    #[inline]
    fn source_id<'s>(&'s self) -> Self::SourceId<'s> { <Span<S>>::source_id(self) }

    #[inline]
    fn slice<'s>(&'s self, range: Range) -> Self::Slice<'s> { <Span<S>>::slice(self, range) }

    #[inline]
    fn len(&self) -> usize { <Span<S>>::len(self) }
}

// Pointer-like impls
impl<'a, T: Spannable> Spannable for &'a T {
    type Slice<'s>
        = <T as Spannable>::Slice<'s>
    where
        Self: 's;
    type SourceId<'s>
        = <T as Spannable>::SourceId<'s>
    where
        Self: 's;


    #[inline]
    fn source_id<'s>(&'s self) -> Self::SourceId<'s> { <T as Spannable>::source_id(self) }

    #[inline]
    fn slice<'s>(&'s self, range: Range) -> Self::Slice<'s> { <T as Spannable>::slice(self, range) }

    #[inline]
    fn len(&self) -> usize { <T as Spannable>::len(self) }

    #[inline]
    fn is_empty(&self) -> bool { <T as Spannable>::is_empty(self) }
}
impl<'a, T: Spannable> Spannable for &'a mut T {
    type Slice<'s>
        = <T as Spannable>::Slice<'s>
    where
        Self: 's;
    type SourceId<'s>
        = <T as Spannable>::SourceId<'s>
    where
        Self: 's;

    #[inline]
    fn source_id<'s>(&'s self) -> Self::SourceId<'s> { <T as Spannable>::source_id(self) }

    #[inline]
    fn slice<'s>(&'s self, range: Range) -> Self::Slice<'s> { <T as Spannable>::slice(self, range) }

    #[inline]
    fn len(&self) -> usize { <T as Spannable>::len(self) }

    #[inline]
    fn is_empty(&self) -> bool { <T as Spannable>::is_empty(self) }
}
impl<T: Spannable> Spannable for Box<T> {
    type Slice<'s>
        = <T as Spannable>::Slice<'s>
    where
        Self: 's;
    type SourceId<'s>
        = <T as Spannable>::SourceId<'s>
    where
        Self: 's;

    #[inline]
    fn source_id<'s>(&'s self) -> Self::SourceId<'s> { <T as Spannable>::source_id(self) }

    #[inline]
    fn slice<'s>(&'s self, range: Range) -> Self::Slice<'s> { <T as Spannable>::slice(self, range) }

    #[inline]
    fn len(&self) -> usize { <T as Spannable>::len(self) }

    #[inline]
    fn is_empty(&self) -> bool { <T as Spannable>::is_empty(self) }
}
impl<T: Spannable> Spannable for Rc<T> {
    type Slice<'s>
        = <T as Spannable>::Slice<'s>
    where
        Self: 's;
    type SourceId<'s>
        = <T as Spannable>::SourceId<'s>
    where
        Self: 's;

    #[inline]
    fn source_id<'s>(&'s self) -> Self::SourceId<'s> { <T as Spannable>::source_id(self) }

    #[inline]
    fn slice<'s>(&'s self, range: Range) -> Self::Slice<'s> { <T as Spannable>::slice(self, range) }

    #[inline]
    fn len(&self) -> usize { <T as Spannable>::len(self) }

    #[inline]
    fn is_empty(&self) -> bool { <T as Spannable>::is_empty(self) }
}
impl<T: Spannable> Spannable for Arc<T> {
    type Slice<'s>
        = <T as Spannable>::Slice<'s>
    where
        Self: 's;
    type SourceId<'s>
        = <T as Spannable>::SourceId<'s>
    where
        Self: 's;

    #[inline]
    fn source_id<'s>(&'s self) -> Self::SourceId<'s> { <T as Spannable>::source_id(self) }

    #[inline]
    fn slice<'s>(&'s self, range: Range) -> Self::Slice<'s> { <T as Spannable>::slice(self, range) }

    #[inline]
    fn len(&self) -> usize { <T as Spannable>::len(self) }

    #[inline]
    fn is_empty(&self) -> bool { <T as Spannable>::is_empty(self) }
}
impl<'a, T: Spannable> Spannable for MutexGuard<'a, T> {
    type Slice<'s>
        = <T as Spannable>::Slice<'s>
    where
        Self: 's;
    type SourceId<'s>
        = <T as Spannable>::SourceId<'s>
    where
        Self: 's;

    #[inline]
    fn source_id<'s>(&'s self) -> Self::SourceId<'s> { <T as Spannable>::source_id(self) }

    #[inline]
    fn slice<'s>(&'s self, range: Range) -> Self::Slice<'s> { <T as Spannable>::slice(self, range) }

    #[inline]
    fn len(&self) -> usize { <T as Spannable>::len(self) }

    #[inline]
    fn is_empty(&self) -> bool { <T as Spannable>::is_empty(self) }
}
impl<'a, T: Spannable> Spannable for RwLockReadGuard<'a, T> {
    type Slice<'s>
        = <T as Spannable>::Slice<'s>
    where
        Self: 's;
    type SourceId<'s>
        = <T as Spannable>::SourceId<'s>
    where
        Self: 's;

    #[inline]
    fn source_id<'s>(&'s self) -> Self::SourceId<'s> { <T as Spannable>::source_id(self) }

    #[inline]
    fn slice<'s>(&'s self, range: Range) -> Self::Slice<'s> { <T as Spannable>::slice(self, range) }

    #[inline]
    fn len(&self) -> usize { <T as Spannable>::len(self) }

    #[inline]
    fn is_empty(&self) -> bool { <T as Spannable>::is_empty(self) }
}
impl<'a, T: Spannable> Spannable for RwLockWriteGuard<'a, T> {
    type Slice<'s>
        = <T as Spannable>::Slice<'s>
    where
        Self: 's;
    type SourceId<'s>
        = <T as Spannable>::SourceId<'s>
    where
        Self: 's;

    #[inline]
    fn source_id<'s>(&'s self) -> Self::SourceId<'s> { <T as Spannable>::source_id(self) }

    #[inline]
    fn slice<'s>(&'s self, range: Range) -> Self::Slice<'s> { <T as Spannable>::slice(self, range) }

    #[inline]
    fn len(&self) -> usize { <T as Spannable>::len(self) }

    #[inline]
    fn is_empty(&self) -> bool { <T as Spannable>::is_empty(self) }
}
impl<'a, T: Spannable> Spannable for Ref<'a, T> {
    type Slice<'s>
        = <T as Spannable>::Slice<'s>
    where
        Self: 's;
    type SourceId<'s>
        = <T as Spannable>::SourceId<'s>
    where
        Self: 's;

    #[inline]
    fn source_id<'s>(&'s self) -> Self::SourceId<'s> { <T as Spannable>::source_id(self) }

    #[inline]
    fn slice<'s>(&'s self, range: Range) -> Self::Slice<'s> { <T as Spannable>::slice(self, range) }

    #[inline]
    fn len(&self) -> usize { <T as Spannable>::len(self) }

    #[inline]
    fn is_empty(&self) -> bool { <T as Spannable>::is_empty(self) }
}
impl<'a, T: Spannable> Spannable for RefMut<'a, T> {
    type Slice<'s>
        = <T as Spannable>::Slice<'s>
    where
        Self: 's;
    type SourceId<'s>
        = <T as Spannable>::SourceId<'s>
    where
        Self: 's;

    #[inline]
    fn source_id<'s>(&'s self) -> Self::SourceId<'s> { <T as Spannable>::source_id(self) }

    #[inline]
    fn slice<'s>(&'s self, range: Range) -> Self::Slice<'s> { <T as Spannable>::slice(self, range) }

    #[inline]
    fn len(&self) -> usize { <T as Spannable>::len(self) }

    #[inline]
    fn is_empty(&self) -> bool { <T as Spannable>::is_empty(self) }
}





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
#[derive(Clone, Copy, Debug)]
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
impl<S> Eq for Span<S>
where
    S: Spannable,
    for<'a> S::Slice<'a>: Eq,
{
}
impl<S> Hash for Span<S>
where
    S: Spannable,
    for<'a> S::Slice<'a>: Hash,
{
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {}
}
impl<S> PartialEq for Span<S>
where
    S: Spannable,
    for<'a> S::Slice<'a>: PartialEq,
{
    /// Note that equality on Spans requires that:
    /// - They are [from the same source](Span::same_source()); and
    /// - They span the same area.
    #[inline]
    fn eq(&self, other: &Self) -> bool { self.same_source(other) && self.range == other.range }
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
