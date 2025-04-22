//  SPANNABLE 2.rs
//    by Lut99
//
//  Created:
//    17 Mar 2025, 10:14:05
//  Last edited:
//    22 Apr 2025, 10:40:38
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements a trait abstracting over things that are sensible to put
//!   in a [`Span`].
//

use std::cell::{Ref, RefMut};
use std::fmt::Debug;
use std::rc::Rc;
use std::sync::{Arc, MutexGuard, RwLockReadGuard, RwLockWriteGuard};

use crate::range::Range;


/***** LIBRARY *****/
/// Defines things that can be [`Span`]ned.
///
/// Remember that this always concerns a array of sorts. For more information, see the
/// [`Span`](crate::span::Span).
///
/// Note that the implementations using spans (and therefore [`Spannable`]) will assume that the
/// span as a whole is cheaply [`Clone`]able. Therefore, the trait is designed to hold a reference
/// to the full source text somewhere else instead of owning it.
///
/// # Lifetime
/// The given lifetime `'s` represents the lifetime of the spanned item. This is used to commute
/// the lifetime of the spanned item over the container referencing it (i.e., the span).
///
/// If your object does not have a lifetime (e.g., an owned object like [`String`]), then this
/// lifetime should be `'static`.
pub trait Spannable<'s> {
    /// Describes the sliced version of this Spannable.
    type Slice;
    /// Describes the ID returned by [`Spannable::source_id()`].
    type SourceId: Debug + Eq + PartialEq;


    /// Returns some identifier of a source that is used to acertain uniqueness.
    ///
    /// # Returns
    /// A formatter of type [`Spannable::SourceId`] that implements [`Display`].
    fn source_id(&self) -> Self::SourceId;

    /// Returns a sliced version of this string.
    ///
    /// # Arguments
    /// - `range`: A [`Range`] that determines how to slice the Spannable.
    ///
    /// # Returns
    /// A [`Spannable::Slice`] that representes the sliced part.
    fn slice(&self, range: Range) -> Self::Slice;

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
impl<'a> Spannable<'a> for &'a str {
    type Slice = &'a str;
    type SourceId = usize;


    #[inline]
    fn source_id(&self) -> Self::SourceId { (*self as *const str).addr() }

    #[inline]
    fn slice(&self, range: Range) -> Self::Slice {
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
impl<'a, T> Spannable<'a> for &'a [T] {
    type Slice = &'a [T];
    type SourceId = usize;


    #[inline]
    fn source_id(&self) -> Self::SourceId { (*self as *const [T]).addr() }

    #[inline]
    fn slice(&self, range: Range) -> Self::Slice {
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

    #[inline]
    fn is_empty(&self) -> bool { <[T]>::is_empty(self) }
}
impl<'a, T: Clone + Debug + Eq + PartialEq, U: Spannable<'a>> Spannable<'a> for (T, U) {
    type Slice = <U as Spannable<'a>>::Slice;
    type SourceId = T;


    #[inline]
    fn source_id(&self) -> Self::SourceId { self.0.clone() }

    #[inline]
    fn slice(&self, range: Range) -> Self::Slice { <U as Spannable>::slice(&self.1, range) }

    #[inline]
    fn len(&self) -> usize { self.1.len() }
}

// Pointer-like impls
impl<'a, 'b, T: ?Sized + Spannable<'a>> Spannable<'a> for &'b T {
    type Slice = <T as Spannable<'a>>::Slice;
    type SourceId = <T as Spannable<'a>>::SourceId;


    #[inline]
    fn source_id(&self) -> Self::SourceId { <T as Spannable>::source_id(self) }

    #[inline]
    fn slice(&self, range: Range) -> Self::Slice { <T as Spannable>::slice(self, range) }

    #[inline]
    fn len(&self) -> usize { <T as Spannable>::len(self) }

    #[inline]
    fn is_empty(&self) -> bool { <T as Spannable>::is_empty(self) }
}
impl<'a, 'b, T: ?Sized + Spannable<'a>> Spannable<'a> for &'b mut T {
    type Slice = <T as Spannable<'a>>::Slice;
    type SourceId = <T as Spannable<'a>>::SourceId;


    #[inline]
    fn source_id(&self) -> Self::SourceId { <T as Spannable>::source_id(self) }

    #[inline]
    fn slice(&self, range: Range) -> Self::Slice { <T as Spannable>::slice(self, range) }

    #[inline]
    fn len(&self) -> usize { <T as Spannable>::len(self) }

    #[inline]
    fn is_empty(&self) -> bool { <T as Spannable>::is_empty(self) }
}
impl<'a, T: ?Sized + Spannable<'a>> Spannable<'a> for Box<T> {
    type Slice = <T as Spannable<'a>>::Slice;
    type SourceId = <T as Spannable<'a>>::SourceId;


    #[inline]
    fn source_id(&self) -> Self::SourceId { <T as Spannable>::source_id(self) }

    #[inline]
    fn slice(&self, range: Range) -> Self::Slice { <T as Spannable>::slice(self, range) }

    #[inline]
    fn len(&self) -> usize { <T as Spannable>::len(self) }

    #[inline]
    fn is_empty(&self) -> bool { <T as Spannable>::is_empty(self) }
}
impl<'a, T: ?Sized + Spannable<'a>> Spannable<'a> for Rc<T> {
    type Slice = <T as Spannable<'a>>::Slice;
    type SourceId = <T as Spannable<'a>>::SourceId;


    #[inline]
    fn source_id(&self) -> Self::SourceId { <T as Spannable>::source_id(self) }

    #[inline]
    fn slice(&self, range: Range) -> Self::Slice { <T as Spannable>::slice(self, range) }

    #[inline]
    fn len(&self) -> usize { <T as Spannable>::len(self) }

    #[inline]
    fn is_empty(&self) -> bool { <T as Spannable>::is_empty(self) }
}
impl<'a, T: ?Sized + Spannable<'a>> Spannable<'a> for Arc<T> {
    type Slice = <T as Spannable<'a>>::Slice;
    type SourceId = <T as Spannable<'a>>::SourceId;


    #[inline]
    fn source_id(&self) -> Self::SourceId { <T as Spannable>::source_id(self) }

    #[inline]
    fn slice(&self, range: Range) -> Self::Slice { <T as Spannable>::slice(self, range) }

    #[inline]
    fn len(&self) -> usize { <T as Spannable>::len(self) }

    #[inline]
    fn is_empty(&self) -> bool { <T as Spannable>::is_empty(self) }
}
impl<'a, 'b, T: ?Sized + Spannable<'a>> Spannable<'a> for MutexGuard<'b, T> {
    type Slice = <T as Spannable<'a>>::Slice;
    type SourceId = <T as Spannable<'a>>::SourceId;


    #[inline]
    fn source_id(&self) -> Self::SourceId { <T as Spannable>::source_id(self) }

    #[inline]
    fn slice(&self, range: Range) -> Self::Slice { <T as Spannable>::slice(self, range) }

    #[inline]
    fn len(&self) -> usize { <T as Spannable>::len(self) }

    #[inline]
    fn is_empty(&self) -> bool { <T as Spannable>::is_empty(self) }
}
impl<'a, 'b, T: ?Sized + Spannable<'a>> Spannable<'a> for RwLockReadGuard<'b, T> {
    type Slice = <T as Spannable<'a>>::Slice;
    type SourceId = <T as Spannable<'a>>::SourceId;


    #[inline]
    fn source_id(&self) -> Self::SourceId { <T as Spannable>::source_id(self) }

    #[inline]
    fn slice(&self, range: Range) -> Self::Slice { <T as Spannable>::slice(self, range) }

    #[inline]
    fn len(&self) -> usize { <T as Spannable>::len(self) }

    #[inline]
    fn is_empty(&self) -> bool { <T as Spannable>::is_empty(self) }
}
impl<'a, 'b, T: ?Sized + Spannable<'a>> Spannable<'a> for RwLockWriteGuard<'b, T> {
    type Slice = <T as Spannable<'a>>::Slice;
    type SourceId = <T as Spannable<'a>>::SourceId;


    #[inline]
    fn source_id(&self) -> Self::SourceId { <T as Spannable>::source_id(self) }

    #[inline]
    fn slice(&self, range: Range) -> Self::Slice { <T as Spannable>::slice(self, range) }

    #[inline]
    fn len(&self) -> usize { <T as Spannable>::len(self) }

    #[inline]
    fn is_empty(&self) -> bool { <T as Spannable>::is_empty(self) }
}
impl<'a, 'b, T: ?Sized + Spannable<'a>> Spannable<'a> for Ref<'b, T> {
    type Slice = <T as Spannable<'a>>::Slice;
    type SourceId = <T as Spannable<'a>>::SourceId;


    #[inline]
    fn source_id(&self) -> Self::SourceId { <T as Spannable>::source_id(self) }

    #[inline]
    fn slice(&self, range: Range) -> Self::Slice { <T as Spannable>::slice(self, range) }

    #[inline]
    fn len(&self) -> usize { <T as Spannable>::len(self) }

    #[inline]
    fn is_empty(&self) -> bool { <T as Spannable>::is_empty(self) }
}
impl<'a, 'b, T: ?Sized + Spannable<'a>> Spannable<'a> for RefMut<'b, T> {
    type Slice = <T as Spannable<'a>>::Slice;
    type SourceId = <T as Spannable<'a>>::SourceId;


    #[inline]
    fn source_id(&self) -> Self::SourceId { <T as Spannable>::source_id(self) }

    #[inline]
    fn slice(&self, range: Range) -> Self::Slice { <T as Spannable>::slice(self, range) }

    #[inline]
    fn len(&self) -> usize { <T as Spannable>::len(self) }

    #[inline]
    fn is_empty(&self) -> bool { <T as Spannable>::is_empty(self) }
}
