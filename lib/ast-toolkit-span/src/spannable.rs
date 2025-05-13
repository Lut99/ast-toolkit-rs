//  SPANNABLE 2.rs
//    by Lut99
//
//  Created:
//    17 Mar 2025, 10:14:05
//  Last edited:
//    08 May 2025, 11:54:28
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

use unicode_segmentation::UnicodeSegmentation;

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
    /// Describes the elements in this spannable array.
    type Elem: 's;
    /// Describes the sliced version of this Spannable.
    type Slice;
    /// Describes the ID returned by [`Spannable::source_id()`].
    type SourceId: Debug + Eq + PartialEq;


    // Mandatory implementation
    /// Returns some identifier of a source that is used to acertain uniqueness.
    ///
    /// # Returns
    /// A formatter of type [`Spannable::SourceId`] that implements [`Display`].
    fn source_id(&self) -> Self::SourceId;

    /// Returns a subrange of this array.
    ///
    /// Note that this needn't be the same as slicing the result of [`Spannable::as_slice()`]. This
    /// version may return a type more appropriate (e.g., a [`str`] for [`str`]s).
    ///
    /// # Arguments
    /// - `range`: A [`Range`] that determines how to slice the Spannable.
    ///
    /// # Returns
    /// A [`Spannable::Slice`] that representes the sliced part.
    fn slice(&self, range: Range) -> Self::Slice;

    /// Returns slice version of the underlying array.
    ///
    /// This function imposes quite some requirements on the underlying memory, namely that it is
    /// a) present and b) continuous. However, it does align with the intended usage of a [`Span`],
    /// and offers for great convenience in interoperating with other libraries (i.e., get a byte
    /// slice of [`str`] as value of a [`Span`]).
    ///
    /// # Returns
    /// A slice of internal elements that should be a counterpart of the spanned area.
    fn as_slice(&self) -> &'s [Self::Elem];


    // Derived functions
    /// Returns the index up to where the elements at the head of the array match the given
    /// predicate.
    ///
    /// By default, acts as a convenience function for something like:
    /// ```ignore
    /// let mut i: usize = 0;
    /// for elem in Spannable::as_slice() {
    ///     if !pred(elem) {
    ///         return i;
    ///     }
    ///     i += 1;
    /// }
    /// i
    /// ```
    ///
    /// # Arguments
    /// - `pred`: Some predicate to match the elements at the head of self with.
    ///
    /// # Returns
    /// The index of the first element that does not match the predicate. If all elements match it,
    /// then this equals the length of the array.
    #[inline]
    fn match_while(&self, mut pred: impl FnMut(&'s Self::Elem) -> bool) -> usize {
        let self_slice: &[Self::Elem] = self.as_slice();
        for (i, elem) in self_slice.iter().enumerate() {
            if !pred(elem) {
                return i;
            }
        }
        self_slice.len()
    }

    /// Returns the total length of the array.
    ///
    /// By default, acts as a convenience function for:
    /// ```ignore
    /// Spannable::as_slice().len()
    /// ```
    ///
    /// # Returns
    /// The total number of elements in the array.
    #[inline]
    fn len(&self) -> usize { self.as_slice().len() }

    /// Checks whether there is anything in this Spannable.
    ///
    /// By default, acts as a convenience function for:
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
    type Elem = u8;
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
    fn as_slice(&self) -> &'a [Self::Elem] { <str>::as_bytes(self) }
}
impl<'a, T> Spannable<'a> for &'a [T] {
    type Elem = T;
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
    fn as_slice(&self) -> &'a [Self::Elem] { self }
}
impl<'a, T: Clone + Debug + Eq + PartialEq, U: Spannable<'a>> Spannable<'a> for (T, U) {
    type Elem = <U as Spannable<'a>>::Elem;
    type Slice = <U as Spannable<'a>>::Slice;
    type SourceId = T;


    #[inline]
    fn source_id(&self) -> Self::SourceId { self.0.clone() }

    #[inline]
    fn slice(&self, range: Range) -> Self::Slice { <U as Spannable>::slice(&self.1, range) }

    #[inline]
    fn as_slice(&self) -> &'a [Self::Elem] { <U as Spannable>::as_slice(&self.1) }

    #[inline]
    fn match_while(&self, pred: impl FnMut(&'a Self::Elem) -> bool) -> usize { <U as Spannable>::match_while(&self.1, pred) }

    #[inline]
    fn len(&self) -> usize { self.1.len() }

    #[inline]
    fn is_empty(&self) -> bool { self.1.is_empty() }
}
// The `Spannable` impl for Span is over at its own definition

// Pointer-like impls
impl<'a, 'b, T: ?Sized + Spannable<'a>> Spannable<'a> for &'b T {
    type Elem = <T as Spannable<'a>>::Elem;
    type Slice = <T as Spannable<'a>>::Slice;
    type SourceId = <T as Spannable<'a>>::SourceId;


    #[inline]
    fn source_id(&self) -> Self::SourceId { <T as Spannable>::source_id(self) }

    #[inline]
    fn slice(&self, range: Range) -> Self::Slice { <T as Spannable>::slice(self, range) }

    #[inline]
    fn as_slice(&self) -> &'a [Self::Elem] { <T as Spannable>::as_slice(self) }

    #[inline]
    fn match_while(&self, pred: impl FnMut(&'a Self::Elem) -> bool) -> usize { <T as Spannable>::match_while(self, pred) }

    #[inline]
    fn len(&self) -> usize { <T as Spannable>::len(self) }

    #[inline]
    fn is_empty(&self) -> bool { <T as Spannable>::is_empty(self) }
}
impl<'a, 'b, T: ?Sized + Spannable<'a>> Spannable<'a> for &'b mut T {
    type Elem = <T as Spannable<'a>>::Elem;
    type Slice = <T as Spannable<'a>>::Slice;
    type SourceId = <T as Spannable<'a>>::SourceId;


    #[inline]
    fn source_id(&self) -> Self::SourceId { <T as Spannable>::source_id(self) }

    #[inline]
    fn slice(&self, range: Range) -> Self::Slice { <T as Spannable>::slice(self, range) }

    #[inline]
    fn as_slice(&self) -> &'a [Self::Elem] { <T as Spannable>::as_slice(self) }

    #[inline]
    fn match_while(&self, pred: impl FnMut(&'a Self::Elem) -> bool) -> usize { <T as Spannable>::match_while(self, pred) }

    #[inline]
    fn len(&self) -> usize { <T as Spannable>::len(self) }

    #[inline]
    fn is_empty(&self) -> bool { <T as Spannable>::is_empty(self) }
}
impl<'a, T: ?Sized + Spannable<'a>> Spannable<'a> for Box<T> {
    type Elem = <T as Spannable<'a>>::Elem;
    type Slice = <T as Spannable<'a>>::Slice;
    type SourceId = <T as Spannable<'a>>::SourceId;


    #[inline]
    fn source_id(&self) -> Self::SourceId { <T as Spannable>::source_id(self) }

    #[inline]
    fn slice(&self, range: Range) -> Self::Slice { <T as Spannable>::slice(self, range) }

    #[inline]
    fn as_slice(&self) -> &'a [Self::Elem] { <T as Spannable>::as_slice(self) }

    #[inline]
    fn match_while(&self, pred: impl FnMut(&'a Self::Elem) -> bool) -> usize { <T as Spannable>::match_while(self, pred) }

    #[inline]
    fn len(&self) -> usize { <T as Spannable>::len(self) }

    #[inline]
    fn is_empty(&self) -> bool { <T as Spannable>::is_empty(self) }
}
impl<'a, T: ?Sized + Spannable<'a>> Spannable<'a> for Rc<T> {
    type Elem = <T as Spannable<'a>>::Elem;
    type Slice = <T as Spannable<'a>>::Slice;
    type SourceId = <T as Spannable<'a>>::SourceId;


    #[inline]
    fn source_id(&self) -> Self::SourceId { <T as Spannable>::source_id(self) }

    #[inline]
    fn slice(&self, range: Range) -> Self::Slice { <T as Spannable>::slice(self, range) }

    #[inline]
    fn as_slice(&self) -> &'a [Self::Elem] { <T as Spannable>::as_slice(self) }

    #[inline]
    fn match_while(&self, pred: impl FnMut(&'a Self::Elem) -> bool) -> usize { <T as Spannable>::match_while(self, pred) }

    #[inline]
    fn len(&self) -> usize { <T as Spannable>::len(self) }

    #[inline]
    fn is_empty(&self) -> bool { <T as Spannable>::is_empty(self) }
}
impl<'a, T: ?Sized + Spannable<'a>> Spannable<'a> for Arc<T> {
    type Elem = <T as Spannable<'a>>::Elem;
    type Slice = <T as Spannable<'a>>::Slice;
    type SourceId = <T as Spannable<'a>>::SourceId;


    #[inline]
    fn source_id(&self) -> Self::SourceId { <T as Spannable>::source_id(self) }

    #[inline]
    fn slice(&self, range: Range) -> Self::Slice { <T as Spannable>::slice(self, range) }

    #[inline]
    fn as_slice(&self) -> &'a [Self::Elem] { <T as Spannable>::as_slice(self) }

    #[inline]
    fn match_while(&self, pred: impl FnMut(&'a Self::Elem) -> bool) -> usize { <T as Spannable>::match_while(self, pred) }

    #[inline]
    fn len(&self) -> usize { <T as Spannable>::len(self) }

    #[inline]
    fn is_empty(&self) -> bool { <T as Spannable>::is_empty(self) }
}
impl<'a, 'b, T: ?Sized + Spannable<'a>> Spannable<'a> for MutexGuard<'b, T> {
    type Elem = <T as Spannable<'a>>::Elem;
    type Slice = <T as Spannable<'a>>::Slice;
    type SourceId = <T as Spannable<'a>>::SourceId;


    #[inline]
    fn source_id(&self) -> Self::SourceId { <T as Spannable>::source_id(self) }

    #[inline]
    fn slice(&self, range: Range) -> Self::Slice { <T as Spannable>::slice(self, range) }

    #[inline]
    fn as_slice(&self) -> &'a [Self::Elem] { <T as Spannable>::as_slice(self) }

    #[inline]
    fn match_while(&self, pred: impl FnMut(&'a Self::Elem) -> bool) -> usize { <T as Spannable>::match_while(self, pred) }

    #[inline]
    fn len(&self) -> usize { <T as Spannable>::len(self) }

    #[inline]
    fn is_empty(&self) -> bool { <T as Spannable>::is_empty(self) }
}
impl<'a, 'b, T: ?Sized + Spannable<'a>> Spannable<'a> for RwLockReadGuard<'b, T> {
    type Elem = <T as Spannable<'a>>::Elem;
    type Slice = <T as Spannable<'a>>::Slice;
    type SourceId = <T as Spannable<'a>>::SourceId;


    #[inline]
    fn source_id(&self) -> Self::SourceId { <T as Spannable>::source_id(self) }

    #[inline]
    fn slice(&self, range: Range) -> Self::Slice { <T as Spannable>::slice(self, range) }

    #[inline]
    fn as_slice(&self) -> &'a [Self::Elem] { <T as Spannable>::as_slice(self) }

    #[inline]
    fn match_while(&self, pred: impl FnMut(&'a Self::Elem) -> bool) -> usize { <T as Spannable>::match_while(self, pred) }

    #[inline]
    fn len(&self) -> usize { <T as Spannable>::len(self) }

    #[inline]
    fn is_empty(&self) -> bool { <T as Spannable>::is_empty(self) }
}
impl<'a, 'b, T: ?Sized + Spannable<'a>> Spannable<'a> for RwLockWriteGuard<'b, T> {
    type Elem = <T as Spannable<'a>>::Elem;
    type Slice = <T as Spannable<'a>>::Slice;
    type SourceId = <T as Spannable<'a>>::SourceId;


    #[inline]
    fn source_id(&self) -> Self::SourceId { <T as Spannable>::source_id(self) }

    #[inline]
    fn slice(&self, range: Range) -> Self::Slice { <T as Spannable>::slice(self, range) }

    #[inline]
    fn as_slice(&self) -> &'a [Self::Elem] { <T as Spannable>::as_slice(self) }

    #[inline]
    fn match_while(&self, pred: impl FnMut(&'a Self::Elem) -> bool) -> usize { <T as Spannable>::match_while(self, pred) }

    #[inline]
    fn len(&self) -> usize { <T as Spannable>::len(self) }

    #[inline]
    fn is_empty(&self) -> bool { <T as Spannable>::is_empty(self) }
}
impl<'a, 'b, T: ?Sized + Spannable<'a>> Spannable<'a> for Ref<'b, T> {
    type Elem = <T as Spannable<'a>>::Elem;
    type Slice = <T as Spannable<'a>>::Slice;
    type SourceId = <T as Spannable<'a>>::SourceId;


    #[inline]
    fn source_id(&self) -> Self::SourceId { <T as Spannable>::source_id(self) }

    #[inline]
    fn slice(&self, range: Range) -> Self::Slice { <T as Spannable>::slice(self, range) }

    #[inline]
    fn as_slice(&self) -> &'a [Self::Elem] { <T as Spannable>::as_slice(self) }

    #[inline]
    fn match_while(&self, pred: impl FnMut(&'a Self::Elem) -> bool) -> usize { <T as Spannable>::match_while(self, pred) }

    #[inline]
    fn len(&self) -> usize { <T as Spannable>::len(self) }

    #[inline]
    fn is_empty(&self) -> bool { <T as Spannable>::is_empty(self) }
}
impl<'a, 'b, T: ?Sized + Spannable<'a>> Spannable<'a> for RefMut<'b, T> {
    type Elem = <T as Spannable<'a>>::Elem;
    type Slice = <T as Spannable<'a>>::Slice;
    type SourceId = <T as Spannable<'a>>::SourceId;


    #[inline]
    fn source_id(&self) -> Self::SourceId { <T as Spannable>::source_id(self) }

    #[inline]
    fn slice(&self, range: Range) -> Self::Slice { <T as Spannable>::slice(self, range) }

    #[inline]
    fn as_slice(&self) -> &'a [Self::Elem] { <T as Spannable>::as_slice(self) }

    #[inline]
    fn match_while(&self, pred: impl FnMut(&'a Self::Elem) -> bool) -> usize { <T as Spannable>::match_while(self, pred) }

    #[inline]
    fn len(&self) -> usize { <T as Spannable>::len(self) }

    #[inline]
    fn is_empty(&self) -> bool { <T as Spannable>::is_empty(self) }
}



/// Defines an alias for [`Spannable`]s over [bytes](u8).
///
/// See [`Spannable`]'s docs for more information.
pub trait SpannableBytes<'s>: Spannable<'s, Elem = u8> {
    /// Alias for [`Spannable::as_slice()`] that has a more topical name.
    ///
    /// # Returns
    /// A byte slice representing this array.
    fn as_bytes(&self) -> &'s [u8];

    /// Alias for [`Spannable::match_while()`] that allows one to match bytes by ownership instead
    /// of reference.
    ///
    /// See [`Spannable::match_while()`] for more information.
    ///
    /// # Arguments
    /// - `pred`: Some predicate to match the bytes at the head of self with.
    ///
    /// # Returns
    /// The index of the first byte that does not match the predicate. If all elements match it,
    /// then this equals the length of the array.
    fn match_bytes_while(&self, pred: impl FnMut(u8) -> bool) -> usize;
}

// Default impl to make it an alias
impl<'s, T: Spannable<'s, Elem = u8>> SpannableBytes<'s> for T {
    #[inline]
    fn as_bytes(&self) -> &'s [u8] { self.as_slice() }

    #[inline]
    fn match_bytes_while(&self, mut pred: impl FnMut(u8) -> bool) -> usize { self.match_while(|b| pred(*b)) }
}



/// Defines an extension over [`SpannableBytes`] that allows them to be matched by grapheme instead
/// of byte.
///
/// Note: this function can be implemented more general iff we find a way to parse graphemes off
/// the head of a stream (i.e., through [`Spannable::match_while()`]). For now, though, we just
/// implement it specifically for [`str`].
///
/// See [`Spannable`]'s docs for more information.
pub trait SpannableUtf8<'s>: SpannableBytes<'s> {
    /// Alias for [`Spannable::as_slice()`] that returns this array as a valid UTF-8 slice (i.e.,
    /// a [`str`]).
    ///
    /// # Returns
    /// A [`str`] representing this array.
    fn as_str(&self) -> &'s str;

    /// Interpreting this byte array as unicode segments, will match up to where graphemes at the
    /// head of the byte array match the given predicate.
    ///
    /// Note that it is iterated by _extended_ unicode graphemes.
    ///
    /// By default, does something like:
    /// ```ignore
    /// let mut i: usize = 0;
    /// for elem in SpannableUtf8::as_str().graphemes(true) {
    ///     if !pred(elem) {
    ///         return i;
    ///     }
    ///     i += elem.len();
    /// }
    /// i
    /// ```
    ///
    /// # Arguments
    /// - `pred`: Some predicate to match the graphemes at the head of self with.
    ///
    /// # Returns
    /// The _byte_ index of the first grapheme that does not match the predicate. If all elements
    /// match it, then this equals the (byte) length of the array.
    #[inline]
    fn match_utf8_while(&self, mut pred: impl FnMut(&'s str) -> bool) -> usize {
        let self_str: &str = self.as_str();
        for (i, elem) in self_str.grapheme_indices(true) {
            if !pred(elem) {
                return i;
            }
        }
        self_str.len()
    }
}

// Impl for string-like types & spans
impl<'s> SpannableUtf8<'s> for &'s str {
    #[inline]
    fn as_str(&self) -> &'s str { self }
}
impl<'a, T: Clone + Debug + Eq + PartialEq, U: SpannableUtf8<'a>> SpannableUtf8<'a> for (T, U) {
    #[inline]
    fn as_str(&self) -> &'a str { <U as SpannableUtf8<'a>>::as_str(&self.1) }

    #[inline]
    fn match_utf8_while(&self, pred: impl FnMut(&'a str) -> bool) -> usize { <U as SpannableUtf8<'a>>::match_utf8_while(&self.1, pred) }
}
// The `SpannableUtf8` impl for Span is over at its own definition

// Pointer-like impls
impl<'a, 'b, T: ?Sized + SpannableUtf8<'a>> SpannableUtf8<'a> for &'b T {
    #[inline]
    fn as_str(&self) -> &'a str { <T as SpannableUtf8<'a>>::as_str(self) }

    #[inline]
    fn match_utf8_while(&self, pred: impl FnMut(&'a str) -> bool) -> usize { <T as SpannableUtf8<'a>>::match_utf8_while(self, pred) }
}
impl<'a, 'b, T: ?Sized + SpannableUtf8<'a>> SpannableUtf8<'a> for &'b mut T {
    #[inline]
    fn as_str(&self) -> &'a str { <T as SpannableUtf8<'a>>::as_str(self) }

    #[inline]
    fn match_utf8_while(&self, pred: impl FnMut(&'a str) -> bool) -> usize { <T as SpannableUtf8<'a>>::match_utf8_while(self, pred) }
}
impl<'a, T: ?Sized + SpannableUtf8<'a>> SpannableUtf8<'a> for Box<T> {
    #[inline]
    fn as_str(&self) -> &'a str { <T as SpannableUtf8<'a>>::as_str(self) }

    #[inline]
    fn match_utf8_while(&self, pred: impl FnMut(&'a str) -> bool) -> usize { <T as SpannableUtf8<'a>>::match_utf8_while(self, pred) }
}
impl<'a, T: ?Sized + SpannableUtf8<'a>> SpannableUtf8<'a> for Rc<T> {
    #[inline]
    fn as_str(&self) -> &'a str { <T as SpannableUtf8<'a>>::as_str(self) }

    #[inline]
    fn match_utf8_while(&self, pred: impl FnMut(&'a str) -> bool) -> usize { <T as SpannableUtf8<'a>>::match_utf8_while(self, pred) }
}
impl<'a, T: ?Sized + SpannableUtf8<'a>> SpannableUtf8<'a> for Arc<T> {
    #[inline]
    fn as_str(&self) -> &'a str { <T as SpannableUtf8<'a>>::as_str(self) }

    #[inline]
    fn match_utf8_while(&self, pred: impl FnMut(&'a str) -> bool) -> usize { <T as SpannableUtf8<'a>>::match_utf8_while(self, pred) }
}
impl<'a, 'b, T: ?Sized + SpannableUtf8<'a>> SpannableUtf8<'a> for MutexGuard<'b, T> {
    #[inline]
    fn as_str(&self) -> &'a str { <T as SpannableUtf8<'a>>::as_str(self) }

    #[inline]
    fn match_utf8_while(&self, pred: impl FnMut(&'a str) -> bool) -> usize { <T as SpannableUtf8<'a>>::match_utf8_while(self, pred) }
}
impl<'a, 'b, T: ?Sized + SpannableUtf8<'a>> SpannableUtf8<'a> for RwLockReadGuard<'b, T> {
    #[inline]
    fn as_str(&self) -> &'a str { <T as SpannableUtf8<'a>>::as_str(self) }

    #[inline]
    fn match_utf8_while(&self, pred: impl FnMut(&'a str) -> bool) -> usize { <T as SpannableUtf8<'a>>::match_utf8_while(self, pred) }
}
impl<'a, 'b, T: ?Sized + SpannableUtf8<'a>> SpannableUtf8<'a> for RwLockWriteGuard<'b, T> {
    #[inline]
    fn as_str(&self) -> &'a str { <T as SpannableUtf8<'a>>::as_str(self) }

    #[inline]
    fn match_utf8_while(&self, pred: impl FnMut(&'a str) -> bool) -> usize { <T as SpannableUtf8<'a>>::match_utf8_while(self, pred) }
}
impl<'a, 'b, T: ?Sized + SpannableUtf8<'a>> SpannableUtf8<'a> for Ref<'b, T> {
    #[inline]
    fn as_str(&self) -> &'a str { <T as SpannableUtf8<'a>>::as_str(self) }

    #[inline]
    fn match_utf8_while(&self, pred: impl FnMut(&'a str) -> bool) -> usize { <T as SpannableUtf8<'a>>::match_utf8_while(self, pred) }
}
impl<'a, 'b, T: ?Sized + SpannableUtf8<'a>> SpannableUtf8<'a> for RefMut<'b, T> {
    #[inline]
    fn as_str(&self) -> &'a str { <T as SpannableUtf8<'a>>::as_str(self) }

    #[inline]
    fn match_utf8_while(&self, pred: impl FnMut(&'a str) -> bool) -> usize { <T as SpannableUtf8<'a>>::match_utf8_while(self, pred) }
}





/***** TESTS *****/
#[cfg(test)]
mod tests {
    use super::*;
    use crate::span::Span;

    #[test]
    fn test_spannable_match_while() {
        assert_eq!(Span::new("abcabc").match_bytes_while(|b| b >= b'a' && b <= b'c'), 6);
        assert_eq!(Span::new("abcdef").match_bytes_while(|b| b >= b'a' && b <= b'c'), 3);
        assert_eq!(Span::new("defghi").match_bytes_while(|b| b >= b'a' && b <= b'c'), 0);

        assert_eq!(Span::ranged("abcabc", 3..).match_bytes_while(|b| b >= b'a' && b <= b'c'), 3);
        assert_eq!(Span::ranged("abcdef", 3..).match_bytes_while(|b| b >= b'a' && b <= b'c'), 0);
        assert_eq!(Span::ranged("defghi", 3..).match_bytes_while(|b| b >= b'a' && b <= b'c'), 0);
    }
}
