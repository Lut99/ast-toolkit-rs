//  SPANNABLE 2.rs
//    by Lut99
//
//  Created:
//    17 Mar 2025, 10:14:05
//  Last edited:
//    24 Mar 2025, 11:42:19
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements a trait abstracting over things that are sensible to put
//!   in a [`Span`].
//

use std::cell::{Ref, RefMut};
use std::fmt::Debug;
use std::hash::{DefaultHasher, Hash, Hasher as _};
use std::rc::Rc;
use std::sync::{Arc, MutexGuard, RwLockReadGuard, RwLockWriteGuard};

use crate::range::Range;


/***** LIBRARY *****/
/// Defines things that can be [`Span`]ned.
///
/// Remember that this always concerns a array of sorts.
pub trait Spannable {
    /// Describes the sliced version of this Spannable.
    type Slice<'s>
    where
        Self: 's;
    /// Describes the ID returned by [`Spannable::source_id()`].
    type SourceId<'s>: Debug + Eq + PartialEq
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

    #[inline]
    fn is_empty(&self) -> bool { <[T]>::is_empty(self) }
}
impl<const LEN: usize, T: Hash> Spannable for [T; LEN] {
    type SourceId<'s>
        = u64
    where
        Self: 's;
    type Slice<'s>
        = &'s [T]
    where
        Self: 's;

    #[inline]
    fn source_id<'s>(&'s self) -> Self::SourceId<'s> {
        let mut hasher = DefaultHasher::new();
        for elem in self {
            elem.hash(&mut hasher);
        }
        hasher.finish()
    }

    #[inline]
    fn slice<'s>(&'s self, range: Range) -> Self::Slice<'s> { <[T]>::slice(self.as_slice(), range) }

    #[inline]
    fn len(&self) -> usize { <[T]>::len(self.as_slice()) }

    #[inline]
    fn is_empty(&self) -> bool { <[T]>::is_empty(self.as_slice()) }
}
impl<T: Debug + Eq + PartialEq, U: Spannable> Spannable for (T, U) {
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

// Pointer-like impls
impl<'a, T: ?Sized + Spannable> Spannable for &'a T {
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
impl<'a, T: ?Sized + Spannable> Spannable for &'a mut T {
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
impl<T: ?Sized + Spannable> Spannable for Box<T> {
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
impl<T: ?Sized + Spannable> Spannable for Rc<T> {
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
impl<T: ?Sized + Spannable> Spannable for Arc<T> {
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
impl<'a, T: ?Sized + Spannable> Spannable for MutexGuard<'a, T> {
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
impl<'a, T: ?Sized + Spannable> Spannable for RwLockReadGuard<'a, T> {
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
impl<'a, T: ?Sized + Spannable> Spannable for RwLockWriteGuard<'a, T> {
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
impl<'a, T: ?Sized + Spannable> Spannable for Ref<'a, T> {
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
impl<'a, T: ?Sized + Spannable> Spannable for RefMut<'a, T> {
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
