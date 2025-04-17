//  SPANNING 2.rs
//    by Lut99
//
//  Created:
//    17 Mar 2025, 10:19:29
//  Last edited:
//    18 Mar 2025, 17:16:22
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements traits for things that contain [`Span`]s.
//

use std::borrow::Cow;
use std::cell::{Ref, RefMut};
use std::convert::Infallible;
use std::rc::Rc;
use std::sync::{Arc, MutexGuard, RwLockReadGuard, RwLockWriteGuard};

use crate::span::Span;


/***** LIBRARY *****/
/// Abstracts over things (e.g., nodes in ASTs) that span some area of source text.
pub trait Spanning<S: Clone> {
    /// Returns the span within this source text in it.
    ///
    /// # Returns
    /// A [`Cow`] over a [`Span`] in order to cover one of two use-cases:
    /// - The [`Span`] is stored directly in this type. In that case, return a [`Cow::Borrowed`];
    ///   or
    /// - The [`Span`] must be constructed first (e.g., by joining other [`Span`]s). In that case,
    ///   a [`Cow::Owned`] is necessary.
    fn span(&self) -> Cow<Span<S>>;

    /// An optmization where the whole span can be returned instead of referenced or copied.
    ///
    /// # Returns
    /// A [`Span`] that was either stored directly in Self, or either constructed from stored
    /// [`Span`]s.
    fn into_span(self) -> Span<S>;
}

// Default impls
impl<S: Clone> Spanning<S> for Infallible {
    /// NOTE: This implementation will always fail. However, since [`Infallible`] cannot be
    /// constructed, this is fine.
    ///
    /// It exists in order for it to implement `ParseError` in `ast-toolkit-snack`.
    #[inline]
    fn span(&self) -> Cow<Span<S>> { unreachable!() }

    /// NOTE: This implementation will always fail. However, since [`Infallible`] cannot be
    /// constructed, this is fine.
    ///
    /// It exists in order for it to implement `ParseError` in `ast-toolkit-snack`.
    #[inline]
    fn into_span(self) -> Span<S> { unreachable!() }
}
impl<S: Clone> Spanning<S> for Span<S> {
    #[inline]
    fn span(&self) -> Cow<Span<S>> { Cow::Borrowed(self) }

    #[inline]
    fn into_span(self) -> Span<S> { self }
}

// Pointer-like impls
impl<'a, T: ?Sized + Spanning<S>, S: Clone> Spanning<S> for &'a T {
    #[inline]
    fn span(&self) -> Cow<Span<S>> { <T as Spanning<S>>::span(self) }
    #[inline]
    fn into_span(self) -> Span<S> { <T as Spanning<S>>::span(&self).into_owned() }
}
impl<'a, T: ?Sized + Spanning<S>, S: Clone> Spanning<S> for &'a mut T {
    #[inline]
    fn span(&self) -> Cow<Span<S>> { <T as Spanning<S>>::span(self) }
    #[inline]
    fn into_span(self) -> Span<S> { <T as Spanning<S>>::span(&self).into_owned() }
}
impl<T: ?Sized + Spanning<S>, S: Clone> Spanning<S> for Box<T> {
    #[inline]
    fn span(&self) -> Cow<Span<S>> { <T as Spanning<S>>::span(self) }
    #[inline]
    fn into_span(self) -> Span<S> { <T as Spanning<S>>::span(&self).into_owned() }
}
impl<T: ?Sized + Spanning<S>, S: Clone> Spanning<S> for Rc<T> {
    #[inline]
    fn span(&self) -> Cow<Span<S>> { <T as Spanning<S>>::span(self) }
    #[inline]
    fn into_span(self) -> Span<S> { <T as Spanning<S>>::span(&self).into_owned() }
}
impl<T: ?Sized + Spanning<S>, S: Clone> Spanning<S> for Arc<T> {
    #[inline]
    fn span(&self) -> Cow<Span<S>> { <T as Spanning<S>>::span(self) }
    #[inline]
    fn into_span(self) -> Span<S> { <T as Spanning<S>>::span(&self).into_owned() }
}
impl<'a, T: ?Sized + Spanning<S>, S: Clone> Spanning<S> for MutexGuard<'a, T> {
    #[inline]
    fn span(&self) -> Cow<Span<S>> { <T as Spanning<S>>::span(self) }
    #[inline]
    fn into_span(self) -> Span<S> { <T as Spanning<S>>::span(&self).into_owned() }
}
impl<'a, T: ?Sized + Spanning<S>, S: Clone> Spanning<S> for RwLockReadGuard<'a, T> {
    #[inline]
    fn span(&self) -> Cow<Span<S>> { <T as Spanning<S>>::span(self) }
    #[inline]
    fn into_span(self) -> Span<S> { <T as Spanning<S>>::span(&self).into_owned() }
}
impl<'a, T: ?Sized + Spanning<S>, S: Clone> Spanning<S> for RwLockWriteGuard<'a, T> {
    #[inline]
    fn span(&self) -> Cow<Span<S>> { <T as Spanning<S>>::span(self) }
    #[inline]
    fn into_span(self) -> Span<S> { <T as Spanning<S>>::span(&self).into_owned() }
}
impl<'a, T: ?Sized + Spanning<S>, S: Clone> Spanning<S> for Ref<'a, T> {
    #[inline]
    fn span(&self) -> Cow<Span<S>> { <T as Spanning<S>>::span(self) }
    #[inline]
    fn into_span(self) -> Span<S> { <T as Spanning<S>>::span(&self).into_owned() }
}
impl<'a, T: ?Sized + Spanning<S>, S: Clone> Spanning<S> for RefMut<'a, T> {
    #[inline]
    fn span(&self) -> Cow<Span<S>> { <T as Spanning<S>>::span(self) }
    #[inline]
    fn into_span(self) -> Span<S> { <T as Spanning<S>>::span(&self).into_owned() }
}
