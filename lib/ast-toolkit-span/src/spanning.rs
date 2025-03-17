//  SPANNING 2.rs
//    by Lut99
//
//  Created:
//    17 Mar 2025, 10:19:29
//  Last edited:
//    17 Mar 2025, 14:04:55
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements traits for things that contain [`Span`]s.
//

use std::borrow::Cow;

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
impl<S: Clone> Spanning<S> for Span<S> {
    #[inline]
    fn span(&self) -> Cow<Span<S>> { Cow::Borrowed(self) }

    #[inline]
    fn into_span(self) -> Span<S> { self }
}
