//  SPAN.rs
//    by Lut99
//
//  Created:
//    05 Apr 2024, 18:10:59
//  Last edited:
//    18 Mar 2025, 10:31:52
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements some extensions on [`Span`]s that allow us to perform
//!   very generic operations on them.
//

use std::cell::{Ref, RefMut};
use std::fmt::Debug;
use std::hash::Hash;
use std::rc::Rc;
use std::sync::{Arc, MutexGuard, RwLockReadGuard, RwLockWriteGuard};

use ast_toolkit_span::{Span, Spannable};
use unicode_segmentation::{GraphemeIndices, Graphemes, UnicodeSegmentation as _};


/***** ITERATORS *****/
/// Defines an iterator over the graphemes (with indices) in a [`Span`].
///
/// Note that this is separate because we'd need an unnamable closure to build it within a separate
/// iterator, which we cannot use in the [`Utf8Parsable::GraphIndicesIter`].
#[derive(Debug)]
pub struct GraphemesIndicesIter<I> {
    /// The wrapped iterator of the source.
    iter:  I,
    /// The offset to skip past upon first iter (inclusive).
    start: usize,
    /// The offset up to which point to iterate (exclusive).
    end:   usize,
}
impl<'a, I: Iterator<Item = (usize, &'a str)>> Iterator for GraphemesIndicesIter<I> {
    type Item = (usize, &'a str);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            // Get the next element, if any
            let (i, c): (usize, &'a str) = self.iter.next()?;

            // Check if we're still skipping
            if i < self.start || i >= self.end {
                continue;
            }
            return Some((i, c));
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) { (0, self.iter.size_hint().1) }
}





/***** LIBRARY *****/
/// Extends a [`Spannable`] with the power to yield elements from the head of the input.
///
/// # Generics
/// - `E`: The type of input yielded.
pub trait Parsable: Spannable {
    /// The type of element iterated.
    type Elem: ?Sized;
    /// The type of iterator that yields elements.
    type Iter<'s>: Iterator<Item = &'s Self::Elem>
    where
        Self: 's;


    /// Returns an [`Iterator`] that will yield elements from the head of the input stream, in-
    /// order.
    ///
    /// # Returns
    /// A [`Parsable::Iter`]ator over [`Parsable::Elem`] that represents the elements of the source
    /// array.
    fn elems<'s>(&'s self) -> Self::Iter<'s>;
}

// Default impls
impl Parsable for str {
    type Elem = u8;
    type Iter<'s> = std::slice::Iter<'s, u8>;

    #[inline]
    fn elems<'s>(&'s self) -> Self::Iter<'s> { self.as_bytes().iter() }
}
impl<T> Parsable for [T] {
    type Elem = T;
    type Iter<'s>
        = std::slice::Iter<'s, T>
    where
        T: 's;

    #[inline]
    fn elems<'s>(&'s self) -> Self::Iter<'s> { self.iter() }
}
impl<const LEN: usize, T: Hash> Parsable for [T; LEN] {
    type Elem = T;
    type Iter<'s>
        = <[T] as Parsable>::Iter<'s>
    where
        Self: 's;

    #[inline]
    fn elems<'s>(&'s self) -> Self::Iter<'s> { <[T]>::elems(self.as_slice()) }
}
impl<T: Debug + Eq + PartialEq, U: Parsable> Parsable for (T, U) {
    type Elem = <U as Parsable>::Elem;
    type Iter<'s>
        = <U as Parsable>::Iter<'s>
    where
        Self: 's;

    #[inline]
    fn elems<'s>(&'s self) -> Self::Iter<'s> { <U as Parsable>::elems(&self.1) }
}

// Span impl
impl<S: Clone + Parsable> Parsable for Span<S> {
    type Elem = <S as Parsable>::Elem;
    type Iter<'s>
        = std::iter::Take<std::iter::Skip<<S as Parsable>::Iter<'s>>>
    where
        Self: 's;

    #[inline]
    fn elems<'s>(&'s self) -> Self::Iter<'s> {
        // Find the bounds of the range
        let start: usize = match self.range().start_resolved(self.source().len()) {
            Some(idx) => idx,
            None => self.len(),
        };
        let end: usize = match self.range().end_resolved(self.source().len()) {
            Some(idx) => idx,
            None => self.len(),
        };

        // Hack time: instead of taking the slice of ourselves, we use the fact we know the offset
        // to simply offset the main iterator
        self.source().elems().skip(start).take(end.saturating_sub(start))
    }
}

// Pointer-like impls
impl<'a, T: ?Sized + Parsable> Parsable for &'a T {
    type Elem = <T as Parsable>::Elem;
    type Iter<'s>
        = <T as Parsable>::Iter<'s>
    where
        Self: 's;

    #[inline]
    fn elems<'s>(&'s self) -> Self::Iter<'s> { <T as Parsable>::elems(self) }
}
impl<'a, T: ?Sized + Parsable> Parsable for &'a mut T {
    type Elem = <T as Parsable>::Elem;
    type Iter<'s>
        = <T as Parsable>::Iter<'s>
    where
        Self: 's;

    #[inline]
    fn elems<'s>(&'s self) -> Self::Iter<'s> { <T as Parsable>::elems(self) }
}
impl<T: ?Sized + Parsable> Parsable for Box<T> {
    type Elem = <T as Parsable>::Elem;
    type Iter<'s>
        = <T as Parsable>::Iter<'s>
    where
        Self: 's;

    #[inline]
    fn elems<'s>(&'s self) -> Self::Iter<'s> { <T as Parsable>::elems(self) }
}
impl<T: ?Sized + Parsable> Parsable for Rc<T> {
    type Elem = <T as Parsable>::Elem;
    type Iter<'s>
        = <T as Parsable>::Iter<'s>
    where
        Self: 's;

    #[inline]
    fn elems<'s>(&'s self) -> Self::Iter<'s> { <T as Parsable>::elems(self) }
}
impl<T: ?Sized + Parsable> Parsable for Arc<T> {
    type Elem = <T as Parsable>::Elem;
    type Iter<'s>
        = <T as Parsable>::Iter<'s>
    where
        Self: 's;

    #[inline]
    fn elems<'s>(&'s self) -> Self::Iter<'s> { <T as Parsable>::elems(self) }
}
impl<'a, T: ?Sized + Parsable> Parsable for MutexGuard<'a, T> {
    type Elem = <T as Parsable>::Elem;
    type Iter<'s>
        = <T as Parsable>::Iter<'s>
    where
        Self: 's;

    #[inline]
    fn elems<'s>(&'s self) -> Self::Iter<'s> { <T as Parsable>::elems(self) }
}
impl<'a, T: ?Sized + Parsable> Parsable for RwLockReadGuard<'a, T> {
    type Elem = <T as Parsable>::Elem;
    type Iter<'s>
        = <T as Parsable>::Iter<'s>
    where
        Self: 's;

    #[inline]
    fn elems<'s>(&'s self) -> Self::Iter<'s> { <T as Parsable>::elems(self) }
}
impl<'a, T: ?Sized + Parsable> Parsable for RwLockWriteGuard<'a, T> {
    type Elem = <T as Parsable>::Elem;
    type Iter<'s>
        = <T as Parsable>::Iter<'s>
    where
        Self: 's;

    #[inline]
    fn elems<'s>(&'s self) -> Self::Iter<'s> { <T as Parsable>::elems(self) }
}
impl<'a, T: ?Sized + Parsable> Parsable for Ref<'a, T> {
    type Elem = <T as Parsable>::Elem;
    type Iter<'s>
        = <T as Parsable>::Iter<'s>
    where
        Self: 's;

    #[inline]
    fn elems<'s>(&'s self) -> Self::Iter<'s> { <T as Parsable>::elems(self) }
}
impl<'a, T: ?Sized + Parsable> Parsable for RefMut<'a, T> {
    type Elem = <T as Parsable>::Elem;
    type Iter<'s>
        = <T as Parsable>::Iter<'s>
    where
        Self: 's;

    #[inline]
    fn elems<'s>(&'s self) -> Self::Iter<'s> { <T as Parsable>::elems(self) }
}



/// Alias for a [`Parsable`] that yields bytes.
pub trait BytesParsable: Parsable<Elem = u8> {
    /// Implements a more contextually-accurate alias for [`Parsable::elems()`].
    ///
    /// # Returns
    /// An [`Iterator`] over bytes, as implemented by [`Parsable::elems()`].
    fn bytes<'s>(&'s self) -> Self::Iter<'s>;
}
impl<T: ?Sized + Parsable<Elem = u8>> BytesParsable for T {
    #[inline]
    fn bytes<'s>(&'s self) -> Self::Iter<'s> { <Self as Parsable>::elems(self) }
}



/// An extension of a [`BytesParsable`] that also yields unicode graphemes.
pub trait Utf8Parsable: BytesParsable {
    /// The type of iterator that yields graphemes.
    type GraphsIter<'s>: Iterator<Item = &'s str>
    where
        Self: 's;
    /// The type of the iterator that yields graphemes + their byte indices.
    type GraphIndicesIter<'s>: Iterator<Item = (usize, &'s str)>
    where
        Self: 's;

    /// Returns an [`Iterator`] that will yield unicode graphemes from the head of the input
    /// stream, in-order.
    ///
    /// # Returns
    /// A [`Parsable::GraphsIter`]ator over [`&str`](str)s that represents the graphemes at the
    /// head of the source.
    fn graphs<'s>(&'s self) -> Self::GraphsIter<'s>;

    /// Returns an [`Iterator`] that will yield not just unicode graphemes from the head of the
    /// input stream, in-order, but also matching byte indices for slicing.
    ///
    /// # Returns
    /// A [`Parsable::GraphIndicesIter`]ator over [`&str`](str)s that represents the graphemes
    /// and their indices at the head of the source.
    fn graph_indices<'s>(&'s self) -> Self::GraphIndicesIter<'s>;
}

// Default impls
impl Utf8Parsable for str {
    type GraphsIter<'s> = Graphemes<'s>;
    type GraphIndicesIter<'s> = GraphemeIndices<'s>;

    #[inline]
    fn graphs<'s>(&'s self) -> Self::GraphsIter<'s> { self.graphemes(true) }

    #[inline]
    fn graph_indices<'s>(&'s self) -> Self::GraphIndicesIter<'s> { self.grapheme_indices(true) }
}
impl<S: Clone + Utf8Parsable> Utf8Parsable for Span<S> {
    type GraphsIter<'s>
        = std::iter::Map<GraphemesIndicesIter<<S as Utf8Parsable>::GraphIndicesIter<'s>>, fn((usize, &'s str)) -> &'s str>
    where
        Self: 's;
    type GraphIndicesIter<'s>
        = GraphemesIndicesIter<<S as Utf8Parsable>::GraphIndicesIter<'s>>
    where
        Self: 's;

    /// # Safety
    /// We assume that the current span ALWAYS lies _exactly_ on a grapheme boundary.
    #[inline]
    fn graphs<'s>(&'s self) -> Self::GraphsIter<'s> { self.graph_indices().map(|(_, c)| c) }

    /// # Safety
    /// We assume that the current span ALWAYS lies _exactly_ on a grapheme boundary.
    #[inline]
    fn graph_indices<'s>(&'s self) -> Self::GraphIndicesIter<'s> {
        // Do some wizardry to find up to where to cut the input
        let start: usize = match self.range().start_resolved(self.source().len()) {
            Some(idx) => idx,
            None => self.len(),
        };
        let end: usize = match self.range().end_resolved(self.source().len()) {
            Some(idx) => idx,
            None => self.len(),
        };
        GraphemesIndicesIter { iter: self.source().graph_indices(), start, end }
    }
}

// Pointer-like impls
impl<'a, T: ?Sized + Utf8Parsable> Utf8Parsable for &'a T {
    type GraphsIter<'s>
        = <T as Utf8Parsable>::GraphsIter<'s>
    where
        Self: 's;
    type GraphIndicesIter<'s>
        = <T as Utf8Parsable>::GraphIndicesIter<'s>
    where
        Self: 's;

    #[inline]
    fn graphs<'s>(&'s self) -> Self::GraphsIter<'s> { <T as Utf8Parsable>::graphs(self) }

    #[inline]
    fn graph_indices<'s>(&'s self) -> Self::GraphIndicesIter<'s> { <T as Utf8Parsable>::graph_indices(self) }
}
impl<'a, T: ?Sized + Utf8Parsable> Utf8Parsable for &'a mut T {
    type GraphsIter<'s>
        = <T as Utf8Parsable>::GraphsIter<'s>
    where
        Self: 's;
    type GraphIndicesIter<'s>
        = <T as Utf8Parsable>::GraphIndicesIter<'s>
    where
        Self: 's;

    #[inline]
    fn graphs<'s>(&'s self) -> Self::GraphsIter<'s> { <T as Utf8Parsable>::graphs(self) }

    #[inline]
    fn graph_indices<'s>(&'s self) -> Self::GraphIndicesIter<'s> { <T as Utf8Parsable>::graph_indices(self) }
}
impl<T: ?Sized + Utf8Parsable> Utf8Parsable for Box<T> {
    type GraphsIter<'s>
        = <T as Utf8Parsable>::GraphsIter<'s>
    where
        Self: 's;
    type GraphIndicesIter<'s>
        = <T as Utf8Parsable>::GraphIndicesIter<'s>
    where
        Self: 's;

    #[inline]
    fn graphs<'s>(&'s self) -> Self::GraphsIter<'s> { <T as Utf8Parsable>::graphs(self) }

    #[inline]
    fn graph_indices<'s>(&'s self) -> Self::GraphIndicesIter<'s> { <T as Utf8Parsable>::graph_indices(self) }
}
impl<T: ?Sized + Utf8Parsable> Utf8Parsable for Rc<T> {
    type GraphsIter<'s>
        = <T as Utf8Parsable>::GraphsIter<'s>
    where
        Self: 's;
    type GraphIndicesIter<'s>
        = <T as Utf8Parsable>::GraphIndicesIter<'s>
    where
        Self: 's;

    #[inline]
    fn graphs<'s>(&'s self) -> Self::GraphsIter<'s> { <T as Utf8Parsable>::graphs(self) }

    #[inline]
    fn graph_indices<'s>(&'s self) -> Self::GraphIndicesIter<'s> { <T as Utf8Parsable>::graph_indices(self) }
}
impl<T: ?Sized + Utf8Parsable> Utf8Parsable for Arc<T> {
    type GraphsIter<'s>
        = <T as Utf8Parsable>::GraphsIter<'s>
    where
        Self: 's;
    type GraphIndicesIter<'s>
        = <T as Utf8Parsable>::GraphIndicesIter<'s>
    where
        Self: 's;

    #[inline]
    fn graphs<'s>(&'s self) -> Self::GraphsIter<'s> { <T as Utf8Parsable>::graphs(self) }

    #[inline]
    fn graph_indices<'s>(&'s self) -> Self::GraphIndicesIter<'s> { <T as Utf8Parsable>::graph_indices(self) }
}
impl<'a, T: ?Sized + Utf8Parsable> Utf8Parsable for MutexGuard<'a, T> {
    type GraphsIter<'s>
        = <T as Utf8Parsable>::GraphsIter<'s>
    where
        Self: 's;
    type GraphIndicesIter<'s>
        = <T as Utf8Parsable>::GraphIndicesIter<'s>
    where
        Self: 's;

    #[inline]
    fn graphs<'s>(&'s self) -> Self::GraphsIter<'s> { <T as Utf8Parsable>::graphs(self) }

    #[inline]
    fn graph_indices<'s>(&'s self) -> Self::GraphIndicesIter<'s> { <T as Utf8Parsable>::graph_indices(self) }
}
impl<'a, T: ?Sized + Utf8Parsable> Utf8Parsable for RwLockReadGuard<'a, T> {
    type GraphsIter<'s>
        = <T as Utf8Parsable>::GraphsIter<'s>
    where
        Self: 's;
    type GraphIndicesIter<'s>
        = <T as Utf8Parsable>::GraphIndicesIter<'s>
    where
        Self: 's;

    #[inline]
    fn graphs<'s>(&'s self) -> Self::GraphsIter<'s> { <T as Utf8Parsable>::graphs(self) }

    #[inline]
    fn graph_indices<'s>(&'s self) -> Self::GraphIndicesIter<'s> { <T as Utf8Parsable>::graph_indices(self) }
}
impl<'a, T: ?Sized + Utf8Parsable> Utf8Parsable for RwLockWriteGuard<'a, T> {
    type GraphsIter<'s>
        = <T as Utf8Parsable>::GraphsIter<'s>
    where
        Self: 's;
    type GraphIndicesIter<'s>
        = <T as Utf8Parsable>::GraphIndicesIter<'s>
    where
        Self: 's;

    #[inline]
    fn graphs<'s>(&'s self) -> Self::GraphsIter<'s> { <T as Utf8Parsable>::graphs(self) }

    #[inline]
    fn graph_indices<'s>(&'s self) -> Self::GraphIndicesIter<'s> { <T as Utf8Parsable>::graph_indices(self) }
}
impl<'a, T: ?Sized + Utf8Parsable> Utf8Parsable for Ref<'a, T> {
    type GraphsIter<'s>
        = <T as Utf8Parsable>::GraphsIter<'s>
    where
        Self: 's;
    type GraphIndicesIter<'s>
        = <T as Utf8Parsable>::GraphIndicesIter<'s>
    where
        Self: 's;

    #[inline]
    fn graphs<'s>(&'s self) -> Self::GraphsIter<'s> { <T as Utf8Parsable>::graphs(self) }

    #[inline]
    fn graph_indices<'s>(&'s self) -> Self::GraphIndicesIter<'s> { <T as Utf8Parsable>::graph_indices(self) }
}
impl<'a, T: ?Sized + Utf8Parsable> Utf8Parsable for RefMut<'a, T> {
    type GraphsIter<'s>
        = <T as Utf8Parsable>::GraphsIter<'s>
    where
        Self: 's;
    type GraphIndicesIter<'s>
        = <T as Utf8Parsable>::GraphIndicesIter<'s>
    where
        Self: 's;

    #[inline]
    fn graphs<'s>(&'s self) -> Self::GraphsIter<'s> { <T as Utf8Parsable>::graphs(self) }

    #[inline]
    fn graph_indices<'s>(&'s self) -> Self::GraphIndicesIter<'s> { <T as Utf8Parsable>::graph_indices(self) }
}





/***** TESTS *****/
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bytes() {
        let span = Span::new(b"5+5");
        let mut iter = span.bytes();
        assert_eq!(iter.next(), Some(&b'5'));
        assert_eq!(iter.next(), Some(&b'+'));
        assert_eq!(iter.next(), Some(&b'5'));
        assert_eq!(iter.next(), None);

        let span = Span::ranged(b"5+5", 0..1);
        let mut iter = span.bytes();
        assert_eq!(iter.next(), Some(&b'5'));
        assert_eq!(iter.next(), None);

        let span = Span::ranged(b"5+5", 1..2);
        let mut iter = span.bytes();
        assert_eq!(iter.next(), Some(&b'+'));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_graph_indices() {
        let span = Span::new("5+5");
        let mut iter = span.graph_indices();
        assert_eq!(iter.next(), Some((0, "5")));
        assert_eq!(iter.next(), Some((1, "+")));
        assert_eq!(iter.next(), Some((2, "5")));
        assert_eq!(iter.next(), None);

        let span = Span::ranged("5+5", 0..1);
        let mut iter = span.graph_indices();
        assert_eq!(iter.next(), Some((0, "5")));
        assert_eq!(iter.next(), None);

        let span = Span::ranged("5+5", 1..2);
        let mut iter = span.graph_indices();
        assert_eq!(iter.next(), Some((1, "+")));
        assert_eq!(iter.next(), None);
    }
}
