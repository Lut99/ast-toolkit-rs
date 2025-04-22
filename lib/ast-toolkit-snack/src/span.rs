//  SPAN.rs
//    by Lut99
//
//  Created:
//    05 Apr 2024, 18:10:59
//  Last edited:
//    22 Apr 2025, 13:26:13
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements some extensions on [`Span`]s that allow us to perform
//!   very generic operations on them.
//

use std::cell::{Ref, RefMut};
use std::fmt::Debug;
use std::iter::Cloned;
use std::rc::Rc;
use std::sync::{Arc, MutexGuard, RwLockReadGuard, RwLockWriteGuard};

use ast_toolkit_span::{Span, Spannable};
use unicode_segmentation::{GraphemeIndices, Graphemes, UnicodeSegmentation as _};


/***** LIBRARY *****/
/// Extends a [`Spannable`] with the power to yield elements from the head of the input.
///
/// # Generics
/// - `'s`: The lifetime of the spanned object. This allows yielded elements to refer to the
///   original source instead of the container referring to it (e.g., a [`Span`]).
pub trait Parsable<'s>: Spannable<'s> {
    /// The type of element iterated.
    type Elem;
    /// The type of iterator that yields elements.
    type Iter: Iterator<Item = Self::Elem>;


    /// Returns an [`Iterator`] that will yield elements from the head of the input stream, in-
    /// order.
    ///
    /// # Returns
    /// A [`Parsable::Iter`]ator over [`Parsable::Elem`] that represents the elements of the source
    /// array.
    fn elems(&self) -> Self::Iter;
}

// Default impls
impl<'s> Parsable<'s> for &'s str {
    type Elem = &'s u8;
    type Iter = std::slice::Iter<'s, u8>;

    #[inline]
    fn elems(&self) -> Self::Iter { self.as_bytes().iter() }
}
impl<'s, T> Parsable<'s> for &'s [T] {
    type Elem = &'s T;
    type Iter = std::slice::Iter<'s, T>;

    #[inline]
    fn elems(&self) -> Self::Iter { self.iter() }
}
impl<'s, T: Clone + Debug + Eq + PartialEq, U: Parsable<'s>> Parsable<'s> for (T, U) {
    type Elem = <U as Parsable<'s>>::Elem;
    type Iter = <U as Parsable<'s>>::Iter;

    #[inline]
    fn elems(&self) -> Self::Iter { <U as Parsable>::elems(&self.1) }
}

// Span impl
impl<'b, S: Clone + Spannable<'b>> Parsable<'b> for Span<S>
where
    S::Slice: Parsable<'b>,
{
    type Elem = <S::Slice as Parsable<'b>>::Elem;
    type Iter = <S::Slice as Parsable<'b>>::Iter;

    #[inline]
    fn elems(&self) -> Self::Iter {
        // // Find the bounds of the range
        // let start: usize = match self.range().start_resolved(self.source().len()) {
        //     Some(idx) => idx,
        //     None => self.len(),
        // };
        // let end: usize = match self.range().end_resolved(self.source().len()) {
        //     Some(idx) => idx,
        //     None => self.len(),
        // };

        // // Hack time: instead of taking the slice of ourselves, we use the fact we know the offset
        // // to simply offset the main iterator
        // self.source().elems().skip(start).take(end.saturating_sub(start))
        self.value().elems()
    }
}

// Pointer-like impls
impl<'s, 'b, T: ?Sized + Parsable<'s>> Parsable<'s> for &'b T {
    type Elem = <T as Parsable<'s>>::Elem;
    type Iter = <T as Parsable<'s>>::Iter;

    #[inline]
    fn elems(&self) -> Self::Iter { <T as Parsable>::elems(self) }
}
impl<'s, 'b, T: ?Sized + Parsable<'s>> Parsable<'s> for &'b mut T {
    type Elem = <T as Parsable<'s>>::Elem;
    type Iter = <T as Parsable<'s>>::Iter;

    #[inline]
    fn elems(&self) -> Self::Iter { <T as Parsable>::elems(self) }
}
impl<'s, T: ?Sized + Parsable<'s>> Parsable<'s> for Box<T> {
    type Elem = <T as Parsable<'s>>::Elem;
    type Iter = <T as Parsable<'s>>::Iter;

    #[inline]
    fn elems(&self) -> Self::Iter { <T as Parsable>::elems(self) }
}
impl<'s, T: ?Sized + Parsable<'s>> Parsable<'s> for Rc<T> {
    type Elem = <T as Parsable<'s>>::Elem;
    type Iter = <T as Parsable<'s>>::Iter;

    #[inline]
    fn elems(&self) -> Self::Iter { <T as Parsable>::elems(self) }
}
impl<'s, T: ?Sized + Parsable<'s>> Parsable<'s> for Arc<T> {
    type Elem = <T as Parsable<'s>>::Elem;
    type Iter = <T as Parsable<'s>>::Iter;

    #[inline]
    fn elems(&self) -> Self::Iter { <T as Parsable>::elems(self) }
}
impl<'s, 'b, T: ?Sized + Parsable<'s>> Parsable<'s> for MutexGuard<'b, T> {
    type Elem = <T as Parsable<'s>>::Elem;
    type Iter = <T as Parsable<'s>>::Iter;

    #[inline]
    fn elems(&self) -> Self::Iter { <T as Parsable>::elems(self) }
}
impl<'s, 'b, T: ?Sized + Parsable<'s>> Parsable<'s> for RwLockReadGuard<'b, T> {
    type Elem = <T as Parsable<'s>>::Elem;
    type Iter = <T as Parsable<'s>>::Iter;

    #[inline]
    fn elems(&self) -> Self::Iter { <T as Parsable>::elems(self) }
}
impl<'s, 'b, T: ?Sized + Parsable<'s>> Parsable<'s> for RwLockWriteGuard<'b, T> {
    type Elem = <T as Parsable<'s>>::Elem;
    type Iter = <T as Parsable<'s>>::Iter;

    #[inline]
    fn elems(&self) -> Self::Iter { <T as Parsable>::elems(self) }
}
impl<'s, 'b, T: ?Sized + Parsable<'s>> Parsable<'s> for Ref<'b, T> {
    type Elem = <T as Parsable<'s>>::Elem;
    type Iter = <T as Parsable<'s>>::Iter;

    #[inline]
    fn elems(&self) -> Self::Iter { <T as Parsable>::elems(self) }
}
impl<'s, 'b, T: ?Sized + Parsable<'s>> Parsable<'s> for RefMut<'b, T> {
    type Elem = <T as Parsable<'s>>::Elem;
    type Iter = <T as Parsable<'s>>::Iter;

    #[inline]
    fn elems(&self) -> Self::Iter { <T as Parsable>::elems(self) }
}



/// Alias for a [`Parsable`] that yields bytes.
pub trait BytesParsable<'s>: Parsable<'s, Elem = &'s u8> {
    /// Implements a more contextually-accurate alias for [`Parsable::elems()`].
    ///
    /// # Returns
    /// An [`Iterator`] over bytes, as implemented by [`Parsable::elems()`]. Note, however, that it
    /// yields bytes by ownership (`u8`) instead of reference (`&u8`).
    #[inline]
    fn bytes(&self) -> Cloned<Self::Iter> { self.elems().cloned() }
}
impl<'s, T: ?Sized + Parsable<'s, Elem = &'s u8>> BytesParsable<'s> for T {}



/// An extension of a [`BytesParsable`] that also yields unicode graphemes.
pub trait Utf8Parsable<'s>: BytesParsable<'s> {
    /// The type of iterator that yields graphemes.
    type GraphsIter: Iterator<Item = &'s str>;
    /// The type of the iterator that yields graphemes + their byte indices.
    type GraphIndicesIter: Iterator<Item = (usize, &'s str)>;

    /// Returns an [`Iterator`] that will yield unicode graphemes from the head of the input
    /// stream, in-order.
    ///
    /// # Returns
    /// A [`Parsable::GraphsIter`]ator over [`&str`](str)s that represents the graphemes at the
    /// head of the source.
    fn graphs(&self) -> Self::GraphsIter;

    /// Returns an [`Iterator`] that will yield not just unicode graphemes from the head of the
    /// input stream, in-order, but also matching byte indices for slicing.
    ///
    /// # Returns
    /// A [`Parsable::GraphIndicesIter`]ator over [`&str`](str)s that represents the graphemes
    /// and their indices at the head of the source.
    fn graph_indices(&self) -> Self::GraphIndicesIter;
}

// Default impls
impl<'s> Utf8Parsable<'s> for &'s str {
    type GraphsIter = Graphemes<'s>;
    type GraphIndicesIter = GraphemeIndices<'s>;

    #[inline]
    fn graphs(&self) -> Self::GraphsIter { self.graphemes(true) }

    #[inline]
    fn graph_indices(&self) -> Self::GraphIndicesIter { self.grapheme_indices(true) }
}
impl<'s, S> Utf8Parsable<'s> for Span<S>
where
    S: Clone + Spannable<'s>,
    S::Slice: Utf8Parsable<'s>,
{
    type GraphsIter = <S::Slice as Utf8Parsable<'s>>::GraphsIter;
    type GraphIndicesIter = <S::Slice as Utf8Parsable<'s>>::GraphIndicesIter;

    /// # Safety
    /// We assume that the current span ALWAYS lies _exactly_ on a grapheme boundary.
    #[inline]
    fn graphs(&self) -> Self::GraphsIter { self.value().graphs() }

    /// # Safety
    /// We assume that the current span ALWAYS lies _exactly_ on a grapheme boundary.
    #[inline]
    fn graph_indices(&self) -> Self::GraphIndicesIter { self.value().graph_indices() }
}

// Pointer-like impls
impl<'s, 'b, T: ?Sized + Utf8Parsable<'s>> Utf8Parsable<'s> for &'b T {
    type GraphsIter = <T as Utf8Parsable<'s>>::GraphsIter;
    type GraphIndicesIter = <T as Utf8Parsable<'s>>::GraphIndicesIter;

    #[inline]
    fn graphs(&self) -> Self::GraphsIter { <T as Utf8Parsable>::graphs(self) }

    #[inline]
    fn graph_indices(&self) -> Self::GraphIndicesIter { <T as Utf8Parsable>::graph_indices(self) }
}
impl<'s, 'b, T: ?Sized + Utf8Parsable<'s>> Utf8Parsable<'s> for &'s mut T {
    type GraphsIter = <T as Utf8Parsable<'s>>::GraphsIter;
    type GraphIndicesIter = <T as Utf8Parsable<'s>>::GraphIndicesIter;

    #[inline]
    fn graphs(&self) -> Self::GraphsIter { <T as Utf8Parsable>::graphs(self) }

    #[inline]
    fn graph_indices(&self) -> Self::GraphIndicesIter { <T as Utf8Parsable>::graph_indices(self) }
}
impl<'s, T: ?Sized + Utf8Parsable<'s>> Utf8Parsable<'s> for Box<T> {
    type GraphsIter = <T as Utf8Parsable<'s>>::GraphsIter;
    type GraphIndicesIter = <T as Utf8Parsable<'s>>::GraphIndicesIter;

    #[inline]
    fn graphs(&self) -> Self::GraphsIter { <T as Utf8Parsable>::graphs(self) }

    #[inline]
    fn graph_indices(&self) -> Self::GraphIndicesIter { <T as Utf8Parsable>::graph_indices(self) }
}
impl<'s, T: ?Sized + Utf8Parsable<'s>> Utf8Parsable<'s> for Rc<T> {
    type GraphsIter = <T as Utf8Parsable<'s>>::GraphsIter;
    type GraphIndicesIter = <T as Utf8Parsable<'s>>::GraphIndicesIter;

    #[inline]
    fn graphs(&self) -> Self::GraphsIter { <T as Utf8Parsable>::graphs(self) }

    #[inline]
    fn graph_indices(&self) -> Self::GraphIndicesIter { <T as Utf8Parsable>::graph_indices(self) }
}
impl<'s, T: ?Sized + Utf8Parsable<'s>> Utf8Parsable<'s> for Arc<T> {
    type GraphsIter = <T as Utf8Parsable<'s>>::GraphsIter;
    type GraphIndicesIter = <T as Utf8Parsable<'s>>::GraphIndicesIter;

    #[inline]
    fn graphs(&self) -> Self::GraphsIter { <T as Utf8Parsable>::graphs(self) }

    #[inline]
    fn graph_indices(&self) -> Self::GraphIndicesIter { <T as Utf8Parsable>::graph_indices(self) }
}
impl<'s, 'b, T: ?Sized + Utf8Parsable<'s>> Utf8Parsable<'s> for MutexGuard<'b, T> {
    type GraphsIter = <T as Utf8Parsable<'s>>::GraphsIter;
    type GraphIndicesIter = <T as Utf8Parsable<'s>>::GraphIndicesIter;

    #[inline]
    fn graphs(&self) -> Self::GraphsIter { <T as Utf8Parsable>::graphs(self) }

    #[inline]
    fn graph_indices(&self) -> Self::GraphIndicesIter { <T as Utf8Parsable>::graph_indices(self) }
}
impl<'s, 'b, T: ?Sized + Utf8Parsable<'s>> Utf8Parsable<'s> for RwLockReadGuard<'b, T> {
    type GraphsIter = <T as Utf8Parsable<'s>>::GraphsIter;
    type GraphIndicesIter = <T as Utf8Parsable<'s>>::GraphIndicesIter;

    #[inline]
    fn graphs(&self) -> Self::GraphsIter { <T as Utf8Parsable>::graphs(self) }

    #[inline]
    fn graph_indices(&self) -> Self::GraphIndicesIter { <T as Utf8Parsable>::graph_indices(self) }
}
impl<'s, 'b, T: ?Sized + Utf8Parsable<'s>> Utf8Parsable<'s> for RwLockWriteGuard<'b, T> {
    type GraphsIter = <T as Utf8Parsable<'s>>::GraphsIter;
    type GraphIndicesIter = <T as Utf8Parsable<'s>>::GraphIndicesIter;

    #[inline]
    fn graphs(&self) -> Self::GraphsIter { <T as Utf8Parsable>::graphs(self) }

    #[inline]
    fn graph_indices(&self) -> Self::GraphIndicesIter { <T as Utf8Parsable>::graph_indices(self) }
}
impl<'s, 'b, T: ?Sized + Utf8Parsable<'s>> Utf8Parsable<'s> for Ref<'b, T> {
    type GraphsIter = <T as Utf8Parsable<'s>>::GraphsIter;
    type GraphIndicesIter = <T as Utf8Parsable<'s>>::GraphIndicesIter;

    #[inline]
    fn graphs(&self) -> Self::GraphsIter { <T as Utf8Parsable>::graphs(self) }

    #[inline]
    fn graph_indices(&self) -> Self::GraphIndicesIter { <T as Utf8Parsable>::graph_indices(self) }
}
impl<'s, 'b, T: ?Sized + Utf8Parsable<'s>> Utf8Parsable<'s> for RefMut<'b, T> {
    type GraphsIter = <T as Utf8Parsable<'s>>::GraphsIter;
    type GraphIndicesIter = <T as Utf8Parsable<'s>>::GraphIndicesIter;

    #[inline]
    fn graphs(&self) -> Self::GraphsIter { <T as Utf8Parsable>::graphs(self) }

    #[inline]
    fn graph_indices(&self) -> Self::GraphIndicesIter { <T as Utf8Parsable>::graph_indices(self) }
}





/***** TESTS *****/
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bytes() {
        let span = Span::new("5+5".as_bytes());
        let mut iter = span.bytes();
        assert_eq!(iter.next(), Some(b'5'));
        assert_eq!(iter.next(), Some(b'+'));
        assert_eq!(iter.next(), Some(b'5'));
        assert_eq!(iter.next(), None);

        let span = Span::ranged("5+5".as_bytes(), 0..1);
        let mut iter = span.bytes();
        assert_eq!(iter.next(), Some(b'5'));
        assert_eq!(iter.next(), None);

        let span = Span::ranged("5+5".as_bytes(), 1..2);
        let mut iter = span.bytes();
        assert_eq!(iter.next(), Some(b'+'));
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
        assert_eq!(iter.next(), Some((0, "+")));
        assert_eq!(iter.next(), None);
    }
}
