//  SPAN.rs
//    by Lut99
//
//  Created:
//    05 Apr 2024, 18:10:59
//  Last edited:
//    06 May 2025, 16:24:46
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements some extensions on [`Span`]s that allow us to perform
//!   very generic operations on them.
//

use std::cell::{Ref, RefMut};
use std::fmt::Debug;
use std::iter::{Cloned, Map};
use std::num::NonZeroUsize;
use std::rc::Rc;
use std::sync::{Arc, MutexGuard, RwLockReadGuard, RwLockWriteGuard};

use ast_toolkit_span::{Span, Spannable};
use unicode_segmentation::{GraphemeIndices, Graphemes, UnicodeSegmentation as _};


/***** ITERATORS *****/
/// Sliding window iterator for slices.
#[derive(Clone, Copy, Debug)]
pub struct SlidingWindowIter<'s, T> {
    /// The slice we're iterating over.
    slice: &'s [T],
    /// The sliding window size.
    len:   usize,
    /// The current index.
    i:     usize,
}
impl<'s, T> Iterator for SlidingWindowIter<'s, T> {
    type Item = &'s [T];

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.i + self.len <= self.slice.len() {
            let i: usize = self.i;
            self.i += 1;
            Some(&self.slice[i..i + self.len])
        } else {
            None
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let size: usize = self.len();
        (size, Some(size))
    }
}
impl<'s, T> ExactSizeIterator for SlidingWindowIter<'s, T> {
    #[inline]
    fn len(&self) -> usize { if self.i + self.len <= self.slice.len() { 1 + self.slice.len() - (self.i + self.len) } else { 0 } }
}

/// Allows one to use [`sliding_window()`](SlidingWindow::sliding_window()) on slices.
pub trait SlidingWindow<T> {
    /// Returns an iterator that does sliding window iteration on the underyling slice.
    ///
    /// # Arguments
    /// - `len`: The size of the sliding window.
    ///
    /// # Returns
    /// A [`SlidingWindowIter`] doing the iteration. Note that, if this slice is smaller than
    /// `len`, it will not yield elements.
    fn sliding_window(&self, len: NonZeroUsize) -> SlidingWindowIter<T>;
}
impl<T> SlidingWindow<T> for [T] {
    #[inline]
    fn sliding_window(&self, len: NonZeroUsize) -> SlidingWindowIter<T> { SlidingWindowIter { slice: self, len: len.into(), i: 0 } }
}





/***** LIBRARY *****/
/// Extends a [`Spannable`] with the power to yield elements from the head of the input.
///
/// # Generics
/// - `'s`: The lifetime of the spanned object. This allows yielded elements to refer to the
///   original source instead of the container referring to it (e.g., a [`Span`]).
pub trait Parsable<'s>: Spannable<'s> {
    /// The type of element iterated.
    type Elem: 's;
    /// The type of iterator that yields slices of elements.
    type Iter: Iterator<Item = &'s [Self::Elem]>;


    /// Returns an [`Iterator`] that will yield elements from the head of the input stream, in-
    /// order.
    ///
    /// # Returns
    /// A [`Parsable::Iter`]ator over [`Parsable::Elem`] that represents the elements of the source
    /// array.
    // Note: Will do prettier if associated type defaults ever stabilize.
    #[inline]
    fn elems(&self) -> Map<Self::Iter, fn(&'s [Self::Elem]) -> &'s Self::Elem> {
        self.elem_slices(NonZeroUsize::new(1).unwrap())
            .map(|slice| if let [elem] = slice { elem } else { panic!("Parsable::elem_windows(1) did not produce a slice of length 1") })
    }

    /// Returns an [`Iterator`] that produces windows sliding over the parent array.
    ///
    /// # Arguments
    /// - `len`: The window size. Use 1 to effectively not do sliding window at all, but just yield
    ///   individual elements.
    ///
    /// # Returns
    /// A [`Parsable::Iter`]ator over slices of [`Parsable::Elem`]s that represents windows sliding
    /// over the source array.
    ///
    /// If the source array is smaller than the window size, then this iterator will not produce
    /// anything.
    fn elem_slices(&self, len: NonZeroUsize) -> Self::Iter;
}

// Default impls
impl<'s> Parsable<'s> for &'s str {
    type Elem = u8;
    type Iter = SlidingWindowIter<'s, u8>;

    #[inline]
    fn elem_slices(&self, len: NonZeroUsize) -> Self::Iter { self.as_bytes().sliding_window(len) }
}
impl<'s, T> Parsable<'s> for &'s [T] {
    type Elem = T;
    type Iter = SlidingWindowIter<'s, T>;

    #[inline]
    fn elem_slices(&self, len: NonZeroUsize) -> Self::Iter { self.sliding_window(len) }
}
impl<'s, T: Clone + Debug + Eq + PartialEq, U: Parsable<'s>> Parsable<'s> for (T, U) {
    type Elem = <U as Parsable<'s>>::Elem;
    type Iter = <U as Parsable<'s>>::Iter;

    #[inline]
    fn elem_slices(&self, len: NonZeroUsize) -> Self::Iter { <U as Parsable>::elem_slices(&self.1, len) }
}

// Span impl
impl<'b, S: Clone + Spannable<'b>> Parsable<'b> for Span<S>
where
    S::Slice: Parsable<'b>,
{
    type Elem = <S::Slice as Parsable<'b>>::Elem;
    type Iter = <S::Slice as Parsable<'b>>::Iter;

    #[inline]
    fn elem_slices(&self, len: NonZeroUsize) -> Self::Iter {
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
        self.value().elem_slices(len)
    }
}

// Pointer-like impls
impl<'s, 'b, T: ?Sized + Parsable<'s>> Parsable<'s> for &'b T {
    type Elem = <T as Parsable<'s>>::Elem;
    type Iter = <T as Parsable<'s>>::Iter;

    #[inline]
    fn elem_slices(&self, len: NonZeroUsize) -> Self::Iter { <T as Parsable>::elem_slices(self, len) }
}
impl<'s, 'b, T: ?Sized + Parsable<'s>> Parsable<'s> for &'b mut T {
    type Elem = <T as Parsable<'s>>::Elem;
    type Iter = <T as Parsable<'s>>::Iter;

    #[inline]
    fn elem_slices(&self, len: NonZeroUsize) -> Self::Iter { <T as Parsable>::elem_slices(self, len) }
}
impl<'s, T: ?Sized + Parsable<'s>> Parsable<'s> for Box<T> {
    type Elem = <T as Parsable<'s>>::Elem;
    type Iter = <T as Parsable<'s>>::Iter;

    #[inline]
    fn elem_slices(&self, len: NonZeroUsize) -> Self::Iter { <T as Parsable>::elem_slices(self, len) }
}
impl<'s, T: ?Sized + Parsable<'s>> Parsable<'s> for Rc<T> {
    type Elem = <T as Parsable<'s>>::Elem;
    type Iter = <T as Parsable<'s>>::Iter;

    #[inline]
    fn elem_slices(&self, len: NonZeroUsize) -> Self::Iter { <T as Parsable>::elem_slices(self, len) }
}
impl<'s, T: ?Sized + Parsable<'s>> Parsable<'s> for Arc<T> {
    type Elem = <T as Parsable<'s>>::Elem;
    type Iter = <T as Parsable<'s>>::Iter;

    #[inline]
    fn elem_slices(&self, len: NonZeroUsize) -> Self::Iter { <T as Parsable>::elem_slices(self, len) }
}
impl<'s, 'b, T: ?Sized + Parsable<'s>> Parsable<'s> for MutexGuard<'b, T> {
    type Elem = <T as Parsable<'s>>::Elem;
    type Iter = <T as Parsable<'s>>::Iter;

    #[inline]
    fn elem_slices(&self, len: NonZeroUsize) -> Self::Iter { <T as Parsable>::elem_slices(self, len) }
}
impl<'s, 'b, T: ?Sized + Parsable<'s>> Parsable<'s> for RwLockReadGuard<'b, T> {
    type Elem = <T as Parsable<'s>>::Elem;
    type Iter = <T as Parsable<'s>>::Iter;

    #[inline]
    fn elem_slices(&self, len: NonZeroUsize) -> Self::Iter { <T as Parsable>::elem_slices(self, len) }
}
impl<'s, 'b, T: ?Sized + Parsable<'s>> Parsable<'s> for RwLockWriteGuard<'b, T> {
    type Elem = <T as Parsable<'s>>::Elem;
    type Iter = <T as Parsable<'s>>::Iter;

    #[inline]
    fn elem_slices(&self, len: NonZeroUsize) -> Self::Iter { <T as Parsable>::elem_slices(self, len) }
}
impl<'s, 'b, T: ?Sized + Parsable<'s>> Parsable<'s> for Ref<'b, T> {
    type Elem = <T as Parsable<'s>>::Elem;
    type Iter = <T as Parsable<'s>>::Iter;

    #[inline]
    fn elem_slices(&self, len: NonZeroUsize) -> Self::Iter { <T as Parsable>::elem_slices(self, len) }
}
impl<'s, 'b, T: ?Sized + Parsable<'s>> Parsable<'s> for RefMut<'b, T> {
    type Elem = <T as Parsable<'s>>::Elem;
    type Iter = <T as Parsable<'s>>::Iter;

    #[inline]
    fn elem_slices(&self, len: NonZeroUsize) -> Self::Iter { <T as Parsable>::elem_slices(self, len) }
}



/// Alias for a [`Parsable`] that yields bytes.
pub trait BytesParsable<'s>: Parsable<'s, Elem = u8> {
    /// Implements a more contextually-accurate alias for [`Parsable::elems()`].
    ///
    /// # Returns
    /// An [`Iterator`] over bytes, as implemented by [`Parsable::elems()`]. Note, however, that it
    /// yields bytes by ownership (`u8`) instead of reference (`&u8`).
    #[inline]
    fn bytes(&self) -> Cloned<Map<Self::Iter, fn(&'s [Self::Elem]) -> &'s Self::Elem>> { self.elems().cloned() }

    /// Implements a more contextually-accurate alias for [`Parsable::elem_slices()`].
    ///
    /// # Arguments
    /// - `len`: The window size. Use 1 to effectively not do sliding window at all, but just yield
    ///   individual elements.
    ///
    /// # Returns
    /// An [`Iterator`] over bytes, as implemented by [`Parsable::elems()`].
    ///
    /// If the source array is smaller than the window size, then this iterator will not produce
    /// anything.
    #[inline]
    fn byte_slices(&self, len: NonZeroUsize) -> Self::Iter { self.elem_slices(len) }
}
impl<'s, T: ?Sized + Parsable<'s, Elem = u8>> BytesParsable<'s> for T {}



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
    fn test_sliding_window_iter() {
        let mut iter = [1, 2, 3, 4, 5].sliding_window(NonZeroUsize::new(3).unwrap());
        assert_eq!(iter.len(), 3);
        assert_eq!(iter.next(), Some([1, 2, 3].as_slice()));
        assert_eq!(iter.len(), 2);
        assert_eq!(iter.next(), Some([2, 3, 4].as_slice()));
        assert_eq!(iter.len(), 1);
        assert_eq!(iter.next(), Some([3, 4, 5].as_slice()));
        assert_eq!(iter.len(), 0);
        assert_eq!(iter.next(), None);

        let mut iter = [1, 2].sliding_window(NonZeroUsize::new(3).unwrap());
        assert_eq!(iter.len(), 0);
        assert_eq!(iter.next(), None);
    }

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
