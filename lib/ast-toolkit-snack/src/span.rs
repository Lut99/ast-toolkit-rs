//  SPAN.rs
//    by Lut99
//
//  Created:
//    05 Apr 2024, 18:10:59
//  Last edited:
//    17 Mar 2025, 15:20:44
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
/// iterator, which we cannot use in the [`Utf8Parsable::GraphemeIndicesIter`].
#[derive(Debug)]
pub struct GraphemesIndicesIter<I> {
    /// The wrapped iterator of the source.
    iter:   I,
    /// The offset to skip past upon first iter.
    offset: usize,
}
impl<'a, I: Iterator<Item = (usize, &'a str)>> Iterator for GraphemesIndicesIter<I> {
    type Item = (usize, &'a str);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            // Get the next element, if any
            let (i, c): (usize, &'a str) = self.iter.next()?;

            // Check if we're still skipping
            if i < self.offset {
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
    /// A [`Parsable::Iter`]ator over [`Parsable::Item`] that represents the elements of the source
    /// array.
    fn head<'s>(&'s self) -> Self::Iter<'s>;
}

// Default impls
impl Parsable for str {
    type Elem = u8;
    type Iter<'s> = std::slice::Iter<'s, u8>;

    #[inline]
    fn head<'s>(&'s self) -> Self::Iter<'s> { self.as_bytes().iter() }
}
impl<T> Parsable for [T] {
    type Elem = T;
    type Iter<'s>
        = std::slice::Iter<'s, T>
    where
        T: 's;

    #[inline]
    fn head<'s>(&'s self) -> Self::Iter<'s> { self.iter() }
}
impl<const LEN: usize, T: Hash> Parsable for [T; LEN] {
    type Elem = T;
    type Iter<'s>
        = <[T] as Parsable>::Iter<'s>
    where
        Self: 's;

    #[inline]
    fn head<'s>(&'s self) -> Self::Iter<'s> { <[T]>::head(self.as_slice()) }
}
impl<T: Debug + Eq + PartialEq, U: Parsable> Parsable for (T, U) {
    type Elem = <U as Parsable>::Elem;
    type Iter<'s>
        = <U as Parsable>::Iter<'s>
    where
        Self: 's;

    #[inline]
    fn head<'s>(&'s self) -> Self::Iter<'s> { <U as Parsable>::head(&self.1) }
}

// Span impl
impl<S: Clone + Parsable> Parsable for Span<S> {
    type Elem = <S as Parsable>::Elem;
    type Iter<'s>
        = std::iter::Skip<<S as Parsable>::Iter<'s>>
    where
        Self: 's;

    #[inline]
    fn head<'s>(&'s self) -> Self::Iter<'s> {
        // Hack time: instead of taking the slice of ourselves, we use the fact we know the offset
        // to simply offset the main iterator
        self.source().head().skip(match self.range().start_resolved(self.len()) {
            Some(idx) => idx,
            None => self.len(),
        })
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
    fn head<'s>(&'s self) -> Self::Iter<'s> { <T as Parsable>::head(self) }
}
impl<'a, T: ?Sized + Parsable> Parsable for &'a mut T {
    type Elem = <T as Parsable>::Elem;
    type Iter<'s>
        = <T as Parsable>::Iter<'s>
    where
        Self: 's;

    #[inline]
    fn head<'s>(&'s self) -> Self::Iter<'s> { <T as Parsable>::head(self) }
}
impl<T: ?Sized + Parsable> Parsable for Box<T> {
    type Elem = <T as Parsable>::Elem;
    type Iter<'s>
        = <T as Parsable>::Iter<'s>
    where
        Self: 's;

    #[inline]
    fn head<'s>(&'s self) -> Self::Iter<'s> { <T as Parsable>::head(self) }
}
impl<T: ?Sized + Parsable> Parsable for Rc<T> {
    type Elem = <T as Parsable>::Elem;
    type Iter<'s>
        = <T as Parsable>::Iter<'s>
    where
        Self: 's;

    #[inline]
    fn head<'s>(&'s self) -> Self::Iter<'s> { <T as Parsable>::head(self) }
}
impl<T: ?Sized + Parsable> Parsable for Arc<T> {
    type Elem = <T as Parsable>::Elem;
    type Iter<'s>
        = <T as Parsable>::Iter<'s>
    where
        Self: 's;

    #[inline]
    fn head<'s>(&'s self) -> Self::Iter<'s> { <T as Parsable>::head(self) }
}
impl<'a, T: ?Sized + Parsable> Parsable for MutexGuard<'a, T> {
    type Elem = <T as Parsable>::Elem;
    type Iter<'s>
        = <T as Parsable>::Iter<'s>
    where
        Self: 's;

    #[inline]
    fn head<'s>(&'s self) -> Self::Iter<'s> { <T as Parsable>::head(self) }
}
impl<'a, T: ?Sized + Parsable> Parsable for RwLockReadGuard<'a, T> {
    type Elem = <T as Parsable>::Elem;
    type Iter<'s>
        = <T as Parsable>::Iter<'s>
    where
        Self: 's;

    #[inline]
    fn head<'s>(&'s self) -> Self::Iter<'s> { <T as Parsable>::head(self) }
}
impl<'a, T: ?Sized + Parsable> Parsable for RwLockWriteGuard<'a, T> {
    type Elem = <T as Parsable>::Elem;
    type Iter<'s>
        = <T as Parsable>::Iter<'s>
    where
        Self: 's;

    #[inline]
    fn head<'s>(&'s self) -> Self::Iter<'s> { <T as Parsable>::head(self) }
}
impl<'a, T: ?Sized + Parsable> Parsable for Ref<'a, T> {
    type Elem = <T as Parsable>::Elem;
    type Iter<'s>
        = <T as Parsable>::Iter<'s>
    where
        Self: 's;

    #[inline]
    fn head<'s>(&'s self) -> Self::Iter<'s> { <T as Parsable>::head(self) }
}
impl<'a, T: ?Sized + Parsable> Parsable for RefMut<'a, T> {
    type Elem = <T as Parsable>::Elem;
    type Iter<'s>
        = <T as Parsable>::Iter<'s>
    where
        Self: 's;

    #[inline]
    fn head<'s>(&'s self) -> Self::Iter<'s> { <T as Parsable>::head(self) }
}



/// Alias for a [`Parsable`] that yields bytes.
pub trait BytesParsable: Parsable<Elem = u8> {}
impl<T: ?Sized + Parsable<Elem = u8>> BytesParsable for T {}



/// An extension of a [`BytesParsable`] that also yields unicode graphemes.
pub trait Utf8Parsable: BytesParsable {
    /// The type of iterator that yields graphemes.
    type GraphemesIter<'s>: Iterator<Item = &'s str>
    where
        Self: 's;
    /// The type of the iterator that yields graphemes + their byte indices.
    type GraphemeIndicesIter<'s>: Iterator<Item = (usize, &'s str)>
    where
        Self: 's;

    /// Returns an [`Iterator`] that will yield unicode graphemes from the head of the input
    /// stream, in-order.
    ///
    /// # Returns
    /// A [`Parsable::GraphemesIter`]ator over [`&str`](str)s that represents the graphemes at the
    /// head of the source.
    fn head_graphemes<'s>(&'s self) -> Self::GraphemesIter<'s>;

    /// Returns an [`Iterator`] that will yield not just unicode graphemes from the head of the
    /// input stream, in-order, but also matching byte indices for slicing.
    ///
    /// # Returns
    /// A [`Parsable::GraphemeIndicesIter`]ator over [`&str`](str)s that represents the graphemes
    /// and their indices at the head of the source.
    fn head_grapheme_indices<'s>(&'s self) -> Self::GraphemeIndicesIter<'s>;
}

// Default impls
impl Utf8Parsable for str {
    type GraphemesIter<'s> = Graphemes<'s>;
    type GraphemeIndicesIter<'s> = GraphemeIndices<'s>;

    #[inline]
    fn head_graphemes<'s>(&'s self) -> Self::GraphemesIter<'s> { self.graphemes(true) }

    #[inline]
    fn head_grapheme_indices<'s>(&'s self) -> Self::GraphemeIndicesIter<'s> { self.grapheme_indices(true) }
}
impl<S: Clone + Utf8Parsable> Utf8Parsable for Span<S> {
    type GraphemesIter<'s>
        = std::iter::Map<GraphemesIndicesIter<<S as Utf8Parsable>::GraphemeIndicesIter<'s>>, fn((usize, &'s str)) -> &'s str>
    where
        Self: 's;
    type GraphemeIndicesIter<'s>
        = GraphemesIndicesIter<<S as Utf8Parsable>::GraphemeIndicesIter<'s>>
    where
        Self: 's;

    /// # Safety
    /// We assume that the current span ALWAYS lies _exactly_ on a grapheme boundary.
    #[inline]
    fn head_graphemes<'s>(&'s self) -> Self::GraphemesIter<'s> { self.head_grapheme_indices().map(|(_, c)| c) }

    /// # Safety
    /// We assume that the current span ALWAYS lies _exactly_ on a grapheme boundary.
    #[inline]
    fn head_grapheme_indices<'s>(&'s self) -> Self::GraphemeIndicesIter<'s> {
        // Do some wizardry to find up to where to cut the input
        let offset: usize = match self.range().start_resolved(self.len()) {
            Some(idx) => idx,
            None => self.len(),
        };
        GraphemesIndicesIter { iter: self.source().head_grapheme_indices(), offset }
    }
}

// Pointer-like impls
impl<'a, T: ?Sized + Utf8Parsable> Utf8Parsable for &'a T {
    type GraphemesIter<'s>
        = <T as Utf8Parsable>::GraphemesIter<'s>
    where
        Self: 's;
    type GraphemeIndicesIter<'s>
        = <T as Utf8Parsable>::GraphemeIndicesIter<'s>
    where
        Self: 's;

    #[inline]
    fn head_graphemes<'s>(&'s self) -> Self::GraphemesIter<'s> { <T as Utf8Parsable>::head_graphemes(self) }

    #[inline]
    fn head_grapheme_indices<'s>(&'s self) -> Self::GraphemeIndicesIter<'s> { <T as Utf8Parsable>::head_grapheme_indices(self) }
}
impl<'a, T: ?Sized + Utf8Parsable> Utf8Parsable for &'a mut T {
    type GraphemesIter<'s>
        = <T as Utf8Parsable>::GraphemesIter<'s>
    where
        Self: 's;
    type GraphemeIndicesIter<'s>
        = <T as Utf8Parsable>::GraphemeIndicesIter<'s>
    where
        Self: 's;

    #[inline]
    fn head_graphemes<'s>(&'s self) -> Self::GraphemesIter<'s> { <T as Utf8Parsable>::head_graphemes(self) }

    #[inline]
    fn head_grapheme_indices<'s>(&'s self) -> Self::GraphemeIndicesIter<'s> { <T as Utf8Parsable>::head_grapheme_indices(self) }
}
impl<T: ?Sized + Utf8Parsable> Utf8Parsable for Box<T> {
    type GraphemesIter<'s>
        = <T as Utf8Parsable>::GraphemesIter<'s>
    where
        Self: 's;
    type GraphemeIndicesIter<'s>
        = <T as Utf8Parsable>::GraphemeIndicesIter<'s>
    where
        Self: 's;

    #[inline]
    fn head_graphemes<'s>(&'s self) -> Self::GraphemesIter<'s> { <T as Utf8Parsable>::head_graphemes(self) }

    #[inline]
    fn head_grapheme_indices<'s>(&'s self) -> Self::GraphemeIndicesIter<'s> { <T as Utf8Parsable>::head_grapheme_indices(self) }
}
impl<T: ?Sized + Utf8Parsable> Utf8Parsable for Rc<T> {
    type GraphemesIter<'s>
        = <T as Utf8Parsable>::GraphemesIter<'s>
    where
        Self: 's;
    type GraphemeIndicesIter<'s>
        = <T as Utf8Parsable>::GraphemeIndicesIter<'s>
    where
        Self: 's;

    #[inline]
    fn head_graphemes<'s>(&'s self) -> Self::GraphemesIter<'s> { <T as Utf8Parsable>::head_graphemes(self) }

    #[inline]
    fn head_grapheme_indices<'s>(&'s self) -> Self::GraphemeIndicesIter<'s> { <T as Utf8Parsable>::head_grapheme_indices(self) }
}
impl<T: ?Sized + Utf8Parsable> Utf8Parsable for Arc<T> {
    type GraphemesIter<'s>
        = <T as Utf8Parsable>::GraphemesIter<'s>
    where
        Self: 's;
    type GraphemeIndicesIter<'s>
        = <T as Utf8Parsable>::GraphemeIndicesIter<'s>
    where
        Self: 's;

    #[inline]
    fn head_graphemes<'s>(&'s self) -> Self::GraphemesIter<'s> { <T as Utf8Parsable>::head_graphemes(self) }

    #[inline]
    fn head_grapheme_indices<'s>(&'s self) -> Self::GraphemeIndicesIter<'s> { <T as Utf8Parsable>::head_grapheme_indices(self) }
}
impl<'a, T: ?Sized + Utf8Parsable> Utf8Parsable for MutexGuard<'a, T> {
    type GraphemesIter<'s>
        = <T as Utf8Parsable>::GraphemesIter<'s>
    where
        Self: 's;
    type GraphemeIndicesIter<'s>
        = <T as Utf8Parsable>::GraphemeIndicesIter<'s>
    where
        Self: 's;

    #[inline]
    fn head_graphemes<'s>(&'s self) -> Self::GraphemesIter<'s> { <T as Utf8Parsable>::head_graphemes(self) }

    #[inline]
    fn head_grapheme_indices<'s>(&'s self) -> Self::GraphemeIndicesIter<'s> { <T as Utf8Parsable>::head_grapheme_indices(self) }
}
impl<'a, T: ?Sized + Utf8Parsable> Utf8Parsable for RwLockReadGuard<'a, T> {
    type GraphemesIter<'s>
        = <T as Utf8Parsable>::GraphemesIter<'s>
    where
        Self: 's;
    type GraphemeIndicesIter<'s>
        = <T as Utf8Parsable>::GraphemeIndicesIter<'s>
    where
        Self: 's;

    #[inline]
    fn head_graphemes<'s>(&'s self) -> Self::GraphemesIter<'s> { <T as Utf8Parsable>::head_graphemes(self) }

    #[inline]
    fn head_grapheme_indices<'s>(&'s self) -> Self::GraphemeIndicesIter<'s> { <T as Utf8Parsable>::head_grapheme_indices(self) }
}
impl<'a, T: ?Sized + Utf8Parsable> Utf8Parsable for RwLockWriteGuard<'a, T> {
    type GraphemesIter<'s>
        = <T as Utf8Parsable>::GraphemesIter<'s>
    where
        Self: 's;
    type GraphemeIndicesIter<'s>
        = <T as Utf8Parsable>::GraphemeIndicesIter<'s>
    where
        Self: 's;

    #[inline]
    fn head_graphemes<'s>(&'s self) -> Self::GraphemesIter<'s> { <T as Utf8Parsable>::head_graphemes(self) }

    #[inline]
    fn head_grapheme_indices<'s>(&'s self) -> Self::GraphemeIndicesIter<'s> { <T as Utf8Parsable>::head_grapheme_indices(self) }
}
impl<'a, T: ?Sized + Utf8Parsable> Utf8Parsable for Ref<'a, T> {
    type GraphemesIter<'s>
        = <T as Utf8Parsable>::GraphemesIter<'s>
    where
        Self: 's;
    type GraphemeIndicesIter<'s>
        = <T as Utf8Parsable>::GraphemeIndicesIter<'s>
    where
        Self: 's;

    #[inline]
    fn head_graphemes<'s>(&'s self) -> Self::GraphemesIter<'s> { <T as Utf8Parsable>::head_graphemes(self) }

    #[inline]
    fn head_grapheme_indices<'s>(&'s self) -> Self::GraphemeIndicesIter<'s> { <T as Utf8Parsable>::head_grapheme_indices(self) }
}
impl<'a, T: ?Sized + Utf8Parsable> Utf8Parsable for RefMut<'a, T> {
    type GraphemesIter<'s>
        = <T as Utf8Parsable>::GraphemesIter<'s>
    where
        Self: 's;
    type GraphemeIndicesIter<'s>
        = <T as Utf8Parsable>::GraphemeIndicesIter<'s>
    where
        Self: 's;

    #[inline]
    fn head_graphemes<'s>(&'s self) -> Self::GraphemesIter<'s> { <T as Utf8Parsable>::head_graphemes(self) }

    #[inline]
    fn head_grapheme_indices<'s>(&'s self) -> Self::GraphemeIndicesIter<'s> { <T as Utf8Parsable>::head_grapheme_indices(self) }
}





// /// Extends a [`Spannable`] with the power to have a length in bytes.
// pub trait LenBytes {
//     /// Returns the number of bytes in this array.
//     ///
//     /// # Returns
//     /// A `usize` with the number of bytes of length.
//     fn len(&self) -> usize;

//     /// A convenience function for checking [`LenBytes::len() == 0`](LenBytes::len()).
//     ///
//     /// # Returns
//     /// True if the span is empty, or false otherwise.
//     #[inline]
//     fn is_empty(&self) -> bool { self.len() == 0 }
// }

// // Default binary impls for [`LenBytes`]
// impl<'b> LenBytes for &'b [u8] {
//     #[inline]
//     fn len(&self) -> usize { <[u8]>::len(self) }
// }
// impl<'b, const LEN: usize> LenBytes for &'b [u8; LEN] {
//     #[inline]
//     fn len(&self) -> usize { <[u8]>::len(self.as_slice()) }
// }
// impl<'b> LenBytes for Cow<'b, [u8]> {
//     #[inline]
//     fn len(&self) -> usize { <[u8]>::len(self.as_ref()) }
// }
// impl LenBytes for Vec<u8> {
//     #[inline]
//     fn len(&self) -> usize { <Vec<u8>>::len(self) }
// }

// // Default string impls for [`LenBytes`]
// impl<'s> LenBytes for &'s str {
//     #[inline]
//     fn len(&self) -> usize { <str>::len(self) }
// }
// impl<'s> LenBytes for Cow<'s, str> {
//     #[inline]
//     fn len(&self) -> usize { <str>::len(self.as_ref()) }
// }
// impl LenBytes for String {
//     #[inline]
//     fn len(&self) -> usize { <String>::len(self) }
// }

// // The implementation for a [`Span`].
// impl<F, S: LenBytes> LenBytes for Span<F, S> {
//     #[inline]
//     fn len(&self) -> usize {
//         match self.range() {
//             SpanRange::Closed(s, e) => e - s,
//             SpanRange::ClosedOpen(s) => self.source_ref().len() - s,
//             SpanRange::OpenClosed(e) => e,
//             SpanRange::Open => self.source_ref().len(),
//             SpanRange::Empty => 0,
//         }
//     }
// }



// /// Extends a [`Spannable`] with the power to be match-prefix'ed by a byte-like object.
// pub trait MatchBytes: LenBytes {
//     /// Returns the position up to which the given bytes are a match.
//     ///
//     /// # Arguments
//     /// - `range`: The actual range of `self` to match.
//     /// - `bytes`: The byte pattern to match.
//     ///
//     /// # Returns
//     /// A `usize` that indicates the first "wrong" character. Some notes:
//     /// - If result is the length of `self`, the entire source was matched (but `bytes` may be longer!)
//     /// - If result is 0, then none of `self` could be matched (i.e., first characters are wrong ...or `self` is empty!)
//     fn match_bytes(&self, range: SpanRange, bytes: &[u8]) -> usize;
// }

// // Default binary impls for [`MatchBytes`]
// impl<'b> MatchBytes for &'b [u8] {
//     #[inline]
//     fn match_bytes(&self, range: SpanRange, bytes: &[u8]) -> usize {
//         // Match the prefixes
//         let mut i: usize = 0;
//         for (b1, b2) in range.apply_to(self).iter().zip(bytes.iter()) {
//             if b1 != b2 {
//                 return i;
//             }
//             i += 1;
//         }
//         i
//     }
// }
// impl<'b, const LEN: usize> MatchBytes for &'b [u8; LEN] {
//     #[inline]
//     fn match_bytes(&self, range: SpanRange, bytes: &[u8]) -> usize { self.as_slice().match_bytes(range, bytes) }
// }
// impl<'b> MatchBytes for Cow<'b, [u8]> {
//     #[inline]
//     fn match_bytes(&self, range: SpanRange, bytes: &[u8]) -> usize { <&[u8]>::match_bytes(&&**self, range, bytes) }
// }
// impl MatchBytes for Vec<u8> {
//     #[inline]
//     fn match_bytes(&self, range: SpanRange, bytes: &[u8]) -> usize { <&[u8]>::match_bytes(&self.as_slice(), range, bytes) }
// }

// // Default string impls for [`MatchBytes`]
// impl<'s> MatchBytes for &'s str {
//     fn match_bytes(&self, range: SpanRange, bytes: &[u8]) -> usize {
//         // Do a byte-wise comparison
//         self.as_bytes().match_bytes(range, bytes)
//     }
// }
// impl<'s> MatchBytes for Cow<'s, str> {
//     #[inline]
//     fn match_bytes(&self, range: SpanRange, bytes: &[u8]) -> usize { <&str>::match_bytes(&&**self, range, bytes) }
// }
// impl MatchBytes for String {
//     #[inline]
//     fn match_bytes(&self, range: SpanRange, bytes: &[u8]) -> usize { <&str>::match_bytes(&self.as_str(), range, bytes) }
// }

// // The implementation for a [`Span`].
// impl<F, S: MatchBytes> MatchBytes for Span<F, S> {
//     #[inline]
//     fn match_bytes(&self, range: SpanRange, bytes: &[u8]) -> usize { self.source_ref().match_bytes(self.range().span(&range), bytes) }
// }



// /// Extends a [`Spannable`] with the power to iterate over characters.
// pub trait NextChar {
//     /// Returns the next character as parsed from the underlying spannable.
//     ///
//     /// # Arguments
//     /// - `range`: The actual range of `self` to match.
//     ///
//     /// # Returns
//     /// A [`&str`] that represents the parsed character, or [`None`] if there is no such character left.
//     ///
//     /// # Panics
//     /// This function may panic if we are not at the UTF-8 character boundary.
//     fn next_char(&self, range: SpanRange) -> Option<&str>;
// }

// // Default string impls for [`NextChar`]
// impl<'s> NextChar for &'s str {
//     #[inline]
//     #[track_caller]
//     fn next_char(&self, range: SpanRange) -> Option<&str> { range.apply_to_str(self).graphemes(true).next() }
// }
// impl<'s> NextChar for Cow<'s, str> {
//     #[inline]
//     #[track_caller]
//     fn next_char(&self, range: SpanRange) -> Option<&str> { range.apply_to_str(self).graphemes(true).next() }
// }
// impl NextChar for String {
//     #[inline]
//     #[track_caller]
//     fn next_char(&self, range: SpanRange) -> Option<&str> { range.apply_to_str(self).graphemes(true).next() }
// }

// // The implementation for a [`Span`].
// impl<F, S: NextChar> NextChar for Span<F, S> {
//     #[inline]
//     #[track_caller]
//     fn next_char(&self, range: SpanRange) -> Option<&str> { self.source_ref().next_char(self.range().span(&range)) }
// }



// /// Extends a [`Spannable`] with the power to have its prefix longest-match'ed a set of possible bytes.
// pub trait OneOfBytes {
//     /// Returns the position up to which self starts with the given bytes.
//     ///
//     /// Precisely, given a set of bytes, will attempt to match the prefix of self for as long as it consists of one of those bytes.
//     ///
//     /// # Arguments
//     /// - `range`: The actual range of `self` to match.
//     /// - `bytes`: The set of bytes to match.
//     ///
//     /// # Returns
//     /// A `usize` that indicates the first "wrong" character. Some notes:
//     /// - If result is the length of `self`, the entire source was matched
//     /// - If result is 0, then none of `self` could be matched (i.e., first characters are wrong ...or `self` is empty!)
//     fn one_of_bytes(&self, range: SpanRange, bytes: &[u8]) -> usize;
// }

// // Default binary impls for [`OneOfBytes`]
// impl<'b> OneOfBytes for &'b [u8] {
//     fn one_of_bytes(&self, range: SpanRange, bytes: &[u8]) -> usize {
//         // Match the prefixes
//         let mut i: usize = 0;
//         for b in range.apply_to(self).iter() {
//             if !bytes.contains(b) {
//                 return i;
//             }
//             i += 1;
//         }
//         i
//     }
// }
// impl<'b, const LEN: usize> OneOfBytes for &'b [u8; LEN] {
//     #[inline]
//     fn one_of_bytes(&self, range: SpanRange, bytes: &[u8]) -> usize { self.as_slice().one_of_bytes(range, bytes) }
// }
// impl<'b> OneOfBytes for Cow<'b, [u8]> {
//     #[inline]
//     fn one_of_bytes(&self, range: SpanRange, bytes: &[u8]) -> usize { <&[u8]>::one_of_bytes(&&**self, range, bytes) }
// }
// impl OneOfBytes for Vec<u8> {
//     #[inline]
//     fn one_of_bytes(&self, range: SpanRange, bytes: &[u8]) -> usize { <&[u8]>::one_of_bytes(&self.as_slice(), range, bytes) }
// }

// // Default string impls for [`OneOfBytes`]
// impl<'s> OneOfBytes for &'s str {
//     fn one_of_bytes(&self, range: SpanRange, bytes: &[u8]) -> usize {
//         // Do a byte-wise comparison
//         self.as_bytes().one_of_bytes(range, bytes)
//     }
// }
// impl<'s> OneOfBytes for Cow<'s, str> {
//     #[inline]
//     fn one_of_bytes(&self, range: SpanRange, bytes: &[u8]) -> usize { <&str>::one_of_bytes(&&**self, range, bytes) }
// }
// impl OneOfBytes for String {
//     #[inline]
//     fn one_of_bytes(&self, range: SpanRange, bytes: &[u8]) -> usize { <&str>::one_of_bytes(&self.as_str(), range, bytes) }
// }

// // The implementation for a [`Span`].
// impl<F, S: OneOfBytes> OneOfBytes for Span<F, S> {
//     #[inline]
//     fn one_of_bytes(&self, range: SpanRange, bytes: &[u8]) -> usize { self.source_ref().one_of_bytes(self.range().span(&range), bytes) }
// }



// /// Extends a [`Spannable`] with the power to have its prefix longest-match'ed a set of possible graphemes.
// pub trait OneOfUtf8 {
//     /// Returns the position up to which self starts with the given characters.
//     ///
//     /// Precisely, given a set of graphemes, will attempt to match the prefix of self for as long as it consists of one of those graphemes.
//     ///
//     /// # Arguments
//     /// - `range`: The actual range of `self` to match.
//     /// - `chars`: The set of graphemes (given as [`str`]-pointers) to match.
//     ///
//     /// # Returns
//     /// A `usize` that indicates the first "wrong" character. Some notes:
//     /// - If result is the length of `self`, the entire source was matched
//     /// - If result is 0, then none of `self` could be matched (i.e., first characters are wrong ...or `self` is empty!)
//     fn one_of_utf8(&self, range: SpanRange, chars: &[&str]) -> usize;
// }

// // Default string impls for [`OneOfUtf8`]
// impl<'s> OneOfUtf8 for &'s str {
//     fn one_of_utf8(&self, range: SpanRange, chars: &[&str]) -> usize {
//         // Match the prefixes
//         let mut i: usize = 0;
//         for c in range.apply_to_str(self).graphemes(true) {
//             if !chars.contains(&c) {
//                 return i;
//             }
//             i += c.len();
//         }
//         i
//     }
// }
// impl<'s> OneOfUtf8 for Cow<'s, str> {
//     #[inline]
//     fn one_of_utf8(&self, range: SpanRange, chars: &[&str]) -> usize { <&str>::one_of_utf8(&&**self, range, chars) }
// }
// impl OneOfUtf8 for String {
//     #[inline]
//     fn one_of_utf8(&self, range: SpanRange, chars: &[&str]) -> usize { <&str>::one_of_utf8(&self.as_str(), range, chars) }
// }

// // The implementation for a [`Span`].
// impl<F, S: OneOfUtf8> OneOfUtf8 for Span<F, S> {
//     #[inline]
//     fn one_of_utf8(&self, range: SpanRange, chars: &[&str]) -> usize { self.source_ref().one_of_utf8(self.range().span(&range), chars) }
// }



// /// Extends a [`Spannable`] with the power to be turned into a [`String`].
// pub trait ToStr {
//     /// Returns the spanned value as a [`Cow<str>`].
//     ///
//     /// # Arguments
//     /// - `range`: The actual range of `self` to match.
//     ///
//     /// # Returns
//     /// A [`Cow<str>`] that represents the inner value.
//     fn to_str(&self, range: SpanRange) -> Cow<str>;
// }

// // Default string impls for [`OneOfUtf8`]
// impl<'s> ToStr for &'s str {
//     #[inline]
//     fn to_str(&self, range: SpanRange) -> Cow<str> { Cow::Borrowed(range.apply_to_str(self)) }
// }
// impl<'s> ToStr for Cow<'s, str> {
//     #[inline]
//     fn to_str(&self, range: SpanRange) -> Cow<str> { Cow::Borrowed(range.apply_to_str(self)) }
// }
// impl ToStr for String {
//     #[inline]
//     fn to_str(&self, range: SpanRange) -> Cow<str> { Cow::Borrowed(range.apply_to_str(self)) }
// }

// // The implementation for a [`Span`].
// impl<F, S: ToStr> ToStr for Span<F, S> {
//     #[inline]
//     fn to_str(&self, range: SpanRange) -> Cow<str> { self.source_ref().to_str(self.range().span(&range)) }
// }



// /// Extends a [`Spannable`] with the power to have its prefix longest-match'ed based on a predicate over graphemes.
// pub trait WhileBytes {
//     /// Returns the position up to which a predicate over bytes returns false.
//     ///
//     /// # Arguments
//     /// - `range`: The actual range of `self` to match.
//     /// - `predicate`: Some predicate (accepting bytes, returning true or false) that will be used to decide if a byte matches the predicate.
//     ///
//     /// # Returns
//     /// A `usize` that indicates the first unmatched byte. Some notes:
//     /// - If result is the length of `self`, the entire source was matched
//     /// - If result is 0, then none of `self` could be matched (i.e., first byte is wrong ...or `self` is empty!)
//     fn while_bytes(&self, range: SpanRange, predicate: impl FnMut(u8) -> bool) -> usize;
// }

// // Default binary impls for [`WhileBytes`]
// impl<'b> WhileBytes for &'b [u8] {
//     fn while_bytes(&self, range: SpanRange, mut predicate: impl FnMut(u8) -> bool) -> usize {
//         // Match the prefixes
//         let mut i: usize = 0;
//         for b in range.apply_to(self).iter() {
//             if !predicate(*b) {
//                 return i;
//             }
//             i += 1;
//         }
//         i
//     }
// }
// impl<'b, const LEN: usize> WhileBytes for &'b [u8; LEN] {
//     #[inline]
//     fn while_bytes(&self, range: SpanRange, predicate: impl FnMut(u8) -> bool) -> usize { self.as_slice().while_bytes(range, predicate) }
// }
// impl<'b> WhileBytes for Cow<'b, [u8]> {
//     #[inline]
//     fn while_bytes(&self, range: SpanRange, predicate: impl FnMut(u8) -> bool) -> usize { <&[u8]>::while_bytes(&&**self, range, predicate) }
// }
// impl WhileBytes for Vec<u8> {
//     #[inline]
//     fn while_bytes(&self, range: SpanRange, predicate: impl FnMut(u8) -> bool) -> usize { <&[u8]>::while_bytes(&self.as_slice(), range, predicate) }
// }

// // Default string impls for [`OneOfUtf8`]
// impl<'s> WhileBytes for &'s str {
//     #[inline]
//     fn while_bytes(&self, range: SpanRange, predicate: impl FnMut(u8) -> bool) -> usize { self.as_bytes().while_bytes(range, predicate) }
// }
// impl<'s> WhileBytes for Cow<'s, str> {
//     #[inline]
//     fn while_bytes(&self, range: SpanRange, predicate: impl FnMut(u8) -> bool) -> usize { <&str>::while_bytes(&&**self, range, predicate) }
// }
// impl WhileBytes for String {
//     #[inline]
//     fn while_bytes(&self, range: SpanRange, predicate: impl FnMut(u8) -> bool) -> usize { <&str>::while_bytes(&self.as_str(), range, predicate) }
// }

// // The implementation for a [`Span`].
// impl<F, S: WhileBytes> WhileBytes for Span<F, S> {
//     #[inline]
//     fn while_bytes(&self, range: SpanRange, predicate: impl FnMut(u8) -> bool) -> usize {
//         self.source_ref().while_bytes(self.range().span(&range), predicate)
//     }
// }



// /// Extends a [`Spannable`] with the power to have its prefix longest-match'ed based on a predicate over graphemes.
// pub trait WhileUtf8 {
//     /// Returns the position up to which a predicate over graphemes returns false.
//     ///
//     /// # Arguments
//     /// - `range`: The actual range of `self` to match.
//     /// - `predicate`: Some predicate (accepting graphemes, returning true or false) that will be used to decide if a grapheme matches the predicate.
//     ///
//     /// # Returns
//     /// A `usize` that indicates the first unmatched character. Some notes:
//     /// - If result is the length of `self`, the entire source was matched
//     /// - If result is 0, then none of `self` could be matched (i.e., first characters are wrong ...or `self` is empty!)
//     fn while_utf8(&self, range: SpanRange, predicate: impl FnMut(&str) -> bool) -> usize;
// }

// // Default string impls for [`OneOfUtf8`]
// impl<'s> WhileUtf8 for &'s str {
//     fn while_utf8(&self, range: SpanRange, mut predicate: impl FnMut(&str) -> bool) -> usize {
//         // Match the prefixes
//         let mut i: usize = 0;
//         for c in range.apply_to_str(self).graphemes(true) {
//             if !predicate(c) {
//                 return i;
//             }
//             i += c.len();
//         }
//         i
//     }
// }
// impl<'s> WhileUtf8 for Cow<'s, str> {
//     #[inline]
//     fn while_utf8(&self, range: SpanRange, predicate: impl FnMut(&str) -> bool) -> usize { <&str>::while_utf8(&&**self, range, predicate) }
// }
// impl WhileUtf8 for String {
//     #[inline]
//     fn while_utf8(&self, range: SpanRange, predicate: impl FnMut(&str) -> bool) -> usize { <&str>::while_utf8(&self.as_str(), range, predicate) }
// }

// // The implementation for a [`Span`].
// impl<F, S: WhileUtf8> WhileUtf8 for Span<F, S> {
//     #[inline]
//     fn while_utf8(&self, range: SpanRange, predicate: impl FnMut(&str) -> bool) -> usize {
//         self.source_ref().while_utf8(self.range().span(&range), predicate)
//     }
// }
