//  LOCATE.rs
//    by Lut99
//
//  Created:
//    27 May 2024, 11:05:40
//  Last edited:
//    27 May 2024, 13:56:31
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`SpannableLocate`], which encodes that some
//!   members have a useful notion of column/line positions.
//

use std::borrow::Cow;
use std::rc::Rc;
use std::sync::Arc;

use unicode_segmentation::UnicodeSegmentation;

use crate::spannable::Spannable;


/***** LIBRARY *****/
/// An extension for [`Spannable`]s that make them return column/line positions.
pub trait SpannableLocate: Spannable {
    /// Finds the column/line pair of the given position in the underlying source text.
    ///
    /// The position is in _logical coordinates_, i.e., whatever are logical chunks for the spanned
    /// object instead of bytes. For example, for strings, this would be graphemes.
    ///
    /// This function assumes that the start of the underlying source text is (0, 0). I.e., it
    /// ignores the range.
    ///
    /// # Arguments
    /// - `idx`: Some offset in the spanned object's logical space (e.g., graphemes).
    ///
    /// # Returns
    /// A pair of column and line numbers, respectively. Both are zero-indexed.
    ///
    /// If the given `idx` is out-of-bounds, then [`None`] is returned.
    fn coords_of(&self, idx: usize) -> Option<(usize, usize)>;
}

// Default binary impls for [`SpannableLocate`]
impl SpannableLocate for [u8] {
    fn coords_of(&self, idx: usize) -> Option<(usize, usize)> {
        // Catch the would-be-out-of-bounds case
        if idx >= self.len() {
            return None;
        }

        // Simply count newlines, then return
        let n_newlines: usize = self.iter().enumerate().filter(|(i, b)| *i < idx && **b == b'\n').count();
        Some((idx / n_newlines, idx % n_newlines))
    }
}
impl<const LEN: usize> SpannableLocate for [u8; LEN] {
    #[inline]
    fn coords_of(&self, idx: usize) -> Option<(usize, usize)> { <[u8] as SpannableLocate>::coords_of(&self.as_slice(), idx) }
}
impl<'b> SpannableLocate for Cow<'b, [u8]> {
    #[inline]
    fn coords_of(&self, idx: usize) -> Option<(usize, usize)> { <[u8] as SpannableLocate>::coords_of(&self.as_ref(), idx) }
}
impl SpannableLocate for Vec<u8> {
    #[inline]
    fn coords_of(&self, idx: usize) -> Option<(usize, usize)> { <[u8] as SpannableLocate>::coords_of(&self.as_slice(), idx) }
}
impl SpannableLocate for Rc<[u8]> {
    #[inline]
    fn coords_of(&self, idx: usize) -> Option<(usize, usize)> { <[u8] as SpannableLocate>::coords_of(&self.as_ref(), idx) }
}
impl SpannableLocate for Arc<[u8]> {
    #[inline]
    fn coords_of(&self, idx: usize) -> Option<(usize, usize)> { <[u8] as SpannableLocate>::coords_of(&self.as_ref(), idx) }
}

// Default string impls for [`SpannableLocate`]
impl SpannableLocate for str {
    fn coords_of(&self, idx: usize) -> Option<(usize, usize)> {
        // Simply loop to find
        let mut line: usize = 0;
        let mut col: usize = 0;
        for (i, c) in self.graphemes(true).enumerate() {
            // Quit if we reached our token
            if i == idx {
                return Some((line, col));
            }

            // Else, increment as necessary
            if c == "\n" {
                line += 1;
                col = 0;
            } else {
                col += 1;
            }
        }
        None
    }
}
impl<'s> SpannableLocate for Cow<'s, str> {
    #[inline]
    fn coords_of(&self, idx: usize) -> Option<(usize, usize)> { <str as SpannableLocate>::coords_of(&self.as_ref(), idx) }
}
impl SpannableLocate for String {
    #[inline]
    fn coords_of(&self, idx: usize) -> Option<(usize, usize)> { <str as SpannableLocate>::coords_of(&self.as_str(), idx) }
}
impl SpannableLocate for Rc<str> {
    #[inline]
    fn coords_of(&self, idx: usize) -> Option<(usize, usize)> { <str as SpannableLocate>::coords_of(&self.as_ref(), idx) }
}
impl SpannableLocate for Arc<str> {
    #[inline]
    fn coords_of(&self, idx: usize) -> Option<(usize, usize)> { <str as SpannableLocate>::coords_of(&self.as_ref(), idx) }
}

// Default pointer-like impls for [`SpannableLocate`]
impl<'t, T: ?Sized + SpannableLocate> SpannableLocate for &'t T {
    #[inline]
    #[track_caller]
    fn coords_of(&self, idx: usize) -> Option<(usize, usize)> { T::coords_of(self, idx) }
}
