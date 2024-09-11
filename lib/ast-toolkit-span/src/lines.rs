//  LINES.rs
//    by Lut99
//
//  Created:
//    27 May 2024, 13:54:41
//  Last edited:
//    27 May 2024, 14:22:01
//  Auto updated?
//    Yes
//
//  Description:
//!   Contains functions for splitting [`Spannable`]s in lines.
//

use unicode_segmentation::UnicodeSegmentation as _;

use crate::range::{resolve_range, SpanRange};
use crate::spannable::Spannable;


/***** ITERATORS *****/
/// Produces slices for UTF-8 lines in the given heap of bytes.
#[derive(Clone, Copy, Debug)]
pub struct Lines<'b> {
    /// The slice to split
    bytes: &'b [u8],
    /// The last newline we saw
    last:  usize,
    /// The current position
    i:     usize,
}
impl<'b> Iterator for Lines<'b> {
    type Item = &'b [u8];

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        // Fuse the iterator
        if self.i >= self.bytes.len() {
            return None;
        }

        // Find the earliest newline
        while self.i < self.bytes.len() {
            if self.bytes[self.i] == b'\n' || (self.i + 1 < self.bytes.len() && self.bytes[self.i] == b'\r' && self.bytes[self.i + 1] == b'\n') {
                // Increment i to skip over the newlines for next time
                let i: usize = self.i;
                self.i += 1;
                if self.bytes[self.i] == b'\r' {
                    self.i += 1;
                }

                // Found one
                let last: usize = self.last;
                self.last = i;
                return Some(&self.bytes[last..i]);
            }
        }

        // Otherwise, return until the end of time
        Some(&self.bytes[self.last..])
    }
}





/***** LIBRARY *****/
/// An extension for [`Spannable`]s that make them splittable on lines.
pub trait SpannableLines: Spannable {
    /// Some iterator that can iterate over the spanned lines in `self`.
    type Lines<'s>: Iterator<Item = Self::Slice<'s>>
    where
        Self: 's;


    /// Returns an iterator of lines in the source text, but only those that overlap with the
    /// spanned area.
    ///
    /// Note that the full lines are returned, not just the spanned parts of the overlapping lines.
    /// This is useful for producing nice looking error messages.
    ///
    /// The lines are split by `\r\n` or `\n` (in that order). See
    /// [`SpannableLines::slice_chunks()`] for an alternative that produces fixed-width lines,
    /// which is more friendly to raw data.
    ///
    /// # Arguments
    /// - `range`: A [`SpanRange`] that denotes the spanned area.
    ///
    /// # Returns
    /// An iterator over [`S::Slice`] that captures the highlighted area plus the start- and ends
    /// of its lines. Note that it should **exclude** the splitting newlines from the produced
    /// slices.
    fn slice_lines<'s>(&'s self, range: SpanRange) -> Self::Lines<'s>;
}

// Default binary impls
impl SpannableLines for [u8] {
    type Lines<'s> = Lines<'s>;

    #[track_caller]
    fn slice_lines<'s>(&'s self, range: SpanRange) -> Self::Lines<'s> {
        // Resolve the range
        let (mut start, mut end): (usize, usize) = match resolve_range!(range, self.len()) {
            Some(range) => range,
            None => return Lines { bytes: &[], last: 0, i: 0 },
        };

        // Move the start back to find the earliest newline
        while start > 0 && self[start - 1] != b'\n' {
            start -= 1;
        }
        // Move the end forwards to find the earliest newline
        while end < self.len() && self[end] != b'\n' {
            end += 1;
        }

        // OK, that's the range
        Lines { bytes: &self[start..end], last: 0, i: 0 }
    }
}

// Default string impls
impl SpannableLines for str {
    type Lines<'s> = std::str::Lines<'s>;

    #[track_caller]
    fn slice_lines<'s>(&'s self, range: SpanRange) -> Self::Lines<'s> {
        // Resolve the range
        let (start, end): (usize, usize) = match resolve_range!(range, self.len()) {
            Some(range) => range,
            None => return "".lines(),
        };

        // No choice but to count
        let mut prev_newline: usize = 0;
        for (i, c) in self.grapheme_indices(true) {
            // Keep track of the latest newlines while we still haven't passed start
            if i < start && c == "\n" {
                prev_newline = i;
                continue;
            } else if i >= end && c == "\n" {
                // This is the end position!
                return self[prev_newline + 1..i + 1].lines();
            }
        }

        // Consume the entire range, because there was no closing newline
        self[prev_newline + 1..].lines()
    }
}
