//  SPAN.rs
//    by Lut99
//
//  Created:
//    05 Apr 2024, 18:10:59
//  Last edited:
//    24 Apr 2024, 15:07:44
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements some extensions on [`Span`]s that allow us to perform
//!   very generic operations on them.
//

use std::borrow::Cow;

use ast_toolkit_span::{Span, SpanRange};
use unicode_segmentation::UnicodeSegmentation as _;


/// Extends a [`Spannable`] with the power to be match-prefix'ed by a byte-like object.
pub trait MatchBytes {
    /// Returns the position up to which the given bytes are a match.
    ///
    /// # Arguments
    /// - `range`: The actual range of `self` to match.
    /// - `bytes`: The byte pattern to match.
    ///
    /// # Returns
    /// A `usize` that indicates the first "wrong" character. Some notes:
    /// - If result is the length of `self`, the entire source was matched (but `bytes` may be longer!)
    /// - If result is 0, then none of `self` could be matched (i.e., first characters are wrong ...or `self` is empty!)
    fn match_bytes(&self, range: SpanRange, bytes: &[u8]) -> usize;
}

// Default binary impls for [`MatchBytes`]
impl<'b> MatchBytes for &'b [u8] {
    fn match_bytes(&self, range: SpanRange, bytes: &[u8]) -> usize {
        // Match the prefixes
        let mut i: usize = 0;
        for (b1, b2) in range.apply_to(self).iter().zip(bytes.iter()) {
            if b1 != b2 {
                return i;
            }
            i += 1;
        }
        i
    }
}
impl<'b> MatchBytes for Cow<'b, [u8]> {
    #[inline]
    fn match_bytes(&self, range: SpanRange, bytes: &[u8]) -> usize { <&[u8]>::match_bytes(&&**self, range, bytes) }
}
impl MatchBytes for Vec<u8> {
    #[inline]
    fn match_bytes(&self, range: SpanRange, bytes: &[u8]) -> usize { <&[u8]>::match_bytes(&self.as_slice(), range, bytes) }
}

// Default string impls for [`MatchBytes`]
impl<'s> MatchBytes for &'s str {
    fn match_bytes(&self, range: SpanRange, bytes: &[u8]) -> usize {
        // Do a byte-wise comparison
        self.as_bytes().match_bytes(range, bytes)
    }
}
impl<'s> MatchBytes for Cow<'s, str> {
    #[inline]
    fn match_bytes(&self, range: SpanRange, bytes: &[u8]) -> usize { <&str>::match_bytes(&&**self, range, bytes) }
}
impl MatchBytes for String {
    #[inline]
    fn match_bytes(&self, range: SpanRange, bytes: &[u8]) -> usize { <&str>::match_bytes(&self.as_str(), range, bytes) }
}

// The implementation for a [`Span`].
impl<F, S: MatchBytes> MatchBytes for Span<F, S> {
    #[inline]
    fn match_bytes(&self, range: SpanRange, bytes: &[u8]) -> usize { self.source_ref().match_bytes(self.range().span(&range), bytes) }
}



/// Extends a [`Spannable`] with the power to have its prefix longest-match'ed a set of possible bytes.
pub trait OneOfBytes {
    /// Returns the position up to which self starts with the given bytes.
    ///
    /// Precisely, given a set of bytes, will attempt to match the prefix of self for as long as it consists of one of those bytes.
    ///
    /// # Arguments
    /// - `range`: The actual range of `self` to match.
    /// - `bytes`: The set of bytes to match.
    ///
    /// # Returns
    /// A `usize` that indicates the first "wrong" character. Some notes:
    /// - If result is the length of `self`, the entire source was matched
    /// - If result is 0, then none of `self` could be matched (i.e., first characters are wrong ...or `self` is empty!)
    fn one_of_bytes(&self, range: SpanRange, bytes: &[u8]) -> usize;
}

// Default binary impls for [`OneOfBytes`]
impl<'b> OneOfBytes for &'b [u8] {
    fn one_of_bytes(&self, range: SpanRange, bytes: &[u8]) -> usize {
        // Match the prefixes
        let mut i: usize = 0;
        for b in range.apply_to(self).iter() {
            if !bytes.contains(b) {
                return i;
            }
            i += 1;
        }
        i
    }
}
impl<'b> OneOfBytes for Cow<'b, [u8]> {
    #[inline]
    fn one_of_bytes(&self, range: SpanRange, bytes: &[u8]) -> usize { <&[u8]>::one_of_bytes(&&**self, range, bytes) }
}
impl OneOfBytes for Vec<u8> {
    #[inline]
    fn one_of_bytes(&self, range: SpanRange, bytes: &[u8]) -> usize { <&[u8]>::one_of_bytes(&self.as_slice(), range, bytes) }
}

// Default string impls for [`OneOfBytes`]
impl<'s> OneOfBytes for &'s str {
    fn one_of_bytes(&self, range: SpanRange, bytes: &[u8]) -> usize {
        // Do a byte-wise comparison
        self.as_bytes().one_of_bytes(range, bytes)
    }
}
impl<'s> OneOfBytes for Cow<'s, str> {
    #[inline]
    fn one_of_bytes(&self, range: SpanRange, bytes: &[u8]) -> usize { <&str>::one_of_bytes(&&**self, range, bytes) }
}
impl OneOfBytes for String {
    #[inline]
    fn one_of_bytes(&self, range: SpanRange, bytes: &[u8]) -> usize { <&str>::one_of_bytes(&self.as_str(), range, bytes) }
}

// The implementation for a [`Span`].
impl<F, S: OneOfBytes> OneOfBytes for Span<F, S> {
    #[inline]
    fn one_of_bytes(&self, range: SpanRange, bytes: &[u8]) -> usize { self.source_ref().one_of_bytes(self.range().span(&range), bytes) }
}



/// Extends a [`Spannable`] with the power to have its prefix longest-match'ed a set of possible graphemes.
pub trait OneOfUtf8 {
    /// Returns the position up to which self starts with the given characters.
    ///
    /// Precisely, given a set of graphemes, will attempt to match the prefix of self for as long as it consists of one of those graphemes.
    ///
    /// # Arguments
    /// - `range`: The actual range of `self` to match.
    /// - `chars`: The set of graphemes (given as [`str`]-pointers) to match.
    ///
    /// # Returns
    /// A `usize` that indicates the first "wrong" character. Some notes:
    /// - If result is the length of `self`, the entire source was matched
    /// - If result is 0, then none of `self` could be matched (i.e., first characters are wrong ...or `self` is empty!)
    fn one_of_utf8(&self, range: SpanRange, chars: &[&str]) -> usize;
}

// Default string impls for [`OneOfUtf8`]
impl<'s> OneOfUtf8 for &'s str {
    fn one_of_utf8(&self, range: SpanRange, chars: &[&str]) -> usize {
        // Match the prefixes
        for (i, c) in range.apply_to_str(self).grapheme_indices(true) {
            if !chars.contains(&c) {
                return i;
            }
        }
        self.len()
    }
}
impl<'s> OneOfUtf8 for Cow<'s, str> {
    #[inline]
    fn one_of_utf8(&self, range: SpanRange, chars: &[&str]) -> usize { <&str>::one_of_utf8(&&**self, range, chars) }
}
impl OneOfUtf8 for String {
    #[inline]
    fn one_of_utf8(&self, range: SpanRange, chars: &[&str]) -> usize { <&str>::one_of_utf8(&self.as_str(), range, chars) }
}

// The implementation for a [`Span`].
impl<F, S: OneOfUtf8> OneOfUtf8 for Span<F, S> {
    #[inline]
    fn one_of_utf8(&self, range: SpanRange, chars: &[&str]) -> usize { self.source_ref().one_of_utf8(self.range().span(&range), chars) }
}



/// Extends a [`Spannable`] with the power to have its prefix longest-match'ed based on a predicate over graphemes.
pub trait WhileUtf8 {
    /// Returns the position up to which a predicate over graphemes returns false.
    ///
    /// # Arguments
    /// - `range`: The actual range of `self` to match.
    /// - `predicate`: Some predicate (accepting graphemes, returning true or false) that will be used to decide if a grapheme matches the predicate.
    ///
    /// # Returns
    /// A `usize` that indicates the first unmatched character. Some notes:
    /// - If result is the length of `self`, the entire source was matched
    /// - If result is 0, then none of `self` could be matched (i.e., first characters are wrong ...or `self` is empty!)
    fn while_utf8(&self, range: SpanRange, predicate: impl FnMut(&str) -> bool) -> usize;
}

// Default string impls for [`OneOfUtf8`]
impl<'s> WhileUtf8 for &'s str {
    fn while_utf8(&self, range: SpanRange, mut predicate: impl FnMut(&str) -> bool) -> usize {
        // Match the prefixes
        for (i, c) in range.apply_to_str(self).grapheme_indices(true) {
            if !predicate(c) {
                return i;
            }
        }
        self.len()
    }
}
impl<'s> WhileUtf8 for Cow<'s, str> {
    #[inline]
    fn while_utf8(&self, range: SpanRange, predicate: impl FnMut(&str) -> bool) -> usize { <&str>::while_utf8(&&**self, range, predicate) }
}
impl WhileUtf8 for String {
    #[inline]
    fn while_utf8(&self, range: SpanRange, predicate: impl FnMut(&str) -> bool) -> usize { <&str>::while_utf8(&self.as_str(), range, predicate) }
}

// The implementation for a [`Span`].
impl<F, S: WhileUtf8> WhileUtf8 for Span<F, S> {
    #[inline]
    fn while_utf8(&self, range: SpanRange, predicate: impl FnMut(&str) -> bool) -> usize {
        self.source_ref().while_utf8(self.range().span(&range), predicate)
    }
}
