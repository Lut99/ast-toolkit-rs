//  SPAN.rs
//    by Lut99
// 
//  Created:
//    02 Jul 2023, 16:40:44
//  Last edited:
//    22 Aug 2023, 13:42:22
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines the [`Span`] (and [`Position`]) structs which we use to keep
//!   track of a node's position in the source text.
// 

use std::ops::{RangeFrom, RangeFull, RangeInclusive, RangeToInclusive};

use enum_debug::EnumDebug;
use num_traits::AsPrimitive;
use unicode_segmentation::UnicodeSegmentation as _;

use crate::position::Position;


/***** TESTS *****/
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_from_pos() {
        let span: Span = Span::from_pos("<example>", "Hello, World!", Position::new1(1, 1), Position::new1(1, 1));
        assert_eq!(span.text(), "H");
        let span: Span = Span::from_pos("<example>", "Hello, World!", Position::new1(1, 1), Position::new1(1, 5));
        assert_eq!(span.text(), "Hello");

        let span: Span = Span::from_pos("<example>", "Hello\nWorld!", Position::new1(1, 1), Position::new1(1, 1));
        assert_eq!(span.text(), "H");
        let span: Span = Span::from_pos("<example>", "Hello\nWorld!", Position::new1(1, 1), Position::new1(1, 5));
        assert_eq!(span.text(), "Hello");
        let span: Span = Span::from_pos("<example>", "Hello\nWorld!", Position::new1(2, 1), Position::new1(2, 1));
        assert_eq!(span.text(), "W");
        let span: Span = Span::from_pos("<example>", "Hello\nWorld!", Position::new1(2, 1), Position::new1(2, 5));
        assert_eq!(span.text(), "World");
        let span: Span = Span::from_pos("<example>", "Hello\nWorld!", Position::new1(1, 1), Position::new1(2, 6));
        assert_eq!(span.text(), "Hello\nWorld!");
    }
}

#[cfg(feature = "nom")]
#[cfg(test)]
mod nom_tests {
    use itertools::Itertools as _;
    use super::*;

    #[test]
    fn test_span_nom_as_bytes() {
        // Create a few spans and see if they byte version equates what we expect
        assert_eq!(<Span as nom::AsBytes>::as_bytes(&Span::new("<example>", "Example text")), b"Example text");
        assert_eq!(<Span as nom::AsBytes>::as_bytes(&Span::from_idx("<example>", "Example text", 0, 6)), b"Example");
        assert_eq!(<Span as nom::AsBytes>::as_bytes(&Span::from_idx("<example>", "Example text", 8, 11)), b"text");
        assert_eq!(<Span as nom::AsBytes>::as_bytes(&Span::from_idx("<example>", "Example text", 3, 9)), b"mple te");
    }
    #[test]
    fn test_span_nom_compare() {
        // Do some comparisons
        assert_eq!(<Span as nom::Compare<&str>>::compare(&Span::new("<example>", "Example text"), "Example text"), nom::CompareResult::Ok);
        assert_eq!(<Span as nom::Compare<&str>>::compare(&Span::from_idx("<example>", "Example text", 0, 6), "Example"), nom::CompareResult::Ok);
        assert_eq!(<Span as nom::Compare<&str>>::compare(&Span::from_idx("<example>", "Example text", 8, 11), "text"), nom::CompareResult::Ok);
        assert_eq!(<Span as nom::Compare<&str>>::compare(&Span::new("<example>", "Example text"), "Example text 2"), nom::CompareResult::Incomplete);
        assert_eq!(<Span as nom::Compare<&str>>::compare(&Span::new("<example>", "Example text"), "Example2 text"), nom::CompareResult::Error);
    }
    #[test]
    fn test_span_nom_extend_into() {
        let file: &str = "<test>";
        let code: &str = "let test: &str = \"Hello, there!\"\nprintln!(\"{test}\");";

        // Attempt to combine a few spans
        let spans = vec![
            Span::from_idx(file, code, 0, 2),
            Span::from_idx(file, code, 4, 7),
            Span::from_idx(file, code, 11, 13),
            Span::from_idx(file, code, 18, 22),
            Span::from_idx(file, code, 25, 29),
            Span::from_idx(file, code, 32, 39),
            Span::from_idx(file, code, 44, 47),
        ];

        // Try various combinations
        for spans in spans.iter().combinations(2) {
            assert_eq!(spans.len(), 2);
            let span1: &Span = spans[0];
            let span2: &Span = spans[1];

            // Compare them
            let mut builder = <Span as nom::ExtendInto>::new_builder(&span1);
            <Span as nom::ExtendInto>::extend_into(&span2, &mut builder);
            assert_eq!(builder.text(), SpanRange::combined(span1.range, span2.range).slice(code));
        }
    }
    #[test]
    fn test_span_nom_find_substring() {
        // Find substrings as indices in the source
        assert_eq!(<Span as nom::FindSubstring<&str>>::find_substring(&Span::new("<example>", "Hello, world!"), "Hello"), Some(0));
        assert_eq!(<Span as nom::FindSubstring<&str>>::find_substring(&Span::new("<example>", "Hello, world!"), "world"), Some(7));
        assert_eq!(<Span as nom::FindSubstring<&str>>::find_substring(&Span::new("<example>", "Hello, world!"), "!"), Some(12));
        assert_eq!(<Span as nom::FindSubstring<&str>>::find_substring(&Span::new("<example>", "Hello, world!"), "Bananas"), None);
        assert_eq!(<Span as nom::FindSubstring<&str>>::find_substring(&Span::new("<example>", "Hello, world!"), "Helol"), None);

        // Do the same but with limited source
        assert_eq!(<Span as nom::FindSubstring<&str>>::find_substring(&Span::from_idx("<example>", "Hello, world!", 7, 12), "Hello"), None);
        assert_eq!(<Span as nom::FindSubstring<&str>>::find_substring(&Span::from_idx("<example>", "Hello, world!", 7, 12), "world"), Some(7));
        assert_eq!(<Span as nom::FindSubstring<&str>>::find_substring(&Span::from_idx("<example>", "Hello, world!", 7, 12), "!"), Some(12));
        assert_eq!(<Span as nom::FindSubstring<&str>>::find_substring(&Span::from_idx("<example>", "Hello, world!", 7, 12), "Bananas"), None);
        assert_eq!(<Span as nom::FindSubstring<&str>>::find_substring(&Span::from_idx("<example>", "Hello, world!", 7, 12), "Helol"), None);
    }
    #[test]
    fn test_span_nom_find_token() {
        let file: &str = "<test>";
        let text: &str = "Hello there!ÿ";

        // Find some bytes in full source...
        assert_eq!(<Span as nom::FindToken<u8>>::find_token(&Span::new(file, text), b'H'), true);
        assert_eq!(<Span as nom::FindToken<u8>>::find_token(&Span::new(file, text), b'o'), true);
        assert_eq!(<Span as nom::FindToken<u8>>::find_token(&Span::new(file, text), b'!'), true);
        assert_eq!(<Span as nom::FindToken<u8>>::find_token(&Span::new(file, text), b'q'), false);
        // ...and ranges source
        assert_eq!(<Span as nom::FindToken<u8>>::find_token(&Span::from_idx(file, text, 6, 13), b'H'), false);
        assert_eq!(<Span as nom::FindToken<u8>>::find_token(&Span::from_idx(file, text, 6, 13), b'o'), false);
        assert_eq!(<Span as nom::FindToken<u8>>::find_token(&Span::from_idx(file, text, 6, 13), b'!'), true);
        assert_eq!(<Span as nom::FindToken<u8>>::find_token(&Span::from_idx(file, text, 6, 13), b'q'), false);

        // Find some characters in full source...
        assert_eq!(<Span as nom::FindToken<char>>::find_token(&Span::new(file, text), 'H'), true);
        assert_eq!(<Span as nom::FindToken<char>>::find_token(&Span::new(file, text), 'o'), true);
        assert_eq!(<Span as nom::FindToken<char>>::find_token(&Span::new(file, text), '!'), true);
        assert_eq!(<Span as nom::FindToken<char>>::find_token(&Span::new(file, text), 'q'), false);
        assert_eq!(<Span as nom::FindToken<char>>::find_token(&Span::new(file, text), 'ÿ'), true);
        // ...and ranges source
        assert_eq!(<Span as nom::FindToken<char>>::find_token(&Span::from_idx(file, text, 6, 13), 'H'), false);
        assert_eq!(<Span as nom::FindToken<char>>::find_token(&Span::from_idx(file, text, 6, 13), 'o'), false);
        assert_eq!(<Span as nom::FindToken<char>>::find_token(&Span::from_idx(file, text, 6, 13), '!'), true);
        assert_eq!(<Span as nom::FindToken<char>>::find_token(&Span::from_idx(file, text, 6, 13), 'q'), false);
        assert_eq!(<Span as nom::FindToken<char>>::find_token(&Span::from_idx(file, text, 6, 13), 'ÿ'), true);

        // Find some graphemes in full source...
        assert_eq!(<Span as nom::FindToken<&str>>::find_token(&Span::new(file, text), "H"), true);
        assert_eq!(<Span as nom::FindToken<&str>>::find_token(&Span::new(file, text), "o"), true);
        assert_eq!(<Span as nom::FindToken<&str>>::find_token(&Span::new(file, text), "!"), true);
        assert_eq!(<Span as nom::FindToken<&str>>::find_token(&Span::new(file, text), "q"), false);
        assert_eq!(<Span as nom::FindToken<&str>>::find_token(&Span::new(file, text), "ÿ"), true);
        // ...and ranges source
        assert_eq!(<Span as nom::FindToken<&str>>::find_token(&Span::from_idx(file, text, 6, 13), "H"), false);
        assert_eq!(<Span as nom::FindToken<&str>>::find_token(&Span::from_idx(file, text, 6, 13), "o"), false);
        assert_eq!(<Span as nom::FindToken<&str>>::find_token(&Span::from_idx(file, text, 6, 13), "!"), true);
        assert_eq!(<Span as nom::FindToken<&str>>::find_token(&Span::from_idx(file, text, 6, 13), "q"), false);
        assert_eq!(<Span as nom::FindToken<&str>>::find_token(&Span::from_idx(file, text, 6, 13), "ÿ"), true);
    }
    #[test]
    fn test_span_nom_input_iter() {
        let target: &str = "Example text";
        let span = Span::new("<example>", target);

        // Try to iterate index/element-wise
        for (i, b) in <Span as nom::InputIter>::iter_indices(&span) {
            assert_eq!(b, target.as_bytes()[i]);
        }

        // Try to iterate element-wise
        let mut chars = target.bytes();
        for b in <Span as nom::InputIter>::iter_elements(&span) {
            let c: u8 = if let Some(c) = chars.next() { c } else { panic!("Too many bytes in iter_elements()"); };
            assert_eq!(b, c);
        }

        // Take the position
        assert_eq!(<Span as nom::InputIter>::position(&span, |b: u8| b == b'l'), Some(5));
        assert_eq!(<Span as nom::InputIter>::position(&span, |b: u8| b == b't'), Some(8));

        // Take the slice
        match <Span as nom::InputIter>::slice_index(&span, 5) {
            Ok(idx)  => { assert_eq!(idx, 5); },
            Err(err) => { panic!("Taking the slice index of Span failed: {err:?}"); },
        }
    }
    #[test]
    fn test_span_nom_input_length() {
        // Create a few spans and see if their length matches with what we expect
        assert_eq!(<Span as nom::InputLength>::input_len(&Span::new("<example>", "Example text")), 12);
        assert_eq!(<Span as nom::InputLength>::input_len(&Span::from_idx("<example>", "Example text", 0, 6)), 7);
        assert_eq!(<Span as nom::InputLength>::input_len(&Span::from_idx("<example>", "Example text", 8, 11)), 4);
        assert_eq!(<Span as nom::InputLength>::input_len(&Span::from_idx("<example>", "Example text", 3, 9)), 7);
    }
    #[test]
    fn test_span_nom_input_take() {
        // See if we can take and split how we expect
        assert_eq!(<Span as nom::InputTake>::take(&Span::new("<example>", "Example text"), 7), Span::from_idx("<example>", "Example text", 0, 6));
        assert_eq!(<Span as nom::InputTake>::take(&Span::from_idx("<example>", "Example text", 8, 11), 3), Span::from_idx("<example>", "Example text", 8, 10));
        assert!(std::panic::catch_unwind(|| <Span as nom::InputTake>::take(&Span::from_idx("<example>", "Example text", 8, 11), 0)).is_err());

        // Now compare the split
        assert_eq!(<Span as nom::InputTake>::take_split(&Span::new("<example>", "Example text"), 7), (Span::from_idx("<example>", "Example text", 0, 6), Span::from_idx("<example>", "Example text", 7, 11)));
        assert_eq!(<Span as nom::InputTake>::take_split(&Span::from_idx("<example>", "Example text", 8, 11), 3), (Span::from_idx("<example>", "Example text", 8, 10), Span::from_idx("<example>", "Example text", 11, 11)));
        assert!(std::panic::catch_unwind(|| <Span as nom::InputTake>::take_split(&Span::from_idx("<example>", "Example text", 8, 11), 0)).is_err());
    }
    #[test]
    fn test_span_nom_input_take_at_position() {
        use nom::InputTakeAtPosition as _;
        type I<'f, 's> = (Span<'f, 's>, Span<'f, 's>);
        type E<'f, 's> = nom::Err<nom::error::VerboseError<Span<'f, 's>>>;

        // Attempt to split at some points
        assert_eq!(Span::new("<example>", "Example text!").split_at_position(|c| c == 'E'), Ok::<I, E>((Span::empty("<example>", "Example text!"), Span::new("<example>", "Example text!"))));
        assert_eq!(Span::new("<example>", "Example text!").split_at_position(|c| c == 't'), Ok::<I, E>((Span::from_range("<example>", "Example text!", ..=7), Span::from_range("<example>", "Example text!", 8..))));
        assert_eq!(Span::new("<example>", "Example text!").split_at_position(|c| c == '!'), Ok::<I, E>((Span::from_range("<example>", "Example text!", ..=11), Span::from_range("<example>", "Example text!", 12..))));

        /* TODO */
        panic!("LOL");
    }
}





/***** AUXILLARY *****/
/// Helper trait that abstracts over a string or a direct length.
/// 
/// # Example
/// ```rust
/// use ast_toolkit::span::Length;
/// 
/// assert_eq!(<usize as Length>::len(42), 42);
/// assert_eq!(<str as Length>::len(""), 0);
/// assert_eq!(<str as Length>::len("Hello there!"), 12);
/// ```
pub trait Length {
    /// Returns the length that Self represents.
    /// 
    /// # Returns
    /// The length as an unsigned integer.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::span::Length;
    /// 
    /// assert_eq!(<usize as Length>::len(42), 42);
    /// assert_eq!(<str as Length>::len(""), 0);
    /// assert_eq!(<str as Length>::len("Hello there!"), 12);
    /// ```
    fn len(&self) -> usize;
}

impl Length for usize {
    #[inline]
    fn len(&self) -> usize { *self }
}
impl<'s> Length for &'s str {
    #[inline]
    fn len(&self) -> usize { str::len(self) }
}



/// Defines a start- or end bound.
/// 
/// A start- or end bound can be either [`Bounded`](SpanBound::Bounded) (i.e., a specific index) or [`Unbounded`](SpanBound::Unbounded) (i.e., until the end of the range in this direction).
/// 
/// See the [`SpanRange`] for more details on how to use this enum.
/// ```
#[derive(Clone, Copy, Debug, EnumDebug, Eq, Hash, PartialEq)]
pub enum SpanBound {
    /// There is a specific index to which this bound runs. The index is always zero-indexed, inclusive.
    Bounded(usize),
    /// This bound runs for the remainder of the spanned text.
    Unbounded,
}

impl SpanBound {
    /// Returns whether this SpanBound is [`Bounded`](SpanBound::Bounded).
    /// 
    /// # Returns
    /// True if it is, false if it is another variant.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::span::SpanBound;
    /// 
    /// assert_eq!(SpanBound::Bounded(42).is_bounded(), true);
    /// assert_eq!(SpanBound::Unbounded.is_bounded(), false);
    /// ```
    #[inline]
    pub fn is_bounded(&self) -> bool { matches!(self, Self::Bounded(_)) }
    /// Returns the internal bounded index.
    /// 
    /// # Returns
    /// The bounded index in this SpanBound.
    /// 
    /// # Panics
    /// This function panics if the SpanBound is not [`Bounded`](SpanBound::Bounded).
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::span::SpanBound;
    /// 
    /// assert_eq!(SpanBound::Bounded(42).bound(), 42);
    /// assert!(std::panic::catch_unwind(|| SpanBound::Unbounded.bound()).is_err());
    /// ```
    #[inline]
    #[track_caller]
    pub fn bound(&self) -> usize { if let Self::Bounded(bound) = self { *bound } else { panic!("Cannot unwrap a {:?} as a SpanBound::Bounded", self.variant()); } }

    /// Returns whether this SpanBound is [`Unbounded`](SpanBound::Unbounded).
    /// 
    /// # Returns
    /// True if it is, false if it is another variant.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::span::SpanBound;
    /// 
    /// assert_eq!(SpanBound::Bounded(42).is_unbounded(), false);
    /// assert_eq!(SpanBound::Unbounded.is_unbounded(), true);
    /// ```
    #[inline]
    pub fn is_unbounded(&self) -> bool { matches!(self, Self::Unbounded) }
}

impl<N: AsPrimitive<usize>> From<N> for SpanBound {
    #[inline]
    fn from(value: N) -> Self { Self::Bounded(value.as_()) }
}

impl AsRef<SpanBound> for SpanBound {
    #[inline]
    fn as_ref(&self) -> &Self { self }
}
impl AsMut<SpanBound> for SpanBound {
    #[inline]
    fn as_mut(&mut self) -> &mut Self { self }
}
impl From<&SpanBound> for SpanBound {
    #[inline]
    fn from(value: &Self) -> Self { *value }
}
impl From<&mut SpanBound> for SpanBound {
    #[inline]
    fn from(value: &mut Self) -> Self { *value }
}



/// Properly defines the possible ranges that a [`Span`] can span over.
/// 
/// This takes into account the fact that spans might be unbounded _or_ empty.
/// 
/// # Example
/// ```rust
/// /* TODO */
/// ```
#[derive(Clone, Copy, Debug, EnumDebug, Eq, Hash, PartialEq)]
pub enum SpanRange {
    /// There is some range to span over, given as a (start, end)-tuple.
    Range(SpanBound, SpanBound),
    /// The range is empty.
    Empty,
}

impl SpanRange {
    /// Constructor for a SpanRange.
    /// 
    /// # Arguments
    /// - `start`: The start [`SpanBound`].
    /// - `end`: The end [`SpanBound`].
    /// 
    /// # Returns
    /// A new SpanRange with the given start and end ranges. If they are both [`Bounded`](SpanBound::Bounded) and `start` > `end`, then the range is empty.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::span::{SpanBound, SpanRange};
    /// 
    /// let range = SpanRange::new(SpanBound::Bounded(0), SpanBound::Unbounded);
    /// assert_eq!(range, SpanRange::Range(SpanBound::Bounded(0), SpanBound::Unbounded));
    /// 
    /// let range = SpanRange::new(0, 9);
    /// assert_eq!(range, SpanRange::Range(SpanBound::Bounded(0), SpanBound::Bounded(9)));
    /// 
    /// let range = SpanRange::new(SpanBound::Bounded(1), SpanBound::Bounded(0));
    /// assert_eq!(range, SpanRange::Empty);
    /// ```
    #[inline]
    pub fn new(start: impl Into<SpanBound>, end: impl Into<SpanBound>) -> Self {
        let (start, end): (SpanBound, SpanBound) = (start.into(), end.into());
        match (start, end) {
            (SpanBound::Bounded(start), SpanBound::Bounded(end)) => if start <= end { Self::Range(SpanBound::Bounded(start), SpanBound::Bounded(end)) } else { Self::Empty },
            (start, end) => Self::Range(start, end),
        }
    }

    /// Constructor for a SpanRange that assumes the start is unbounded.
    /// 
    /// # Arguments
    /// - `end`: The end [`SpanBound`].
    /// 
    /// # Returns
    /// A new SpanRange with the start unbounded and the given end range.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::span::{SpanBound, SpanRange};
    /// 
    /// assert_eq!(SpanRange::start_unbounded(9), SpanRange::new(SpanBound::Unbounded, SpanBound::Bounded(9)));
    /// assert_eq!(SpanRange::start_unbounded(SpanBound::Unbounded), SpanRange::new(SpanBound::Unbounded, SpanBound::Unbounded));
    /// ```
    #[inline]
    pub fn start_unbounded(end: impl Into<SpanBound>) -> Self { Self::Range(SpanBound::Unbounded, end.into()) }

    /// Constructor for a SpanRange that assumes the end is unbounded.
    /// 
    /// # Arguments
    /// - `start`: The start [`SpanBound`].
    /// 
    /// # Returns
    /// A new SpanRange with the given start and unbounded end.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::span::{SpanBound, SpanRange};
    /// 
    /// assert_eq!(SpanRange::end_unbounded(9), SpanRange::new(SpanBound::Bounded(9), SpanBound::Unbounded));
    /// assert_eq!(SpanRange::end_unbounded(SpanBound::Unbounded), SpanRange::new(SpanBound::Unbounded, SpanBound::Unbounded));
    /// ```
    #[inline]
    pub fn end_unbounded(start: impl Into<SpanBound>) -> Self { Self::Range(start.into(), SpanBound::Unbounded) }

    /// Constructor for a SpanRange that assumes both the start and end are unbounded.
    /// 
    /// # Returns
    /// A new SpanRange with an unbounded start and end.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::span::{SpanBound, SpanRange};
    /// 
    /// assert_eq!(SpanRange::unbounded(), SpanRange::new(SpanBound::Unbounded, SpanBound::Unbounded));
    /// ```
    #[inline]
    pub fn unbounded() -> Self { Self::Range(SpanBound::Unbounded, SpanBound::Unbounded) }

    /// Constructor for a SpanRange that initializes it as empty.
    /// 
    /// # Returns
    /// A new SpanRange that is initialized as empty.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::span::{SpanBound, SpanRange};
    /// 
    /// let range: SpanRange::empty();
    /// assert_eq!(range, SpanRange::Empty);
    /// ```
    #[inline]
    pub fn empty() -> Self { Self::Empty }

    /// Constructor for a SpanRange that encapsulates the given two ranges.
    /// 
    /// Some remarks:
    /// - An unbounded side always travels (e.g., any range has an unbounded start, the result will also be unbounded start)
    /// - An empty range counts as no change (e.g., if `range1` is empty, `range2` is passed as-is; only if both are empty is empty returned)
    /// 
    /// # Arguments
    /// - `range1`: The first SpanRange to span over.
    /// - `range2`: The second SpanRange to span over.
    /// 
    /// # Returns
    /// A new instance of Self which contains a continious area that snugly spans both `range1` and `range2`.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::span::{SpanBound, SpanRange};
    /// 
    /// let range1 = SpanRange::new(0, 9);
    /// let range2 = SpanRange::new(15, 30);
    /// let range3 = SpanRange::new(SpanBound::Unbounded, 9);
    /// let range4 = SpanRange::new(9, SpanBound::Unbounded);
    /// let range5 = SpanRange::empty();
    /// 
    /// // If everything is bounded, then the range snugly fits the result (communitatively).
    /// assert_eq!(SpanRange::combined(range1, range2), SpanRange::new(0, 30));
    /// assert_eq!(SpanRange::combined(range2, range1), SpanRange::new(0, 30));
    /// 
    /// // If either side is unbounded, it propagates
    /// assert_eq!(SpanRange::combined(range1, range3), SpanRange::new(SpanBound::Unbounded, 9));
    /// assert_eq!(SpanRange::combined(range1, range4), SpanRange::new(0, SpanBound::Unbounded));
    /// assert_eq!(SpanRange::combined(range3, range4), SpanRange::new(SpanBound::Unbounded, SpanBound::Unbounded));
    /// 
    /// // Finally, empty does not do anything
    /// assert_eq!(SpanRange::combined(range5, range1), range1);
    /// assert_eq!(SpanRange::combined(range3, range5), range3);
    /// assert_eq!(SpanRange::combined(range5, range5), SpanRange::empty());
    /// ```
    pub fn combined(range1: impl Into<SpanRange>, range2: impl Into<SpanRange>) -> Self {
        let (range1, range2): (SpanRange, SpanRange) = (range1.into(), range2.into());

        // Unpack the ranges and process any empty ranges
        let (start1, end1, start2, end2): (SpanBound, SpanBound, SpanBound, SpanBound) = match (range1, range2) {
            (Self::Range(start1, end1), Self::Range(start2, end2)) => (start1, end1, start2, end2),
            (range1, Self::Empty)                                  => { return range1; },
            (Self::Empty, range2)                                  => { return range2; },
            (Self::Empty, Self::Empty)                             => { return Self::Empty; },
        };

        // Match a new start range
        let start: SpanBound = match (start1, start2) {
            (SpanBound::Bounded(start1), SpanBound::Bounded(start2)) => SpanBound::Bounded(std::cmp::min(start1, start2)),
            (SpanBound::Bounded(_), SpanBound::Unbounded)            => SpanBound::Unbounded,
            (SpanBound::Unbounded, SpanBound::Bounded(_))            => SpanBound::Unbounded,
            (SpanBound::Unbounded, SpanBound::Unbounded)             => SpanBound::Unbounded,
        };
        // Match a new end range
        let end: SpanBound = match (end1, end2) {
            (SpanBound::Bounded(end1), SpanBound::Bounded(end2)) => SpanBound::Bounded(std::cmp::max(end1, end2)),
            (SpanBound::Bounded(_), SpanBound::Unbounded)        => SpanBound::Unbounded,
            (SpanBound::Unbounded, SpanBound::Bounded(_))        => SpanBound::Unbounded,
            (SpanBound::Unbounded, SpanBound::Unbounded)         => SpanBound::Unbounded,
        };

        // Create a new range out of it
        Self::Range(start, end)
    }



    /// Makes this SpanRange "swallow" the given one.
    /// 
    /// `self` will become a continious area that snugle spans both `self` and `other`.
    /// 
    /// Some remarks:
    /// - An unbounded side always travels (e.g., any range has an unbounded start, the result will also be unbounded start)
    /// - An empty range counts as no change (e.g., if `range1` is empty, `range2` is passed as-is; only if both are empty is empty returned)
    /// 
    /// # Arguments
    /// - `other`: The other SpanRange to consume.
    /// 
    /// # Returns
    /// A mutable reference to self for chaining.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::span::{SpanBound, SpanRange};
    /// 
    /// let mut range1 = SpanRange::new(0, 9);
    /// range1.consume(SpanRange::new(15, 30));
    /// assert_eq!(range1, SpanRange::new(0, 30));
    /// ```
    /// (See [`Self::combined()`](SpanRange::combined()) for more examples about this function's behaviour)
    #[inline]
    pub fn consume(&mut self, other: impl Into<SpanRange>) -> &mut Self {
        *self = Self::combined(*self, other);
        self
    }

    /// Slices a given string appropriately for this SpanRange.
    /// 
    /// # Arguments
    /// - `text`: The string to slice.
    /// 
    /// # Returns
    /// A slice of the given string, or else a static empty `""` if the range is... well, empty.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::span::{SpanBound, SpanRange};
    /// 
    /// let text = "Hello, world!";
    /// assert_eq!(SpanRange::new(0, 4).slice(text), "Hello");
    /// assert_eq!(SpanRange::new(6, 11).slice(text), "world");
    /// assert_eq!(SpanRange::new(12, 12).slice(text), "!");
    /// assert_eq!(SpanRange::new(0, 255).slice(text), "Hello, world!");
    /// assert_eq!(SpanRange::new(SpanBound::Unbounded, 4).slice(text), "Hello");
    /// assert_eq!(SpanRange::new(6, SpanBound::Unbounded).slice(text), "world!");
    /// assert_eq!(SpanRange::new(SpanBound::Unbounded, SpanBound::Unbounded).slice(text), "Hello, world!");
    /// assert_eq!(SpanRange::empty().slice(text), "");
    /// ```
    #[inline]
    pub fn slice<'s>(&'_ self, text: &'s str) -> &'s str {
        match self {
            Self::Range(SpanBound::Bounded(start), SpanBound::Bounded(end)) => if *start <= *end && *start < text.len() { if *end < text.len() { &text[*start..=*end] } else { &text[*start..] } } else { "" },
            Self::Range(SpanBound::Bounded(start), SpanBound::Unbounded)    => if *start < text.len() { &text[*start..] } else { "" },
            Self::Range(SpanBound::Unbounded, SpanBound::Bounded(end))      => if *end < text.len() { &text[..=*end] } else { text },
            Self::Range(SpanBound::Unbounded, SpanBound::Unbounded)         => text,
            Self::Empty                                                     => "",
        }
    }



    /// Returns if this SpanRange is a [`Range`](SpanRange::Range) (i.e., non-empty).
    /// 
    /// # Returns
    /// True if it is, or false otherwise.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::span::{SpanBound, SpanRange};
    /// 
    /// assert_eq!(SpanRange::new(0, 9).is_range(), true);
    /// assert_eq!(SpanRange::new(SpanBound::Unbounded, 9).is_range(), true);
    /// assert_eq!(SpanRange::empty().is_range(), false);
    /// assert_eq!(SpanRange::new(9, 0).is_range(), false);
    /// ```
    #[inline]
    pub fn is_range(&self) -> bool { matches!(self, Self::Range(_, _)) }
    /// Returns the internal start bound.
    /// 
    /// # Returns
    /// The start [`SpanBound`].
    /// 
    /// # Panics
    /// This function panics if we are not a [`Range`](SpanRange::Range) but empty instead.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::span::{SpanBound, SpanRange};
    /// 
    /// assert_eq!(SpanRange::new(0, 9).start(), SpanBound::Bounded(0));
    /// assert_eq!(SpanRange::new(SpanBound::Unbounded, 9).start(), SpanBound::Unbounded);
    /// assert_eq!(std::panic::catch_unwind(|| SpanRange::empty().start()).is_err());
    /// assert_eq!(std::panic::catch_unwind(|| SpanRange::new(9, 0).start()).is_err());
    /// ```
    #[inline]
    pub fn start(&self) -> SpanBound { if let Self::Range(start, _) = self { *start } else { panic!("Cannot unwrap {:?} as a SpanRange::Range", self.variant()); } }
    /// Returns the internal end bound.
    /// 
    /// # Returns
    /// The end [`SpanBound`].
    /// 
    /// # Panics
    /// This function panics if we are not a [`Range`](SpanRange::Range) but empty instead.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::span::{SpanBound, SpanRange};
    /// 
    /// assert_eq!(SpanRange::new(0, 9).end(), SpanBound::Bounded(9));
    /// assert_eq!(SpanRange::new(0, SpanBound::Unbounded).end(), SpanBound::Unbounded);
    /// assert_eq!(std::panic::catch_unwind(|| SpanRange::empty().end()).is_err());
    /// assert_eq!(std::panic::catch_unwind(|| SpanRange::new(9, 0).end()).is_err());
    /// ```
    #[inline]
    pub fn end(&self) -> SpanBound { if let Self::Range(_, end) = self { *end } else { panic!("Cannot unwrap {:?} as a SpanRange::Range", self.variant()); } }
    /// Returns the internal start- and end bounds.
    /// 
    /// # Returns
    /// A tuple with the start- and end [`SpanBound`]s, respectively.
    /// 
    /// # Panics
    /// This function panics if we are not a [`Range`](SpanRange::Range) but empty instead.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::span::{SpanBound, SpanRange};
    /// 
    /// assert_eq!(SpanRange::new(0, 9).range(), (SpanBound::Bounded(0), SpanBound::Bounded(9)));
    /// assert_eq!(SpanRange::new(SpanBound::Unbounded, 9).range(), (SpanBound::Unbounded, SpanBound::Bounded(9)));
    /// assert_eq!(std::panic::catch_unwind(|| SpanRange::empty().range()).is_err());
    /// assert_eq!(std::panic::catch_unwind(|| SpanRange::new(9, 0).range()).is_err());
    /// ```
    #[inline]
    pub fn range(&self) -> (SpanBound, SpanBound) { if let Self::Range(start, end) = self { (*start, *end) } else { panic!("Cannot unwrap {:?} as a SpanRange::Range", self.variant()); } }

    /// Returns if this SpanRange is [`Empty`](SpanRange::Empty).
    /// 
    /// # Returns
    /// True if it is, or false otherwise.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::span::{SpanBound, SpanRange};
    /// 
    /// assert_eq!(SpanRange::new(0, 9).is_empty(), false);
    /// assert_eq!(SpanRange::new(SpanBound::Unbounded, 9).is_empty(), false);
    /// assert_eq!(SpanRange::empty().is_empty(), true);
    /// assert_eq!(SpanRange::new(9, 0).is_empty(), true);
    /// ```
    #[inline]
    pub fn is_empty(&self) -> bool { matches!(self, Self::Empty) }

    /// Returns the length of the range, i.e., the "number of graphemes" it catches.
    /// 
    /// # Arguments
    /// - `length`: Something that gives us the size of the thing we're ranging over. This can be either a number, or the string itself. See [`Length`] for more details.
    /// 
    /// # Returns
    /// The number of things captured by this range.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::span::{SpanBound, SpanRange};
    /// 
    /// let text = "Hello, world!";
    /// assert_eq!(SpanRange::new(0, 4).slice(text.len()), 5);
    /// assert_eq!(SpanRange::new(6, 11).slice(text), 5);
    /// assert_eq!(SpanRange::new(12, 12).slice(text.len()), 1);
    /// assert_eq!(SpanRange::new(0, 255).slice(text), 13);
    /// assert_eq!(SpanRange::new(SpanBound::Unbounded, 4).slice(text.len()), 5);
    /// assert_eq!(SpanRange::new(6, SpanBound::Unbounded).slice(text), 6);
    /// assert_eq!(SpanRange::new(SpanBound::Unbounded, SpanBound::Unbounded).slice(text.len()), 13);
    /// assert_eq!(SpanRange::empty().slice(text), 0);
    /// ```
    #[inline]
    pub fn len(&self, length: impl Length) -> usize {
        let length: usize = length.len();
        match self {
            Self::Range(SpanBound::Bounded(start), SpanBound::Bounded(end)) => if *start <= *end && *start < length { if *end < length { 1 + *end - *start } else { length - *start } } else { 0 },
            Self::Range(SpanBound::Bounded(start), SpanBound::Unbounded)    => if *start < length { length - *start } else { 0 },
            Self::Range(SpanBound::Unbounded, SpanBound::Bounded(end))      => if *end < length { 1 + *end } else { length },
            Self::Range(SpanBound::Unbounded, SpanBound::Unbounded)         => length,
            Self::Empty                                                     => 0,
        }
    }
}

impl<I: AsPrimitive<usize>> From<(Option<I>, Option<I>)> for SpanRange {
    #[inline]
    fn from(value: (Option<I>, Option<I>)) -> Self {
        match value {
            (Some(left), Some(right)) => Self::from(left..=right),
            (Some(left), None)        => Self::from(left..),
            (None, Some(right))       => Self::from(..=right),
            (None, None)              => Self::from(..),
        }
    }
}
impl<I: AsPrimitive<usize>> From<RangeFrom<I>> for SpanRange {
    #[inline]
    fn from(value: RangeFrom<I>) -> Self { Self::Range(SpanBound::Bounded(value.start.as_()), SpanBound::Unbounded) }
}
impl From<RangeFull> for SpanRange {
    #[inline]
    fn from(value: RangeFull) -> Self { Self::Range(SpanBound::Unbounded, SpanBound::Unbounded) }
}
impl<I: AsPrimitive<usize>> From<RangeInclusive<I>> for SpanRange {
    #[inline]
    fn from(value: RangeInclusive<I>) -> Self { Self::Range(SpanBound::Bounded(value.start().as_()), SpanBound::Bounded(value.end().as_())) }
}
impl<I: AsPrimitive<usize>> From<RangeToInclusive<I>> for SpanRange {
    #[inline]
    fn from(value: RangeToInclusive<I>) -> Self { Self::Range(SpanBound::Unbounded, SpanBound::Bounded(value.end.as_())) }
}

impl AsRef<SpanRange> for SpanRange {
    #[inline]
    fn as_ref(&self) -> &Self { self }
}
impl AsMut<SpanRange> for SpanRange {
    #[inline]
    fn as_mut(&mut self) -> &mut Self { self }
}
impl From<&SpanRange> for SpanRange {
    #[inline]
    fn from(value: &Self) -> Self { *value }
}
impl From<&mut SpanRange> for SpanRange {
    #[inline]
    fn from(value: &mut Self) -> Self { *value }
}





/***** LIBRARY *****/
/// Represents a snippet of parsed source text, which is used to link a node to a particular set of it.
/// 
/// # Generics
/// - `F`: Decides the type of the filename string embedded in [`Span`]s compatible with this diagnostic.
/// - `S`: Decides the type of the source string embedded in [`Span`]s compatible with this diagnostic.
/// 
/// # Example
/// ```rust
/// use std::borrow::Cow;
/// use std::path::PathBuf;
/// use ast_toolkit::Span;
/// 
/// // Create some strings
/// let file: String = PathBuf::from("/tmp/test").display().to_string();
/// let bytes: Cow<str> = String::from_utf8_lossy(b"Hello, world!");
/// 
/// // Build spans over them!
/// let _span = Span::new("<example>", "Hello, world!");
/// let _span = Span::new(file.as_str(), "Hello, world!");
/// let span = Span::new("<example>", bytes.as_ref());
/// 
/// // Which can then be queried to be informed over the spanned text
/// assert_eq!(span.text(), "Hello, world!");
/// assert_eq!(span.start().line, 0);
/// assert_eq!(span.start().col, 0);
/// assert_eq!(span.end().line, 0);
/// assert_eq!(span.end().col, 12);
/// ```
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Span<'f, 's> {
    /// The filename (or other description) of the file we are spanning.
    pub file   : &'f str,
    /// The entire source text to snippet.
    pub source : &'s str,
    /// The start & end position of this span, both inclusive.
    pub range  : SpanRange,
}

impl<'f, 's> Span<'f, 's> {
    /// Constructor for the Span, which will encompass the entire source.
    /// 
    /// Note that the Span will be bound to the given filename and source types, and, more importantly, to its lifetimes.
    /// 
    /// # Arguments
    /// - `file`: The filename (or other description) of the source's origin.
    /// - `source`: The entire source text that we might parse.
    /// 
    /// # Returns
    /// A new instance of Self that spans the entire source.
    /// 
    /// # Example
    /// ```rust
    /// use std::borrow::Cow;
    /// use std::path::PathBuf;
    /// use ast_toolkit::Span;
    /// 
    /// // Create some strings
    /// let file: String = PathBuf::from("/tmp/test").display().to_string();
    /// let bytes: Cow<str> = String::from_utf8_lossy(b"Hello, world!");
    /// 
    /// // Build spans over them!
    /// let _span = Span::new("<example>", "Hello, world!");
    /// let _span = Span::new(file.as_str(), "Hello, world!");
    /// let span = Span::new("<example>", bytes.as_ref());
    /// ```
    #[inline]
    pub fn new(file: impl Into<&'f str>, source: impl Into<&'s str>) -> Self {
        let source: &str = source.into();
        let source_len: usize = source.len();
        Self {
            file  : file.into(),
            source,
            range : SpanRange::unbounded(),
        }
    }

    /// Creates an empty Span.
    /// 
    /// # Returns
    /// A new instance of Self that spans nothing of the source text.
    #[inline]
    pub fn empty(file: impl Into<&'f str>, source: impl Into<&'s str>) -> Self {
        Self {
            file   : file.into(),
            source : source.into(),
            range  : SpanRange::Empty,
        }
    }

    /// Constructor for the Span that takes a custom range (in [`Position`]s) to span.
    /// 
    /// The given positions are indices over graphemes, not bytes. For example:
    /// ```rust
    /// # use ast_toolkit::{Position, Span};
    /// let span = Span::from_pos("<example>", "Hÿllo, world!", Position::new0(0, 0), Position::new0(0, 2));
    /// assert_eq!(span.text(), "Hÿl");
    /// assert_eq!(span.text().len(), 4);
    /// ```
    /// 
    /// Note that the Span will be bound to the given filename and source types, and, more importantly, to its lifetimes.
    /// 
    /// # Arguments
    /// - `file`: The filename (or other description) of the source's origin.
    /// - `source`: The entire source text that we might parse.
    /// - `start`: The start position, as [`Position`], inclusive.
    /// - `end`: The end position, as [`Position`], inclusive.
    /// 
    /// # Returns
    /// A new instance of Self that spans only the given range.
    /// 
    /// # Example
    /// ```rust
    /// use std::borrow::Cow;
    /// use std::path::PathBuf;
    /// use ast_toolkit::{Position, Span};
    /// 
    /// let file: String = PathBuf::from("/tmp/test").display().to_string();
    /// let bytes: Cow<str> = String::from_utf8_lossy(b"Hello, world!");
    /// 
    /// let span1 = Span::from_pos("<example>", "Hello, world!", Position::new0(0, 0), Position::new0(0, 4));
    /// let span2 = Span::from_pos(file.as_str(), "Hello, world!", Position::new0(0, 7), Position::new0(0, 11));
    /// let span3 = Span::from_pos("<example>", bytes.as_ref(), Position::new0(0, 0), Position::new0(0, 12));
    /// 
    /// assert_eq!(span1.text(), "Hello");
    /// assert_eq!(span2.text(), "world");
    /// assert_eq!(span3.text(), "Hello, world!");
    /// ```
    pub fn from_pos(file: impl Into<&'f str>, source: impl Into<&'s str>, start: impl Into<Position>, end: impl Into<Position>) -> Self {
        let file: &str = file.into();
        let source: &str = source.into();
        let (start, end): (Position, Position) = (start.into(), end.into());

        // Examine the source to find the end
        let (mut istart, mut iend): (Position, Position) = (start, end);
        let (mut rstart, mut rend): (Option<usize>, Option<usize>) = (None, None);
        for (i, c) in source.grapheme_indices(true) {
            // If we've reached the end of any, mark it
            if rstart.is_none() && istart.line == 0 && istart.col == 0 { rstart = Some(i); }
            if rend.is_none() && iend.line == 0 && iend.col == 0 { rend = Some(i); }

            // Otherwise, count them down
            if istart.line > 0 && c == "\n" {
                istart.line -= 1;
            } else if istart.line == 0 && istart.col > 0 {
                // If we're skipping a (non-terminating) newline, the Position is ill-formed
                istart.col -= 1;
            }
            if iend.line > 0 && c == "\n" {
                iend.line -= 1;
            } else if iend.line == 0 && iend.col > 0 {
                // If we're skipping a (non-terminating) newline, the Position is ill-formed
                iend.col -= 1;
            }
        }

        // Create self
        Self {
            file,
            source,
            range : SpanRange::from((rstart, rend)),
        }
    }

    /// Constructor for the Span that takes a custom range (in character indices) to span.
    /// 
    /// The given positions are indices over bytes, not graphemes. For example:
    /// ```rust
    /// # use ast_toolkit::{Position, Span};
    /// let span = Span::from_idx("<example>", "Hÿllo, world!", 0, 2);
    /// assert_eq!(span.text(), "Hÿ");
    /// assert_eq!(span.text().len(), 3);
    /// ```
    /// 
    /// # Arguments
    /// - `file`: The filename (or other description) of the source's origin.
    /// - `source`: The entire source text that we might parse.
    /// - `start`: The start position, as character index, inclusive.
    /// - `end`: The end position, as character index, inclusive.
    /// 
    /// # Returns
    /// A new instance of Self that spans only the given range.
    /// 
    /// # Example
    /// ```rust
    /// use std::borrow::Cow;
    /// use std::path::PathBuf;
    /// use ast_toolkit::Span;
    /// 
    /// let file: String = PathBuf::from("/tmp/test").display().to_string();
    /// let bytes: Cow<str> = String::from_utf8_lossy(b"Hello, world!");
    /// 
    /// let span1 = Span::from_idx("<example>", "Hello, world!", 0, 4);
    /// let span2 = Span::from_idx(file.as_str(), "Hello, world!", 7, 11);
    /// let span3 = Span::from_idx("<example>", bytes.as_ref(), 0, 12);
    /// let span3 = Span::from_idx("<example>", bytes.as_ref(), 1, 0);
    /// 
    /// assert_eq!(span1.text(), "Hello");
    /// assert_eq!(span2.text(), "world");
    /// assert_eq!(span3.text(), "Hello, world!");
    /// assert_eq!(span4.text(), "");
    /// ```
    #[inline]
    pub fn from_idx(file: impl Into<&'f str>, source: impl Into<&'s str>, start: impl AsPrimitive<usize>, end: impl AsPrimitive<usize>) -> Self {
        Self {
            file   : file.into(),
            source : source.into(),
            range  : SpanRange::from(start.as_()..=end.as_()),
        }
    }

    /// Constructor for the Span that takes a custom range to span.
    /// 
    /// The given positions are indices over graphemes, not bytes. For example:
    /// ```rust
    /// # use ast_toolkit::{Position, Span};
    /// let span = Span::from_range("<example>", "Hÿllo, world!", 0..=2);
    /// assert_eq!(span.text(), "Hÿl");
    /// assert_eq!(span.text().len(), 4);
    /// ```
    /// 
    /// # Arguments
    /// - `file`: The filename (or other description) of the source's origin.
    /// - `source`: The entire source text that we might parse.
    /// - `range`: The range to span. This is essentially any range with an inclusive end.
    /// 
    /// # Returns
    /// A new instance of Self that spans only the given range.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::Span;
    /// 
    /// let span1 = Span::from_range("<example>", "Hello, world!", 0..=4);
    /// let span2 = Span::from_range("<example>", "Hello, world!", ..=4);
    /// let span3 = Span::from_range("<example>", "Hello, world!", 7..);
    /// let span3 = Span::from_range("<example>", "Hello, world!", ..);
    /// 
    /// assert_eq!(span1.text(), "Hello");
    /// assert_eq!(span2.text(), "Hello");
    /// assert_eq!(span3.text(), "world!");
    /// assert_eq!(span4.text(), "Hello, world!");
    /// ```
    #[inline]
    fn from_range(file: impl Into<&'f str>, source: impl Into<&'s str>, range: impl Into<SpanRange>) -> Self {
        Self {
            file   : file.into(),
            source : source.into(),
            range  : range.into(),
        }
    }

    /// Constructor for the Span that encapsulates both ranges of the given spans.
    /// 
    /// # Arguments
    /// - `span1`: The first span to take into account.
    /// - `span2`: The second span to take into account.
    /// 
    /// # Returns
    /// A new instance of Self that spans both input spans and everything in between.
    /// 
    /// Note that, for lifetime purposes, the file and source text from the first span are referenced.
    /// 
    /// # Panics
    /// This function panics if the given spans do not have the same `file` or `source`. Note that this goes by *pointer equality*.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::Span;
    /// 
    /// let file: &str = "<example>";
    /// let text: &str = "Hello, world!";
    /// let span1 = Span::from_idx(file, text, 0, 4);
    /// let span2 = Span::from_idx(file, text, 7, 12);
    /// 
    /// assert_eq!(Span::combined(span1, span2).text(), "Hello, world!");
    /// ```
    #[inline]
    #[track_caller]
    pub fn combined<'f2, 's2>(span1: impl AsRef<Span<'f, 's>>, span2: impl AsRef<Span<'f2, 's2>>) -> Self {
        let (span1, span2): (&Span, &Span) = (span1.as_ref(), span2.as_ref());

        // Assert they talk about the same thing
        if !std::ptr::eq(span1.file as *const str, span2.file as *const str) { panic!("Given spans do not have the same `file`"); }
        if !std::ptr::eq(span1.source as *const str, span2.source as *const str) { panic!("Given spans do not have the same `source`"); }

        // Construct a new self out of it
        Self {
            file   : span1.file,
            source : span1.source,
            range  : SpanRange::combined(span1.range, span2.range),
        }
    }



    /// Will expand the range in this Span to include the given Span.
    /// 
    /// # Arguments
    /// - `other`: The other Span to consume.
    /// 
    /// # Returns
    /// A reference to self for chaining.
    /// 
    /// # Panics
    /// This function panics if the given spans do not have the same `file` or `source`. Note that this goes by *pointer equality*.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::Span;
    /// 
    /// let file: &str = "<example>";
    /// let text: &str = "Hello, world!";
    /// let mut span1 = Span::from_idx(file, text, 0, 4);
    /// let span2 = Span::from_idx(file, text, 7, 12);
    /// 
    /// span1.consume(span2);
    /// assert_eq!(span1.text(), "Hello, world!");
    /// ```
    #[track_caller]
    pub fn consume(&mut self, other: impl AsRef<Span<'f, 's>>) -> &mut Self {
        let other: &Span = other.as_ref();

        // Assert they talk about the same thing
        if !std::ptr::eq(self.file as *const str, other.file as *const str) { panic!("Given spans do not have the same `file`"); }
        if !std::ptr::eq(self.source as *const str, other.source as *const str) { panic!("Given spans do not have the same `source`"); }

        // Compute the new range and we're done
        self.range.consume(other.range);
        self
    }



    /// Converts a character index to a [`Position`] within this span's source text.
    /// 
    /// Note that the position given is an absolute index of the source text; this function ignores the spanned area.
    /// 
    /// # Arguments
    /// - `index`: The index to translate.
    /// 
    /// # Returns
    /// A new [`Position`] representing the index as a (line, column) coordinate.
    /// 
    /// # Panics
    /// This function may panic if the given index is not at the grapheme boundary.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{Position, Span};
    /// 
    /// let span1 = Span::new("<example>", "Hello\nworld!");
    /// let span2 = Span::from_idx("<example>", "Hello\nworld!", 0, 4);
    /// 
    /// assert_eq!(span1.pos_of(3), Position::new0(0, 3));
    /// assert_eq!(span1.pos_of(7), Position::new0(1, 1));
    /// assert_eq!(span2.pos_of(3), Position::new0(0, 3));
    /// assert_eq!(span2.pos_of(7), Position::new0(1, 1));
    /// ```
    /// ```should_panic
    /// # use ast_toolkit::Span;
    /// // This will panic!
    /// Span::new("<example>", "Hÿllo\nworld!").pos_of(2);
    /// ```
    pub fn pos_of(&self, index: impl AsPrimitive<usize>) -> Position {
        let index: usize = index.as_();

        // Assert it is correctly sized
        if index >= self.source.len() { panic!("Given index '{}' is out-of-bounds for Span of length {}", index, self.source.len()); }

        // Iterate over the source to find the line & column
        let (mut line, mut col): (usize, usize) = (0, 0);
        for (i, c) in self.source.grapheme_indices(true) {
            // If we reached it, we done
            if i == index { break; }
            else if i > index { panic!("Index {} does not point to grapheme boundary", index); }

            // Otherwise, count
            if c == "\n" { line += 1; col = 0; }
            else { col += 1; }
        }

        // Done, return it as a position
        Position::new0(line, col)
    }

    /// Returns the start position of this span as a [`Position`].
    /// 
    /// # Returns
    /// A [`Position`] describing the start position in the source text, or [`None`] if we are empty.
    /// 
    /// # Panics
    /// This function may panic if `start` does not point at the unicode grapheme boundary.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{Position, Span};
    /// 
    /// let span1 = Span::new("<example>", "Hello\nworld!");
    /// let span2 = Span::from_idx("<example>", "Hello\nworld!", 2, 2);
    /// let span3 = Span::from_idx("<example>", "Hello\nworld!", 6, 10);
    /// let span4 = Span::from_idx("<example>", "Hello\nworld!", 1, 0);
    /// 
    /// assert_eq!(span1.start(), Some(Position::new0(0, 0)));
    /// assert_eq!(span2.start(), Some(Position::new0(0, 2)));
    /// assert_eq!(span3.start(), None);
    /// ```
    /// ```should_panic
    /// # use ast_toolkit::Span;
    /// // This will panic!
    /// Span::from_idx("<example>", "Hÿllo\nworld!", 2, 6).start();
    /// ```
    #[inline]
    #[track_caller]
    pub fn start(&self) -> Option<Position> {
        match self.range {
            SpanRange::Range(start, _) => if let SpanBound::Bounded(start) = start { Some(self.pos_of(start)) } else if self.source.len() > 0 { Some(self.pos_of(0)) } else { None },
            _ => None,
        }
    }

    /// Returns the end position of this span as a [`Position`].
    /// 
    /// # Returns
    /// A [`Position`] describing the end position in the source text, or [`None`] if we are empty.
    /// 
    /// # Panics
    /// This function may panic if the internal `start`- or `end`-fields are not within bounds of the internal `source`, or if `start > end`. It may also panic if `end` does not point at the unicode grapheme boundary.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{Position, Span};
    /// 
    /// let span1 = Span::new("<example>", "Hello world!");
    /// let span2 = Span::new("<example>", "Hello\nworld!");
    /// let span3 = Span::from_idx("<example>", "Hello\nworld!", 2, 2);
    /// let span4 = Span::from_idx("<example>", "Hello\nworld!", 6, 10);
    /// let span5 = Span::from_idx("<example>", "Hello\nworld!", 1, 0);
    /// 
    /// assert_eq!(span1.end(), Some(Position::new0(0, 11)));
    /// assert_eq!(span2.end(), Some(Position::new0(1, 5)));
    /// assert_eq!(span3.end(), Some(Position::new0(0, 2)));
    /// assert_eq!(span4.end(), Some(Position::new0(1, 4)));
    /// assert_eq!(span5.end(), None);
    /// ```
    /// ```should_panic
    /// # use ast_toolkit::Span;
    /// // This will panic!
    /// Span::from_idx("<example>", "Hÿllo\nworld!", 2, 6).end();
    /// ```
    #[inline]
    #[track_caller]
    pub fn end(&self) -> Option<Position> {
        match self.range {
            SpanRange::Range(_, end) => if let SpanBound::Bounded(end) = end { Some(self.pos_of(end)) } else if self.source.len() > 0 { Some(self.pos_of(self.source.len() - 1)) } else { None },
            _ => None,
        }
    }

    /// Returns the text referred by this span.
    /// 
    /// # Returns
    /// A [`str`] referring to the source text we span.
    /// 
    /// # Panics
    /// This function may panic if the internal `start`- or `end`-fields are not within bounds of the internal `source`, or if `start > end`.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::Span;
    /// 
    /// assert_eq!(Span::new("<example>", "Hello, world!").text(), "Hello, world!");
    /// assert_eq!(Span::from_idx("<example>", "Hello, world!", 0, 4).text(), "Hello");
    /// assert_eq!(Span::from_idx("<example>", "Hello, world!", 7, 12).text(), "world!");
    /// assert_eq!(Span::from_idx("<example>", "Hello, world!", 5, 5).text(), ",");
    /// assert_eq!(Span::from_idx("<example>", "Hello, world!", 6, 5).text(), "");
    /// ```
    #[inline]
    #[track_caller]
    pub fn text(&self) -> &str { self.range.slice(self.source) }



    /// Returns the number of logical units that this Span spans.
    /// 
    /// # Returns
    /// The number of characters.
    #[inline]
    pub fn len(&self) -> usize { self.range.len(self.source) }
}

impl<'f, 's> AsRef<Span<'f, 's>> for Span<'f, 's> {
    #[inline]
    fn as_ref(&self) -> &Self { self }
}
impl<'f, 's> AsMut<Span<'f, 's>> for Span<'f, 's> {
    #[inline]
    fn as_mut(&mut self) -> &mut Self { self }
}
impl<'f, 's> From<&Span<'f, 's>> for Span<'f, 's> {
    #[inline]
    fn from(value: &Span<'f, 's>) -> Self { value.clone() }
}
impl<'f, 's> From<&mut Span<'f, 's>> for Span<'f, 's> {
    #[inline]
    fn from(value: &mut Span<'f, 's>) -> Self { value.clone() }
}

impl<'f, 's> AsRef<str> for Span<'f, 's> {
    #[inline]
    fn as_ref(&self) -> &str { self.text() }
}


// nom-related things
#[cfg(feature = "nom")]
impl<'f, 's> nom::AsBytes for Span<'f, 's> {
    #[inline]
    fn as_bytes(&self) -> &[u8] { self.range.slice(self.source).as_bytes() }
}
#[cfg(feature = "nom")]
impl<'f, 's, S: AsRef<str>> nom::Compare<S> for Span<'f, 's> {
    #[inline]
    fn compare(&self, t: S) -> nom::CompareResult {
        let s: &str = self.range.slice(self.source);
        let t: &str = t.as_ref();

        // Compare string-wise
        let mut ss = s.graphemes(true);
        for tc in t.graphemes(true) {
            let sc: &str = if let Some(sc) = ss.next() { sc } else { return nom::CompareResult::Incomplete; };
            if sc != tc { return nom::CompareResult::Error; }
        }
        nom::CompareResult::Ok
    }

    #[inline]
    fn compare_no_case(&self, t: S) -> nom::CompareResult {
        let s: &str = self.range.slice(self.source);
        let t: &str = t.as_ref();

        // Compare string-wise
        let mut ss = s.graphemes(true);
        for tc in t.graphemes(true) {
            let sc: &str = if let Some(sc) = ss.next() { sc } else { return nom::CompareResult::Incomplete; };

            // Equalize the case
            let sc: String = sc.to_lowercase();
            let tc: String = tc.to_lowercase();

            // Compare now
            if sc != tc { return nom::CompareResult::Error; }
        }
        nom::CompareResult::Ok
    }
}
#[cfg(feature = "nom")]
impl<'f, 's> nom::ExtendInto for Span<'f, 's> {
    type Item = Self;
    type Extender = Self;

    #[inline]
    fn new_builder(&self) -> Self::Extender { *self }
    #[inline]
    fn extend_into(&self, acc: &mut Self::Extender) { acc.consume(self); }
}
#[cfg(feature = "nom")]
impl<'f, 's> nom::FindToken<u8> for Span<'f, 's> {
    #[track_caller]
    fn find_token(&self, token: u8) -> bool {
        for b in self.range.slice(self.source).bytes() {
            if b == token { return true; }
        }
        false
    }
}
#[cfg(feature = "nom")]
impl<'f, 's> nom::FindToken<char> for Span<'f, 's> {
    #[track_caller]
    fn find_token(&self, token: char) -> bool {
        for c in self.range.slice(self.source).chars() {
            if c == token { return true; }
        }
        false
    }
}
#[cfg(feature = "nom")]
impl<'f, 's, 's2> nom::FindToken<&'s2 str> for Span<'f, 's> {
    #[track_caller]
    fn find_token(&self, token: &'s2 str) -> bool {
        for c in self.range.slice(self.source).graphemes(true) {
            if c == token { return true; }
        }
        false
    }
}
#[cfg(feature = "nom")]
impl<'f, 's, T: AsRef<str>> nom::FindSubstring<T> for Span<'f, 's> {
    /// NOTE: We return the full index in the source so that it's compatible as idx for Spans.
    fn find_substring(&self, substr: T) -> Option<usize> {
        let source: &str = self.range.slice(self.source);
        let substr: &str = substr.as_ref();
        for (i, _) in source.grapheme_indices(true) {
            if i + substr.len() <= source.len() && &source[i..i + substr.len()] == substr {
                // NOTE: We can safely unwrap because this loops never loops if the string is empty.
                return Some(self.range.start().bound() + i);
            }
        }
        None
    }
}
#[cfg(feature = "nom")]
impl<'f, 's> nom::InputIter for Span<'f, 's> {
    type Item = u8;
    type Iter = std::iter::Enumerate<std::vec::IntoIter<u8>>;
    type IterElem = std::vec::IntoIter<u8>;

    #[inline]
    fn iter_indices(&self) -> Self::Iter { self.text().as_bytes().to_vec().into_iter().enumerate() }
    fn iter_elements(&self) -> Self::IterElem { self.text().as_bytes().to_vec().into_iter() }

    #[inline]
    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        <Span as nom::InputIter>::iter_indices(self).find_map(|(i, b)| if predicate(b) { Some(i) } else { None })
    }
    fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
        use std::num::NonZeroUsize;

        match <Span as nom::InputIter>::iter_indices(self).nth(count) {
            Some((i, _)) => Ok(i),
            None => Err(match NonZeroUsize::new(self.len() - 1 - count) {
                Some(res) => nom::Needed::Size(res),
                None => nom::Needed::Unknown,
            }),
        }
    }
}
#[cfg(feature = "nom")]
impl<'f, 's> nom::InputLength for Span<'f, 's> {
    #[track_caller]
    fn input_len(&self) -> usize { self.range.len(self.source.len()) }
}
#[cfg(feature = "nom")]
impl<'f, 's> nom::InputTake for Span<'f, 's> {
    #[inline]
    #[track_caller]
    fn take(&self, count: usize) -> Self {
        if count > 0 {
            Self {
                file   : self.file,
                source : self.source,
                range  : SpanRange::from(..=count - 1),
            }
        } else {
            Self {
                file   : self.file,
                source : self.source,
                range  : SpanRange::Empty,
            }
        }
    }

    #[inline]
    #[track_caller]
    fn take_split(&self, count: usize) -> (Self, Self) {
        if count > self.range.len(self.source.len()) { panic!("Given count {} is out-of-range for Span of size {}", count, self.range.len(self.source.len())); }
        (
            self.take(count),
            Self {
                file   : self.file,
                source : self.source,
                range  : SpanRange::from(count..),
            }
        )
    }
}
#[cfg(feature = "nom")]
impl<'f, 's> nom::InputTakeAtPosition for Span<'f, 's> {
    type Item = char;

    fn split_at_position<P, E: nom::error::ParseError<Self>>(&self, predicate: P) -> nom::IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        let mut chars = self.range.slice(self.source).char_indices().peekable();
        while let Some((i, c)) = chars.next() {
            // Check if this is the character the user is looking for
            if predicate(c) {
                // It is; so perform the split at this location
                return Ok((
                    // We create a new span if it's non-empty
                    if i > 0 {
                        // NOTE: We can safely call `.start()` because this loop is only ever run if the range is non-empty.
                        Span { file: self.file, source: self.source, range: SpanRange::Range(self.range.start(), SpanBound::Bounded(i - 1)) }
                    } else {
                        Span { file: self.file, source: self.source, range: SpanRange::Empty }
                    },
                    // This one will never be empty because the c is part of the split
                    // NOTE: We can safely call `.end()` because this loop is only ever run if the range is non-empty.
                    Span { file: self.file, source: self.source, range: SpanRange::Range(SpanBound::Bounded(i), self.range.end()) },
                ));
            }
        }
        Err(nom::Err::Incomplete(nom::Needed::Unknown))
    }
    fn split_at_position1<P, E: nom::error::ParseError<Self>>(&self, predicate: P, e: nom::error::ErrorKind) -> nom::IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        let mut chars = self.range.slice(self.source).char_indices().peekable();
        while let Some((i, c)) = chars.next() {
            // Check if this is the character the user is looking for
            if predicate(c) {
                // It is; so perform the split at this location
                return Ok((
                    // We create a new span if it's non-empty
                    if i > 0 {
                        // NOTE: We can safely call `.start()` because this loop is only ever run if the range is non-empty.
                        Span { file: self.file, source: self.source, range: SpanRange::Range(self.range.start(), SpanBound::Bounded(i - 1)) }
                    } else {
                        // We don't want no empty slices! Instead, fail
                        return Err(nom::Err::Failure(E::from_error_kind(*self, e)));
                    },
                    // This one will never be empty because the c is part of the split
                    // NOTE: We can safely call `.end()` because this loop is only ever run if the range is non-empty.
                    Span { file: self.file, source: self.source, range: SpanRange::Range(SpanBound::Bounded(i), self.range.end()) },
                ));
            }
        }
        Err(nom::Err::Incomplete(nom::Needed::Unknown))
    }

    fn split_at_position1_complete<P, E: nom::error::ParseError<Self>>(&self, predicate: P, e: nom::error::ErrorKind) -> nom::IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        let mut chars = self.range.slice(self.source).char_indices().peekable();
        while let Some((i, c)) = chars.next() {
            // Check if this is the character the user is looking for
            if predicate(c) {
                // It is; so perform the split at this location
                return Ok((
                    // We create a new span if it's non-empty
                    if i > 0 {
                        // NOTE: We can safely call `.start()` because this loop is only ever run if the range is non-empty.
                        Span { file: self.file, source: self.source, range: SpanRange::Range(self.range.start(), SpanBound::Bounded(i - 1)) }
                    } else {
                        // We don't want no empty slices! Instead, fail
                        return Err(nom::Err::Failure(E::from_error_kind(*self, e)));
                    },
                    // This one will never be empty because the c is part of the split
                    // NOTE: We can safely call `.end()` because this loop is only ever run if the range is non-empty.
                    Span { file: self.file, source: self.source, range: SpanRange::Range(SpanBound::Bounded(i), self.range.end()) },
                ));
            }
        }

        // Instead of crashing, return the full slice
        Ok((*self, Span::empty(self.file, self.source)))
    }
    fn split_at_position_complete<P, E: nom::error::ParseError<Self>>(&self, predicate: P) -> nom::IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        let mut chars = self.range.slice(self.source).char_indices().peekable();
        while let Some((i, c)) = chars.next() {
            // Check if this is the character the user is looking for
            if predicate(c) {
                // It is; so perform the split at this location
                return Ok((
                    // We create a new span if it's non-empty
                    if i > 0 {
                        // NOTE: We can safely call `.start()` because this loop is only ever run if the range is non-empty.
                        Span { file: self.file, source: self.source, range: SpanRange::Range(self.range.start(), SpanBound::Bounded(i - 1)) }
                    } else {
                        Span { file: self.file, source: self.source, range: SpanRange::Empty }
                    },
                    // This one will never be empty because the c is part of the split
                    // NOTE: We can safely call `.end()` because this loop is only ever run if the range is non-empty.
                    Span { file: self.file, source: self.source, range: SpanRange::Range(SpanBound::Bounded(i), self.range.end()) },
                ));
            }
        }

        // Instead of crashing, return the full slice
        Ok((*self, Span::empty(self.file, self.source)))
    }
}
