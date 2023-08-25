//  SPAN.rs
//    by Lut99
// 
//  Created:
//    02 Jul 2023, 16:40:44
//  Last edited:
//    25 Aug 2023, 22:45:26
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines the [`Span`] (and [`Position`]) structs which we use to keep
//!   track of a node's position in the source text.
// 

use std::slice::SliceIndex;
use std::ops::{Bound, Range, RangeBounds, RangeFrom, RangeFull, RangeInclusive, RangeTo, RangeToInclusive};

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
        assert_eq!(<Span as nom::InputTake>::take(&Span::from_idx("<example>", "Example text", 8, 11), 0), Span::empty("<example>", "Example text"));
        assert!(std::panic::catch_unwind(|| <Span as nom::InputTake>::take(&Span::from_idx("<example>", "Example text", 8, 11), 5)).is_err());

        // Now compare the split
        assert_eq!(<Span as nom::InputTake>::take_split(&Span::new("<example>", "Example text"), 7), (Span::from_idx("<example>", "Example text", 0, 6), Span::from_idx("<example>", "Example text", 7, 11)));
        assert_eq!(<Span as nom::InputTake>::take_split(&Span::from_idx("<example>", "Example text", 8, 11), 3), (Span::from_idx("<example>", "Example text", 8, 10), Span::from_idx("<example>", "Example text", 11, 11)));
        assert_eq!(<Span as nom::InputTake>::take_split(&Span::from_idx("<example>", "Example text", 8, 11), 0), (Span::empty("<example>", "Example text"), Span::from_idx("<example>", "Example text", 8, 11)));
        assert!(std::panic::catch_unwind(|| <Span as nom::InputTake>::take_split(&Span::from_idx("<example>", "Example text", 8, 11), 5)).is_err());
    }
    #[test]
    fn test_span_nom_input_take_at_position() {
        use nom::InputTakeAtPosition as _;
        use nom::error::{ErrorKind, ParseError as _, VerboseError};
        type I<'f, 's> = (Span<'f, 's>, Span<'f, 's>);
        type E<'f, 's> = nom::Err<VerboseError<Span<'f, 's>>>;

        // Attempt to split at some points
        let span = Span::new("<example>", "Example text!");
        assert_eq!(span.split_at_position(|c| c == 'E'), Ok::<I, E>((Span::empty("<example>", "Example text!"), Span::new("<example>", "Example text!"))));
        assert_eq!(span.split_at_position(|c| c == 't'), Ok::<I, E>((Span::from_range("<example>", "Example text!", ..=7), Span::from_range("<example>", "Example text!", 8..))));
        assert_eq!(span.split_at_position(|c| c == '!'), Ok::<I, E>((Span::from_range("<example>", "Example text!", ..=11), Span::from_range("<example>", "Example text!", 12..))));
        assert_eq!(span.split_at_position(|c| c == '_'), Err(nom::Err::<VerboseError<Span>>::Incomplete(nom::Needed::Unknown)));

        // Do it again with a different function
        assert_eq!(span.split_at_position1(|c| c == 'E', ErrorKind::Many0), Err(nom::Err::Failure(VerboseError::from_error_kind(span, ErrorKind::Many0))));
        assert_eq!(span.split_at_position1(|c| c == 't', ErrorKind::Many0), Ok::<I, E>((Span::from_range("<example>", "Example text!", ..=7), Span::from_range("<example>", "Example text!", 8..))));
        assert_eq!(span.split_at_position1(|c| c == '!', ErrorKind::Many0), Ok::<I, E>((Span::from_range("<example>", "Example text!", ..=11), Span::from_range("<example>", "Example text!", 12..))));
        assert_eq!(span.split_at_position1(|c| c == '_', ErrorKind::Many0), Err(nom::Err::<VerboseError<Span>>::Incomplete(nom::Needed::Unknown)));

        // And complete counterpart 1
        assert_eq!(span.split_at_position1_complete(|c| c == 'E', ErrorKind::Many0), Err(nom::Err::Failure(VerboseError::from_error_kind(span, ErrorKind::Many0))));
        assert_eq!(span.split_at_position1_complete(|c| c == 't', ErrorKind::Many0), Ok::<I, E>((Span::from_range("<example>", "Example text!", ..=7), Span::from_range("<example>", "Example text!", 8..))));
        assert_eq!(span.split_at_position1_complete(|c| c == '!', ErrorKind::Many0), Ok::<I, E>((Span::from_range("<example>", "Example text!", ..=11), Span::from_range("<example>", "Example text!", 12..))));
        assert_eq!(span.split_at_position1_complete(|c| c == '_', ErrorKind::Many0), Ok::<I, E>((span, Span::empty("<example>", "Example text!"))));

        // Finally, complete counterpart lenient.
        assert_eq!(span.split_at_position_complete(|c| c == 'E'), Ok::<I, E>((Span::empty("<example>", "Example text!"), Span::new("<example>", "Example text!"))));
        assert_eq!(span.split_at_position_complete(|c| c == 't'), Ok::<I, E>((Span::from_range("<example>", "Example text!", ..=7), Span::from_range("<example>", "Example text!", 8..))));
        assert_eq!(span.split_at_position_complete(|c| c == '!'), Ok::<I, E>((Span::from_range("<example>", "Example text!", ..=11), Span::from_range("<example>", "Example text!", 12..))));
        assert_eq!(span.split_at_position_complete(|c| c == '_'), Ok::<I, E>((span, Span::empty("<example>", "Example text!"))));
    }
}





/***** HELPER FUNCTIONS *****/
/// Resolves a [`SpanRange`] to a concrete set of indices.
/// 
/// # Arguments
/// - `span`: The [`SpanRange`] to resolve.
/// - `max_len`: The length of the thing is spans. This is needed to resolve unbounded.
/// 
/// # Returns
/// A tuple with the start (inclusive), end (inclusive). May be out-of-range.
fn resolve_span(span: impl Into<SpanRange>, max_len: impl AsPrimitive<usize>) -> (usize, usize) {
    let span: SpanRange = span.into();
    let max_len: usize = max_len.as_();

    // Just match the possible bounds
    match (span.start_bound(), span.end_bound()) {
        (Bound::Excluded(start), Bound::Excluded(end)) => (if *start > 0 { *start - 1 } else { 0 }, if *end > 0 { *end - 1 } else { 0 }),
        (Bound::Excluded(start), Bound::Included(end)) => (if *start > 0 { *start - 1 } else { 0 }, *end),
        (Bound::Excluded(start), Bound::Unbounded)     => (if *start > 0 { *start - 1 } else { 0 }, if max_len > 0 { max_len - 1 } else { 0 }),
        (Bound::Included(start), Bound::Excluded(end)) => (*start, if *end > 0 { *end - 1 } else { 0 }),
        (Bound::Included(start), Bound::Included(end)) => (*start, *end),
        (Bound::Included(start), Bound::Unbounded)     => (*start, if max_len > 0 { max_len - 1 } else { 0 }),
        (Bound::Unbounded, Bound::Excluded(end))       => (0, if *end > 0 { *end - 1 } else { 0 }),
        (Bound::Unbounded, Bound::Included(end))       => (0, *end),
        (Bound::Unbounded, Bound::Unbounded)           => (0, if max_len > 0 { max_len - 1 } else { 0 }),
    }
}





/***** AUXILLARY *****/
// /// Defines a start- or end bound.
// /// 
// /// A start- or end bound can be either [`Bounded`](SpanBound::Bounded) (i.e., a specific index) or [`Unbounded`](SpanBound::Unbounded) (i.e., until the end of the range in this direction).
// /// 
// /// See the [`SpanRange`] for more details on how to use this enum.
// /// ```
// #[derive(Clone, Copy, Debug, EnumDebug, Eq, Hash, PartialEq)]
// pub enum SpanBound {
//     /// There is a specific index to which this bound runs. The index is always zero-indexed, inclusive.
//     Bounded(usize),
//     /// This bound runs for the remainder of the spanned text.
//     Unbounded,
// }

// impl SpanBound {
//     /// Returns whether this SpanBound is [`Bounded`](SpanBound::Bounded).
//     /// 
//     /// # Returns
//     /// True if it is, false if it is another variant.
//     /// 
//     /// # Example
//     /// ```rust
//     /// use ast_toolkit::span::SpanBound;
//     /// 
//     /// assert_eq!(SpanBound::Bounded(42).is_bounded(), true);
//     /// assert_eq!(SpanBound::Unbounded.is_bounded(), false);
//     /// ```
//     #[inline]
//     pub fn is_bounded(&self) -> bool { matches!(self, Self::Bounded(_)) }
//     /// Returns the internal bounded index.
//     /// 
//     /// # Returns
//     /// The bounded index in this SpanBound.
//     /// 
//     /// # Panics
//     /// This function panics if the SpanBound is not [`Bounded`](SpanBound::Bounded).
//     /// 
//     /// # Example
//     /// ```rust
//     /// use ast_toolkit::span::SpanBound;
//     /// 
//     /// assert_eq!(SpanBound::Bounded(42).bound(), 42);
//     /// assert!(std::panic::catch_unwind(|| SpanBound::Unbounded.bound()).is_err());
//     /// ```
//     #[inline]
//     #[track_caller]
//     pub fn bound(&self) -> usize { if let Self::Bounded(bound) = self { *bound } else { panic!("Cannot unwrap a {:?} as a SpanBound::Bounded", self.variant()); } }

//     /// Returns whether this SpanBound is [`Unbounded`](SpanBound::Unbounded).
//     /// 
//     /// # Returns
//     /// True if it is, false if it is another variant.
//     /// 
//     /// # Example
//     /// ```rust
//     /// use ast_toolkit::span::SpanBound;
//     /// 
//     /// assert_eq!(SpanBound::Bounded(42).is_unbounded(), false);
//     /// assert_eq!(SpanBound::Unbounded.is_unbounded(), true);
//     /// ```
//     #[inline]
//     pub fn is_unbounded(&self) -> bool { matches!(self, Self::Unbounded) }
// }

// impl<N: AsPrimitive<usize>> From<N> for SpanBound {
//     #[inline]
//     fn from(value: N) -> Self { Self::Bounded(value.as_()) }
// }

// impl AsRef<SpanBound> for SpanBound {
//     #[inline]
//     fn as_ref(&self) -> &Self { self }
// }
// impl AsMut<SpanBound> for SpanBound {
//     #[inline]
//     fn as_mut(&mut self) -> &mut Self { self }
// }
// impl From<&SpanBound> for SpanBound {
//     #[inline]
//     fn from(value: &Self) -> Self { *value }
// }
// impl From<&mut SpanBound> for SpanBound {
//     #[inline]
//     fn from(value: &mut Self) -> Self { *value }
// }



// /// Properly defines the possible ranges that a [`Span`] can span over.
// /// 
// /// This takes into account the fact that spans might be unbounded _or_ empty.
// /// 
// /// # Example
// /// ```rust
// /// /* TODO */
// /// ```
// #[derive(Clone, Copy, Debug, EnumDebug, Eq, Hash, PartialEq)]
// pub enum SpanRange {
//     /// There is some range to span over, given as a (start, end)-tuple.
//     Range(SpanBound, SpanBound),
//     /// The range is empty, but it can still have a particular position.
//     Empty(Option<usize>),
// }

// impl SpanRange {
//     /// Constructor for a SpanRange.
//     /// 
//     /// # Arguments
//     /// - `start`: The start [`SpanBound`].
//     /// - `end`: The end [`SpanBound`].
//     /// 
//     /// # Returns
//     /// A new SpanRange with the given start and end ranges. If they are both [`Bounded`](SpanBound::Bounded) and `start` > `end`, then the range is empty.
//     /// 
//     /// # Example
//     /// ```rust
//     /// use ast_toolkit::span::{SpanBound, SpanRange};
//     /// 
//     /// let range = SpanRange::new(SpanBound::Bounded(0), SpanBound::Unbounded);
//     /// assert_eq!(range, SpanRange::Range(SpanBound::Bounded(0), SpanBound::Unbounded));
//     /// 
//     /// let range = SpanRange::new(0, 9);
//     /// assert_eq!(range, SpanRange::Range(SpanBound::Bounded(0), SpanBound::Bounded(9)));
//     /// 
//     /// let range = SpanRange::new(SpanBound::Bounded(1), SpanBound::Bounded(0));
//     /// assert_eq!(range, SpanRange::Empty);
//     /// ```
//     #[inline]
//     pub fn new(start: impl Into<SpanBound>, end: impl Into<SpanBound>) -> Self {
//         let (start, end): (SpanBound, SpanBound) = (start.into(), end.into());
//         match (start, end) {
//             (SpanBound::Bounded(start), SpanBound::Bounded(end)) => if start <= end { Self::Range(SpanBound::Bounded(start), SpanBound::Bounded(end)) } else { Self::Empty(Some(start)) },
//             (start, end) => Self::Range(start, end),
//         }
//     }

//     /// Constructor for a SpanRange that assumes the start is unbounded.
//     /// 
//     /// # Arguments
//     /// - `end`: The end [`SpanBound`].
//     /// 
//     /// # Returns
//     /// A new SpanRange with the start unbounded and the given end range.
//     /// 
//     /// # Example
//     /// ```rust
//     /// use ast_toolkit::span::{SpanBound, SpanRange};
//     /// 
//     /// assert_eq!(SpanRange::start_unbounded(9), SpanRange::Range(SpanBound::Unbounded, SpanBound::Bounded(9)));
//     /// assert_eq!(SpanRange::start_unbounded(SpanBound::Unbounded), SpanRange::Range(SpanBound::Unbounded, SpanBound::Unbounded));
//     /// ```
//     #[inline]
//     pub fn start_unbounded(end: impl Into<SpanBound>) -> Self { Self::Range(SpanBound::Unbounded, end.into()) }

//     /// Constructor for a SpanRange that assumes the end is unbounded.
//     /// 
//     /// # Arguments
//     /// - `start`: The start [`SpanBound`].
//     /// 
//     /// # Returns
//     /// A new SpanRange with the given start and unbounded end.
//     /// 
//     /// # Example
//     /// ```rust
//     /// use ast_toolkit::span::{SpanBound, SpanRange};
//     /// 
//     /// assert_eq!(SpanRange::end_unbounded(9), SpanRange::Range(SpanBound::Bounded(9), SpanBound::Unbounded));
//     /// assert_eq!(SpanRange::end_unbounded(SpanBound::Unbounded), SpanRange::Range(SpanBound::Unbounded, SpanBound::Unbounded));
//     /// ```
//     #[inline]
//     pub fn end_unbounded(start: impl Into<SpanBound>) -> Self { Self::Range(start.into(), SpanBound::Unbounded) }

//     /// Constructor for a SpanRange that assumes both the start and end are unbounded.
//     /// 
//     /// # Returns
//     /// A new SpanRange with an unbounded start and end.
//     /// 
//     /// # Example
//     /// ```rust
//     /// use ast_toolkit::span::{SpanBound, SpanRange};
//     /// 
//     /// assert_eq!(SpanRange::unbounded(), SpanRange::Range(SpanBound::Unbounded, SpanBound::Unbounded));
//     /// ```
//     #[inline]
//     pub fn unbounded() -> Self { Self::Range(SpanBound::Unbounded, SpanBound::Unbounded) }

//     /// Constructor for a SpanRange that initializes it as empty.
//     /// 
//     /// # Returns
//     /// A new SpanRange that is initialized as empty.
//     /// 
//     /// # Example
//     /// ```rust
//     /// use ast_toolkit::span::{SpanBound, SpanRange};
//     /// 
//     /// assert_eq!(SpanRange::empty(), SpanRange::Empty(None));
//     /// ```
//     #[inline]
//     pub fn empty() -> Self { Self::Empty(None) }
//     /// Constructor for a SpanRange that initializes it as empty, but with a position.
//     /// 
//     /// # Returns
//     /// A new SpanRange that is initialized as empty, but with a set start index.
//     /// 
//     /// # Example
//     /// ```rust
//     /// use ast_toolkit::span::{SpanBound, SpanRange};
//     /// 
//     /// assert_eq!(SpanRange::empty_with_idx(42), SpanRange::Empty(Some(42)));
//     /// ```
//     #[inline]
//     pub fn empty_with_idx(start: impl AsPrimitive<usize>) -> Self { Self::Empty(Some(start.as_())) }

//     /// Constructor for a SpanRange that encapsulates the given two ranges.
//     /// 
//     /// Some remarks:
//     /// - An unbounded side always travels (e.g., any range has an unbounded start, the result will also be unbounded start)
//     /// - An empty range counts as no change (e.g., if `range1` is empty, `range2` is passed as-is; only if both are empty is empty returned)
//     /// 
//     /// # Arguments
//     /// - `range1`: The first SpanRange to span over.
//     /// - `range2`: The second SpanRange to span over.
//     /// 
//     /// # Returns
//     /// A new instance of Self which contains a continious area that snugly spans both `range1` and `range2`.
//     /// 
//     /// # Example
//     /// ```rust
//     /// use ast_toolkit::span::{SpanBound, SpanRange};
//     /// 
//     /// let range1 = SpanRange::new(0, 9);
//     /// let range2 = SpanRange::new(15, 30);
//     /// let range3 = SpanRange::new(SpanBound::Unbounded, 9);
//     /// let range4 = SpanRange::new(9, SpanBound::Unbounded);
//     /// let range5 = SpanRange::empty();
//     /// 
//     /// // If everything is bounded, then the range snugly fits the result (communitatively).
//     /// assert_eq!(SpanRange::combined(range1, range2), SpanRange::new(0, 30));
//     /// assert_eq!(SpanRange::combined(range2, range1), SpanRange::new(0, 30));
//     /// 
//     /// // If either side is unbounded, it propagates
//     /// assert_eq!(SpanRange::combined(range1, range3), SpanRange::new(SpanBound::Unbounded, 9));
//     /// assert_eq!(SpanRange::combined(range1, range4), SpanRange::new(0, SpanBound::Unbounded));
//     /// assert_eq!(SpanRange::combined(range3, range4), SpanRange::new(SpanBound::Unbounded, SpanBound::Unbounded));
//     /// 
//     /// // Finally, empty does not do anything
//     /// assert_eq!(SpanRange::combined(range5, range1), range1);
//     /// assert_eq!(SpanRange::combined(range3, range5), range3);
//     /// assert_eq!(SpanRange::combined(range5, range5), SpanRange::empty());
//     /// ```
//     pub fn combined(range1: impl Into<SpanRange>, range2: impl Into<SpanRange>) -> Self {
//         let (range1, range2): (SpanRange, SpanRange) = (range1.into(), range2.into());

//         // Unpack the ranges and process any empty ranges
//         let (start1, end1, start2, end2): (SpanBound, SpanBound, SpanBound, SpanBound) = match (range1, range2) {
//             (Self::Range(start1, end1), Self::Range(start2, end2)) => (start1, end1, start2, end2),
//             (range1, Self::Empty(_))                               => { return range1; },
//             (Self::Empty(_), range2)                               => { return range2; },
//         };

//         // Match a new start range
//         let start: SpanBound = match (start1, start2) {
//             (SpanBound::Bounded(start1), SpanBound::Bounded(start2)) => SpanBound::Bounded(std::cmp::min(start1, start2)),
//             (SpanBound::Bounded(_), SpanBound::Unbounded)            => SpanBound::Unbounded,
//             (SpanBound::Unbounded, SpanBound::Bounded(_))            => SpanBound::Unbounded,
//             (SpanBound::Unbounded, SpanBound::Unbounded)             => SpanBound::Unbounded,
//         };
//         // Match a new end range
//         let end: SpanBound = match (end1, end2) {
//             (SpanBound::Bounded(end1), SpanBound::Bounded(end2)) => SpanBound::Bounded(std::cmp::max(end1, end2)),
//             (SpanBound::Bounded(_), SpanBound::Unbounded)        => SpanBound::Unbounded,
//             (SpanBound::Unbounded, SpanBound::Bounded(_))        => SpanBound::Unbounded,
//             (SpanBound::Unbounded, SpanBound::Unbounded)         => SpanBound::Unbounded,
//         };

//         // Create a new range out of it
//         match (start, end) {
//             (SpanBound::Bounded(start), SpanBound::Bounded(end)) => if start <= end { Self::Range(SpanBound::Bounded(start), SpanBound::Bounded(end)) } else { Self::Empty(Some(start)) },
//             (start, end) => Self::Range(start, end),
//         }
//     }



//     /// Makes this SpanRange "swallow" the given one.
//     /// 
//     /// `self` will become a continious area that snugle spans both `self` and `other`.
//     /// 
//     /// Some remarks:
//     /// - An unbounded side always travels (e.g., any range has an unbounded start, the result will also be unbounded start)
//     /// - An empty range counts as no change (e.g., if `range1` is empty, `range2` is passed as-is; only if both are empty is empty returned)
//     /// 
//     /// # Arguments
//     /// - `other`: The other SpanRange to consume.
//     /// 
//     /// # Returns
//     /// A mutable reference to self for chaining.
//     /// 
//     /// # Example
//     /// ```rust
//     /// use ast_toolkit::span::{SpanBound, SpanRange};
//     /// 
//     /// let mut range1 = SpanRange::new(0, 9);
//     /// range1.consume(SpanRange::new(15, 30));
//     /// assert_eq!(range1, SpanRange::new(0, 30));
//     /// ```
//     /// (See [`Self::combined()`](SpanRange::combined()) for more examples about this function's behaviour)
//     #[inline]
//     pub fn consume(&mut self, other: impl Into<SpanRange>) -> &mut Self {
//         *self = Self::combined(*self, other);
//         self
//     }

//     /// Slices a given string appropriately for this SpanRange.
//     /// 
//     /// # Arguments
//     /// - `text`: The string to slice.
//     /// 
//     /// # Returns
//     /// A slice of the given string, or else a static empty `""` if the range is... well, empty.
//     /// 
//     /// # Example
//     /// ```rust
//     /// use ast_toolkit::span::{SpanBound, SpanRange};
//     /// 
//     /// let text = "Hello, world!";
//     /// assert_eq!(SpanRange::new(0, 4).slice(text), "Hello");
//     /// assert_eq!(SpanRange::new(7, 11).slice(text), "world");
//     /// assert_eq!(SpanRange::new(12, 12).slice(text), "!");
//     /// assert_eq!(SpanRange::new(0, 255).slice(text), "Hello, world!");
//     /// assert_eq!(SpanRange::new(SpanBound::Unbounded, 4).slice(text), "Hello");
//     /// assert_eq!(SpanRange::new(7, SpanBound::Unbounded).slice(text), "world!");
//     /// assert_eq!(SpanRange::new(SpanBound::Unbounded, SpanBound::Unbounded).slice(text), "Hello, world!");
//     /// assert_eq!(SpanRange::empty().slice(text), "");
//     /// ```
//     #[inline]
//     pub fn slice<'s>(&'_ self, text: &'s str) -> &'s str {
//         match self {
//             Self::Range(SpanBound::Bounded(start), SpanBound::Bounded(end)) => if *start <= *end && *start < text.len() { if *end < text.len() { &text[*start..=*end] } else { &text[*start..] } } else { "" },
//             Self::Range(SpanBound::Bounded(start), SpanBound::Unbounded)    => if *start < text.len() { &text[*start..] } else { "" },
//             Self::Range(SpanBound::Unbounded, SpanBound::Bounded(end))      => if *end < text.len() { &text[..=*end] } else { text },
//             Self::Range(SpanBound::Unbounded, SpanBound::Unbounded)         => text,
//             Self::Empty(_)                                                  => "",
//         }
//     }



//     /// Returns if this SpanRange is a [`Range`](SpanRange::Range) (i.e., non-empty).
//     /// 
//     /// # Returns
//     /// True if it is, or false otherwise.
//     /// 
//     /// # Example
//     /// ```rust
//     /// use ast_toolkit::span::{SpanBound, SpanRange};
//     /// 
//     /// assert_eq!(SpanRange::new(0, 9).is_range(), true);
//     /// assert_eq!(SpanRange::new(SpanBound::Unbounded, 9).is_range(), true);
//     /// assert_eq!(SpanRange::empty().is_range(), false);
//     /// assert_eq!(SpanRange::new(9, 0).is_range(), false);
//     /// ```
//     #[inline]
//     pub fn is_range(&self) -> bool { matches!(self, Self::Range(_, _)) }
//     /// Returns the internal start bound.
//     /// 
//     /// # Returns
//     /// The start [`SpanBound`].
//     /// 
//     /// # Panics
//     /// This function panics if we are not a [`Range`](SpanRange::Range) but empty instead.
//     /// 
//     /// # Example
//     /// ```rust
//     /// use ast_toolkit::span::{SpanBound, SpanRange};
//     /// 
//     /// assert_eq!(SpanRange::new(0, 9).start(), SpanBound::Bounded(0));
//     /// assert_eq!(SpanRange::new(SpanBound::Unbounded, 9).start(), SpanBound::Unbounded);
//     /// assert!(std::panic::catch_unwind(|| SpanRange::empty().start()).is_err());
//     /// assert!(std::panic::catch_unwind(|| SpanRange::new(9, 0).start()).is_err());
//     /// ```
//     #[inline]
//     pub fn start(&self) -> SpanBound { if let Self::Range(start, _) = self { *start } else { panic!("Cannot unwrap {:?} as a SpanRange::Range", self.variant()); } }
//     /// Returns the internal end bound.
//     /// 
//     /// # Returns
//     /// The end [`SpanBound`].
//     /// 
//     /// # Panics
//     /// This function panics if we are not a [`Range`](SpanRange::Range) but empty instead.
//     /// 
//     /// # Example
//     /// ```rust
//     /// use ast_toolkit::span::{SpanBound, SpanRange};
//     /// 
//     /// assert_eq!(SpanRange::new(0, 9).end(), SpanBound::Bounded(9));
//     /// assert_eq!(SpanRange::new(0, SpanBound::Unbounded).end(), SpanBound::Unbounded);
//     /// assert!(std::panic::catch_unwind(|| SpanRange::empty().end()).is_err());
//     /// assert!(std::panic::catch_unwind(|| SpanRange::new(9, 0).end()).is_err());
//     /// ```
//     #[inline]
//     pub fn end(&self) -> SpanBound { if let Self::Range(_, end) = self { *end } else { panic!("Cannot unwrap {:?} as a SpanRange::Range", self.variant()); } }
//     /// Returns the internal start- and end bounds.
//     /// 
//     /// # Returns
//     /// A tuple with the start- and end [`SpanBound`]s, respectively.
//     /// 
//     /// # Panics
//     /// This function panics if we are not a [`Range`](SpanRange::Range) but empty instead.
//     /// 
//     /// # Example
//     /// ```rust
//     /// use ast_toolkit::span::{SpanBound, SpanRange};
//     /// 
//     /// assert_eq!(SpanRange::new(0, 9).range(), (SpanBound::Bounded(0), SpanBound::Bounded(9)));
//     /// assert_eq!(SpanRange::new(SpanBound::Unbounded, 9).range(), (SpanBound::Unbounded, SpanBound::Bounded(9)));
//     /// assert!(std::panic::catch_unwind(|| SpanRange::empty().range()).is_err());
//     /// assert!(std::panic::catch_unwind(|| SpanRange::new(9, 0).range()).is_err());
//     /// ```
//     #[inline]
//     pub fn range(&self) -> (SpanBound, SpanBound) { if let Self::Range(start, end) = self { (*start, *end) } else { panic!("Cannot unwrap {:?} as a SpanRange::Range", self.variant()); } }

//     /// Returns if this SpanRange is [`Empty`](SpanRange::Empty).
//     /// 
//     /// # Returns
//     /// True if it is, or false otherwise.
//     /// 
//     /// # Example
//     /// ```rust
//     /// use ast_toolkit::span::{SpanBound, SpanRange};
//     /// 
//     /// assert_eq!(SpanRange::new(0, 9).is_empty(), false);
//     /// assert_eq!(SpanRange::new(SpanBound::Unbounded, 9).is_empty(), false);
//     /// assert_eq!(SpanRange::empty().is_empty(), true);
//     /// assert_eq!(SpanRange::new(9, 0).is_empty(), true);
//     /// ```
//     #[inline]
//     pub fn is_empty(&self) -> bool { matches!(self, Self::Empty(_)) }

//     /// Returns the length of the range, i.e., the "number of graphemes" it catches.
//     /// 
//     /// # Arguments
//     /// - `length`: Something that gives us the size of the thing we're ranging over. This can be either a number, or the string itself. See [`Length`] for more details.
//     /// 
//     /// # Returns
//     /// The number of things captured by this range.
//     /// 
//     /// # Example
//     /// ```rust
//     /// use ast_toolkit::span::{SpanBound, SpanRange};
//     /// 
//     /// let text = "Hello, world!";
//     /// assert_eq!(SpanRange::new(0, 4).len(text.len()), 5);
//     /// assert_eq!(SpanRange::new(7, 11).len(text), 5);
//     /// assert_eq!(SpanRange::new(12, 12).len(text.len()), 1);
//     /// assert_eq!(SpanRange::new(0, 255).len(text), 13);
//     /// assert_eq!(SpanRange::new(SpanBound::Unbounded, 4).len(text.len()), 5);
//     /// assert_eq!(SpanRange::new(7, SpanBound::Unbounded).len(text), 6);
//     /// assert_eq!(SpanRange::new(SpanBound::Unbounded, SpanBound::Unbounded).len(text.len()), 13);
//     /// assert_eq!(SpanRange::empty().len(text), 0);
//     /// ```
//     #[inline]
//     pub fn len(&self, length: impl Length) -> usize {
//         let length: usize = length.len();
//         match self {
//             Self::Range(SpanBound::Bounded(start), SpanBound::Bounded(end)) => if *start <= *end && *start < length { if *end < length { 1 + *end - *start } else { length - *start } } else { 0 },
//             Self::Range(SpanBound::Bounded(start), SpanBound::Unbounded)    => if *start < length { length - *start } else { 0 },
//             Self::Range(SpanBound::Unbounded, SpanBound::Bounded(end))      => if *end < length { 1 + *end } else { length },
//             Self::Range(SpanBound::Unbounded, SpanBound::Unbounded)         => length,
//             Self::Empty(_)                                                  => 0,
//         }
//     }
// }

// impl<I: AsPrimitive<usize>> From<(Option<I>, Option<I>)> for SpanRange {
//     #[inline]
//     fn from(value: (Option<I>, Option<I>)) -> Self {
//         match value {
//             (Some(left), Some(right)) => Self::from(left..=right),
//             (Some(left), None)        => Self::from(left..),
//             (None, Some(right))       => Self::from(..=right),
//             (None, None)              => Self::from(..),
//         }
//     }
// }
// impl<I: AsPrimitive<usize>> From<RangeFrom<I>> for SpanRange {
//     #[inline]
//     fn from(value: RangeFrom<I>) -> Self { Self::Range(SpanBound::Bounded(value.start.as_()), SpanBound::Unbounded) }
// }
// impl From<RangeFull> for SpanRange {
//     #[inline]
//     fn from(_value: RangeFull) -> Self { Self::Range(SpanBound::Unbounded, SpanBound::Unbounded) }
// }
// impl<I: AsPrimitive<usize>> From<RangeInclusive<I>> for SpanRange {
//     #[inline]
//     fn from(value: RangeInclusive<I>) -> Self {
//         let (start, end): (usize, usize) = (value.start().as_(), value.end().as_());
//         if start <= end {
//             Self::Range(SpanBound::Bounded(start), SpanBound::Bounded(end))
//         } else {
//             Self::Empty(Some(start))
//         }
//     }
// }
// impl<I: AsPrimitive<usize>> From<RangeToInclusive<I>> for SpanRange {
//     #[inline]
//     fn from(value: RangeToInclusive<I>) -> Self { Self::Range(SpanBound::Unbounded, SpanBound::Bounded(value.end.as_())) }
// }

// impl AsRef<SpanRange> for SpanRange {
//     #[inline]
//     fn as_ref(&self) -> &Self { self }
// }
// impl AsMut<SpanRange> for SpanRange {
//     #[inline]
//     fn as_mut(&mut self) -> &mut Self { self }
// }
// impl From<&SpanRange> for SpanRange {
//     #[inline]
//     fn from(value: &Self) -> Self { *value }
// }
// impl From<&mut SpanRange> for SpanRange {
//     #[inline]
//     fn from(value: &mut Self) -> Self { *value }
// }

/// Abstracts over Rust's (current set of) ranges such that we won't need a dynamic trait object when referring to all of them (and can thus still support [`Copy`]).
/// 
/// # Example
/// ```rust
/// 
/// ```
#[derive(Clone, Copy, Debug, EnumDebug, Eq, Hash, PartialEq)]
pub enum SpanRange {
    /// Unofficially unpacks to a [`Range`]. This means that `start` is inclusive, `end` exclusive.
    Range { start: usize, end: usize },
    /// Unofficially unpacks to a [`RangeFrom`]. This means that `start` is inclusive, and the end is unbounded.
    RangeFrom { start: usize },
    /// Unofficially unpacks to a [`RangeFull`]. This means that `start` is unbounded, `end` unbounded.
    RangeFull,
    /// Unofficially unpacks to a [`RangeInclusive`]. This means that `start` is inclusive, `end` inclusive.
    RangeInclusive { start: usize, end: usize },
    /// Unofficially unpacks to a [`RangeTo`]. This means that `start` is unbounded, `end` exclusive.
    RangeTo { end: usize },
    /// Unofficially unpacks to a [`RangeToInclusive`]. This means that `start` is unbounded, `end` inclusive.
    RangeToInclusive { end: usize },
}

impl SpanRange {
    /// Slices the given `str` according to this range.
    /// 
    /// # Arguments
    /// - `text`: The `str` to slice.
    /// 
    /// # Returns
    /// A new `str` that is slices appropriately.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::span::SpanRange;
    /// 
    /// let text: &str = "Hello, world!";
    /// assert_eq!(SpanRange::from(0..5).slice(text), "Hello");
    /// assert_eq!(SpanRange::from(..5).slice(text), "Hello");
    /// assert_eq!(SpanRange::from(..=5).slice(text), "Hello,");
    /// assert_eq!(SpanRange::from(7..).slice(text), "world!");
    /// assert_eq!(SpanRange::from(7..12).slice(text), "world");
    /// assert_eq!(SpanRange::from(7..=12).slice(text), "world!");
    /// assert_eq!(SpanRange::from(7..42).slice(text), "world!");
    /// assert_eq!(SpanRange::from(42..42).slice(text), "");
    /// assert_eq!(SpanRange::from(1..=0).slice(text), "");
    /// ```
    #[inline]
    pub fn slice<'s>(&'_ self, text: &'s str) -> &'s str {
        match (self.start_bound(), self.end_bound()) {
            (Bound::Included(start), Bound::Excluded(end)) => &text[*start..*end],
            (Bound::Included(start), Bound::Included(end)) => &text[*start..=*end],
            (Bound::Included(start), Bound::Unbounded)     => &text[*start..],
            (Bound::Unbounded, Bound::Excluded(end))       => &text[..*end],
            (Bound::Unbounded, Bound::Included(end))       => &text[..=*end],
            (Bound::Unbounded, Bound::Unbounded)           => &text[..],

            // Dunno what to do with these
            (Bound::Excluded(start), _) => { panic!("Rust does not appear to support ranges with exclusive starts at the time of writing"); },
        }
    }

    /// Computes the number of elements in this range.
    /// 
    /// # Arguments
    /// - `max_len`: The length of the thing is spans. This is needed to resolve unbounded.
    /// 
    /// # Returns
    /// The number of characters or w/e this range denotes.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::span::SpanRange;
    /// 
    /// let text: &str = "Hello, world!";
    /// assert_eq!(SpanRange::from(0..5).len(text.len()), 5);
    /// assert_eq!(SpanRange::from(..5).len(text.len()), 5);
    /// assert_eq!(SpanRange::from(..=5).len(text.len()), 6);
    /// assert_eq!(SpanRange::from(7..).len(text.len()), 6);
    /// assert_eq!(SpanRange::from(7..12).len(text.len()), 5);
    /// assert_eq!(SpanRange::from(7..=12).len(text.len()), 6);
    /// assert_eq!(SpanRange::from(7..42).len(text.len()), 6);
    /// assert_eq!(SpanRange::from(42..42).len(text.len()), 0);
    /// assert_eq!(SpanRange::from(1..=0).len(text.len()), 0);
    /// ```
    #[inline]
    pub fn len(&self, max_len: impl AsPrimitive<usize>) -> usize {
        let max_len: usize = max_len.as_();
        match (self.start_bound(), self.end_bound()) {
            (Bound::Included(start), Bound::Excluded(end)) => if *start < *end { if *end - *start < max_len { *end - *start } else { max_len } } else { 0 },
            (Bound::Included(start), Bound::Included(end)) => if *start < *end { if 1 + *end - *start < max_len { 1 + *end - *start } else { max_len } } else { 0 },
            (Bound::Included(start), Bound::Unbounded)     => if *start < max_len { max_len - *start } else { 0 },
            (Bound::Unbounded, Bound::Excluded(end))       => *end,
            (Bound::Unbounded, Bound::Included(end))       => if *end < usize::MAX { *end + 1 } else { panic!("Cannot express length of a range that has an inclusive `usize::MAX` as end (will overflow)"); },
            (Bound::Unbounded, Bound::Unbounded)           => max_len,

            // Dunno what to do with these
            (Bound::Excluded(start), _) => { panic!("Rust does not appear to support ranges with exclusive starts at the time of writing"); },
        }
    }
}

impl RangeBounds<usize> for SpanRange {
    fn start_bound(&self) -> Bound<&usize> {
        use SpanRange::*;
        match self {
            Range { start, end }          => (start..end).start_bound(),
            RangeFrom { start }           => (start..).start_bound(),
            RangeFull                     => (..).start_bound(),
            RangeInclusive { start, end } => (start..=end).start_bound(),
            RangeTo { end }               => (..end).start_bound(),
            RangeToInclusive { end }      => (..=end).start_bound(),
        }
    }
    fn end_bound(&self) -> Bound<&usize> {
        use SpanRange::*;
        match self {
            Range { start, end }          => (start..end).end_bound(),
            RangeFrom { start }           => (start..).end_bound(),
            RangeFull                     => (..).end_bound(),
            RangeInclusive { start, end } => (start..=end).end_bound(),
            RangeTo { end }               => (..end).end_bound(),
            RangeToInclusive { end }      => (..=end).end_bound(),
        }
    }
}

impl From<(Bound<usize>, Bound<usize>)> for SpanRange {
    #[inline]
    #[track_caller]
    fn from(value: (Bound<usize>, Bound<usize>)) -> Self {
        match value {
            (Bound::Included(start), Bound::Excluded(end)) => Self::from(start..end),
            (Bound::Included(start), Bound::Included(end)) => Self::from(start..=end),
            (Bound::Included(start), Bound::Unbounded)     => Self::from(start..),
            (Bound::Unbounded, Bound::Excluded(end))       => Self::from(..end),
            (Bound::Unbounded, Bound::Included(end))       => Self::from(..=end),
            (Bound::Unbounded, Bound::Unbounded)           => Self::from(..),

            // Dunno what to do with these
            (Bound::Excluded(start), _) => { panic!("Rust does not appear to support ranges with exclusive starts at the time of writing"); },
        }
    }
}
impl From<Range<usize>> for SpanRange {
    #[inline]
    fn from(value: Range<usize>) -> Self {
        Self::Range { start: value.start, end: value.end }
    }
}
impl From<RangeFrom<usize>> for SpanRange {
    #[inline]
    fn from(value: RangeFrom<usize>) -> Self {
        Self::RangeFrom { start: value.start }
    }
}
impl From<RangeFull> for SpanRange {
    #[inline]
    fn from(_value: RangeFull) -> Self {
        Self::RangeFull
    }
}
impl From<RangeInclusive<usize>> for SpanRange {
    #[inline]
    fn from(value: RangeInclusive<usize>) -> Self {
        Self::RangeInclusive { start: *value.start(), end: *value.end() }
    }
}
impl From<RangeTo<usize>> for SpanRange {
    #[inline]
    fn from(value: RangeTo<usize>) -> Self {
        Self::RangeTo { end: value.end }
    }
}
impl From<RangeToInclusive<usize>> for SpanRange {
    #[inline]
    fn from(value: RangeToInclusive<usize>) -> Self {
        Self::RangeToInclusive { end: value.end }
    }
}

impl AsRef<SpanRange> for SpanRange {
    #[inline]
    fn as_ref(&self) -> &SpanRange { self }
}
impl AsMut<SpanRange> for SpanRange {
    #[inline]
    fn as_mut(&mut self) -> &mut SpanRange { self }
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
/// use ast_toolkit::{Position, Span};
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
/// // Which can then be queried to be informed over the spanned text...
/// assert_eq!(span.text(), "Hello, world!");
/// 
/// // ...or, if the span is non-empty, about coordinate information:
/// let start: Position = span.start().unwrap();
/// let end: Position = span.end().unwrap();
/// assert_eq!(start.line, 0);
/// assert_eq!(start.col, 0);
/// assert_eq!(end.line, 0);
/// assert_eq!(end.col, 12);
/// ```
#[derive(Clone, Copy, Debug, Eq, Hash)]
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
        Self {
            file   : file.into(),
            source : source.into(),
            range  : SpanRange::from(..),
        }
    }

    /// Creates an empty Span.
    /// 
    /// # Returns
    /// A new instance of Self that spans nothing of the source text.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::Span;
    /// 
    /// let span1 = Span::empty("<example>", "Hello there!");
    /// let span2 = Span::from_idx("<example>", "Hello there!", 0, 0);
    /// assert_eq!(span1.text() == "");
    /// assert_eq!(Span::combined(span1, span2).text() == "H");
    /// ```
    #[inline]
    pub fn empty(file: impl Into<&'f str>, source: impl Into<&'s str>) -> Self {
        Self {
            file   : file.into(),
            source : source.into(),
            range  : (1..0).into(),
        }
    }
    /// Creates an empty Span, but one that is tied to a particular position.
    /// 
    /// # Arguments
    /// - `start`: The position from where we are Empty.
    /// 
    /// # Returns
    /// A new instance of Self that spans nothing of the source text.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::Span;
    /// 
    /// let span1 = Span::empty_with_idx("<example>", "Hello there!", 6);
    /// let span2 = Span::from_idx("<example>", "Hello there!", 0, 0);
    /// assert_eq!(span1.text() == "");
    /// assert_eq!(Span::combined(span1, span2).text() == "H");
    /// ```
    #[inline]
    pub fn empty_with_idx(file: impl Into<&'f str>, source: impl Into<&'s str>, start: impl AsPrimitive<usize>) -> Self {
        Self {
            file   : file.into(),
            source : source.into(),
            range  : (start.as_()..0).into(),
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

        // Unpack the options to something out-of-scope
        let (rstart, rend): (usize, usize) = match (rstart, rend) {
            (Some(rstart), Some(rend)) => (rstart, rend),
            (Some(rstart), None)       => (rstart, source.len()),
            (None, Some(rend))         => (source.len(), rend),
            (None, None)               => (source.len(), source.len()),
        };

        // Create self
        Self {
            file,
            source,
            range : (rstart..=rend).into(),
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
    /// let span4 = Span::from_idx("<example>", bytes.as_ref(), 1, 0);
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
            range  : (start.as_()..=end.as_()).into(),
        }
    }

    /// Constructor for the Span that takes a custom range to span.
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
    /// let span4 = Span::from_range("<example>", "Hello, world!", ..);
    /// 
    /// assert_eq!(span1.text(), "Hello");
    /// assert_eq!(span2.text(), "Hello");
    /// assert_eq!(span3.text(), "world!");
    /// assert_eq!(span4.text(), "Hello, world!");
    /// ```
    #[inline]
    pub fn from_range(file: impl Into<&'f str>, source: impl Into<&'s str>, range: impl Into<SpanRange>) -> Self {
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

        // Combine the start into a new bound
        let start: Bound<usize> = match (span1.range.start_bound(), span2.range.start_bound()) {
            (Bound::Excluded(start1), Bound::Excluded(start2)) => Bound::Excluded(std::cmp::min(*start1, *start2)),
            // NOTE: This below works because we're changing the included start to an excluded start, e.g.,
            //   `0, 1, [2, 3, ...]` to `0, (1, 2, 3, ...]`
            // Thus, we have to -1. If the start is already at the 0, then we can instead safely assume it will be the smallest number out of the two.
            (Bound::Excluded(start1), Bound::Included(start2)) => Bound::Excluded(if *start2 > 0 { std::cmp::min(*start1, *start2 - 1) } else { *start2 }),
            (Bound::Excluded(start1), Bound::Unbounded)        => Bound::Unbounded,
            (Bound::Included(start1), Bound::Excluded(start2)) => Bound::Excluded(if *start1 > 0 { std::cmp::min(*start1 - 1, *start2) } else { *start1 }),
            (Bound::Included(start1), Bound::Included(start2)) => Bound::Included(std::cmp::min(*start1, *start2)),
            (Bound::Included(start1), Bound::Unbounded)        => Bound::Unbounded,
            (Bound::Unbounded, Bound::Excluded(start2))        => Bound::Unbounded,
            (Bound::Unbounded, Bound::Included(start2))        => Bound::Unbounded,
            (Bound::Unbounded, Bound::Unbounded)               => Bound::Unbounded,
        };
        // Combine the end into a new bound
        let end: Bound<usize> = match (span1.range.end_bound(), span2.range.end_bound()) {
            (Bound::Excluded(end1), Bound::Excluded(end2)) => Bound::Excluded(std::cmp::max(*end1, *end2)),
            // NOTE: This below works because we're changing the included end to an excluded end, e.g.,
            //   `[..., 4, 5], 6, 7` to `[..., 4, 5, 6), 7`
            // Thus, we have to +1. If the start is already at the max (`usize::MAX`), then we can instead safely assume it will be the largest number out of the two.
            (Bound::Excluded(end1), Bound::Included(end2)) => Bound::Excluded(if *end2 < usize::MAX { std::cmp::max(*end1, *end2 + 1) } else { *end2 }),
            (Bound::Excluded(end1), Bound::Unbounded)      => Bound::Unbounded,
            (Bound::Included(end1), Bound::Excluded(end2)) => Bound::Excluded(if *end1 < usize::MAX { std::cmp::max(*end1 + 1, *end2) } else { *end1 }),
            (Bound::Included(end1), Bound::Included(end2)) => Bound::Included(std::cmp::max(*end1, *end2)),
            (Bound::Included(end1), Bound::Unbounded)      => Bound::Unbounded,
            (Bound::Unbounded, Bound::Excluded(end2))      => Bound::Unbounded,
            (Bound::Unbounded, Bound::Included(end2))      => Bound::Unbounded,
            (Bound::Unbounded, Bound::Unbounded)           => Bound::Unbounded,
        };

        // Construct a new self out of it
        Self {
            file   : span1.file,
            source : span1.source,
            range  : ((start, end)).into(),
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
    #[inline]
    #[track_caller]
    pub fn consume(&mut self, other: impl AsRef<Span<'f, 's>>) -> &mut Self {
        // Define in terms of combined
        *self = Span::combined(*self, other);
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
    /// This function may panic if the given index is out-of-bounds or it is not at the grapheme boundary.
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
    /// 
    /// assert!(std::panic::catch_unwind(|| span1.pos_of(50)).is_err());
    /// assert!(std::panic::catch_unwind(|| span1.pos_of(50)).is_err());
    /// assert!(std::panic::catch_unwind(|| Span::new("<example>", "Hÿllo\nworld!").pos_of(2)).is_err());
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
    /// This function may panic if the internal start is exclusive and [`usize::MAX`]; start is out-of-bounds; or start is not at the grapheme boundary.
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
    /// assert_eq!(span1.start(), Position::new0(0, 0));
    /// assert_eq!(span2.start(), Position::new0(0, 2));
    /// assert_eq!(span3.start(), Position::new0(1, 0));
    /// assert_eq!(span4.start(), Position::new0(0, 1));
    /// 
    /// assert!(std::panic::catch_unwind(|| Span::from_idx("<example>", "Hÿllo\nworld!", 2, 6).start()).is_err());
    /// ```
    #[inline]
    #[track_caller]
    pub fn start(&self) -> Position {
        match self.range.start_bound() {
            Bound::Excluded(start)  => if *start < usize::MAX { self.pos_of(*start + 1) } else { panic!("Cannot rescale start position to beyond usize::MAX (i.e., start is usize::MAX and exclusive)") },
            Bound::Included(start)  => self.pos_of(*start),
            Bound::Unbounded        => self.pos_of(0),
        }
    }

    /// Returns the end position of this span as a [`Position`].
    /// 
    /// # Returns
    /// A [`Position`] describing the end position in the source text, or [`None`] if we are empty.
    /// 
    /// # Panics
    /// This function may panic if the internal end is exclusive and `0`; end is out-of-bounds; or end is not at the grapheme boundary.
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
    /// assert_eq!(span1.end(), Position::new0(0, 11));
    /// assert_eq!(span2.end(), Position::new0(1, 5));
    /// assert_eq!(span3.end(), Position::new0(0, 2));
    /// assert_eq!(span4.end(), Position::new0(1, 4));
    /// assert_eq!(span5.end(), Position::new0(0, 1));
    /// 
    /// assert!(std::panic::catch_unwind(|| Span::from_idx("<example>", "Hÿllo\nworld!", 2, 6).start()).is_err());
    /// ```
    #[inline]
    #[track_caller]
    pub fn end(&self) -> Position {
        match self.range.end_bound() {
            Bound::Excluded(end)  => if *end > 0 { self.pos_of(*end - 1) } else { panic!("Cannot rescale end position to before 0 (i.e., end is 0 and exclusive)") },
            Bound::Included(end)  => self.pos_of(*end),
            Bound::Unbounded      => if self.source.len() > 0 { self.pos_of(self.source.len() - 1) } else { panic!("Cannot rescale end position to before 0 (i.e., end is unbounded and self.source is empty)") },
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
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::Span;
    /// 
    /// assert_eq!(Span::new("<example>", "Hello, world!").len(), 13);
    /// assert_eq!(Span::from_idx("<example>", "Hello, world!", 0, 4).len(), 5);
    /// assert_eq!(Span::from_idx("<example>", "Hello, world!", 7, 12).len(), 6);
    /// assert_eq!(Span::from_idx("<example>", "Hello, world!", 5, 5).len(), 1);
    /// assert_eq!(Span::from_idx("<example>", "Hello, world!", 6, 5).len(), 0);
    /// ```
    #[inline]
    pub fn len(&self) -> usize { self.range.len(self.source.len()) }
}

impl<'f, 's> PartialEq for Span<'f, 's> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.file == other.file && self.text() == other.text()
    }
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
    fn find_substring(&self, substr: T) -> Option<usize> {
        let source: &str = self.range.slice(self.source);
        let substr: &str = substr.as_ref();
        for (i, _) in source.grapheme_indices(true) {
            if i + substr.len() <= source.len() && &source[i..i + substr.len()] == substr {
                return Some(i);
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
        if count > self.range.len(self.source.len()) { panic!("Given count {} is out-of-range for Span of size {}", count, self.range.len(self.source.len())); }
        if count > 0 {
            Self {
                file   : self.file,
                source : self.source,
                range  : match self.range {
                    SpanRange::Range(SpanBound::Bounded(start), _) => SpanRange::Range(SpanBound::Bounded(start), SpanBound::Bounded(start + count - 1)),
                    SpanRange::Range(SpanBound::Unbounded, _)      => SpanRange::Range(SpanBound::Unbounded, SpanBound::Bounded(count - 1)),
                    SpanRange::Empty(_)                            => { unreachable!(); },
                },
            }
        } else {
            Self {
                file   : self.file,
                source : self.source,
                range  : SpanRange::Empty(match self.range { SpanRange::Range(SpanBound::Bounded(start), _) => Some(start), SpanRange::Range(SpanBound::Unbounded, _) => Some(0), SpanRange::Empty(start) => start }),
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
                range  : match self.range {
                    SpanRange::Range(SpanBound::Bounded(start), SpanBound::Bounded(end)) => { let count: usize = start + count; if count <= end { SpanRange::Range(SpanBound::Bounded(count), SpanBound::Bounded(end)) } else { SpanRange::Empty(Some(count)) } },
                    SpanRange::Range(SpanBound::Bounded(start), SpanBound::Unbounded)    => { let count: usize = start + count; SpanRange::Range(SpanBound::Bounded(count), SpanBound::Unbounded) },
                    SpanRange::Range(SpanBound::Unbounded, SpanBound::Bounded(end))      => if count <= end { SpanRange::Range(SpanBound::Bounded(count), SpanBound::Bounded(end)) } else { SpanRange::Empty(Some(count)) },
                    SpanRange::Range(SpanBound::Unbounded, SpanBound::Unbounded)         => SpanRange::Range(SpanBound::Bounded(count), SpanBound::Unbounded),
                    SpanRange::Empty(_)                                                  => { unreachable!(); },
                },
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
                        Span { file: self.file, source: self.source, range: SpanRange::Empty(match self.range { SpanRange::Range(SpanBound::Bounded(start), _) => Some(start), SpanRange::Range(SpanBound::Unbounded, _) => Some(0), SpanRange::Empty(start) => start }) }
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
                        Span { file: self.file, source: self.source, range: SpanRange::Empty(match self.range { SpanRange::Range(SpanBound::Bounded(start), _) => Some(start), SpanRange::Range(SpanBound::Unbounded, _) => Some(0), SpanRange::Empty(start) => start }) }
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
#[cfg(feature = "nom")]
impl<'f, 's> nom::Offset for Span<'f, 's> {
    #[inline]
    #[track_caller]
    fn offset(&self, second: &Self) -> usize {
        // Get some bound values
        let start1: usize = match self.range {
            SpanRange::Range(SpanBound::Bounded(start), _) => start,
            SpanRange::Range(SpanBound::Unbounded, _)      => 0,
            SpanRange::Empty(Some(start))                  => start,
            SpanRange::Empty(None)                         => { panic!("Cannot get offset of empty Span"); },
        };
        let start2: usize = match second.range {
            SpanRange::Range(SpanBound::Bounded(start), _) => start,
            SpanRange::Range(SpanBound::Unbounded, _)      => 0,
            SpanRange::Empty(Some(start))                  => start,
            SpanRange::Empty(None)                         => { panic!("Cannot get offset of empty Span"); },
        };

        // OK simply compare
        if start1 <= start2 {
            start2 - start1
        } else {
            panic!("Cannot find offset with earlier Span");
        }
    }
}
#[cfg(feature = "nom")]
impl<'f, 's, R: RangeBounds<usize>> nom::Slice<R> for Span<'f, 's> {
    #[inline]
    #[track_caller]
    fn slice(&self, range: R) -> Self {
        // Obtain the range
        let range: (usize, Option<usize>) = match (range.start_bound(), range.end_bound()) {
            (Bound::Included(i1), Bound::Included(i2)) => (*i1, Some(*i2)),
            (Bound::Included(i1), Bound::Excluded(i2)) => if *i2 > 0 { (*i1, Some(*i2 - 1)) } else { (*i1, None) },
            (Bound::Included(i1), Bound::Unbounded)    => (*i1, if !self.source.is_empty() { Some(self.source.len() - 1) } else { None }),
            (Bound::Excluded(i1), Bound::Included(i2)) => (*i1 + 1, Some(*i2)),
            (Bound::Excluded(i1), Bound::Excluded(i2)) => if *i2 > 0 { (*i1 + 1, Some(*i2 - 1)) } else { (*i1 + 1, None) },
            (Bound::Excluded(i1), Bound::Unbounded)    => (*i1 + 1, if !self.source.is_empty() { Some(self.source.len() - 1) } else { None }),
            (Bound::Unbounded, Bound::Included(i2))    => (0, Some(*i2)),
            (Bound::Unbounded, Bound::Excluded(i2))    => if *i2 > 0 { if !self.source.is_empty() { (0, Some(*i2 - 1)) } else { (0, None) } } else { (0, None) },
            (Bound::Unbounded, Bound::Unbounded)       => (0, if !self.source.is_empty() { Some(self.source.len() - 1) } else { None }),
        };

        // Now scale that down within context of our own range
        let range: SpanRange = match (self.range, range) => {
            (SpanRange::Range(SpanBound::Bounded(start1), SpanBound::Bounded(end1)), (start2, Some(end2))) => SpanRange::Range(SpanBound::Bounded(())),
        },

        // Create outselves with it
        Self {
            file   : self.file,
            source : self.source,
            range,
        }
    }
}
