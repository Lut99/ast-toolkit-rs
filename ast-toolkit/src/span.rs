//  SPAN.rs
//    by Lut99
// 
//  Created:
//    27 Aug 2023, 12:36:52
//  Last edited:
//    17 Sep 2023, 22:30:02
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines the [`Span`] struct and associates, which we use to keep
//!   track of a node's position in the source text.
// 

use std::ops::{Bound, RangeBounds};

use num_traits::AsPrimitive;
use unicode_segmentation::UnicodeSegmentation as _;

use crate::position::Position;


/***** TESTS *****/
// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[test]
//     fn test_from_pos() {
//         let span: Span = Span::from_pos("<example>", "Hello, World!", Position::new1(1, 1), Position::new1(1, 1));
//         assert_eq!(span.text(), "H");
//         let span: Span = Span::from_pos("<example>", "Hello, World!", Position::new1(1, 1), Position::new1(1, 5));
//         assert_eq!(span.text(), "Hello");

//         let span: Span = Span::from_pos("<example>", "Hello\nWorld!", Position::new1(1, 1), Position::new1(1, 1));
//         assert_eq!(span.text(), "H");
//         let span: Span = Span::from_pos("<example>", "Hello\nWorld!", Position::new1(1, 1), Position::new1(1, 5));
//         assert_eq!(span.text(), "Hello");
//         let span: Span = Span::from_pos("<example>", "Hello\nWorld!", Position::new1(2, 1), Position::new1(2, 1));
//         assert_eq!(span.text(), "W");
//         let span: Span = Span::from_pos("<example>", "Hello\nWorld!", Position::new1(2, 1), Position::new1(2, 5));
//         assert_eq!(span.text(), "World");
//         let span: Span = Span::from_pos("<example>", "Hello\nWorld!", Position::new1(1, 1), Position::new1(2, 6));
//         assert_eq!(span.text(), "Hello\nWorld!");
//     }
// }

#[cfg(feature = "nom")]
#[cfg(test)]
mod nom_tests {
    use itertools::Itertools as _;
    use super::*;

    #[test]
    fn test_span_nom_as_bytes() {
        // Create a few spans and see if they byte version equates what we expect
        assert_eq!(<Span as nom::AsBytes>::as_bytes(&Span::new("<example>", "Example text")), b"Example text");
        assert_eq!(<Span as nom::AsBytes>::as_bytes(&Span::ranged("<example>", "Example text", 0..=6)), b"Example");
        assert_eq!(<Span as nom::AsBytes>::as_bytes(&Span::ranged("<example>", "Example text", 8..=11)), b"text");
        assert_eq!(<Span as nom::AsBytes>::as_bytes(&Span::ranged("<example>", "Example text", 3..=9)), b"mple te");
    }
    #[test]
    fn test_span_nom_compare() {
        // Do some comparisons
        assert_eq!(<Span as nom::Compare<&str>>::compare(&Span::new("<example>", "Example text"), "Example text"), nom::CompareResult::Ok);
        assert_eq!(<Span as nom::Compare<&str>>::compare(&Span::ranged("<example>", "Example text", 0..=6), "Example"), nom::CompareResult::Ok);
        assert_eq!(<Span as nom::Compare<&str>>::compare(&Span::ranged("<example>", "Example text", 8..=11), "text"), nom::CompareResult::Ok);
        assert_eq!(<Span as nom::Compare<&str>>::compare(&Span::new("<example>", "Example text"), "Example text 2"), nom::CompareResult::Incomplete);
        assert_eq!(<Span as nom::Compare<&str>>::compare(&Span::new("<example>", "Example text"), "Example2 text"), nom::CompareResult::Error);
    }
    #[test]
    fn test_span_nom_extend_into() {
        let file: &str = "<test>";
        let code: &str = "let test: &str = \"Hello, there!\"\nprintln!(\"{test}\");";

        // Attempt to combine a few spans
        let spans = vec![
            Span::ranged(file, code, 0..=2),
            Span::ranged(file, code, 4..=7),
            Span::ranged(file, code, 11..=13),
            Span::ranged(file, code, 18..=22),
            Span::ranged(file, code, 25..=29),
            Span::ranged(file, code, 32..=39),
            Span::ranged(file, code, 44..=47),
        ];

        // Try various combinations
        for spans in spans.iter().combinations(2) {
            assert_eq!(spans.len(), 2);
            let span1: &Span = spans[0];
            let span2: &Span = spans[1];

            // Compare them
            let mut builder = <Span as nom::ExtendInto>::new_builder(&span1);
            <Span as nom::ExtendInto>::extend_into(&span2, &mut builder);
            assert_eq!(builder.text(), Span::combined(span1, span2).text());
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
        assert_eq!(<Span as nom::FindSubstring<&str>>::find_substring(&Span::ranged("<example>", "Hello, world!", 7..=12), "Hello"), None);
        assert_eq!(<Span as nom::FindSubstring<&str>>::find_substring(&Span::ranged("<example>", "Hello, world!", 7..=12), "world"), Some(0));
        assert_eq!(<Span as nom::FindSubstring<&str>>::find_substring(&Span::ranged("<example>", "Hello, world!", 7..=12), "!"), Some(5));
        assert_eq!(<Span as nom::FindSubstring<&str>>::find_substring(&Span::ranged("<example>", "Hello, world!", 7..=12), "Bananas"), None);
        assert_eq!(<Span as nom::FindSubstring<&str>>::find_substring(&Span::ranged("<example>", "Hello, world!", 7..=12), "Helol"), None);
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
        assert_eq!(<Span as nom::FindToken<u8>>::find_token(&Span::ranged(file, text, 6..=13), b'H'), false);
        assert_eq!(<Span as nom::FindToken<u8>>::find_token(&Span::ranged(file, text, 6..=13), b'o'), false);
        assert_eq!(<Span as nom::FindToken<u8>>::find_token(&Span::ranged(file, text, 6..=13), b'!'), true);
        assert_eq!(<Span as nom::FindToken<u8>>::find_token(&Span::ranged(file, text, 6..=13), b'q'), false);

        // Find some characters in full source...
        assert_eq!(<Span as nom::FindToken<char>>::find_token(&Span::new(file, text), 'H'), true);
        assert_eq!(<Span as nom::FindToken<char>>::find_token(&Span::new(file, text), 'o'), true);
        assert_eq!(<Span as nom::FindToken<char>>::find_token(&Span::new(file, text), '!'), true);
        assert_eq!(<Span as nom::FindToken<char>>::find_token(&Span::new(file, text), 'q'), false);
        assert_eq!(<Span as nom::FindToken<char>>::find_token(&Span::new(file, text), 'ÿ'), true);
        // ...and ranges source
        assert_eq!(<Span as nom::FindToken<char>>::find_token(&Span::ranged(file, text, 6..=13), 'H'), false);
        assert_eq!(<Span as nom::FindToken<char>>::find_token(&Span::ranged(file, text, 6..=13), 'o'), false);
        assert_eq!(<Span as nom::FindToken<char>>::find_token(&Span::ranged(file, text, 6..=13), '!'), true);
        assert_eq!(<Span as nom::FindToken<char>>::find_token(&Span::ranged(file, text, 6..=13), 'q'), false);
        assert_eq!(<Span as nom::FindToken<char>>::find_token(&Span::ranged(file, text, 6..=13), 'ÿ'), true);

        // Find some graphemes in full source...
        assert_eq!(<Span as nom::FindToken<&str>>::find_token(&Span::new(file, text), "H"), true);
        assert_eq!(<Span as nom::FindToken<&str>>::find_token(&Span::new(file, text), "o"), true);
        assert_eq!(<Span as nom::FindToken<&str>>::find_token(&Span::new(file, text), "!"), true);
        assert_eq!(<Span as nom::FindToken<&str>>::find_token(&Span::new(file, text), "q"), false);
        assert_eq!(<Span as nom::FindToken<&str>>::find_token(&Span::new(file, text), "ÿ"), true);
        // ...and ranges source
        assert_eq!(<Span as nom::FindToken<&str>>::find_token(&Span::ranged(file, text, 6..=13), "H"), false);
        assert_eq!(<Span as nom::FindToken<&str>>::find_token(&Span::ranged(file, text, 6..=13), "o"), false);
        assert_eq!(<Span as nom::FindToken<&str>>::find_token(&Span::ranged(file, text, 6..=13), "!"), true);
        assert_eq!(<Span as nom::FindToken<&str>>::find_token(&Span::ranged(file, text, 6..=13), "q"), false);
        assert_eq!(<Span as nom::FindToken<&str>>::find_token(&Span::ranged(file, text, 6..=13), "ÿ"), true);
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
        assert_eq!(<Span as nom::InputLength>::input_len(&Span::ranged("<example>", "Example text", 0..=6)), 7);
        assert_eq!(<Span as nom::InputLength>::input_len(&Span::ranged("<example>", "Example text", 8..=11)), 4);
        assert_eq!(<Span as nom::InputLength>::input_len(&Span::ranged("<example>", "Example text", 3..=9)), 7);
    }
    #[test]
    fn test_span_nom_input_take() {
        // See if we can take and split how we expect
        assert_eq!(<Span as nom::InputTake>::take(&Span::new("<example>", "Example text"), 7), Span::ranged("<example>", "Example text", 0..=6));
        assert_eq!(<Span as nom::InputTake>::take(&Span::ranged("<example>", "Example text", 8..=11), 3), Span::ranged("<example>", "Example text", 8..=10));
        assert_eq!(<Span as nom::InputTake>::take(&Span::ranged("<example>", "Example text", 8..=11), 0), Span::empty("<example>", "Example text"));
        assert!(std::panic::catch_unwind(|| <Span as nom::InputTake>::take(&Span::ranged("<example>", "Example text", 8..=11), 5)).is_err());

        // Now compare the split
        assert_eq!(<Span as nom::InputTake>::take_split(&Span::new("<example>", "Example text"), 7), (Span::ranged("<example>", "Example text", 0..=6), Span::ranged("<example>", "Example text", 7..=11)));
        assert_eq!(<Span as nom::InputTake>::take_split(&Span::ranged("<example>", "Example text", 8..=11), 3), (Span::ranged("<example>", "Example text", 8..=10), Span::ranged("<example>", "Example text", 11..=11)));
        assert_eq!(<Span as nom::InputTake>::take_split(&Span::ranged("<example>", "Example text", 8..=11), 0), (Span::empty("<example>", "Example text"), Span::ranged("<example>", "Example text", 8..=11)));
        assert!(std::panic::catch_unwind(|| <Span as nom::InputTake>::take_split(&Span::ranged("<example>", "Example text", 8..=11), 5)).is_err());
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
        assert_eq!(span.split_at_position(|c| c == 't'), Ok::<I, E>((Span::ranged("<example>", "Example text!", ..=7), Span::ranged("<example>", "Example text!", 8..))));
        assert_eq!(span.split_at_position(|c| c == '!'), Ok::<I, E>((Span::ranged("<example>", "Example text!", ..=11), Span::ranged("<example>", "Example text!", 12..))));
        assert_eq!(span.split_at_position(|c| c == '_'), Err(nom::Err::<VerboseError<Span>>::Incomplete(nom::Needed::Unknown)));

        // Do it again with a different function
        assert_eq!(span.split_at_position1(|c| c == 'E', ErrorKind::Many0), Err(nom::Err::Error(VerboseError::from_error_kind(span, ErrorKind::Many0))));
        assert_eq!(span.split_at_position1(|c| c == 't', ErrorKind::Many0), Ok::<I, E>((Span::ranged("<example>", "Example text!", ..=7), Span::ranged("<example>", "Example text!", 8..))));
        assert_eq!(span.split_at_position1(|c| c == '!', ErrorKind::Many0), Ok::<I, E>((Span::ranged("<example>", "Example text!", ..=11), Span::ranged("<example>", "Example text!", 12..))));
        assert_eq!(span.split_at_position1(|c| c == '_', ErrorKind::Many0), Err(nom::Err::<VerboseError<Span>>::Incomplete(nom::Needed::Unknown)));

        // And complete counterpart 1
        assert_eq!(span.split_at_position1_complete(|c| c == 'E', ErrorKind::Many0), Err(nom::Err::Error(VerboseError::from_error_kind(span, ErrorKind::Many0))));
        assert_eq!(span.split_at_position1_complete(|c| c == 't', ErrorKind::Many0), Ok::<I, E>((Span::ranged("<example>", "Example text!", ..=7), Span::ranged("<example>", "Example text!", 8..))));
        assert_eq!(span.split_at_position1_complete(|c| c == '!', ErrorKind::Many0), Ok::<I, E>((Span::ranged("<example>", "Example text!", ..=11), Span::ranged("<example>", "Example text!", 12..))));
        assert_eq!(span.split_at_position1_complete(|c| c == '_', ErrorKind::Many0), Ok::<I, E>((span, Span::empty("<example>", "Example text!"))));

        // Finally, complete counterpart lenient.
        assert_eq!(span.split_at_position_complete(|c| c == 'E'), Ok::<I, E>((Span::empty("<example>", "Example text!"), Span::new("<example>", "Example text!"))));
        assert_eq!(span.split_at_position_complete(|c| c == 't'), Ok::<I, E>((Span::ranged("<example>", "Example text!", ..=7), Span::ranged("<example>", "Example text!", 8..))));
        assert_eq!(span.split_at_position_complete(|c| c == '!'), Ok::<I, E>((Span::ranged("<example>", "Example text!", ..=11), Span::ranged("<example>", "Example text!", 12..))));
        assert_eq!(span.split_at_position_complete(|c| c == '_'), Ok::<I, E>((span, Span::empty("<example>", "Example text!"))));
    }

    #[test]
    fn test_span_nom_offset() {
        use std::panic::catch_unwind;
        use nom::Offset as _;

        // Prepare a few spans for comparisons
        let file: &str = "<example>";
        let source: &str = "Example text!";
        let span1 = Span::new(file, source);
        let span2 = Span::ranged(file, source, 0..7);
        let span3 = Span::ranged(file, source, 8..12);
        let span4 = Span::ranged(file, source, 8..8);
        let span5 = Span::empty(file, source);

        // Right-o, let's do the asserts
        assert_eq!(span1.offset(&span1), 0);
        assert_eq!(span1.offset(&span2), 0);
        assert_eq!(span1.offset(&span3), 8);
        assert!(catch_unwind(|| span3.offset(&span1)).is_err());
        assert_eq!(span1.offset(&span4), 8);
        assert_eq!(span3.offset(&span4), 0);
        assert_eq!(span4.offset(&span3), 0);
        assert!(catch_unwind(|| span1.offset(&span5)).is_err());
        assert!(catch_unwind(|| span5.offset(&span1)).is_err());
    }

    #[test]
    fn test_span_nom_slice() {
        use nom::Slice as _;

        // Prepare span(s)
        let file: &str = "<example>";
        let source: &str = "Example text!";
        let span1 = Span::new(file, source);
        let span2 = Span::ranged(file, source, 8..);

        // Produce some slices on the whole span
        assert_eq!(span1.slice(..), span1);
        assert_eq!(span1.slice(0..13), span1);
        assert_eq!(span1.slice(0..7), Span::ranged(file, source, 0..7));
        assert_eq!(span1.slice(0..=6), Span::ranged(file, source, 0..7));
        assert_eq!(span1.slice(1..=0), Span::empty(file, source));

        // Now try for a span with offset
        assert_eq!(span2.slice(..), span2);
        assert_eq!(span2.slice(0..13), span2);
        assert_eq!(span2.slice(0..4), Span::ranged(file, source, 8..12));
        assert_eq!(span2.slice(0..7), span2);
        assert_eq!(span2.slice(0..=6), span2);
        assert_eq!(span2.slice(1..=0), Span::empty(file, source));
    }
}





/***** HELPER FUNCTIONS *****/
/// Extracts the "line wrapped source" of the given spanned area.
/// 
/// This is useful for diagnosting, where we return the spanned area + any line start and line end not captured.
/// 
/// # Arguments
/// - `source`: The source to extract the spanned area from.
/// - `start_idx`: The start index (inclusive) of the source area (or [`None`] if this doesn't make sense, e.g., it's empty).
/// - `end_id`: The end index (inclusive) of the source area (or [`None`] if this doesn't make sense, e.g., it's empty).
/// 
/// # Returns
/// A tuple of the start index of the new range (if any), the end index of the range (if any).
pub(crate) fn find_lines_box(source: &str, start_idx: Option<usize>, end_idx: Option<usize>) -> (Option<usize>, Option<usize>) {
    match (start_idx, end_idx) {
        // If the ranges are defined, then limit by the fact the range has to logically contain elements
        (Some(mut start), Some(mut end)) => if start <= end {
            // Re-scale start and/or end if it makes sense
            if !source.is_empty() && start >= source.len() && end < source.len() {
                start = source.len() - 1;
            } else if !source.is_empty() && start < source.len() && end >= source.len() {
                end = source.len() - 1;
            } else if start >= source.len() && end >= source.len() {
                return (Some(start), Some(end));
            }

            // Move the start backwards to find the nearest line
            let mut found: bool = false;
            let mut prev: usize = start;
            for (i, c) in source[..start].grapheme_indices(true).rev() {
                // If we find a newline, then move start to include the previous character (which is the start of the newline)
                if c == "\n" { start = prev; found = true; break; }
                prev = i;
            }
            if !found { start = 0; }

            // Move the end forwards to find the nearest line
            let mut found: bool = false;
            for (i, c) in source[end..].grapheme_indices(true) {
                // If we find a newline, then accept the line with it
                if c == "\n" { end += i; found = true; break; }
            }
            if !found { end = source.len() - 1; }   // NOTE: This `end - 1` is OK because we already asserted end is less than source.len(), which can only be true if the source len() > 0.

            // OK, slice the string like this
            (Some(start), Some(end))
        } else {
            (Some(start), Some(end))
        },

        // Catch explicit empties
        (start, end) => (start, end),
    }
}





/***** LIBRARY *****/
/// Abstracts over the specific implementation of a span. This allows us to have varying levels of references VS non-references while avoiding lifetime hell.
/// 
/// Spanning implements the internals of a spanning type, so that we can implement the more high-level functions in [`SpanningExt`].
/// 
/// # Example
/// ```rust
/// use ast_toolkit::Spanning;
/// 
/// struct HelloWorldSpan {
///     start : Option<usize>,
///     end   : Option<usize>,
/// }
/// impl Spanning<'static, 'static> for HelloWorldSpan {
///     fn file(&self) -> &'static str { "<example>" }
///     fn source(&self) -> &'static str { "Hello,\nworld!" }
/// 
///     fn start_idx(&self) -> Option<usize> { self.start }
///     fn end_idx(&self) -> Option<usize> { self.end }
/// }
/// 
/// assert_eq!(HelloWorldSpan { start: Some(0), end: Some(12) }.file(), "<example>");
/// assert_eq!(HelloWorldSpan { start: Some(0), end: Some(4) }.source(), "Hello,\nworld!");
/// assert_eq!(HelloWorldSpan { start: Some(0), end: Some(1) }.start_idx(), Some(0));
/// assert_eq!(HelloWorldSpan { start: Some(4), end: None }.end_idx(), None);
/// // ...
/// ```
pub trait Spanning<'f, 's> {
    /// Returns the internal source description (e.g., filename or some other user-friendly description to disambiguate different source texts).
    /// 
    /// # Returns
    /// A reference to the filename, as a string.
    fn file(&self) -> &'f str;

    /// Returns the entire source captured this Spanning object.
    /// 
    /// This does not take any spanning ranges into account!
    /// 
    /// # Returns
    /// A reference to the source, as a string.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{Span, Spanning as _};
    /// 
    /// assert_eq!(Span::new("<example>", "Hello, world!").source(), "Hello, world!");
    /// assert_eq!(Span::empty("<example>", "Hello, world!").source(), "Hello, world!");
    /// assert_eq!(Span::ranged("<example>", "Hello, world!", 0..5).source(), "Hello, world!");
    /// assert_eq!(Span::ranged("<example>", "Hello, world!", 7..12).source(), "Hello, world!");
    /// assert_eq!(Span::ranged("<example>", "Hello, world!", 7..).source(), "Hello, world!");
    /// assert_eq!(Span::ranged("<example>", "Hello, world!", 7..6).source(), "Hello, world!");
    /// ```
    fn source(&self) -> &'s str;


    
    /// Returns the start position, as an index, of this Spanning object.
    /// 
    /// Note that not all Spanned values might have sensible start positions. [`Span`]s created with [`Span::empty()`], for example, have no sensible start; nor do Spans over empty sources.
    /// 
    /// Note that this function is typically only useful for functions really working with the internals of a span, since the index is relative to the source.
    /// 
    /// # Returns
    /// The start position if any is defined. If not, [`None`] is returned.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{Span, Spanning as _};
    /// 
    /// assert_eq!(Span::new("<example>", "").start_idx(), Some(0));
    /// assert_eq!(Span::new("<example>", "Hello, world!").start_idx(), Some(0));
    /// assert_eq!(Span::empty("<example>", "Hello, world!").start_idx(), None);
    /// assert_eq!(Span::ranged("<example>", "Hello, world!", 0..5).start_idx(), Some(0));
    /// assert_eq!(Span::ranged("<example>", "Hello, world!", 7..=11).start_idx(), Some(7));
    /// assert_eq!(Span::ranged("<example>", "Hello, world!", 7..=6).start_idx(), Some(7));
    /// assert_eq!(Span::ranged("<example>", "Hello, world!", 0..=42).start_idx(), Some(0));
    /// assert_eq!(Span::ranged("<example>", "Hello, world!", 42..).start_idx(), Some(42));
    /// assert_eq!(Span::ranged("<example>", "Hello, world!", 42..=42).start_idx(), Some(42));
    /// ```
    fn start_idx(&self) -> Option<usize>;
    
    /// Returns the end position, as an index, of this Spanning object.
    /// 
    /// Note that not all Spanned values might have sensible start positions. [`Span`]s created with [`Span::empty()`], for example, have no sensible end; nor do Spans over empty sources.
    /// 
    /// Note that this function is typically only useful for functions really working with the internals of a span, since the index is relative to the source.
    /// 
    /// # Returns
    /// The end position if any is defined. If not, [`None`] is returned.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{Span, Spanning as _};
    /// 
    /// assert_eq!(Span::new("<example>", "").end_idx(), None);
    /// assert_eq!(Span::new("<example>", "Hello, world!").end_idx(), Some(12));
    /// assert_eq!(Span::empty("<example>", "Hello, world!").end_idx(), None);
    /// assert_eq!(Span::ranged("<example>", "Hello, world!", 0..5).end_idx(), Some(4));
    /// assert_eq!(Span::ranged("<example>", "Hello, world!", 7..=11).end_idx(), Some(11));
    /// assert_eq!(Span::ranged("<example>", "Hello, world!", 7..=6).end_idx(), Some(6));
    /// assert_eq!(Span::ranged("<example>", "Hello, world!", 0..=42).end_idx(), Some(42));
    /// assert_eq!(Span::ranged("<example>", "Hello, world!", 42..).end_idx(), Some(12));
    /// assert_eq!(Span::ranged("<example>", "Hello, world!", 42..=42).end_idx(), Some(42));
    /// ```
    fn end_idx(&self) -> Option<usize>;
}
impl<'f, 's, T: Spanning<'f, 's>> Spanning<'f, 's> for &'_ T {
    #[inline]
    fn file(&self) -> &'f str { T::file(self) }
    #[inline]
    fn source(&self) -> &'s str { T::source(self) }

    #[inline]
    fn start_idx(&self) -> Option<usize> { T::start_idx(self) }
    #[inline]
    fn end_idx(&self) -> Option<usize> { T::end_idx(self) }
}
impl<'f, 's, T: Spanning<'f, 's>> Spanning<'f, 's> for &'_ mut T {
    #[inline]
    fn file(&self) -> &'f str { T::file(self) }
    #[inline]
    fn source(&self) -> &'s str { T::source(self) }

    #[inline]
    fn start_idx(&self) -> Option<usize> { T::start_idx(self) }
    #[inline]
    fn end_idx(&self) -> Option<usize> { T::end_idx(self) }
}

/// Abstracts over the specific implementation of a span. This allows us to have varying levels of references VS non-references while avoiding lifetime hell.
/// 
/// SpanningExt implements the typical usage functions of a spanning type, and is purely implemented by the internal functions provided by [`Spanning`].
/// 
/// # Example
/// ```rust
/// use ast_toolkit::{Position, Spanning, SpanningExt};
/// 
/// struct HelloWorldSpan {
///     start : Option<usize>,
///     end   : Option<usize>,
/// }
/// impl Spanning<'static, 'static> for HelloWorldSpan {
///     fn file(&self) -> &'static str { "<example>" }
///     fn source(&self) -> &'static str { "Hello,\nworld!" }
/// 
///     fn start_idx(&self) -> Option<usize> { self.start }
///     fn end_idx(&self) -> Option<usize> { self.end }
/// }
/// impl SpanningExt<'static, 'static> for HelloWorldSpan {}
/// 
/// assert_eq!(HelloWorldSpan { start: Some(0), end: Some(12) }.pos_of(7), Position::new1(2, 1));
/// assert_eq!(HelloWorldSpan { start: Some(0), end: Some(4) }.text(), "Hello");
/// assert_eq!(HelloWorldSpan { start: Some(0), end: Some(1) }.len(), 2);
/// // ...
/// ```
pub trait SpanningExt<'f, 's>: Spanning<'f, 's> {
    /// Converts a source-relative index to a [`Position`].
    /// 
    /// # Arguments
    /// - `idx`: The index, as a [`usize`]-like, to convert to a [`Position`].
    /// 
    /// # Returns
    /// A new [`Position`] that represents the line and column number of this [`Span`]'s range start.
    /// 
    /// # Panics
    /// This function may panic if the given index is out-of-range for the internal source or if the given index is not on a grapheme boundary.
    /// 
    /// # Example
    /// ```rust
    /// # use std::panic::catch_unwind;
    /// use ast_toolkit::{Position, Span, SpanningExt as _};
    /// 
    /// assert_eq!(Span::new("<example>", "Hello,\nworld!").pos_of(0), Position::new1(1, 1));
    /// assert_eq!(Span::new("<example>", "Hello,\nworld!").pos_of(5), Position::new1(1, 6));
    /// assert_eq!(Span::new("<example>", "Hello,\nworld!").pos_of(6), Position::new1(1, 7));
    /// assert_eq!(Span::new("<example>", "Hello,\nworld!").pos_of(7), Position::new1(2, 1));
    /// assert_eq!(Span::new("<example>", "Hello,\nworld!").pos_of(12), Position::new1(2, 6));
    /// 
    /// // Out-of-range
    /// assert!(catch_unwind(|| Span::new("<example>", "Hello,\nworld!").pos_of(42)).is_err());
    /// // Not on a grapheme boundary (i.e., byte-wise, 2 is in the middle of `ÿ`)
    /// assert!(catch_unwind(|| Span::new("<example>", "Hÿllo,\nworld!").pos_of(2)).is_err());
    /// ```
    #[track_caller]
    fn pos_of(&self, idx: impl AsPrimitive<usize>) -> Position {
        let idx: usize = idx.as_();
        let source: &str = self.source();

        // Assert it is correctly sized
        if idx >= source.len() { panic!("Given index '{}' is out-of-bounds for Span of length {}", idx, source.len()); }

        // Iterate over the source to find the line & column
        let (mut line, mut col): (usize, usize) = (0, 0);
        for (i, c) in source.grapheme_indices(true) {
            // If we reached it, we done
            if i == idx { break; }
            else if i > idx { panic!("Index {idx} does not point on the grapheme boundary"); }

            // Otherwise, count
            if c == "\n" { line += 1; col = 0; }
            else { col += 1; }
        }

        // Done, return it as a position
        Position::new0(line, col)
    }

    /// Returns the start position of this range in the source text.
    /// 
    /// Note that not all Spanned values might have sensible start positions. [`Span`]s created with [`Span::empty()`], for example, have no sensible start; nor do Spans over empty sources.
    /// 
    /// # Returns
    /// The start [`Position`] of this range in the source text, or [`None`] if the range doesn't have a sensible start.
    /// 
    /// # Panics
    /// This function may panic if the internal start index is not on a grapheme boundary.
    /// 
    /// # Example
    /// ```rust
    /// # use std::panic::catch_unwind;
    /// use ast_toolkit::{Position, Span, SpanningExt as _};
    /// 
    /// assert_eq!(Span::new("<example>", "Hello,\nworld!").start(), Some(Position::new1(1, 1)));
    /// assert_eq!(Span::empty("<example>", "Hello,\nworld!").start(), None);
    /// assert_eq!(Span::ranged("<example>", "Hello,\nworld!", 5..=5).start(), Some(Position::new1(1, 6)));
    /// assert_eq!(Span::ranged("<example>", "Hello,\nworld!", 7..12).start(), Some(Position::new1(2, 1)));
    /// assert_eq!(Span::ranged("<example>", "Hello,\nworld!", 12..).start(), Some(Position::new1(2, 6)));
    /// assert_eq!(Span::ranged("<example>", "Hello,\nworld!", 42..=42).start(), None);
    /// 
    /// // Not on a grapheme boundary (i.e., byte-wise, 2 is in the middle of `ÿ`)
    /// assert!(catch_unwind(|| Span::ranged("<example>", "Hÿllo,\nworld!", 2..=2).start()).is_err());
    /// ```
    #[inline]
    #[track_caller]
    fn start(&self) -> Option<Position> {
        self.start_idx().map(|start| if start < self.source().len() { Some(self.pos_of(start)) } else { None }).flatten()
    }

    /// Returns the end position of this range in the source text.
    /// 
    /// Note that not all Spanned values might have sensible end positions. [`Span`]s created with [`Span::empty()`], for example, have no sensible end; nor do Spans over empty sources.
    /// 
    /// # Returns
    /// The end [`Position`] of this range in the source text, or [`None`] if the range doesn't have a sensible end.
    /// 
    /// # Panics
    /// This function may panic if the internal end index is not on a grapheme boundary.
    /// 
    /// # Example
    /// ```rust
    /// # use std::panic::catch_unwind;
    /// use ast_toolkit::{Position, Span, SpanningExt as _};
    /// 
    /// assert_eq!(Span::new("<example>", "Hello,\nworld!").end(), Some(Position::new1(2, 6)));
    /// assert_eq!(Span::empty("<example>", "Hello,\nworld!").end(), None);
    /// assert_eq!(Span::ranged("<example>", "Hello,\nworld!", 5..=5).end(), Some(Position::new1(1, 6)));
    /// assert_eq!(Span::ranged("<example>", "Hello,\nworld!", 7..12).end(), Some(Position::new1(2, 5)));
    /// assert_eq!(Span::ranged("<example>", "Hello,\nworld!", 12..).end(), Some(Position::new1(2, 6)));
    /// assert_eq!(Span::ranged("<example>", "Hello,\nworld!", 42..=42).end(), None);
    /// 
    /// // Not on a grapheme boundary (i.e., byte-wise, 2 is in the middle of `ÿ`)
    /// assert!(catch_unwind(|| Span::ranged("<example>", "Hÿllo,\nworld!", 2..=2).end()).is_err());
    /// ```
    #[inline]
    #[track_caller]
    fn end(&self) -> Option<Position> {
        self.end_idx().map(|end| if end < self.source().len() { Some(self.pos_of(end)) } else { None }).flatten()
    }



    /// Returns the spanned source text.
    /// 
    /// Very similar to [`Spanning::source()`], except this _does_ take the internal ranges into account.
    /// 
    /// # Returns
    /// A string slice containing the spanned source text.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{Span, SpanningExt as _};
    /// 
    /// assert_eq!(Span::new("<example>", "Hello,\nworld!").text(), "Hello,\nworld!");
    /// assert_eq!(Span::empty("<example>", "Hello,\nworld!").text(), "");
    /// assert_eq!(Span::ranged("<example>", "Hello,\nworld!", 0..5).text(), "Hello");
    /// assert_eq!(Span::ranged("<example>", "Hello,\nworld!", 7..12).text(), "world");
    /// assert_eq!(Span::ranged("<example>", "Hello,\nworld!", 7..).text(), "world!");
    /// assert_eq!(Span::ranged("<example>", "Hello,\nworld!", 7..6).text(), "");
    /// ```
    fn text(&'_ self) -> &'s str {
        let source: &str = self.source();
        match (self.start_idx(), self.end_idx()) {
            (Some(start), Some(end)) => if start < source.len() && end < source.len() && start <= end { &source[start..=end] } else { "" },
            (None, _) | (_, None)    => "",
        }
    }

    /// Returns the lines of the original source containing the spanned text.
    /// 
    /// This similar to [`SpanningExt::text()`], except that the start and end are more liberal to include their entire lines.
    /// 
    /// # Returns
    /// A string slice containing the spanned source text.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{Span, SpanningExt as _};
    /// 
    /// assert_eq!(Span::new("<example>", "Hello,\nworld!").lines(), "Hello,\nworld!");
    /// assert_eq!(Span::empty("<example>", "Hello,\nworld!").lines(), "");
    /// assert_eq!(Span::ranged("<example>", "Hello,\nworld!", 0..5).lines(), "Hello,\n");
    /// assert_eq!(Span::ranged("<example>", "Hello,\nworld!", 6..12).lines(), "Hello,\nworld!");
    /// assert_eq!(Span::ranged("<example>", "Hello,\nworld!", 7..12).lines(), "world!");
    /// assert_eq!(Span::ranged("<example>", "Hello,\nworld!", 7..).lines(), "world!");
    /// assert_eq!(Span::ranged("<example>", "Hello,\nworld!", 7..=6).lines(), "");
    /// assert_eq!(Span::ranged("<example>", "Hello,\nworld!", 7..=42).lines(), "world!");
    /// assert_eq!(Span::ranged("<example>", "Hello,\nworld!", 42..=42).lines(), "");
    /// ```
    #[inline]
    fn lines(&'_ self) -> &'s str {
        let source: &str = self.source();
        match find_lines_box(source, self.start_idx(), self.end_idx()) {
            (Some(start), Some(end)) => if start < source.len() && end < source.len() && start <= end {
                // Fully in bounds
                &source[start..=end]
            } else if start < source.len() && start <= end {
                // Now we know the source is non-empty & start is within bounds; clip
                &source[start..]
            } else {
                // Evaluates to an empty slice
                ""
            },

            // The rest is always empty
            (_, _) => "",
        }
    }

 

    /// Returns if this Span would return a non-empty text.
    /// 
    /// # Returns
    /// True if this Span spans nothing, or false otherwise.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{Span, SpanningExt as _};
    /// 
    /// assert_eq!(Span::new("<example>", "Hello, world!").is_empty(), false);
    /// assert_eq!(Span::empty("<example>", "Hello, world!").is_empty(), true);
    /// assert_eq!(Span::ranged("<example>", "Hello, world!", 0..=4).is_empty(), false);
    /// assert_eq!(Span::ranged("<example>", "Hello, world!", 6..=5).is_empty(), true);
    /// ```
    #[inline]
    fn is_empty(&self) -> bool { self.len() == 0 }

    /// Returns the length of the spanned area.
    /// 
    /// Note that this length takes into account the length of the source text.
    /// 
    /// # Returns
    /// The number of characters (or other logical unit) spanned.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{Span, SpanningExt as _};
    /// 
    /// assert_eq!(Span::new("<example>", "Hello, world!").len(), 13);
    /// assert_eq!(Span::ranged("<example>", "Hello, world!", 0..=4).len(), 5);
    /// assert_eq!(Span::ranged("<example>", "Hello, world!", 7..=12).len(), 6);
    /// assert_eq!(Span::ranged("<example>", "Hello, world!", 5..=5).len(), 1);
    /// assert_eq!(Span::ranged("<example>", "Hello, world!", 6..=5).len(), 0);
    /// assert_eq!(Span::ranged("<example>", "Hello, world!", 0..=42).len(), 13);
    /// ```
    #[inline]
    fn len(&self) -> usize {
        let source: &str = self.source();
        match (self.start_idx(), self.end_idx()) {
            (Some(start), Some(end)) => if start <= end && end < source.len() { 1 + end - start } else if start <= end { source.len() } else { 0 },
            (_, _)                   => 0,
        }
    }
}

/// Abstracts over [`Spanning`]-capable types that may be joined together to form a new instance of `self`.
/// 
/// # Example
/// ```rust
/// 
/// ```
pub trait Combining<'f, 's, Other = Self>: Spanning<'f, 's> where Self: Sized {
    /// Constructor for Self that encapsulates `Self` and some other range.
    /// 
    /// # Arguments
    /// - `left`: The first span to take into account, which is Self.
    /// - `right`: The second span to take into account, which is some other range.
    /// 
    /// # Returns
    /// A new instance of Self that spans both input ranges and everything in between.
    /// 
    /// # Panics
    /// This function panics if the two given ranges are not suitable to join, e.g., they are from different source texts.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{Combining as _, Span, SpanningExt as _};
    /// 
    /// let file: &str = "<example>";
    /// let text: &str = "Hello, world!";
    /// let span1 = Span::ranged(file, text, 0..=4);
    /// let span2 = Span::ranged(file, text, 7..=12);
    /// 
    /// assert_eq!(Span::combined(span1, span2).text(), "Hello, world!");
    /// ```
    #[track_caller]
    fn combined(span1: impl Into<Self>, span2: Other) -> Self;



    /// Will expand the range in Self to include the given range.
    /// 
    /// # Arguments
    /// - `other`: The other range to consume.
    /// 
    /// # Returns
    /// A reference to self for chaining.
    /// 
    /// # Panics
    /// This function panics if the two given ranges are not suitable to join, e.g., they are from different source texts.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{Combining as _, Span, SpanningExt as _};
    /// 
    /// let file: &str = "<example>";
    /// let text: &str = "Hello, world!";
    /// let mut span1 = Span::ranged(file, text, 0..=4);
    /// let span2 = Span::ranged(file, text, 7..=12);
    /// 
    /// span1.consume(span2);
    /// assert_eq!(span1.text(), "Hello, world!");
    /// ```
    #[inline]
    #[track_caller]
    fn consume(&mut self, other: Other) -> &mut Self where Self: Clone {
        // Define in terms of combined
        *self = Self::combined(self.clone(), other.into());
        self
    }
}

/// Auxillary trait allowing us to cast types to a [`Span`].
/// 
/// The kicker is that this is always implemented for all [`Spanning`] types.
pub trait IntoSpan<'f, 's> {
    /// Turns this type into a [`Span`].
    /// 
    /// # Returns
    /// A new [`Span`] that can be interchangably used.
    fn into_span(&self) -> Span<'f, 's>;
}
impl<'f, 's, T: Spanning<'f, 's>> IntoSpan<'f, 's> for T {
    #[inline]
    fn into_span(&self) -> Span<'f, 's> {
        Span {
            file   : self.file(),
            source : self.source(),
            range  : (self.start_idx(), self.end_idx()),
        }
    }
}



/// Allows one to span over a string reference.
/// 
/// This [`Spanning`]-capable type is optimised for parsing and keeping in ASTs, such as with [`nom`] (see the `nom`-feature).
/// 
/// # Example
/// ```rust
/// use std::borrow::Cow;
/// use std::path::PathBuf;
/// use ast_toolkit::{Position, Span, Spanning as _, SpanningExt as _};
/// 
/// // Create some strings
/// let file: String = PathBuf::from("/tmp/test").display().to_string();
/// let bytes: Cow<str> = String::from_utf8_lossy(b"Hello, world!");
/// 
/// // Build spans over them!
/// let span1 = Span::new("<example>", "Hello, world!");
/// let span2 = Span::new(file.as_str(), "Hello, world!");
/// let span3 = Span::new("<example>", bytes.as_ref());
/// 
/// // Use them!
/// assert_eq!(span1.text(), "Hello, world!");
/// assert_eq!(span2.file(), "/tmp/test");
/// assert_eq!(span3.end(), Some(Position::new1(1, 13)));
/// ```
#[derive(Clone, Copy, Debug, Eq, Hash)]
pub struct Span<'f, 's> {
    /// The filename (or other description) of the file we are spanning.
    pub file   : &'f str,
    /// The entire source text to snippet.
    pub source : &'s str,
    /// The range spanned.
    /// 
    /// Note that the following holds:
    /// - Both are inclusive bounds;
    /// - If either side is [`None`], the range is empty;
    /// - If `.0 > .1`, then the range is empty; and
    /// - Either may be out-of-range of the `source` still.
    pub range  : (Option<usize>, Option<usize>),
}

impl<'f, 's> Span<'f, 's> {
    /// Constructor for the Span that makes it span the entire given source text.
    /// 
    /// # Arguments
    /// - `file`: The filename or other identifier that helps the user distinguish between source texts.
    /// - `source`: The source text to span.
    /// 
    /// # Returns
    /// A new instance of Self that covers the entire given `source`.
    /// 
    /// # Example
    /// ```rust
    /// use std::borrow::Cow;
    /// use std::path::PathBuf;
    /// use ast_toolkit::{Position, Span, Spanning as _, SpanningExt as _};
    /// 
    /// // Create some strings
    /// let file: String = PathBuf::from("/tmp/test").display().to_string();
    /// let bytes: Cow<str> = String::from_utf8_lossy(b"Hello, world!");
    /// 
    /// // Build spans over them!
    /// let span1 = Span::new("<example>", "Hello, world!");
    /// let span2 = Span::new(file.as_str(), "Hello, world!");
    /// let span3 = Span::new("<example>", bytes.as_ref());
    /// 
    /// // Use them!
    /// assert_eq!(span1.text(), "Hello, world!");
    /// assert_eq!(span2.file(), "/tmp/test");
    /// assert_eq!(span3.end(), Some(Position::new1(1, 13)));
    /// ```
    #[inline]
    pub fn new(file: &'f str, source: &'s str) -> Self {
        let source_len: usize = source.len();
        Self {
            file,
            source,
            // The range is inclusive in the source _unless_ the source is empty; this is only representable using [`None`].
            range : (Some(0), if source_len > 0 { Some(source_len - 1) } else { None }),
        }
    }

    /// Constructor for a Span that has a source text but spans an empty (and undefined) part of it.
    /// 
    /// # Arguments
    /// - `file`: The filename or other identifier that helps the user distinguish between source texts.
    /// - `source`: The source text to span.
    /// 
    /// # Returns
    /// A new instance of Self that covers none of the given `source`.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{Span, SpanningExt as _};
    /// 
    /// let span1 = Span::empty("<example>", "Hello there!");
    /// assert_eq!(span1.text(), "");
    /// ```
    #[inline]
    pub fn empty(file: &'f str, source: &'s str) -> Self {
        Self {
            file,
            source,
            range : (None, None),
        }
    }

    /// Constructor for a Span that spans a particular range of the given source text.
    /// 
    /// # Arguments
    /// - `file`: The filename or other identifier that helps the user distinguish between source texts.
    /// - `source`: The source text to span.
    /// - `range`: Some [`RangeBounds`]-like range. If the start >= end in this range (or start > end if the end bound is inclusive too), then it's an empty range.
    /// 
    /// # Returns
    /// A new instance of Self that covers the given `source` partially.
    #[inline]
    pub fn ranged(file: &'f str, source: &'s str, range: impl RangeBounds<usize>) -> Self {
        let source_len: usize = source.len();
        Self {
            file,
            source,
            range : match (range.start_bound(), range.end_bound()) {
                (Bound::Excluded(start), Bound::Excluded(end)) => (if *start < usize::MAX { Some(*start + 1) } else { None }, if *end > 0 { Some(*end - 1) } else { None }),
                (Bound::Excluded(start), Bound::Included(end)) => (if *start < usize::MAX { Some(*start + 1) } else { None }, Some(*end)),
                (Bound::Excluded(start), Bound::Unbounded)     => (if *start < usize::MAX { Some(*start + 1) } else { None }, if source_len >0 { Some(source_len - 1) } else { None }),
                (Bound::Included(start), Bound::Excluded(end)) => (Some(*start), if *end > 0 { Some(*end - 1) } else { None }),
                (Bound::Included(start), Bound::Included(end)) => (Some(*start), Some(*end)),
                (Bound::Included(start), Bound::Unbounded)     => (Some(*start), if source_len >0 { Some(source_len - 1) } else { None }),
                (Bound::Unbounded, Bound::Excluded(end))       => (Some(0), if *end > 0 { Some(*end - 1) } else { None }),
                (Bound::Unbounded, Bound::Included(end))       => (Some(0), Some(*end)),
                (Bound::Unbounded, Bound::Unbounded)           => (Some(0), if source_len >0 { Some(source_len - 1) } else { None }),
            },
        }
    }
}
impl<'f, 's> Spanning<'f, 's> for Span<'f, 's> {
    #[inline]
    fn file(&self) -> &'f str { self.file }
    #[inline]
    fn source(&self) -> &'s str { self.source }

    #[inline]
    fn start_idx(&self) -> Option<usize> { self.range.0 }
    #[inline]
    fn end_idx(&self) -> Option<usize> { self.range.1 }
}
impl<'f, 's, T: Spanning<'f, 's>> Combining<'f, 's, T> for Span<'f, 's> {
    #[track_caller]
    fn combined(left: impl Into<Self>, right: T) -> Self {
        let left: Span = left.into();

        // Assert they talk about the same thing
        if left.file != right.file() { panic!("Given spans do not have the same `file`"); }
        if left.source != right.source() { panic!("Given spans do not have the same `source`"); }

        // Combine the start bounds into a new one
        let start: Option<usize> = match (left.range.0, right.start_idx()) {
            (Some(start1), Some(start2)) => Some(std::cmp::min(start1, start2)),
            (start1, None)               => start1,
            (None, start2)               => start2,
        };
        // Combine the end bounds into a new one
        let end: Option<usize> = match (left.range.1, right.end_idx()) {
            (Some(end1), Some(end2)) => Some(std::cmp::max(end1, end2)),
            (end1, None)             => end1,
            (None, end2)             => end2,
        };

        // Construct a new self out of it
        Self {
            file   : left.file,
            source : left.source,
            range  : ((start, end)),
        }
    }
}
impl<'f, 's> SpanningExt<'f, 's> for Span<'f, 's> {}

impl<'f, 's, T: SpanningExt<'f, 's>> PartialEq<T> for Span<'f, 's> {
    #[inline]
    fn eq(&self, other: &T) -> bool {
        self.file == other.file() && self.text() == self.text()
    }
}

impl<'f, 's> AsRef<str> for Span<'f, 's> {
    #[inline]
    fn as_ref(&self) -> &str { self.text() }
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

// [`nom`]-related stuff
#[cfg(feature = "nom")]
impl<'f, 's> nom::AsBytes for Span<'f, 's> {
    #[inline]
    fn as_bytes(&self) -> &[u8] { self.text().as_bytes() }
}
#[cfg(feature = "nom")]
impl<'f, 's, S: AsRef<str>> nom::Compare<S> for Span<'f, 's> {
    #[inline]
    fn compare(&self, t: S) -> nom::CompareResult {
        let s: &str = self.text();
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
        let s: &str = self.text();
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
        for b in self.text().bytes() {
            if b == token { return true; }
        }
        false
    }
}
#[cfg(feature = "nom")]
impl<'f, 's> nom::FindToken<char> for Span<'f, 's> {
    #[track_caller]
    fn find_token(&self, token: char) -> bool {
        for c in self.text().chars() {
            if c == token { return true; }
        }
        false
    }
}
#[cfg(feature = "nom")]
impl<'f, 's, 's2> nom::FindToken<&'s2 str> for Span<'f, 's> {
    #[track_caller]
    fn find_token(&self, token: &'s2 str) -> bool {
        for c in self.text().graphemes(true) {
            if c == token { return true; }
        }
        false
    }
}
#[cfg(feature = "nom")]
impl<'f, 's, T: AsRef<str>> nom::FindSubstring<T> for Span<'f, 's> {
    fn find_substring(&self, substr: T) -> Option<usize> {
        let source: &str = self.text();
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
    fn input_len(&self) -> usize { self.len() }
}
#[cfg(feature = "nom")]
impl<'f, 's> nom::InputTake for Span<'f, 's> {
    #[inline]
    #[track_caller]
    fn take(&self, count: usize) -> Self {
        // Panic if out-of-range (required by the function itself)
        let self_len: usize = self.len();
        if count > self_len { panic!("Given count {} is out-of-range for Span of size {}", count, self_len); }

        // Find a new range for the taken span, which is easy because we know that count <= end
        let range: (Option<usize>, Option<usize>) = match self.range.0 {
            Some(start) => if count > 0 { (Some(start), Some(start + count - 1)) } else { (Some(start), None) },
            None        => (None, None),
        };

        // Return a new Span with that
        Span {
            file   : self.file,
            source : self.source,
            range,
        }
    }

    #[inline]
    #[track_caller]
    fn take_split(&self, count: usize) -> (Self, Self) {
        // Panic if out-of-range (required by the function itself)
        let self_len: usize = self.len();
        if count > self_len { panic!("Given count {} is out-of-range for Span of size {}", count, self_len); }

        // Compute the range of the second span
        let range: (Option<usize>, Option<usize>) = match self.range.0 {
            Some(start) => (Some(start + count), self.range.1),
            None        => (None, self.range.1),
        };

        // We can return both of them, but note the reverse order 'cuz nom!
        (
            Span {
                file   : self.file,
                source : self.source,
                range,
            },
            self.take(count),
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
        use nom::InputTake as _;

        let mut chars = self.text().char_indices();
        while let Some((i, c)) = chars.next() {
            // Check if this is the character the user is looking for
            if predicate(c) {
                // It is; so perform the split at this location
                // (note that we can pass i because the fact that `take_split()` takes a count, i.e., the index of the first element in the remainder of the split. And that's what we want here too!)
                return Ok(self.take_split(i));
            }
        }
        Err(nom::Err::Incomplete(nom::Needed::Unknown))
    }
    fn split_at_position1<P, E: nom::error::ParseError<Self>>(&self, predicate: P, e: nom::error::ErrorKind) -> nom::IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        use nom::InputTake as _;

        let mut chars = self.text().char_indices();
        while let Some((i, c)) = chars.next() {
            // Check if this is the character the user is looking for
            if predicate(c) {
                // It is; so perform the split at this location
                // (note that we can pass i because the fact that `take_split()` takes a count, i.e., the index of the first element in the remainder of the split. And that's what we want here too!)
                let (rem, split): (Span, Span) = self.take_split(i);
                if split.is_empty() { return Err(nom::Err::Error(E::from_error_kind(*self, e))); }
                return Ok((rem, split));
            }
        }
        Err(nom::Err::Incomplete(nom::Needed::Unknown))
    }

    fn split_at_position1_complete<P, E: nom::error::ParseError<Self>>(&self, predicate: P, e: nom::error::ErrorKind) -> nom::IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        use nom::InputTake as _;

        let mut chars = self.text().char_indices();
        while let Some((i, c)) = chars.next() {
            // Check if this is the character the user is looking for
            if predicate(c) {
                // It is; so perform the split at this location
                // (note that we can pass i because the fact that `take_split()` takes a count, i.e., the index of the first element in the remainder of the split. And that's what we want here too!)
                let (rem, split): (Span, Span) = self.take_split(i);
                if split.is_empty() { return Err(nom::Err::Error(E::from_error_kind(*self, e))); }
                return Ok((rem, split));
            }
        }

        // Instead of crashing, return the full slice
        Ok((Span::ranged(self.file, self.source, self.source.len()..self.source.len()), *self))
    }
    fn split_at_position_complete<P, E: nom::error::ParseError<Self>>(&self, predicate: P) -> nom::IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        use nom::InputTake as _;

        let mut chars = self.text().char_indices();
        while let Some((i, c)) = chars.next() {
            // Check if this is the character the user is looking for
            if predicate(c) {
                // It is; so perform the split at this location
                // (note that we can pass i because the fact that `take_split()` takes a count, i.e., the index of the first element in the remainder of the split. And that's what we want here too!)
                return Ok(self.take_split(i));
            }
        }

        // Instead of crashing, return the full slice
        Ok((Span::ranged(self.file, self.source, self.source.len()..self.source.len()), *self))
    }
}
#[cfg(feature = "nom")]
impl<'f, 's> nom::Offset for Span<'f, 's> {
    #[inline]
    #[track_caller]
    fn offset(&self, second: &Self) -> usize {
        // Compare the starts
        match (self.range.0, second.range.0) {
            (Some(start1), Some(start2)) => start2 - start1,
            (_, _)                       => { panic!("Cannot get offset of Span that has [`None`] as start"); },
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
        let range: (Option<usize>, Option<usize>) = match (self.range, range) {
            ((Some(start1), Some(end1)), (start2, Some(end2))) => (Some(start1 + start2), Some(std::cmp::min(end1, start1 + end2))),
            ((Some(start1), _), (start2, _))                   => (Some(start1 + start2), None),
            ((None, _), (_, _))                                => (None, None),
        };

        // Create outselves with it
        Self {
            file   : self.file,
            source : self.source,
            range,
        }
    }
}
