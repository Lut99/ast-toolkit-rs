//  SPAN.rs
//    by Lut99
// 
//  Created:
//    02 Jul 2023, 16:40:44
//  Last edited:
//    21 Aug 2023, 14:41:46
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines the [`Span`] (and [`Position`]) structs which we use to keep
//!   track of a node's position in the source text.
// 

use num_traits::AsPrimitive;
use unicode_segmentation::{Graphemes, GraphemeIndices, UnicodeSegmentation as _};

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
            assert_eq!(builder.text(), &code[std::cmp::min(span1.start, span2.start)..=std::cmp::max(span1.end, span2.end)]);
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
}





/***** HELPER MACROS *****/
/// Asserts that the given ranges are valid ranges
macro_rules! assert_range {
    ($start:expr, $end:expr, $source:expr) => {
        if $start >= $source.len() { panic!("Start position {} is out-of-range for source of {} characters", $start, $source.len()); }
        if $end >= $source.len() { panic!("End position {} is out-of-range for source of {} characters", $end, $source.len()); }
        if $start > $end { panic!("Start position {} is larger than end position {}", $start, $end); }
    };
}
pub(crate) use assert_range;





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
    /// The start position of this span (inclusive).
    pub start  : usize,
    /// The end position of this span (inclusive).
    pub end    : usize,
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
    #[track_caller]
    pub fn new(file: impl Into<&'f str>, source: impl Into<&'s str>) -> Self {
        let source: &str = source.into();
        let source_len: usize = source.len();
        if source_len == 0 { panic!("Cannot span an empty string"); }
        Self {
            file  : file.into(),
            source,
            start : 0,
            end   : source_len - 1,
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
    /// # Panics
    /// This function may panic if the given `start` or `end` are out-of-range for the given `source`, or if `start > end`.
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
    /// ```should_panic
    /// # use ast_toolkit::{Position, Span};
    /// // This will panic!
    /// Span::from_pos("<builtin>", "Hello, world!", Position::new0(1, 0), Position::new0(1, 12));
    /// Span::from_pos("<builtin>", "Hello, world!", Position::new0(0, 0), Position::new0(0, 15));
    /// Span::from_pos("<builtin>", "Hello, world!", Position::new0(0, 8), Position::new0(0, 6));
    /// ```
    #[track_caller]
    pub fn from_pos(file: &'f str, source: &'s str, start: impl Into<Position>, end: impl Into<Position>) -> Self {
        // Assert start <= end
        let (start, end): (Position, Position) = (start.into(), end.into());
        if start > end { panic!("Given start {start} is larger than given end {end}"); }

        // Examine the source to find the end
        let (mut istart, mut iend): (Position, Position) = (start, end);
        let (mut rstart, mut rend): (Option<usize>, Option<usize>) = (None, None);
        for (i, c) in source.grapheme_indices(true) {
            // If we've reached the end of any, mark it
            if rstart.is_none() && istart.line == 0 && istart.col == 0 { rstart = Some(i); }
            if rend.is_none() && iend.line == 0 && iend.col == 0 { rend = Some(i); break; }

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
        let start: usize = match rstart { Some(i) => i, None => { panic!("Start {} is out-of-bounds for source of {} characters", start, source.len()); } };
        let end: usize = match rend { Some(i) => i, None => { panic!("End {} is out-of-bounds for source of {} characters", end, source.len()); } };

        // Create self
        Self {
            file,
            source,
            start,
            end,
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
    /// # Panics
    /// This function may panic if the given `start` or `end` are out-of-range for the given `source`, or if `start > end`.
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
    /// 
    /// assert_eq!(span1.text(), "Hello");
    /// assert_eq!(span2.text(), "world");
    /// assert_eq!(span3.text(), "Hello, world!");
    /// ```
    /// ```should_panic
    /// # use ast_toolkit::Span;
    /// // This will panic!
    /// Span::from_idx("<builtin>", "Hello, world!", 0, 15);
    /// Span::from_idx("<builtin>", "Hello, world!", 8, 6);
    /// ```
    #[inline]
    #[track_caller]
    pub fn from_idx(file: &'f str, source: &'s str, start: impl AsPrimitive<usize>, end: impl AsPrimitive<usize>) -> Self {
        // Do a few asserts
        let (start, end): (usize, usize) = (start.as_(), end.as_());
        assert_range!(start, end, source);        

        // Return ourselves
        Self {
            file,
            source,
            start,
            end,
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
    #[track_caller]
    pub fn combined<'f2, 's2>(span1: impl AsRef<Span<'f, 's>>, span2: impl AsRef<Span<'f2, 's2>>) -> Self {
        let span1: &Span = span1.as_ref();
        let span2: &Span = span2.as_ref();

        // Assert they talk about the same thing
        if !std::ptr::eq(span1.file as *const str, span2.file as *const str) { panic!("Given spans do not have the same `file`"); }
        if !std::ptr::eq(span1.source as *const str, span2.source as *const str) { panic!("Given spans do not have the same `source`"); }

        // Compute the new range
        let start: usize = std::cmp::min(span1.start, span2.start);
        let end: usize = std::cmp::max(span1.end, span2.end);

        // Construct the new self
        Self {
            file   : span1.file,
            source : span1.source,
            start,
            end,
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

        // Compute the new range
        self.start = std::cmp::min(self.start, other.start);
        self.end = std::cmp::max(self.end, other.end);

        // Finally return self for chaining
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
    /// Span::new("<example>", "Hello\nworld!").pos_of(50);
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
    /// A [`Position`] describing the start position in the source text.
    /// 
    /// # Panics
    /// This function may panic if the internal `start`- or `end`-fields are not within bounds of the internal `source`, or if `start > end`. It may also panic if `start` does not point at the unicode grapheme boundary.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{Position, Span};
    /// 
    /// let span1 = Span::new("<example>", "Hello\nworld!");
    /// let span2 = Span::from_idx("<example>", "Hello\nworld!", 2, 2);
    /// let span3 = Span::from_idx("<example>", "Hello\nworld!", 6, 10);
    /// 
    /// assert_eq!(span1.start(), Position::new0(0, 0));
    /// assert_eq!(span2.start(), Position::new0(0, 2));
    /// assert_eq!(span3.start(), Position::new0(1, 0));
    /// ```
    /// ```should_panic
    /// # use ast_toolkit::Span;
    /// // This will panic!
    /// Span::from_idx("<example>", "Hello\nworld!", 0, 50).start();
    /// Span::from_idx("<example>", "Hello\nworld!", 50, 0).start();
    /// Span::from_idx("<example>", "Hÿllo\nworld!", 2, 6).start();
    /// ```
    #[inline]
    #[track_caller]
    pub fn start(&self) -> Position {
        // Assert some things
        assert_range!(self.start, self.end, self.source);
        self.pos_of(self.start)
    }

    /// Returns the end position of this span as a [`Position`].
    /// 
    /// # Returns
    /// A [`Position`] describing the end position in the source text.
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
    /// 
    /// assert_eq!(span1.end(), Position::new0(0, 11));
    /// assert_eq!(span2.end(), Position::new0(1, 5));
    /// assert_eq!(span3.end(), Position::new0(0, 2));
    /// assert_eq!(span4.end(), Position::new0(1, 4));
    /// ```
    /// ```should_panic
    /// # use ast_toolkit::Span;
    /// // This will panic!
    /// Span::from_idx("<example>", "Hello\nworld!", 0, 50).end();
    /// Span::from_idx("<example>", "Hello\nworld!", 50, 0).end();
    /// Span::from_idx("<example>", "Hÿllo\nworld!", 2, 6).end();
    /// ```
    #[inline]
    #[track_caller]
    pub fn end(&self) -> Position {
        // Assert some things
        assert_range!(self.start, self.end, self.source);
        self.pos_of(self.end)
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
    /// ```
    #[inline]
    #[track_caller]
    pub fn text(&self) -> &str {
        assert_range!(self.start, self.end, self.source);
        &self.source[self.start..=self.end]
    }



    /// Returns an iterator over the logical units in this Span.
    /// 
    /// # Returns
    /// A [`Range<S::Iter>`] iterator that returns only the spanned characters.
    #[inline]
    #[track_caller]
    pub fn iter(&self) -> Graphemes {
        assert_range!(self.start, self.end, self.source);
        self.source[self.start..=self.end].graphemes(true)
    }

    /// Returns an iterator over the logical units in this Span, annotating them with their physical index.
    /// 
    /// Note that for spans this is trivial, since it views the logical units of the underlying [`Spannable`] as its physical units (so they always align 1-to-1).
    /// 
    /// Therefore, this is equivalent to (but slightly more efficient that) calling [`self.iter().enumerate()`](Span::iter()).
    /// 
    /// # Returns
    /// A [`EnumerateRange<S::Iter>`] iterator that returns only the spanned characters.
    #[inline]
    pub fn iter_indices(&self) -> GraphemeIndices {
        assert_range!(self.start, self.end, self.source);
        self.source[self.start..=self.end].grapheme_indices(true)
    }

    /// Returns the number of logical units that this Span spans.
    /// 
    /// # Returns
    /// The number of characters.
    #[inline]
    pub fn len(&self) -> usize { self.end + 1 - self.start }
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
    #[track_caller]
    fn as_bytes(&self) -> &[u8] {
        assert_range!(self.start, self.end, self.source);
        self.source[self.start..=self.end].as_bytes()
    }
}
#[cfg(feature = "nom")]
impl<'f, 's, S: AsRef<str>> nom::Compare<S> for Span<'f, 's> {
    #[inline]
    #[track_caller]
    fn compare(&self, t: S) -> nom::CompareResult {
        assert_range!(self.start, self.end, self.source);
        let s: &str = &self.source[self.start..=self.end];
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
        assert_range!(self.start, self.end, self.source);
        let s: &str = &self.source[self.start..=self.end];
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
        assert_range!(self.start, self.end, self.source);
        for b in self.source[self.start..=self.end].bytes() {
            if b == token { return true; }
        }
        false
    }
}
#[cfg(feature = "nom")]
impl<'f, 's> nom::FindToken<char> for Span<'f, 's> {
    #[track_caller]
    fn find_token(&self, token: char) -> bool {
        assert_range!(self.start, self.end, self.source);
        for c in self.source[self.start..=self.end].chars() {
            if c == token { return true; }
        }
        false
    }
}
#[cfg(feature = "nom")]
impl<'f, 's, 's2> nom::FindToken<&'s2 str> for Span<'f, 's> {
    #[track_caller]
    fn find_token(&self, token: &'s2 str) -> bool {
        assert_range!(self.start, self.end, self.source);
        for c in self.source[self.start..=self.end].graphemes(true) {
            if c == token { return true; }
        }
        false
    }
}
#[cfg(feature = "nom")]
impl<'f, 's, T: AsRef<str>> nom::FindSubstring<T> for Span<'f, 's> {
    /// NOTE: We return the full index in the source so that it's compatible as idx for Spans.
    fn find_substring(&self, substr: T) -> Option<usize> {
        let source: &str = self.text();
        let substr: &str = substr.as_ref();
        for (i, _) in source.grapheme_indices(true) {
            if i + substr.len() <= source.len() && &source[i..i + substr.len()] == substr {
                return Some(self.start + i);
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
    fn input_len(&self) -> usize { 1 + (self.end - self.start) }
}
#[cfg(feature = "nom")]
impl<'f, 's> nom::InputTake for Span<'f, 's> {
    #[track_caller]
    fn take(&self, count: usize) -> Self {
        let self_len: usize = 1 + (self.end - self.start);
        if count == 0 { panic!("Cannot take span of length 0"); }
        if count > self_len { panic!("Cannot take span of length {count} from span of length {self_len}"); }
        Span {
            file   : self.file.clone(),
            source : self.source.clone(),
            start  : self.start,
            end    : self.start + (count - 1),
        }
    }

    #[track_caller]
    fn take_split(&self, count: usize) -> (Self, Self) {
        let self_len: usize = 1 + (self.end - self.start);
        if count == 0 || count >= self_len { panic!("Cannot split span on index {count} in span of length {self_len}"); }
        (
            Span {
                file   : self.file.clone(),
                source : self.source.clone(),
                start  : self.start,
                end    : self.start + (count - 1),
            },
            Span {
                file   : self.file.clone(),
                source : self.source.clone(),
                start  : self.start + count,
                end    : self.end,
            },
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
        for (i, c) in self.source[self.start..=self.end].char_indices() {
            if predicate(c) { return Ok((Span::from_idx(self.file, self.source, self.start, i - 1), Span::from_idx(self.file, self.source, i, self.end))); }
        }
        Err(nom::Err::Incomplete(nom::Needed::Unknown))
    }
    fn split_at_position1<P, E: nom::error::ParseError<Self>>(&self, predicate: P, e: nom::error::ErrorKind) -> nom::IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        for (i, c) in self.source[self.start..=self.end].char_indices() {
            if predicate(c) {
                if self.start == i || self.end == i { return Err(nom::Err::Failure(E::from_error_kind(*self, e))) }
                return Ok((Span::from_idx(self.file, self.source, self.start, i - 1), Span::from_idx(self.file, self.source, i, self.end)));
            }
        }
        Err(nom::Err::Incomplete(nom::Needed::Unknown))
    }

    fn split_at_position1_complete<P, E: nom::error::ParseError<Self>>(&self, predicate: P, e: nom::error::ErrorKind) -> nom::IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        
    }
    fn split_at_position_complete<P, E: nom::error::ParseError<Self>>(&self, predicate: P) -> nom::IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        
    }
}
