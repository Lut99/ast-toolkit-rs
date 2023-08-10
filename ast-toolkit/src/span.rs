//  SPAN.rs
//    by Lut99
// 
//  Created:
//    02 Jul 2023, 16:40:44
//  Last edited:
//    10 Aug 2023, 21:19:05
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines the [`Span`] (and [`Position`]) structs which we use to keep
//!   track of a node's position in the source text.
// 

use std::fmt::Display;
use std::ops::Deref;

use num_traits::AsPrimitive;
use unicode_segmentation::{Graphemes, GraphemeIndices, UnicodeSegmentation as _};

use crate::position::Position;


/***** TESTS *****/
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_from_pos() {
        let span: Span<&str, &str> = Span::from_pos("<example>", "Hello, World!", Position::new1(1, 1), Position::new1(1, 1));
        assert_eq!(span.text(), "H");
        let span: Span<&str, &str> = Span::from_pos("<example>", "Hello, World!", Position::new1(1, 1), Position::new1(1, 5));
        assert_eq!(span.text(), "Hello");

        let span: Span<&str, &str> = Span::from_pos("<example>", "Hello\nWorld!", Position::new1(1, 1), Position::new1(1, 1));
        assert_eq!(span.text(), "H");
        let span: Span<&str, &str> = Span::from_pos("<example>", "Hello\nWorld!", Position::new1(1, 1), Position::new1(1, 5));
        assert_eq!(span.text(), "Hello");
        let span: Span<&str, &str> = Span::from_pos("<example>", "Hello\nWorld!", Position::new1(2, 1), Position::new1(2, 1));
        assert_eq!(span.text(), "W");
        let span: Span<&str, &str> = Span::from_pos("<example>", "Hello\nWorld!", Position::new1(2, 1), Position::new1(2, 5));
        assert_eq!(span.text(), "World");
        let span: Span<&str, &str> = Span::from_pos("<example>", "Hello\nWorld!", Position::new1(1, 1), Position::new1(2, 6));
        assert_eq!(span.text(), "Hello\nWorld!");
    }
}

#[cfg(feature = "nom")]
#[cfg(test)]
mod nom_tests {
    use super::*;

    // #[test]
    // fn test_span_nom_as_bytes() {
    //     // Create a few spans and see if they byte version equates what we expect
    //     assert_eq!(<Span<&str, &str> as nom::AsBytes>::as_bytes(&Span::new("<example>", "Example text")), b"Example text");
    //     assert_eq!(<Span<&str, &str> as nom::AsBytes>::as_bytes(&Span::from_idx("<example>", "Example text", 0, 6)), b"Example");
    //     assert_eq!(<Span<&str, &str> as nom::AsBytes>::as_bytes(&Span::from_idx("<example>", "Example text", 8, 11)), b"text");
    //     assert_eq!(<Span<&str, &str> as nom::AsBytes>::as_bytes(&Span::from_idx("<example>", "Example text", 3, 9)), b"mple te");
    // }
    // #[test]
    // fn test_span_nom_input_iter() {
    //     // Try some iterations
    //     let target: &str = "Example text";
    //     for (i, b) in <Span<&str, &str> as nom::InputIter>::iter_indices(&Span::new("<example>", target)) {
    //         assert_eq!(b, target.as_bytes()[i]);
    //     }
    // }
//     #[test]
//     fn test_span_nom_input_length() {
//         // Create a few spans and see if their length matches with what we expect
//         assert_eq!(<Span<&str, &str> as nom::InputLength>::input_len(&Span::new("<example>", "Example text")), 12);
//         assert_eq!(<Span<&str, &str> as nom::InputLength>::input_len(&Span::from_idx("<example>", "Example text", 0, 6)), 7);
//         assert_eq!(<Span<&str, &str> as nom::InputLength>::input_len(&Span::from_idx("<example>", "Example text", 8, 11)), 4);
//         assert_eq!(<Span<&str, &str> as nom::InputLength>::input_len(&Span::from_idx("<example>", "Example text", 3, 9)), 7);
//     }
//     #[test]
//     fn test_span_nom_input_take() {
//         // See if we can take and split how we expect
//         assert_eq!(<Span<&str, &str> as nom::InputTake>::take(&Span::new("<example>", "Example text"), 7), Span::from_idx("<example>", "Example text", 0, 6));
//         assert_eq!(<Span<&str, &str> as nom::InputTake>::take(&Span::from_idx("<example>", "Example text", 8, 11), 3), Span::from_idx("<example>", "Example text", 8, 10));
//         assert!(std::panic::catch_unwind(|| <Span<&str, &str> as nom::InputTake>::take(&Span::from_idx("<example>", "Example text", 8, 11), 0)).is_err());

//         // Now compare the split
//         assert_eq!(<Span<&str, &str> as nom::InputTake>::take_split(&Span::new("<example>", "Example text"), 7), (Span::from_idx("<example>", "Example text", 0, 6), Span::from_idx("<example>", "Example text", 7, 11)));
//         assert_eq!(<Span<&str, &str> as nom::InputTake>::take_split(&Span::from_idx("<example>", "Example text", 8, 11), 3), (Span::from_idx("<example>", "Example text", 8, 10), Span::from_idx("<example>", "Example text", 11, 11)));
//         assert!(std::panic::catch_unwind(|| <Span<&str, &str> as nom::InputTake>::take_split(&Span::from_idx("<example>", "Example text", 8, 11), 0)).is_err());
//     }
//     #[test]
//     fn test_span_nom_compare() {
//         // Do some comparisons
//         assert_eq!(<Span<&str, &str> as nom::Compare<&str>>::compare(&Span::new("<example>", "Example text"), "Example text"), nom::CompareResult::Ok);
//         assert_eq!(<Span<&str, &str> as nom::Compare<&str>>::compare(&Span::from_idx("<example>", "Example text", 0, 6), "Example"), nom::CompareResult::Ok);
//         assert_eq!(<Span<&str, &str> as nom::Compare<&str>>::compare(&Span::from_idx("<example>", "Example text", 8, 11), "text"), nom::CompareResult::Ok);
//         assert_eq!(<Span<&str, &str> as nom::Compare<&str>>::compare(&Span::new("<example>", "Example text"), "Example text 2"), nom::CompareResult::Incomplete);
//         assert_eq!(<Span<&str, &str> as nom::Compare<&str>>::compare(&Span::new("<example>", "Example text"), "Example2 text"), nom::CompareResult::Error);
//     }
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
/// let _span: Span<&str, &str> = Span::new("<example>", "Hello, world!");
/// let _span: Span<String, &str> = Span::new(PathBuf::from("/tmp/test").display().to_string(), "Hello, world!");
/// let span: Span<&str, Cow<str>> = Span::new("<example>", String::from_utf8_lossy(b"Hello, world!"));
/// 
/// assert_eq!(span.text(), "Hello, world!");
/// assert_eq!(span.start().line, 0);
/// assert_eq!(span.start().col, 0);
/// assert_eq!(span.end().line, 0);
/// assert_eq!(span.end().col, 12);
/// ```
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
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
    /// let _span: Span<&str, &str> = Span::new("<example>", "Hello, world!");
    /// let _span: Span<String, &str> = Span::new(PathBuf::from("/tmp/test").display().to_string(), "Hello, world!");
    /// let _span: Span<&str, Cow<str>> = Span::new("<example>", String::from_utf8_lossy(b"Hello, world!"));
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
    /// let span1: Span<&str, &str> = Span::from_pos("<example>", "Hello, world!", Position::new0(0, 0), Position::new0(0, 4));
    /// let span2: Span<String, &str> = Span::from_pos(PathBuf::from("/tmp/test").display().to_string(), "Hello, world!", Position::new0(0, 7), Position::new0(0, 11));
    /// let span3: Span<&str, Cow<str>> = Span::from_pos("<example>", String::from_utf8_lossy(b"Hello, world!"), Position::new0(0, 0), Position::new0(0, 12));
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
        for (i, c) in source.iter_indices() {
            // If we've reached the end of any, mark it
            if rstart.is_none() && istart.line == 0 && istart.col == 0 { rstart = Some(i); }
            if rend.is_none() && iend.line == 0 && iend.col == 0 { rend = Some(i); break; }

            // Otherwise, count them down
            if istart.line > 0 && c.is_newline() {
                istart.line -= 1;
            } else if istart.line == 0 && istart.col > 0 {
                // If we're skipping a (non-terminating) newline, the Position is ill-formed
                istart.col -= 1;
            }
            if iend.line > 0 && c.is_newline() {
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
    /// let span1: Span<&str, &str> = Span::from_idx("<example>", "Hello, world!", 0, 4);
    /// let span2: Span<String, &str> = Span::from_idx(PathBuf::from("/tmp/test").display().to_string(), "Hello, world!", 7, 11);
    /// let span3: Span<&str, Cow<str>> = Span::from_idx("<example>", String::from_utf8_lossy(b"Hello, world!"), 0, 12);
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



    /// Converts a character index to a [`Position`] within this span's source text.
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
    /// let span1: Span<&str, &str> = Span::new("<example>", "Hello\nworld!");
    /// let span2: Span<&str, &str> = Span::from_idx("<example>", "Hello\nworld!", 0, 4);
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

        // Iterate over the source to find the line & column
        let (mut line, mut col): (usize, usize) = (0, 0);
        for (i, c) in self.source.grapheme_indices(true) {
            // If we reached it, we done
            if i == index { break; }
            else if i > index { panic!("Index {} does not point to grapheme boundary", index); }

            // Otherwise, count
            if c.is_newline() { line += 1; col = 0; }
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
    /// let span1: Span<&str, &str> = Span::new("<example>", "Hello\nworld!");
    /// let span2: Span<&str, &str> = Span::from_idx("<example>", "Hello\nworld!", 2, 2);
    /// let span3: Span<&str, &str> = Span::from_idx("<example>", "Hello\nworld!", 6, 10);
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
    /// let span1: Span<&str, &str> = Span::new("<example>", "Hello world!");
    /// let span2: Span<&str, &str> = Span::new("<example>", "Hello\nworld!");
    /// let span3: Span<&str, &str> = Span::from_idx("<example>", "Hello\nworld!", 2, 2);
    /// let span4: Span<&str, &str> = Span::from_idx("<example>", "Hello\nworld!", 6, 10);
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
impl<'f, 's> Span<'f, 's> {
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
    /// This function panics if the given spans do not have the same `file` or `source`.
    #[track_caller]
    pub fn combined(span1: impl Into<Span<'f, 's>>, span2: impl Into<Span<'f, 's>>) -> Self {
        let span1: Span = span1.into();
        let span2: Span = span2.into();

        // Assert they talk about the same thing
        if span1.file != span2.file { panic!("Given spans do not have the same `file`"); }
        if span1.source != span2.source { panic!("Given spans do not have the same `source`"); }

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


// nom-related things
#[cfg(feature = "nom")]
impl<'f, 's> nom::AsBytes for Span<'f, 's> {
    #[track_caller]
    fn as_bytes(&self) -> &[u8] {
        assert_range!(self.start, self.end, self.source);
        self.source.subset(self.start, self.end).as_bytes()
    }
}

// #[cfg(feature = "nom")]
// impl<F, S: Deref<Target = str>> nom::ExtendInto for Span<F, S> {
//     type Item = Span<F, S>;
//     type Extender = ();

//     fn new_builder(&self) -> Self::Extender {
        
//     }
//     fn extend_into(&self, acc: &mut Self::Extender) {
        
//     }
// }

// #[cfg(feature = "nom")]
// impl<F, S: Deref<Target = str>> nom::FindSubstring for Span<F, S> {
    
// }

// #[cfg(feature = "nom")]
// impl<F, S: Deref<Target = str>> nom::InputIter for Span<F, S> {
//     type Item = u8;
//     type Iter = std::iter::Enumerate<std::vec::IntoIter<u8>>;
//     type IterElem = std::vec::IntoIter<u8>;

//     #[inline]
//     fn iter_indices(&self) -> Self::Iter { self.text().as_bytes().to_vec().into_iter().enumerate() }
//     fn iter_elements(&self) -> Self::IterElem { self.text().as_bytes().to_vec().into_iter() }

//     #[inline]
//     fn position<P>(&self, predicate: P) -> Option<usize>
//     where
//         P: Fn(Self::Item) -> bool,
//     {
//         self.iter_indices().find_map(|(i, b)| if predicate(b) { Some(i) } else { None })
//     }
//     fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
//         use std::num::NonZeroUsize;

//         match self.iter_indices().nth(count) {
//             Some((i, _)) => Ok(i),
//             None => Err(match NonZeroUsize::new(self.len() - 1 - count) {
//                 Some(res) => nom::Needed::Size(res),
//                 None => nom::Needed::Unknown,
//             }),
//         }
//     }
// }
// #[cfg(feature = "nom")]
// impl<F, S> nom::InputLength for Span<F, S> {
//     #[track_caller]
//     fn input_len(&self) -> usize { 1 + (self.end - self.start) }
// }
// #[cfg(feature = "nom")]
// impl<F: Clone, S: Clone + Deref<Target = str>> nom::InputTake for Span<F, S> {
//     #[track_caller]
//     fn take(&self, count: usize) -> Self {
//         let self_len: usize = 1 + (self.end - self.start);
//         if count == 0 { panic!("Cannot take span of length 0"); }
//         if count > self_len { panic!("Cannot take span of length {count} from span of length {self_len}"); }
//         Span {
//             file   : self.file.clone(),
//             source : self.source.clone(),
//             start  : self.start,
//             end    : self.start + (count - 1),
//         }
//     }

//     #[track_caller]
//     fn take_split(&self, count: usize) -> (Self, Self) {
//         let self_len: usize = 1 + (self.end - self.start);
//         if count == 0 || count >= self_len { panic!("Cannot split span on index {count} in span of length {self_len}"); }
//         (
//             Span {
//                 file   : self.file.clone(),
//                 source : self.source.clone(),
//                 start  : self.start,
//                 end    : self.start + (count - 1),
//             },
//             Span {
//                 file   : self.file.clone(),
//                 source : self.source.clone(),
//                 start  : self.start + count,
//                 end    : self.end,
//             },
//         )
//     }
// }
// #[cfg(feature = "nom")]
// impl<F, S: Deref<Target = str>, S2: Deref<Target = str>> nom::Compare<S2> for Span<F, S> {
//     #[inline]
//     fn compare(&self, t: S2) -> nom::CompareResult {
//         <&str as nom::Compare<&str>>::compare(&&**self, &*t)
//     }

//     #[inline]
//     fn compare_no_case(&self, t: S2) -> nom::CompareResult {
//         <&str as nom::Compare<&str>>::compare_no_case(&&**self, &*t)
//     }
// }
