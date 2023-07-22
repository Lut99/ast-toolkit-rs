//  SPAN.rs
//    by Lut99
// 
//  Created:
//    02 Jul 2023, 16:40:44
//  Last edited:
//    22 Jul 2023, 13:36:44
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines the [`Span`] (and [`Position`]) structs which we use to keep
//!   track of a node's position in the source text.
// 

use std::cmp::Ordering;
use std::fmt::{Display, Formatter, Result as FResult};
use std::ops::Deref;

use num_traits::AsPrimitive;
use unicode_segmentation::UnicodeSegmentation as _;


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

    #[test]
    fn test_span_nom_as_bytes() {
        // Create a few spans and see if they byte version equates what we expect
        assert_eq!(<Span<&str, &str> as nom::AsBytes>::as_bytes(&Span::new("<example>", "Example text")), b"Example text");
        assert_eq!(<Span<&str, &str> as nom::AsBytes>::as_bytes(&Span::from_idx("<example>", "Example text", )), b"Example text");
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





/***** AUXILLARY *****/
/// Defines the position of a character in the source text.
/// 
/// # Example
/// ```rust
/// use ast_toolkit::Position;
/// 
/// // First character in a text
/// assert_eq!(Position::new0(0, 0).to_string(), "1:1");
/// assert_eq!(Position::new1(1, 1).to_string(), "1:1");
/// 
/// // First character on the fourth line
/// assert_eq!(Position::new0(3, 0).to_string(), "4:1");
/// assert_eq!(Position::new1(4, 1).to_string(), "4:1");
/// ```
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct Position {
    /// The line-part of the position. Note that this number is zero-indexed.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::Position;
    /// 
    /// assert_eq!(Position::new0(0, 0).line, 0);
    /// assert_eq!(Position::new1(1, 1).line, 0);
    /// assert_eq!(Position::new1(4, 1).line, 3);
    /// 
    /// let mut pos: Position = Position::new1(4, 1);
    /// pos.line = 2;
    /// assert_eq!(pos.to_string(), "3:1");
    /// ```
    pub line : usize,
    /// The column-part of the position. Note that this number is zero-indexed.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::Position;
    /// 
    /// assert_eq!(Position::new0(0, 0).col, 0);
    /// assert_eq!(Position::new1(1, 1).col, 0);
    /// assert_eq!(Position::new1(1, 4).col, 3);
    /// 
    /// let mut pos: Position = Position::new1(1, 4);
    /// pos.col = 2;
    /// assert_eq!(pos.to_string(), "1:3");
    /// ```
    pub col : usize,
}

impl Position {
    /// Constructor for the Position that accepts zero-indexed input.
    /// 
    /// # Arguments
    /// - `line`: A [`usize`]-like number representing the _zero-indexed_ line number of the character.
    /// - `col`: A [`usize`]-like number representing the _zero-indexed_ column number of the character.
    /// 
    /// # Returns
    /// A new Position instance.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::Position;
    /// 
    /// assert_eq!(Position::new0(0, 0).to_string(), "1:1");
    /// assert_eq!(Position::new0(3, 0).to_string(), "4:1");
    /// assert_eq!(Position::new0(0, 41).to_string(), "1:42");
    /// ```
    #[inline]
    pub fn new0(line: impl AsPrimitive<usize>, col: impl AsPrimitive<usize>) -> Self {
        Self {
            line : line.as_(),
            col  : col.as_(),
        }
    }

    /// Constructor for the Position that accepts one-indexed input.
    /// 
    /// # Arguments
    /// - `line`: A [`usize`]-like number representing the _one-indexed_ line number of the character.
    /// - `col`: A [`usize`]-like number representing the _one-indexed_ column number of the character.
    /// 
    /// # Returns
    /// A new Position instance.
    /// 
    /// # Panics
    /// This function may panic if the given `line` or `col` is `0`.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::Position;
    /// 
    /// assert_eq!(Position::new1(1, 1).to_string(), "1:1");
    /// assert_eq!(Position::new1(4, 1).to_string(), "4:1");
    /// assert_eq!(Position::new1(1, 42).to_string(), "1:42");
    /// ```
    /// ```should_panic
    /// # use ast_toolkit::Position;
    /// // This will panic!
    /// Position::new1(0, 0);
    /// Position::new1(0, 1);
    /// Position::new1(1, 0);
    /// ```
    #[inline]
    #[track_caller]
    pub fn new1(line: impl AsPrimitive<usize>, col: impl AsPrimitive<usize>) -> Self {
        Self {
            line : line.as_() - 1,
            col  : col.as_() - 1,
        }
    }



    /// Returns the line coordinate of this position as a zero-indexed number.
    /// 
    /// This is exactly the same as simply accessing the internal `line`-field.
    /// 
    /// # Returns
    /// The line coordinate in this position, zero-indexed.
    /// 
    /// # Examples
    /// ```rust
    /// use ast_toolkit::Position;
    /// 
    /// assert_eq!(Position::new0(0, 0).line0(), 0);
    /// assert_eq!(Position::new1(1, 1).line0(), 0);
    /// assert_eq!(Position::new1(4, 1).line0(), 3);
    /// ```
    #[inline]
    pub fn line0(&self) -> usize { self.line }

    /// Returns the line coordinate of this position as a one-indexed number.
    /// 
    /// This is exactly the same as simply accessing the internal `line`-field and adding `1` to it.
    /// 
    /// # Returns
    /// The line coordinate in this position, one-indexed.
    /// 
    /// # Panics
    /// This function may panic if the internal `line` is [`usize::MAX`](usize).
    /// 
    /// # Examples
    /// ```rust
    /// use ast_toolkit::Position;
    /// 
    /// assert_eq!(Position::new0(0, 0).line1(), 1);
    /// assert_eq!(Position::new1(1, 1).line1(), 1);
    /// assert_eq!(Position::new1(4, 1).line1(), 4);
    /// ```
    /// ```should_panic
    /// # use ast_toolkit::Position;
    /// // This will panic!
    /// Position::new0(usize::MAX, 0).line1();
    /// ```
    #[inline]
    #[track_caller]
    pub fn line1(&self) -> usize { self.line + 1 }

    /// Returns the column coordinate of this position as a zero-indexed number.
    /// 
    /// This is exactly the same as simply accessing the internal `col`-field.
    /// 
    /// # Returns
    /// The column coordinate in this position, zero-indexed.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::Position;
    /// 
    /// assert_eq!(Position::new0(0, 0).col0(), 0);
    /// assert_eq!(Position::new1(1, 1).col0(), 0);
    /// assert_eq!(Position::new1(1, 4).col0(), 3);
    /// ```
    #[inline]
    pub fn col0(&self) -> usize { self.col }

    /// Returns the column coordinate of this position as a one-indexed number.
    /// 
    /// This is exactly the same as simply accessing the internal `col`-field and adding `1` to it.
    /// 
    /// # Returns
    /// The column coordinate in this position, one-indexed.
    /// 
    /// # Panics
    /// This function may panic if the internal `col` is [`usize::MAX`](usize).
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::Position;
    /// 
    /// assert_eq!(Position::new0(0, 0).col1(), 1);
    /// assert_eq!(Position::new1(1, 1).col1(), 1);
    /// assert_eq!(Position::new1(1, 4).col1(), 4);
    /// ```
    /// ```should_panic
    /// # use ast_toolkit::Position;
    /// // This will panic!
    /// Position::new0(0, usize::MAX).col1();
    /// ```
    #[inline]
    #[track_caller]
    pub fn col1(&self) -> usize { self.col + 1 }

    /// Returns the this position as a tuple.
    /// 
    /// This is exactly the same as simply accessing the internal `line`- and `col`-fields and tupleizing them.
    /// 
    /// # Returns
    /// A tuple with the line number first, and column number second, zero-indexed.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::Position;
    /// 
    /// assert_eq!(Position::new0(0, 0).pair0(), (0, 0));
    /// assert_eq!(Position::new1(1, 1).pair0(), (0, 0));
    /// assert_eq!(Position::new1(1, 4).pair0(), (0, 3));
    /// ```
    #[inline]
    pub fn pair0(&self) -> (usize, usize) { (self.line, self.col) }

    /// Returns the this position as a tuple.
    /// 
    /// This is exactly the same as simply accessing the internal `line`- and `col`-fields, adding 1 to either and tupleizing them.
    /// 
    /// # Returns
    /// A tuple with the line number first, and column number second, one-indexed.
    /// 
    /// # Panics
    /// This function may panic if either the internal `line` or `col` is [`usize::MAX`](usize).
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::Position;
    /// 
    /// assert_eq!(Position::new0(0, 0).pair1(), (1, 1));
    /// assert_eq!(Position::new1(1, 1).pair1(), (1, 1));
    /// assert_eq!(Position::new1(1, 4).pair1(), (1, 4));
    /// ```
    /// ```should_panic
    /// # use ast_toolkit::Position;
    /// // This will panic!
    /// Position::new0(usize::MAX, usize::MAX).pair1();
    /// Position::new0(usize::MAX, 0).pair1();
    /// Position::new0(0, usize::MAX).pair1();
    /// ```
    #[inline]
    #[track_caller]
    pub fn pair1(&self) -> (usize, usize) { (self.line + 1, self.col + 1) }
}
impl Display for Position {
    /// Formats the Position as a colon-separated pair of the line and the column numbers, one-indexed.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::Position;
    /// 
    /// assert_eq!(format!("{}", Position::new0(3, 0)), "4:1");
    /// assert_eq!(format!("{}", Position::new1(4, 1)), "4:1");
    /// ```
    #[track_caller]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "{}:{}", self.line + 1, self.col + 1)
    }
}

impl PartialEq<(usize, usize)> for Position {
    /// Compares this Position with a `(line, col)` pair.
    /// 
    /// # Returns
    /// True if the line and column numbers are the same, respectively.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::Position;
    /// 
    /// assert_eq!(Position::new0(3, 0) == (3, 0), true);
    /// assert_eq!(Position::new0(3, 0) == (3, 1), false);
    /// assert_eq!(Position::new0(3, 0) == (2, 0), false);
    /// assert_eq!(Position::new0(3, 0) == (0, 3), false);
    /// assert_eq!(Position::new1(4, 1) == (3, 0), true);
    /// ```
    #[inline]
    fn eq(&self, other: &(usize, usize)) -> bool { self.line == other.0 && self.col == other.1 }
}
impl PartialOrd for Position {
    /// Computes if this position is before or after the other one.
    /// 
    /// The comparison is performed by first comparing the lines, and if those are equal, comparing the column numbers.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::Position;
    /// 
    /// assert_eq!(Position::new0(3, 2) < Position::new0(4, 0), true);
    /// assert_eq!(Position::new0(3, 2) < Position::new0(2, 0), false);
    /// assert_eq!(Position::new0(3, 2) < Position::new0(3, 3), true);
    /// assert_eq!(Position::new0(3, 2) < Position::new0(3, 1), false);
    /// assert_eq!(Position::new0(3, 2) < Position::new0(3, 2), false);
    /// ```
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> { Some(self.cmp(other)) }
}
impl Ord for Position {
    /// Computes if this position is before or after the other one.
    /// 
    /// The comparison is performed by first comparing the lines, and if those are equal, comparing the column numbers.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::Position;
    /// 
    /// assert_eq!(Position::new0(3, 2) < Position::new0(4, 0), true);
    /// assert_eq!(Position::new0(3, 2) < Position::new0(2, 0), false);
    /// assert_eq!(Position::new0(3, 2) < Position::new0(3, 3), true);
    /// assert_eq!(Position::new0(3, 2) < Position::new0(3, 1), false);
    /// assert_eq!(Position::new0(3, 2) < Position::new0(3, 2), false);
    /// ```
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        if self.line < other.line { Ordering::Less }
        else if self.line > other.line { Ordering::Greater }
        else { self.col.cmp(&other.col) }
    }
}

impl AsRef<Position> for Position {
    #[inline]
    fn as_ref(&self) -> &Self { self }
}
impl AsMut<Position> for Position {
    #[inline]
    fn as_mut(&mut self) -> &mut Self { self }
}
impl From<&Position> for Position {
    #[inline]
    fn from(value: &Position) -> Self { *value }
}
impl From<&mut Position> for Position {
    #[inline]
    fn from(value: &mut Position) -> Self { *value }
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
pub struct Span<F, S> {
    /// The filename (or other description) of the file we are spanning.
    pub file   : F,
    /// The entire source text to snippet.
    pub source : S,
    /// The start position of this span (inclusive).
    pub start  : usize,
    /// The end position of this span (inclusive).
    pub end    : usize,
}

impl<F, S: Deref<Target = str>> Span<F, S> {
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
    pub fn new(file: F, source: S) -> Self {
        let source_len: usize = source.len();
        if source_len == 0 { panic!("Cannot span an empty string"); }
        Self {
            file,
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
    pub fn from_pos(file: F, source: S, start: impl Into<Position>, end: impl Into<Position>) -> Self {
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
    pub fn from_idx(file: F, source: S, start: impl AsPrimitive<usize>, end: impl AsPrimitive<usize>) -> Self {
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

    /// Returns the lines referred by this span.
    /// 
    /// This can be thought of a [`Self::text()`](Span::text()) but then one that only returns in the line-range.
    /// 
    /// # Returns
    /// A vector of the individual lines, stripped of newlines. The first and last lines may not be entirely referred by the span, but the middle ones sure are.
    /// 
    /// # Panics
    /// This function may panic if the internal `start`- or `end`-fields are not within bounds of the internal `source`, or if `start > end`.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{Position, Span};
    /// 
    /// assert_eq!(Span::new("<example>", "Hello world!").lines(), vec![ "Hello world!" ]);
    /// assert_eq!(Span::new("<example>", "Hello\nworld!").lines(), vec![ "Hello", "world!" ]);
    /// assert_eq!(Span::from_pos("<example>", "Hello\nworld!", Position::new0(0, 0), Position::new0(0, 4)).lines(), vec![ "Hello" ]);
    /// assert_eq!(Span::from_pos("<example>", "Hello\nworld!", Position::new0(0, 0), Position::new0(0, 8)).lines(), vec![ "Hello", "world!" ]);
    /// assert_eq!(Span::from_pos("<example>", "Hello\nworld!", Position::new0(0, 0), Position::new0(0, 5)).lines(), vec![ "Hello" ]);
    /// ```
    #[track_caller]
    pub fn lines(&self) -> Vec<&str> {
        // Pre-assert that the start is smaller than the end
        assert_range!(self.start, self.end, self.source);

        // Fetch the start & end lines in the source text
        let mut start : usize = 0;
        let mut lines : Vec<&str> = Vec::with_capacity(1);
        for (i, c) in self.source.grapheme_indices(true) {
            // If it's a newline, then we potentially store and reset
            if c == "\n" {
                // Check if this line overlaps with the span
                if self.start < i && self.end >= start {
                    // Note the line (excluding newline)
                    lines.push(&self.source[start..i]);
                }
                // Reset
                start = i + 1;
            }
        }

        // If the current start is within the range, then add it as well
        if self.start < self.source.len() && self.end >= start {
            lines.push(&self.source[start..]);
        }

        // Return the lines
        lines
    }
}
impl<F: Clone + PartialEq, S: Clone + PartialEq> Span<F, S> {
    /// Constructor for the Span that encapsulates both ranges of the given spans.
    /// 
    /// # Arguments
    /// - `span1`: The first span to take into account.
    /// - `span2`: The second span to take into account.
    /// 
    /// # Returns
    /// A new instance of Self that spans both input spans and everything in between.
    /// 
    /// # Panics
    /// This function panics if the given spans do not have the same `file` or `source`.
    #[track_caller]
    pub fn combined(span1: impl Into<Span<F, S>>, span2: impl Into<Span<F, S>>) -> Self {
        let span1: Span<F, S> = span1.into();
        let span2: Span<F, S> = span2.into();

        // Assert they talk about the same thing
        if span1.file != span2.file { panic!("Given spans do not have the same `file`"); }
        if span1.source != span2.source { panic!("Given spans do not have the same `source`"); }

        // Compute the new range
        let start: usize = std::cmp::min(span1.start, span2.start);
        let end: usize = std::cmp::max(span1.end, span2.end);

        // Construct the new self
        Self {
            file   : span1.file.clone(),
            source : span1.source.clone(),
            start,
            end,
        }
    }
}

impl<F, S: Deref<Target = str>> Deref for Span<F, S> {
    type Target = str;

    #[inline]
    fn deref(&self) -> &Self::Target { self.text() }
}

impl<F, S> AsRef<Span<F, S>> for Span<F, S> {
    #[inline]
    fn as_ref(&self) -> &Self { self }
}
impl<F, S> AsMut<Span<F, S>> for Span<F, S> {
    #[inline]
    fn as_mut(&mut self) -> &mut Self { self }
}
impl<F: Clone, S: Clone> From<&Span<F, S>> for Span<F, S> {
    #[inline]
    fn from(value: &Span<F, S>) -> Self { value.clone() }
}
impl<F: Clone, S: Clone> From<&mut Span<F, S>> for Span<F, S> {
    #[inline]
    fn from(value: &mut Span<F, S>) -> Self { value.clone() }
}


// nom-related things
#[cfg(feature = "nom")]
impl<F, S: Deref<Target = str>> nom::AsBytes for Span<F, S> {
    #[track_caller]
    fn as_bytes(&self) -> &[u8] {
        assert_range!(self.start, self.end, self.source);
        self.source[self.start..=self.end].as_bytes()
    }
}
#[cfg(feature = "nom")]
impl<F, S> nom::InputLength for Span<F, S> {
    #[track_caller]
    fn input_len(&self) -> usize { 1 + (self.end - self.start) }
}
#[cfg(feature = "nom")]
impl<F: Clone, S: Clone + Deref<Target = str>> nom::InputTake for Span<F, S> {
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
