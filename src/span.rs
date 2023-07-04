//  SPAN.rs
//    by Lut99
// 
//  Created:
//    02 Jul 2023, 16:40:44
//  Last edited:
//    04 Jul 2023, 19:11:33
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines the [`Span`] (and [`Position`]) structs which we use to keep
//!   track of a node's position in the source text.
// 

use std::cmp::Ordering;
use std::fmt::{Display, Formatter, Result as FResult};
use std::ops::{Deref, DerefMut};

use num_traits::AsPrimitive;


/***** HELPER MACROS *****/
/// Asserts that the given ranges are valid ranges
macro_rules! assert_range {
    ($start:expr, $end:expr, $source:expr) => {
        if $start < $source.len() { panic!("Start position {} is out-of-range for source of {} characters", $start, $source.len()); }
        if $end < $source.len() { panic!("End position {} is out-of-range for source of {} characters", $end, $source.len()); }
        if $start > $end { panic!("Start position {} is larger than end position {}", $start, $end); }
    };
}





/***** AUXILLARY *****/
/// Defines the position of a character in the source text.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct Position {
    /// The line-part of the position. Note that this number is zero-indexed.
    pub line : usize,
    /// The column-part of the position. Note that this number is zero-indexed.
    pub col  : usize,
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
    #[inline]
    #[track_caller]
    pub fn line1(&self) -> usize { self.line + 1 }

    /// Returns the column coordinate of this position as a zero-indexed number.
    /// 
    /// This is exactly the same as simply accessing the internal `col`-field.
    /// 
    /// # Returns
    /// The column coordinate in this position, zero-indexed.
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
    #[inline]
    #[track_caller]
    pub fn col1(&self) -> usize { self.col + 1 }

    /// Returns the this position as a tuple.
    /// 
    /// This is exactly the same as simply accessing the internal `line`- and `col`-fields and tupleizing them.
    /// 
    /// # Returns
    /// A tuple with the line number first, and column number second, zero-indexed.
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
    #[inline]
    #[track_caller]
    pub fn pair1(&self) -> (usize, usize) { (self.line + 1, self.col + 1) }
}
impl Display for Position {
    #[track_caller]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "{}:{}", self.line + 1, self.col + 1)
    }
}

impl PartialEq<(usize, usize)> for Position {
    #[inline]
    fn eq(&self, other: &(usize, usize)) -> bool { self.line == other.0 && self.col == other.1 }
}
impl PartialOrd for Position {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> { Some(self.cmp(other)) }
}
impl Ord for Position {
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
/// # Lifetimes
/// - `s`: The lifetime of the source text which this Span refers to.
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
    /// # Arguments
    /// - `file`: The filename (or other description) of the source's origin.
    /// - `source`: The entire source text that we might parse.
    /// 
    /// # Returns
    /// A new instance of Self that spans the entire source.
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
    #[track_caller]
    pub fn from_pos(file: F, source: S, start: impl Into<Position>, end: impl Into<Position>) -> Self {
        // Assert start <= end
        let (start, end): (Position, Position) = (start.into(), end.into());
        if start > end { panic!("Given start {start} is larger than given end {end}"); }

        // Examine the source to find the end
        let (mut istart, mut iend): (Position, Position) = (start, end);
        let (mut rstart, mut rend): (Option<usize>, Option<usize>) = (None, None);
        for (i, c) in source.char_indices() {
            // If we've reached the end of any, mark it
            if istart.line == 0 && istart.col == 0 { rstart = Some(i); }
            if iend.line == 0 && iend.col == 0 { rend = Some(i); break; }

            // Otherwise, count them down
            if istart.col > 0 {
                // If we're skipping a newline, the Position is ill-formed
                if c == '\n' { panic!("Found newline while start.col != 0 (start: {start}, character index: {i})"); }
                istart.col -= 1;
            } else if c == '\n' {
                istart.line -= 1;
            }
            if iend.col > 0 {
                // If we're skipping a newline, the Position is ill-formed
                if c == '\n' { panic!("Found newline while end.col != 0 (end: {end}, character index: {i})"); }
                iend.col -= 1;
            } else if c == '\n' {
                iend.line -= 1;
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
    /// # Arguments
    /// - `file`: The filename (or other description) of the source's origin.
    /// - `source`: The entire source text that we might parse.
    /// - `start`: The start position, as character index, inclusive.
    /// - `end`: The end position, as character index, inclusive.
    /// 
    /// # Returns
    /// A new instance of Self that spans only the given range.
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



    /// Returns the text referred by this span.
    /// 
    /// # Returns
    /// A [`str`] referring to the source text we span.
    /// 
    /// # Panics
    /// This function may panic if the internal `start`- or `end`-fields are not within bounds of the internal `source`, or if `start > end`.
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
    /// A vector of the individual lines, stipped of newlines. The first and last lines may not be entirely referred by the span, but the middle ones sure are.
    /// 
    /// # Panics
    /// This function may panic if the internal `start`- or `end`-fields are not within bounds of the internal `source`, or if `start > end`.
    #[track_caller]
    pub fn lines(&self) -> Vec<&str> {
        // Pre-assert that the start is smaller than the end
        assert_range!(self.start, self.end, self.source);

        // Fetch the start & end lines in the source text
        let mut start : usize = 0;
        let mut lines : Vec<&str> = Vec::with_capacity(1);
        for (i, c) in self.source.char_indices() {
            // If it's a newline, then we potentially store and reset
            if c == '\n' {
                // Check if this line overlaps with the span
                if self.start < i && self.end >= start {
                    // Note the line (excluding newline)
                    lines.push(&self.source[start..i]);
                }
                // Reset
                start = i;
            }
        }

        // Return the lines
        lines
    }
}
impl<F, S: Deref<Target = str> + DerefMut> Span<F, S> {
    /// Returns the text referred by this span, mutably.
    /// 
    /// # Returns
    /// A mutable [`str`] referring to the source text we span.
    /// 
    /// # Panics
    /// This function may panic if the internal `start`- or `end`-fields are not within bounds of the internal `source`, or if `start > end`.
    #[inline]
    #[track_caller]
    pub fn text_mut(&mut self) -> &mut str {
        assert_range!(self.start, self.end, self.source);
        &mut self.source[self.start..=self.end]
    }
}

impl<F, S: Deref<Target = str>> Deref for Span<F, S> {
    type Target = str;

    #[inline]
    fn deref(&self) -> &Self::Target { self.text() }
}
impl<F, S: Deref<Target = str> + DerefMut> DerefMut for Span<F, S> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target { self.text_mut() }
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
