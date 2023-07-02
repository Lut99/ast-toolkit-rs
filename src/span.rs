//  SPAN.rs
//    by Lut99
// 
//  Created:
//    02 Jul 2023, 16:40:44
//  Last edited:
//    02 Jul 2023, 17:46:00
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines the [`Span`] (and [`Position`]) structs which we use to keep
//!   track of a node's position in the source text.
// 

use std::cmp::Ordering;
use std::fmt::{Display, Formatter, Result as FResult};

use num_traits::AsPrimitive;


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
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct Span<'s> {
    /// The entire source text to snippet.
    pub source : &'s str,
    /// The start position of this span (inclusive).
    pub start  : Position,
    /// The end position of this span (inclusive).
    pub end    : Position,
}

impl<'s> Span<'s> {
    /// Constructor for the Span.
    /// 
    /// # Arguments
    /// - `source`: The entire source text that we might parse.
    /// - `start`: The start position of this span in the source text, inclusive.
    /// - `end`: The end position of this span in the source text, inclusive.
    /// 
    /// # Returns
    /// A new instance of Self.
    #[inline]
    pub fn new(source: impl Into<&'s str>, start: impl Into<Position>, end: impl Into<Position>) -> Self {
        Self {
            source : source.into(),
            start  : start.into(),
            end    : end.into(),
        }
    }



    /// Returns the text referred by this span.
    /// 
    /// # Returns
    /// A [`str`] referring to the source text we span.
    /// 
    /// # Panics
    /// This function may panic if the internal `start`- or `end`-fields are not within bounds of the internal `source`, or if `start > end`.
    #[track_caller]
    pub fn text(&self) -> &str {
        // Pre-assert that the start is smaller than the end
        if self.start > self.end { panic!("Start position {} is larger than end position {}", self.start, self.end); }

        // Fetch the start & end in the source text
        let mut line  : usize = 0;
        let mut col   : usize = 0;
        let mut start : Option<usize> = None;
        let mut end   : Option<usize> = None;
        for (i, c) in self.source.char_indices() {
            // If we've reached the start or end, mark it so
            if self.start.line == line && self.start.col == col {
                start = Some(i);
            }
            if self.end.line == line && self.end.col == col {
                end = Some(i);
                break;
            }

            // Iterate the (line, col) accordingly
            col += 1;
            if c == '\n' {
                col = 0;
                line += 1;
            }
        }
        let start: usize = match start {
            Some(start) => start,
            None => { panic!("Start position {} is out-of-range for a source text of length {}", self.start, self.source.len()); },
        };
        let end: usize = match end {
            Some(end) => end,
            None => { panic!("End position {} is out-of-range for a source text of length {}", self.end, self.source.len()); },
        };

        // Extract the slice
        &self.source[start..=end]
    }

    /// Returns the lines referred by this span.
    /// 
    /// This can be thought of a [`Self::text()`](Span::text()) but then one that only returns in the line-range.
    /// 
    /// # Returns
    /// A vector of the individual lines. The first and last lines may not be entirely referred by the span, but the middle ones sure are.
    /// 
    /// # Panics
    /// This function may panic if the internal `start`- or `end`-fields are not within bounds of the internal `source`, or if `start > end`.
    pub fn lines(&self) -> Vec<&str> {
        // Pre-assert that the start is smaller than the end
        if self.start > self.end { panic!("Start position {} is larger than end position {}", self.start, self.end); }

        // Fetch the start & end lines in the source text
        let mut line  : usize = 0;
        let mut col   : usize = 0;
        let mut start : Option<usize> = None;
        let mut lines : Vec<&str> = Vec::with_capacity(1);
        for (i, c) in self.source.char_indices() {
            // If we've reached the start or end, mark it so
            if col == 0 {
                start = Some(i);
            }
            if line >= self.start.line && line <= self.end.line && c == '\n' {
                let start_i: usize = match start {
                    Some(i) => i,
                    None => { panic!("Start position {} is out-of-range for a source text of length {}", self.start, self.source.len()); },
                };
                lines.push(&self.source[start_i..i]);
                start = None;
            }
            if line > self.end.line { break; }

            // Iterate the (line, col) accordingly
            col += 1;
            if c == '\n' {
                col = 0;
                line += 1;
            }
        }
        if lines.is_empty() { panic!("End position {} is out-of-range for a source text of length {}", self.end, self.source.len()); }

        // Return the lines
        lines
    }
}

impl<'s> AsRef<Span<'s>> for Span<'s> {
    #[inline]
    fn as_ref(&self) -> &Self { self }
}
impl<'s> AsMut<Span<'s>> for Span<'s> {
    #[inline]
    fn as_mut(&mut self) -> &mut Self { self }
}
impl<'s> From<&Span<'s>> for Span<'s> {
    #[inline]
    fn from(value: &Span<'s>) -> Self { *value }
}
impl<'s> From<&mut Span<'s>> for Span<'s> {
    #[inline]
    fn from(value: &mut Span<'s>) -> Self { *value }
}
