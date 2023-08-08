//  POSITION.rs
//    by Lut99
// 
//  Created:
//    08 Aug 2023, 10:20:39
//  Last edited:
//    08 Aug 2023, 10:22:12
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines the basic [`Position`] that acts as a (line,
//!   column)-coordinate into source text.
// 

use std::cmp::Ordering;
use std::fmt::{Display, Formatter, Result as FResult};

use num_traits::AsPrimitive;


/***** LIBRARY *****/
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
