//  LAYOUT.rs
//    by Lut99
//
//  Description:
//!   Defines an output layout buffer.
//

use std::fmt::{Display, Formatter, Result as FResult};

use ast_toolkit_span::{Span, Spannable};


/***** FORMATTERS *****/
/// Renders a [`LayoutBuffer`] to some [`Formatter`].
pub struct LayoutBufferRenderer<'b, E> {
    /// The buffer to render.
    buffer: &'b LayoutBuffer<E>,
}
impl<'b, E: Display> Display for LayoutBufferRenderer<'b, E> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        let Self { buffer } = self;

        // Scan to find the line width
        let mut max_l_len: usize = 0;
        for line in &buffer.lines {
            max_l_len = std::cmp::max(max_l_len, line.l.as_ref().map(String::len).unwrap_or(0));
        }

        // Format it
        for line in &buffer.lines {
            if let Some(l) = &line.l {
                write!(f, "{l:<len$}", len = max_l_len)?;
            } else {
                write!(f, "{:<len$}", "", len = max_l_len)?;
            }
            write!(f, "| ")?;
            for (i, cell) in line.cells.iter().enumerate() {
                if let Some(value) = &cell.value {
                    write!(f, "{value}")?;
                } else {
                    write!(f, " ")?;
                }
            }
        }
        Ok(())
    }
}





/***** LIBRARY *****/
/// Individual cells in a line.
#[derive(Debug, Clone)]
pub struct Cell<E> {
    /// The original source location, if any.
    i:     Option<usize>,
    /// The value of the cell, if any.
    value: Option<E>,
}

// Constructors
impl<E> Cell<E> {
    /// Constructor for a Cell with a value but not from source.
    ///
    /// # Arguments
    /// - `value`: The value to set.
    ///
    /// # Returns
    /// A new Cell.
    #[inline]
    pub const fn from_value(value: E) -> Self { Self { i: None, value: Some(value) } }

    /// Constructor for a Cell from source with a value.
    ///
    /// # Arguments
    /// - `i`: The original source position.
    /// - `value`: The value to set.
    ///
    /// # Returns
    /// A new Cell.
    #[inline]
    pub const fn from_source(i: usize, value: E) -> Self { Self { i: Some(i), value: Some(value) } }
}



/// Individual lines in the output.
#[derive(Debug, Clone)]
pub struct Line<E> {
    /// The line number
    l:     Option<String>,
    /// A list of cells in this line.
    cells: Vec<Cell<E>>,
}

// Constructors
impl<E> Line<E> {
    /// Constructs a new Line with capacity for at least the given number of cells.
    ///
    /// # Arguments
    /// - `capacity`: The minimum number of cells to reserve space for.
    ///
    /// # Returns
    /// A new Line that will be able to store at least `capacity` cells before reallocation
    /// is necessary.
    #[inline]
    pub fn with_capacity(capacity: usize) -> Self { Self { l: None, cells: Vec::with_capacity(capacity) } }
}

// Collection
impl<E> Line<E> {
    /// Set a line number for this line, already rendered.
    ///
    /// Note that any preceding spaces are added automatically later.
    ///
    /// # Arguments
    /// - `l`: The rendered line number to set.
    ///
    /// # Returns
    /// Self for chaining.
    #[inline]
    pub fn set_line_number(&mut self, l: impl Display) -> &mut Self {
        self.l = Some(l.to_string());
        self
    }

    /// Write a new cell to the buffer.
    ///
    /// # Arguments
    /// - `cell`: Some [`Cell`](-like) to write to the buffer.
    ///
    /// # Returns
    /// Self for chaining.
    #[inline]
    pub fn push(&mut self, cell: impl Into<Cell<E>>) -> &mut Self {
        self.cells.push(cell.into());
        self
    }
}

// Conversion
impl<'s, S: Spannable<'s>> From<Span<S>> for Line<S::Elem>
where
    S::Elem: Clone,
{
    #[inline]
    fn from(value: Span<S>) -> Self {
        // Resolve the start index of the Span
        let source: &S = value.source();
        let source_len: usize = source.len();
        let start: usize = value.range().start_resolved(source_len).unwrap_or(0);

        // Add them
        let mut res: Self = Self::with_capacity(source_len);
        for (i, elem) in value.as_slice().iter().enumerate() {
            res.cells.push(Cell { i: Some(start + i), value: Some(elem.clone()) })
        }

        // Done
        res
    }
}



/// Defines the buffer in which we'll write the output source fragment.
#[derive(Debug, Clone)]
pub struct LayoutBuffer<E> {
    /// The lines of output we'll write to.
    lines: Vec<Line<E>>,
}

// Constructors
impl<E> LayoutBuffer<E> {
    /// Constructs a new LayoutBuffer with capacity for at least the given number of lines.
    ///
    /// # Arguments
    /// - `capacity`: The minimum number of lines to reserve space for.
    ///
    /// # Returns
    /// A new LayoutBuffer that will be able to store at least `capacity` lines before reallocation
    /// is necessary.
    #[inline]
    pub fn with_capacity(capacity: usize) -> Self { Self { lines: Vec::with_capacity(capacity) } }
}

// Collection
impl<E> LayoutBuffer<E> {
    /// Write a new line to the buffer.
    ///
    /// # Arguments
    /// - `line`: Some [`Line`](-like) to write to the buffer.
    ///
    /// # Returns
    /// Self for chaining.
    #[inline]
    pub fn push(&mut self, line: impl Into<Line<E>>) -> &mut Self {
        self.lines.push(line.into());
        self
    }
}

// Rendering
impl<E> LayoutBuffer<E> {
    /// Returns a formatter for the LayoutBuffer.
    ///
    /// # Arguments
    /// - `with_color`: Whether to use ANSI colors or not.
    ///
    /// # Returns
    /// A [`LayoutBufferRenderer`] implementing [`Display`].
    #[inline]
    pub const fn render(&self, with_color: bool) -> LayoutBufferRenderer<E> { LayoutBufferRenderer { buffer: self } }
}

// Iterators
impl<E> FromIterator<Line<E>> for LayoutBuffer<E> {
    #[inline]
    fn from_iter<T: IntoIterator<Item = Line<E>>>(iter: T) -> Self { Self { lines: Vec::from_iter(iter) } }
}
