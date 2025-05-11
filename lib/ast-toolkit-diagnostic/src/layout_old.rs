//  LAYOUT.rs
//    by Lut99
//
//  Description:
//!   Implements the layouting algorithm used to both place annotations and colour source text.
//

use std::fmt::{Debug, Formatter, Result as FResult};

use ast_toolkit_span::{Span, SpannableUtf8};

use crate::annotations::{Annotation, AnnotationInner, Severity};


/***** HELPER FUNCTIONS *****/
/// Takes a [`Span`], and then returns one which is the same but with boundaries on the nearest
/// newlines.
///
/// Note that the _open_ boundary is clipped to the first newline preceding the already open
/// boundary (or the start of the whole source), and the _close_ boudnary is clipped to the first
/// newline _succeeding_ the already existing boundary (or the end of the source algorither).
///
/// Also note that the final newline (if any) is _included_.
///
/// # Arguments
/// - `span`: Some [`Span`] to extend.
///
/// # Returns
/// A [`Span`] that is the same or larger than the given one.
fn extend_range_to_snippet<'s, S: Clone + SpannableUtf8<'s>>(range: Span<S>) -> Span<S> {
    // We start by iterating over the MAIN source
    let source_len: usize = range.source().len();
    let mut range_start: usize = 0;
    let mut range_end: usize = source_len;
    let mut i: usize = 0;
    let mut in_range: bool = false;
    range.source().match_utf8_while(|c| {
        // Keep track of the source start
        if c == "\n" || c == "\r\n" {
            if !in_range {
                // We're not in range, so we have to remember when the line begins
                range_start = i + c.len();
            } else if i > range.range().end_resolved(source_len).unwrap_or(0) {
                // We are in range, so mark when the line ends and be done with it.
                range_end = i + c.len();
                return false;
            }
        }

        // Check if we're in range yet
        if !in_range && i >= range.range().start_resolved(source_len).unwrap_or(0) {
            in_range = true;
        }

        // Keep going, tracking our position
        i += c.len();
        true
    });

    // Now return the range
    Span::ranged(range.source().clone(), range_start..range_end)
}





/***** AUXILLARY *****/
/// Defines possible placement methods for the layouting algorithm.
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub enum Placement {
    /// Directly to the right of the highlighted area.
    Right,
}

/// Defines the colouring options for a cell.
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub enum CellColor {
    Severity(Severity),
    Suggestion,
    None,
}
impl Debug for CellColor {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        match self {
            Self::Severity(Severity::Error) => write!(f, "e"),
            Self::Severity(Severity::Warning) => write!(f, "w"),
            Self::Severity(Severity::Help) => write!(f, "h"),
            Self::Suggestion => write!(f, "s"),
            Self::None => write!(f, " "),
        }
    }
}

/// Defines cell values
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum CellValue<'s> {
    /// A grapheme
    Graph(&'s str),
    /// A highlight marker
    Marker,
    /// Nothing
    None,
}



/// Defines a single cell in the layout buffer.
#[derive(Clone)]
pub struct LayoutUtf8Cell<'s> {
    /// The character stored here, if there is one. [`None`] indicates empty.
    pub value: CellValue<'s>,
    /// The colouring options.
    pub color: CellColor,
    /// If given, this cell comes from the source and this encodes its original location.
    pub pos:   Option<usize>,
}
impl<'s> LayoutUtf8Cell<'s> {
    /// Constructor for a LayoutUtf8Cell that initializes it empty.
    ///
    /// # Returns
    /// A LayoutUtf8Cell representing nothing, an empty slot in the output buffer.
    #[inline]
    pub fn new() -> Self { Self { value: CellValue::None, color: CellColor::None, pos: None } }

    /// Constructor for a LayoutUtf8Cell that initializes it from a source grapheme.
    ///
    /// # Arguments
    /// - `value`: Some [`str`] to set as value.
    /// - `i`: The linear position of the cell in the source text. Zero-indexed.
    ///
    /// # Returns
    /// A LayoutUtf8Cell representing this grapheme, uncoloured.
    #[inline]
    pub fn from_graph(value: &'s str, i: usize) -> Self {
        Self {
            value: if value.chars().all(|c| c.is_whitespace()) { CellValue::None } else { CellValue::Graph(value) },
            color: CellColor::None,
            pos:   Some(i),
        }
    }
}

/// Defines a line in the [`LayoutUtf8Buffer`].
#[derive(Clone)]
pub struct LayoutUtf8Line<'s> {
    /// The cells in this line.
    pub cells:     Vec<LayoutUtf8Cell<'s>>,
    /// Whether this line is from source or if it's annotation-only.
    pub is_source: bool,
}
impl<'s> LayoutUtf8Line<'s> {
    /// Constructor for the LayoutUtf8Line that initializes it as empty.
    ///
    /// # Arguments
    /// - `max_line_width`: The maximum size of this line.
    /// - `is_source`: Whether this line is taken from the source or annotation-only.
    ///
    /// # Returns
    /// An empty LayoutUtf8Line.
    #[inline]
    pub fn new(max_line_width: usize, is_source: bool) -> Self { Self { cells: vec![LayoutUtf8Cell::new(); max_line_width], is_source } }



    /// Pushes a new [`LayoutUtf8Cell`] to the end of the line.
    ///
    /// # Arguments
    /// - `cell`: A cell-like value to push.
    ///
    /// # Returns
    /// Self for chaining.
    #[inline]
    pub fn push(&mut self, cell: LayoutUtf8Cell<'s>) -> &mut Self {
        self.cells.push(cell);
        self
    }



    /// Returns the "real" width of this line.
    ///
    /// This is simple the number of cells in it with any trailing [`None`]s not counted.
    #[inline]
    pub fn real_len(&self) -> usize {
        let mut len: usize = self.cells.len();
        let mut cells = self.cells.iter().rev();
        while let Some(LayoutUtf8Cell { value: CellValue::None, .. }) = cells.next() {
            // SAFETY: Should never overflow, as we're at most iterating over `self.cells.len()`
            // here
            len -= 1;
        }
        len
    }
}



/// Defines a buffer of layout cells that we can create from an overall source text.
#[derive(Clone)]
pub struct LayoutUtf8Buffer<'s> {
    /// The lines of cells.
    pub lines: Vec<LayoutUtf8Line<'s>>,
    /// The maximum width any given line can have.
    pub max_line_width: usize,
}

// Constructors
impl<'s> LayoutUtf8Buffer<'s> {
    /// Constructor for the LayoutUtf8Buffer that initializes it to empty.
    ///
    /// # Arguments
    /// - `max_line_width`: The maximum width any given line can have.
    ///
    /// # Returns
    /// An empty LayoutUtf8Buffer.
    #[inline]
    pub fn new(max_line_width: usize) -> Self { Self { lines: Vec::with_capacity(16), max_line_width } }

    /// Constructor for the LayoutUtf8Buffer that populates it with the contents of the given
    /// [`Span`].
    ///
    /// # Arguments
    /// - `span`: The [`Span`] to snatch the content of.
    /// - `max_line_width`: The maximum width any given line can have.
    ///
    /// # Returns
    /// A LayoutUtf8Buffer that is populated with the contents of `span`.
    pub fn from_span<S: SpannableUtf8<'s>>(span: Span<S>, max_line_width: usize) -> Self {
        let source_len: usize = span.source().len();
        let mut i: usize = span.range().start_resolved(source_len).unwrap_or(0);
        let mut col: usize = 0;
        let mut this = Self::new(max_line_width);
        span.match_utf8_while(|c: &'s str| {
            // Linebreak if there's a risk of overflowing
            if col >= max_line_width {
                this.lines.push(LayoutUtf8Line::new(max_line_width, true));
                col = 0;
            }

            // Then handle newlines or non-newline characters
            if c == "\n" || c == "\r\n" {
                // Add a new line
                this.lines.push(LayoutUtf8Line::new(max_line_width, true));
                col = 0;
            } else {
                // Add to the last line
                match this.lines.last_mut() {
                    Some(line) => {
                        line.cells[col] = LayoutUtf8Cell::from_graph(c, i);
                        col += 1;
                    },
                    None => {
                        let mut line: LayoutUtf8Line<'s> = LayoutUtf8Line::new(max_line_width, true);
                        line.cells[0] = LayoutUtf8Cell::from_graph(c, i);
                        col = 1;
                        this.lines.push(line);
                    },
                }
            }

            // We just scan the whole line
            i += c.len();
            true
        });
        // Trim the last line if it is empty
        let mut keep: usize = this.lines.len();
        let mut iter = this.lines.iter().rev();
        while let Some(line) = iter.next() {
            if line.real_len() == 0 {
                keep -= 1;
            } else {
                break;
            }
        }
        this.lines.truncate(keep);
        this
    }
}

// Buffer ops
impl<'s> LayoutUtf8Buffer<'s> {
    /// Colors the cells with the given coordinates in the given color.
    ///
    /// This will also inject highlight markers below the cells.
    ///
    /// # Arguments
    /// - `posses`: The (line, column)-pairs marking the cells to color.
    /// - `colour`: The [`CellColor`] to colour it with.
    ///
    /// # Panics
    /// This function will panic (very ungracefully) when any of the positions is invalid.
    #[inline]
    pub fn apply_colors(&mut self, posses: impl IntoIterator<Item = (usize, usize)>, color: CellColor) {
        let posses: Vec<(usize, usize)> = posses.into_iter().collect();

        // Decide if a newline is needed
        let mut needs_newline: Option<usize> = None;
        for (line, col) in &posses {
            if line + 1 >= self.lines.len() || self.lines[line + 1].is_source || self.lines[line + 1].cells[*col].value != CellValue::None {
                // There's either no line or where we want to place a marker is not empty. We know
                // enough
                needs_newline = Some(line + 1);
                break;
            }
        }

        // Add it if so
        if let Some(line) = needs_newline {
            if line <= self.lines.len() {
                self.lines.insert(line, LayoutUtf8Line::new(self.max_line_width, false));
            } else {
                self.lines.push(LayoutUtf8Line::new(self.max_line_width, false));
            }
        }

        // Now colour & apply markers
        for (line, col) in posses {
            // Apply the color first
            self.lines[line].cells[col].color = color;
            #[cfg(debug_assertions)]
            assert!(line + 1 < self.lines.len());
            #[cfg(debug_assertions)]
            assert_eq!(self.lines[line + 1].cells[col].value, CellValue::None);
            self.lines[line + 1].cells[col].value = CellValue::Marker;
            self.lines[line + 1].cells[col].color = color;
        }
    }
}

// Operators
impl<'s> Debug for LayoutUtf8Buffer<'s> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        // Write the snippet like, visibly appealing
        writeln!(f, "+{}+", "-".repeat(2 + 5 * self.max_line_width))?;
        for line in &self.lines {
            write!(f, "|{}", if line.is_source { "@" } else { " " })?;
            for cell in &line.cells {
                if let CellValue::Graph(value) = &cell.value {
                    write!(f, " {:?}{value:?}", cell.color)?;
                } else if let CellValue::Marker = &cell.value {
                    write!(f, " {:?}---", cell.color)?;
                } else {
                    write!(f, "     ")?;
                }
            }
            writeln!(f, " |")?;
        }
        writeln!(f, "+{}+", "-".repeat(2 + 5 * self.max_line_width))?;
        Ok(())
    }
}





/***** LIBRARY *****/
/// Implements the layouting algorithm used to both place annotations and colour source text.
///
/// This variation will render the source as UTF8 graphemes. If you're parsing raw bytes, then
/// consider using the byte-specific [`layout_bytes()`] instead.
///
/// Note that it is not the most efficient algorithm out there, probably. However, also note that
/// the space it will reason over (i.e., source text size & number of annotations) probably remains
/// very small. As such, we prefer an understandable algorithm over something directly working on
/// the source text and all that.
///
/// # Arguments
/// - `annots`: The annotations to place. Note that they must be [joinable](Span::join()), i.e.,
///   they must all be from the same `S`ource.
///
///   The order in which you give annotations is _kind of_ relevant. The placing algorithm for the
///   notes will do so in-order, meaning that the first annotations are likely to get nicer
///   placings.
/// - `max_line_width`: The maximum number of graphemes per line.
///
/// # Returns
/// A [`LayoutUtf8Buffer`] that describes the final product, which can be rendered to a terminal.
///
/// # Panics
/// This function will panic if any of the given `annots` is from a different `S`ource then `main`,
/// or if the given `annots` iterator is empty.
#[track_caller]
pub fn layout_utf8<'s, S>(annots: impl IntoIterator<Item = Annotation<S>>, max_line_width: usize) -> LayoutUtf8Buffer<'s>
where
    S: Clone + SpannableUtf8<'s>,
{
    let annots: Vec<Annotation<S>> = annots.into_iter().collect();

    // Collect the source
    let mut range: Option<Span<S>> = None;
    for annot in &annots {
        match &mut range {
            Some(range) => {
                if range.extend(&annot.span).is_none() {
                    panic!("{annot:?} does not match source with ID {:?}", range.source_id());
                }
            },
            None => range = Some(annot.span.clone()),
        }
    }
    let range: Span<S> = match range {
        Some(range) => range,
        None => panic!("Cannot layout no annotations"),
    };
    let source_len: usize = range.source().len();

    // Extract the snippet range from the range (which is the same, but now with the full first-
    // and last newlines, if any).
    let snippet: Span<S> = extend_range_to_snippet(range);

    // Put that in a buffer
    let mut buffer = LayoutUtf8Buffer::from_span(snippet, max_line_width);
    for annot in annots {
        let start: usize = annot.span.range().start_resolved(source_len).unwrap_or(0);
        let end: usize = annot.span.range().end_resolved(source_len).unwrap_or(0);

        // Match on what to do
        match annot.inner {
            AnnotationInner::Highlight(highlight) => {
                // We always colour the highlighted area
                let mut to_color: Vec<(usize, usize)> = Vec::with_capacity(annot.span.len());
                for (y, line) in buffer.lines.iter().enumerate() {
                    for (x, cell) in line.cells.iter().enumerate() {
                        if let Some(i) = cell.pos {
                            // We're at a source-owned cell
                            if start <= i && i < end {
                                // Color it!
                                to_color.push((y, x));
                            }
                        }
                    }
                }

                // Apply the colours, which also injects highlighters
                buffer.apply_colors(to_color, CellColor::Severity(highlight.severity));
            },
            AnnotationInner::Suggestion(suggestion) => todo!(),
        }
    }

    // Done
    buffer
}





/***** TESTS *****/
#[cfg(test)]
mod tests {
    use super::*;
    use crate::annotations::AnnotationInnerHighlight;

    #[test]
    fn test_extend_range_to_snippet() {
        let source: &str = "Hello, world!\nHello, world!\nHello, world!\n";
        assert_eq!(extend_range_to_snippet(Span::ranged(source, 19..20)), Span::ranged(source, 14..28));
        assert_eq!(extend_range_to_snippet(Span::ranged(source, 7..12)), Span::ranged(source, 0..14));
        assert_eq!(extend_range_to_snippet(Span::ranged(source, 28..33)), Span::ranged(source, 28..42));

        let empty: &str = "";
        assert_eq!(extend_range_to_snippet(Span::new(empty)), Span::ranged(empty, 0..0));
    }

    /// Not really a test but useful.
    ///
    /// Run with `--test-threads 1 --nocapture` to see the output.
    #[test]
    fn test_show_line_utf8_buffers() {
        println!();

        let source: &str = "Hello, world!\nHello, world!\nHello, world!\n";
        println!("{:?}", LayoutUtf8Buffer::from_span(extend_range_to_snippet(Span::ranged(source, 19..20)), 15));
        println!("{:?}", LayoutUtf8Buffer::from_span(extend_range_to_snippet(Span::ranged(source, 7..12)), 15));
        println!("{:?}", LayoutUtf8Buffer::from_span(extend_range_to_snippet(Span::ranged(source, 28..33)), 15));

        let empty: &str = "";
        println!("{:?}", LayoutUtf8Buffer::from_span(extend_range_to_snippet(Span::new(empty)), 15));
    }

    #[test]
    fn test_layout_utf8_single_highlight() {
        println!();

        let source: &str = "Hello, world!\nHello, world!\nHello, world!\n";
        println!(
            "{:?}",
            layout_utf8(
                vec![Annotation {
                    inner: AnnotationInner::Highlight(AnnotationInnerHighlight { severity: Severity::Error, message: Some("howdy".into()) }),
                    span:  Span::ranged(source, 19..20),
                }],
                15
            )
        );
        println!(
            "{:?}",
            layout_utf8(
                vec![Annotation {
                    inner: AnnotationInner::Highlight(AnnotationInnerHighlight { severity: Severity::Error, message: Some("howdy".into()) }),
                    span:  Span::ranged(source, 7..12),
                }],
                15
            )
        );
        println!(
            "{:?}",
            layout_utf8(
                vec![Annotation {
                    inner: AnnotationInner::Highlight(AnnotationInnerHighlight { severity: Severity::Error, message: Some("howdy".into()) }),
                    span:  Span::ranged(source, 28..33),
                }],
                15
            )
        );

        let empty: &str = "";
        println!(
            "{:?}",
            layout_utf8(
                vec![Annotation {
                    inner: AnnotationInner::Highlight(AnnotationInnerHighlight { severity: Severity::Error, message: Some("howdy".into()) }),
                    span:  Span::new(empty),
                }],
                15
            )
        );
    }
}
