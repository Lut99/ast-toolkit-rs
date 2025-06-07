//  LAYOUTERS.rs
//    by Lut99
//
//  Description:
//!   Defines so-called layouters, which can take snippets of source text and
//!   arrange them differently suiting with the user's usecase.
//

use ast_toolkit_span::{Span, Spannable, SpannableBytes};

use super::buffers::layout::{LayoutBuffer, Line};
use crate::layout::buffers::layout::Cell;


/***** HELPER FUNCTIONS *****/
/// Converts the _lower_ half of a byte into a single hexadecimal character, uppercase.
///
/// # Arguments
/// - `b`: A bytes with the top four bits set to `0`.
///
/// # Returns
/// A single byte representing the UTF-8 hexadecimal.
///
/// # Panics
/// If the given byte is too high.
#[inline]
#[track_caller]
fn render_byte(b: u8) -> u8 {
    match b {
        0x00 => b'0',
        0x01 => b'1',
        0x02 => b'2',
        0x03 => b'3',
        0x04 => b'4',
        0x05 => b'5',
        0x06 => b'6',
        0x07 => b'7',
        0x08 => b'8',
        0x09 => b'9',
        0x0A => b'A',
        0x0B => b'B',
        0x0C => b'C',
        0x0D => b'D',
        0x0E => b'E',
        0x0F => b'F',
        _ => panic!("Given byte 0x{b:X} is too high (> 0x0F)"),
    }
}



/// Finds the lines that are touched by the given Span.
///
/// # Arguments
/// - `span`: The [`Span`] to find the lines it touches of.
///
/// # Returns
/// A list of [`Span`]s, each of which spanning a line touched by the given `Span`.
fn find_touched_lines<'s, S: Clone + SpannableBytes<'s>>(span: Span<S>) -> Vec<Span<S>> {
    let source: &S = span.source();
    let source_len: usize = source.len();
    let start: usize = span.range().start_resolved(source_len).unwrap_or(0); // Inclusive
    let end: usize = span.range().end_resolved(source_len).unwrap_or(0); // Exclusive

    // Go thru the _source_, not the span, to find the surrounding lines
    let mut i: usize = 0;
    let mut line_start: usize = 0; // Inclusive
    let mut lines: Vec<Span<S>> = Vec::with_capacity(1);
    source.match_while(|elem| {
        // We initially match on newlines
        if *elem == b'\n' {
            // It's the last element of the current line

            // Store the line
            if i >= start {
                lines.push(Span::ranged(source.clone(), line_start..i + 1));
            }

            // Update the new line start
            line_start = i + 1;

            // Potentially quit
            if i + 1 >= end {
                return false;
            }
        }

        // Now update our position according to the element and keep iterating
        i += 1;
        true
    });
    if i + 1 < end {
        // There's still range left; get the remaining
        lines.push(Span::ranged(source.clone(), line_start..));
    }
    lines
}

/// Finds the chunks that are touched by the given Span.
///
/// # Arguments
/// - `chunk_len`: The size of every chunk.
/// - `span`: The [`Span`] to find the chunks it touches.
///
/// # Returns
/// A list of [`Span`]s, each of which spanning a chunk touched by the given `Span`.
fn find_touched_chunks<'s, S: Clone + SpannableBytes<'s>>(chunk_len: usize, span: Span<S>) -> Vec<Span<S>> {
    let source: &S = span.source();
    let source_len: usize = source.len();
    let mut start: usize = span.range().start_resolved(source_len).unwrap_or(0); // Inclusive
    let mut end: usize = span.range().end_resolved(source_len).unwrap_or(0); // Exclusive

    // Now extend start- and end to touch chunk boundaries.
    start -= start % chunk_len;
    end += if end % chunk_len > 0 { chunk_len - (end % chunk_len) } else { 0 };

    // Now slice
    let mut res = Vec::with_capacity((end - start) / chunk_len);
    let mut i: usize = start;
    while i < end {
        res.push(Span::ranged(source.clone(), i..i + chunk_len));
        i += chunk_len;
    }
    res
}





/***** LIBRARY *****/
/// Defines a general Layouter that will take care of physically rendering everything.
pub trait Layouter<'s, S: Spannable<'s>> {
    /// Renders some [`Span`] to a [`LayoutBuffer`].
    ///
    /// # Arguments
    /// - `span`: Some [`Span`] to layout.
    ///
    /// # Returns
    /// A [`LayoutBuffer`] that will do the layouting.
    fn layout(&self, span: Span<S>) -> LayoutBuffer<S::Elem>;
}



/// Defines a Layouter which lays something out as UTF-8 text.
pub struct TextLayouter;
impl TextLayouter {
    /// Constructs a new TextLayouter.
    ///
    /// This function is only here for convenience. This struct does not have any arguments.
    ///
    /// # Returns
    /// A TextLayouter.
    #[inline]
    pub const fn new() -> Self { Self }
}
impl<'s, S: Clone + SpannableBytes<'s>> Layouter<'s, S> for TextLayouter {
    fn layout(&self, span: Span<S>) -> LayoutBuffer<S::Elem> { find_touched_lines(span).into_iter().map(Line::from).collect() }
}

/// Defines a Layouter which lays something out as a hex reader.
pub struct BytesLayouter {
    /// The size of every line.
    pub line_len: usize,
}
impl Default for BytesLayouter {
    #[inline]
    fn default() -> Self { Self { line_len: 16 } }
}
impl BytesLayouter {
    /// Constructs a new BytesLayouter that will create lines of the given size.
    ///
    /// # Arguments
    /// - `line_len`: The number of bytes represented by every line.
    ///
    /// # Returns
    /// A new BytesLayouter.
    #[inline]
    pub const fn new(line_len: usize) -> Self { Self { line_len } }
}
impl<'s, S: Clone + SpannableBytes<'s>> Layouter<'s, S> for BytesLayouter {
    fn layout(&self, span: Span<S>) -> LayoutBuffer<S::Elem> {
        // Get the chunks of 16 which will be our lines
        let lines: Vec<Span<S>> = find_touched_chunks(self.line_len, span);

        // Render them to actual lines
        lines
            .into_iter()
            .enumerate()
            .map(|(i, s)| {
                let mut line = Line::with_capacity(2 * self.line_len + self.line_len);
                line.set_line_number(format!("{:X}", i * self.line_len));
                for (j, b) in s.as_bytes().iter().copied().enumerate() {
                    if j > 0 {
                        line.push(Cell::from_value(b' '));
                    }
                    line.push(Cell::from_source(i * self.line_len + j, render_byte((0xF0 & b) >> 4)));
                    line.push(Cell::from_source(i * self.line_len + j, render_byte(0x0F & b)));
                }
                line
            })
            .collect()
    }
}





/***** TESTS *****/
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_text_layouter() { todo!() }

    #[test]
    fn test_bytes_layouter() { todo!() }
}
