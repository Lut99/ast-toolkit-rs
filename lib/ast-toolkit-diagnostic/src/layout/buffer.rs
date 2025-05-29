//  BUFFER.rs
//    by Lut99
//
//  Description:
//!   Defines a [`Buffer`], which acts as an output buffer for the layouting algorithm.
//

use ast_toolkit_span::{Span, Spannable, SpannableBytes};
use better_derive::{Clone, Copy, Debug};

use crate::annotations::Severity;


/***** AUXILLARY *****/
/// Defines the colouring options for a [`Chunk`].
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ChunkColor {
    Severity(Severity),
    Suggestion,
    Plain,
}



/// Defines a continuous chunk of source text in the given [`Line`].
///
/// You can think of this as a single element from a [`Span`]. Or at least, a slot for one.
#[derive(Clone, Copy, Debug)]
pub struct ChunkSource<S> {
    /// It's a chunk from the source.
    /// The value in this chunk.
    value: Span<S>,
    /// The color applied to this chunk.
    color: ChunkColor,
}

// Constructors
impl<S> ChunkSource<S> {
    /// Creates a new chunk that simply copies the given `Span`.
    ///
    /// # Arguments
    /// - `span`: The [`Span`] to make this chunk out of.
    ///
    /// # Returns
    /// A new Cell that has _space_ for a `T`, but not actually one.
    #[inline]
    pub fn new(span: Span<S>) -> Self { Self { value: span, color: ChunkColor::Plain } }
}



/// Defines a continuous chunk of annotations and the likes in the given [`Line`].
#[derive(Clone, Debug)]
pub enum ChunkAnnot {
    /// Some textual remark.
    Message {
        /// A string value is a message.
        value: String,
    },
    /// N continuous marker symbols.
    Marker(usize),
    /// N continuous whitespace symbols.
    Empty(usize),
    /// A remaining fill.
    Fill,
}



/// Defines a single line in the [`Buffer`].
#[derive(Clone, Debug)]
pub enum Line<S> {
    /// It's a line from the original source, and hence, made up out of [`Span`]s.
    Source {
        /// The continuous chunks that make up this line. Requirement: they are all immediately
        /// following each other.
        chunks: Vec<ChunkSource<S>>,
    },
    /// It's a line populated with annotations.
    Annotations {
        /// The chunks here are simply strings, potentially with some empty buffers in between.
        chunks: Vec<ChunkAnnot>,
    },
}

// Constructors
impl<S> Line<S> {
    /// Initializes a new source line of the given size.
    ///
    /// # Arguments
    /// - `span`: The [`Span`] that makes up this line.
    ///
    /// # Returns
    /// A line that represents a single line of source text.
    #[inline]
    pub fn source(span: Span<S>) -> Self { Self::Source { chunks: vec![ChunkSource::new(span)] } }

    /// Initializes a new annotations line.
    ///
    /// By default, it just contains nothing.
    ///
    /// # Returns
    /// A line that represents a single line containing annotations of source text.
    #[inline]
    pub fn annotations() -> Self { Self::Annotations { chunks: vec![ChunkAnnot::Fill] } }
}





/***** LIBRARY *****/
/// The output buffer of the layouting algorithm. It's quite clever, doing some abstraction in
/// order to have all of this make sense in my head.
///
/// It's also abstract over the actual element `T`, like [`Span`]s.
#[derive(Clone, Debug)]
pub struct Buffer<S> {
    /// The list of lines stored in the buffer.
    lines: Vec<Line<S>>,
}

// Constructors
impl<S> Buffer<S> {
    /// Initializes a new, empty buffer.
    ///
    /// # Returns
    /// A Buffer that will Buf^{TM}.
    #[inline]
    pub fn new() -> Self { Self { lines: Vec::new() } }
}
impl<'s, S: Clone + Spannable<'s>> Buffer<S> {
    /// Initializes the buffer from a [`Span`] over some source `S`.
    ///
    /// # Arguments
    /// - `span`: Some [`Span`] to parse.
    /// - `should_linebreak`: A predicate that, given the current position and element, decides if
    ///   a visual linebreak should be inserted after the given element when rendering the source
    ///   text.
    ///
    /// # Returns
    /// A Buffer initialized to render a snippet matching the `span`.
    pub fn from_span(span: Span<S>, mut should_linebreak: impl FnMut(usize, &'s S::Elem) -> bool) -> Self {
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
            if should_linebreak(i, elem) {
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

        // OK, those are the lines for now
        Self { lines: lines.into_iter().map(Line::source).collect() }
    }
}
impl<'s, S: Clone + SpannableBytes<'s>> Buffer<S> {
    /// Initializes the buffer from a [`Span`] over bytes.
    ///
    /// # Arguments
    /// - `span`: Some [`Span`] to parse.
    ///
    /// # Returns
    /// A Buffer initialized to render a snippet matching the `span`.
    #[inline]
    pub fn from_bytes_span(span: Span<S>) -> Self { Self::from_span(span, |i, _| i > 0 && i % 16 == 0) }

    /// Initializes the buffer from a [`Span`] over graphemes.
    ///
    /// # Arguments
    /// - `span`: Some [`Span`] to parse.
    ///
    /// # Returns
    /// A Buffer initialized to render a snippet matching the `span`.
    #[inline]
    pub fn from_utf8_span(span: Span<S>) -> Self { Self::from_span(span, |_, b| *b == b'\n') }
}

// Ops





/***** TESTS *****/
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_buffer_from_span() {
        // Precisely a line
        let span = Span::new("Line 1\nLine 2\nLine 3\n");
        let buffer = Buffer::from_utf8_span(span.slice(7..14));
        assert_eq!(buffer.lines.len(), 1);
        if let Line::Source { chunks } = &buffer.lines[0] {
            assert_eq!(chunks.len(), 1);
            assert_eq!(chunks[0].value, span.slice(7..14));
            assert_eq!(chunks[0].color, ChunkColor::Plain);
        } else {
            panic!("Line is not a source line");
        }

        // Parse of a line
        let buffer = Buffer::from_utf8_span(span.slice(10..11));
        assert_eq!(buffer.lines.len(), 1);
        if let Line::Source { chunks } = &buffer.lines[0] {
            assert_eq!(chunks.len(), 1);
            assert_eq!(chunks[0].value, span.slice(7..14));
            assert_eq!(chunks[0].color, ChunkColor::Plain);
        } else {
            panic!("Line is not a source line");
        }

        // Multi-line
        let buffer = Buffer::from_utf8_span(span.slice(12..15));
        assert_eq!(buffer.lines.len(), 2);
        if let Line::Source { chunks } = &buffer.lines[0] {
            assert_eq!(chunks.len(), 1);
            assert_eq!(chunks[0].value, span.slice(7..14));
            assert_eq!(chunks[0].color, ChunkColor::Plain);
        } else {
            panic!("Line 1 is not a source line");
        }
        if let Line::Source { chunks } = &buffer.lines[1] {
            assert_eq!(chunks.len(), 1);
            assert_eq!(chunks[0].value, span.slice(14..21));
            assert_eq!(chunks[0].color, ChunkColor::Plain);
        } else {
            panic!("Line 2 is not a source line");
        }

        // Bytes
        let buffer = Buffer::from_bytes_span(span.slice(10..11));
        assert_eq!(buffer.lines.len(), 1);
        if let Line::Source { chunks } = &buffer.lines[0] {
            assert_eq!(chunks.len(), 1);
            assert_eq!(chunks[0].value, span.slice(0..17));
            assert_eq!(chunks[0].color, ChunkColor::Plain);
        } else {
            panic!("Line is not a source line");
        }

        // Windows line-endings
        let windows_span = Span::new("Line 1\r\nLine 2\r\nLine 3\r\n");
        let buffer = Buffer::from_utf8_span(windows_span.slice(11..12));
        assert_eq!(buffer.lines.len(), 1);
        if let Line::Source { chunks } = &buffer.lines[0] {
            assert_eq!(chunks.len(), 1);
            assert_eq!(chunks[0].value, windows_span.slice(8..16));
            assert_eq!(chunks[0].color, ChunkColor::Plain);
        } else {
            panic!("Line is not a source line");
        }
    }
}
