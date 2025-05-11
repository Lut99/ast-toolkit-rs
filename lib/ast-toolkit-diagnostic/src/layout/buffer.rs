//  BUFFER.rs
//    by Lut99
//
//  Description:
//!   Defines a [`Buffer`], which acts as an output buffer for the layouting algorithm.
//

use ast_toolkit_span::{Span, Spannable};
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
    /// - `pred`: Some predicate for helping us recognize newlines when we see 'em.
    ///
    /// # Returns
    /// A Buffer initialized to render a snippet matching the `span`.
    pub fn from_span(span: Span<S>, pred: impl FnMut(&'s S::Elem) -> bool) -> Self { todo!() }
}

// Ops
