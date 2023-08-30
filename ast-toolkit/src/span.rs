//  SPAN.rs
//    by Lut99
// 
//  Created:
//    27 Aug 2023, 12:36:52
//  Last edited:
//    30 Aug 2023, 14:04:10
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines the [`Span`] struct and associates, which we use to keep
//!   track of a node's position in the source text.
// 

use std::ops::{Bound, RangeBounds};

use num_traits::AsPrimitive;
use unicode_segmentation::UnicodeSegmentation as _;

use crate::position::Position;


/***** LIBRARY *****/
/// Abstracts over the specific implementation of a span. This allows us to have varying levels of references VS non-references while avoiding lifetime hell.
/// 
/// Spanning implements the internals of a spanning type, so that we can implement the more high-level functions in [`SpanningExt`].
/// 
/// # Example
/// ```rust
/// use ast_toolkit::Spanning;
/// 
/// struct HelloWorldSpan {
///     start : Option<usize>,
///     end   : Option<usize>,
/// }
/// impl Spanning for HelloWorldSpan {
///     fn file(&self) -> &str { "<example>" }
///     fn source(&self) -> &str { "Hello,\nworld!" }
/// 
///     fn start_idx(&self) -> Option<usize> { self.start }
///     fn end_idx(&self) -> Option<usize> { self.end }
/// }
/// 
/// assert_eq!(HelloWorldSpan { start: Some(0), end: Some(12) }.file(), "<example>");
/// assert_eq!(HelloWorldSpan { start: Some(0), end: Some(4) }.source(), "Hello, world!");
/// assert_eq!(HelloWorldSpan { start: Some(0), end: Some(1) }.start_idx(), Some(0));
/// assert_eq!(HelloWorldSpan { start: Some(4), end: None }.end_idx(), None);
/// // ...
/// ```
pub trait Spanning {
    /// Returns the internal source description (e.g., filename or some other user-friendly description to disambiguate different source texts).
    /// 
    /// # Returns
    /// A reference to the filename, as a string.
    fn file(&self) -> &str;

    /// Returns the entire source captured this Spanning object.
    /// 
    /// This does not take any spanning ranges into account!
    /// 
    /// # Returns
    /// A reference to the source, as a string.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{Span, Spanning as _};
    /// 
    /// assert_eq!(Span::new("<example>", "Hello, world!").source(), "Hello, world!");
    /// assert_eq!(Span::empty("<example>", "Hello, world!").source(), "Hello, world!");
    /// assert_eq!(Span::ranged("<example>", "Hello, world!", 0..5).source(), "Hello, world!");
    /// assert_eq!(Span::ranged("<example>", "Hello, world!", 7..12).source(), "Hello, world!");
    /// assert_eq!(Span::ranged("<example>", "Hello, world!", 7..).source(), "Hello, world!");
    /// assert_eq!(Span::ranged("<example>", "Hello, world!", 7..6).source(), "Hello, world!");
    /// ```
    fn source(&self) -> &str;



    
    /// Returns the start position, as an index, of this Spanning object.
    /// 
    /// Note that not all Spanned values might have sensible start positions. [`Span`]s created with [`Span::empty()`], for example, have no sensible start; nor do Spans over empty sources.
    /// 
    /// Note that this function is typically only useful for functions really working with the internals of a span, since the index is relative to the source.
    /// 
    /// # Returns
    /// The start position if any is defined. If not, [`None`] is returned.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{Span, Spanning as _};
    /// 
    /// assert_eq!(Span::new("<example>", "").start_idx(), None);
    /// assert_eq!(Span::new("<example>", "Hello, world!").start_idx(), Some(0));
    /// assert_eq!(Span::empty("<example>", "Hello, world!").start_idx(), None);
    /// assert_eq!(Span::ranged("<example>", "Hello, world!", 0..5).start_idx(), Some(0));
    /// assert_eq!(Span::ranged("<example>", "Hello, world!", 7..=11).start_idx(), Some(7));
    /// assert_eq!(Span::ranged("<example>", "Hello, world!", 42..=42).start_idx(), Some(42));
    /// ```
    fn start_idx(&self) -> Option<usize>;
    
    /// Returns the end position, as an index, of this Spanning object.
    /// 
    /// Note that not all Spanned values might have sensible start positions. [`Span`]s created with [`Span::empty()`], for example, have no sensible end; nor do Spans over empty sources.
    /// 
    /// Note that this function is typically only useful for functions really working with the internals of a span, since the index is relative to the source.
    /// 
    /// # Returns
    /// The end position if any is defined. If not, [`None`] is returned.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{Span, Spanning as _};
    /// 
    /// assert_eq!(Span::new("<example>", "").end_idx(), None);
    /// assert_eq!(Span::new("<example>", "Hello, world!").end_idx(), Some(12));
    /// assert_eq!(Span::empty("<example>", "Hello, world!").end_idx(), None);
    /// assert_eq!(Span::ranged("<example>", "Hello, world!", 0..5).end_idx(), Some(4));
    /// assert_eq!(Span::ranged("<example>", "Hello, world!", 7..=11).end_idx(), Some(11));
    /// assert_eq!(Span::ranged("<example>", "Hello, world!", 42..=42).end_idx(), Some(42));
    /// ```
    fn end_idx(&self) -> Option<usize>;
}



/// Abstracts over the specific implementation of a span. This allows us to have varying levels of references VS non-references while avoiding lifetime hell.
/// 
/// SpanningExt implements the typical usage functions of a spanning type, and is purely implemented by the internal functions provided by [`Spanning`].
/// 
/// # Example
/// ```rust
/// use ast_toolkit::{Spanning, SpanningExt as _};
/// 
/// struct HelloWorldSpan {
///     start : Option<usize>,
///     end   : Option<usize>,
/// }
/// impl Spanning for HelloWorldSpan {
///     fn file(&self) -> &str { "<example>" }
///     fn source(&self) -> &str { "Hello,\nworld!" }
/// 
///     fn start_idx(&self) -> Option<usize> { self.start }
///     fn end_idx(&self) -> Option<usize> { self.end }
/// }
/// 
/// assert_eq!(HelloWorldSpan { start: Some(0), end: Some(12) }.pos_of(7), Position::new1(2, 1));
/// assert_eq!(HelloWorldSpan { start: Some(0), end: Some(4) }.text(), "Hello");
/// assert_eq!(HelloWorldSpan { start: Some(0), end: Some(1) }.len(), 2);
/// // ...
/// ```
pub trait SpanningExt: Spanning {
    /// Converts a source-relative index to a [`Position`].
    /// 
    /// # Arguments
    /// - `idx`: The index, as a [`usize`]-like, to convert to a [`Position`].
    /// 
    /// # Returns
    /// A new [`Position`] that represents the line and column number of this [`Span`]'s range start.
    /// 
    /// # Panics
    /// This function may panic if the given index is out-of-range for the internal source or if the given index is not on a grapheme boundary.
    /// 
    /// # Example
    /// ```rust
    /// # use std::panic::catch_unwind;
    /// use ast_toolkit::{Position, Span, Spanning as _};
    /// 
    /// assert_eq!(Span::new("<example>", "Hello,\nworld!").pos_of(0), Position::new1(1, 1));
    /// assert_eq!(Span::new("<example>", "Hello,\nworld!").pos_of(5), Position::new1(1, 6));
    /// assert_eq!(Span::new("<example>", "Hello,\nworld!").pos_of(6), Position::new1(1, 7));
    /// assert_eq!(Span::new("<example>", "Hello,\nworld!").pos_of(7), Position::new1(2, 1));
    /// assert_eq!(Span::new("<example>", "Hello,\nworld!").pos_of(12), Position::new1(2, 6));
    /// ```
    /// These will panic:
    /// ```rust
    /// // Out-of-range
    /// assert!(catch_unwind(|| Span::new("<example>", "Hello,\nworld!").pos_of(42)).is_err());
    /// // Not on a grapheme boundary (i.e., byte-wise, 2 is in the middle of `ÿ`)
    /// assert!(catch_unwind(|| Span::new("<example>", "Hÿllo,\nworld!").pos_of(2)).is_err());
    /// ```
    #[track_caller]
    fn pos_of(&self, idx: impl AsPrimitive<usize>) -> Position {
        let idx: usize = idx.as_();
        let source: &str = self.source();

        // Assert it is correctly sized
        if idx >= source.len() { panic!("Given index '{}' is out-of-bounds for Span of length {}", idx, source.len()); }

        // Iterate over the source to find the line & column
        let (mut line, mut col): (usize, usize) = (0, 0);
        for (i, c) in source.grapheme_indices(true) {
            // If we reached it, we done
            if i == idx { break; }
            else if i > idx { panic!("Index {idx} does not point on the grapheme boundary"); }

            // Otherwise, count
            if c == "\n" { line += 1; col = 0; }
            else { col += 1; }
        }

        // Done, return it as a position
        Position::new0(line, col)
    }

    /// Returns the start position of this range in the source text.
    /// 
    /// Note that not all Spanned values might have sensible start positions. [`Span`]s created with [`Span::empty()`], for example, have no sensible start; nor do Spans over empty sources.
    /// 
    /// # Returns
    /// The start [`Position`] of this range in the source text, or [`None`] if the range doesn't have a sensible start.
    /// 
    /// # Panics
    /// This function may panic if the internal start index is not on a grapheme boundary.
    /// 
    /// # Example
    /// ```rust
    /// # use std::panic::catch_unwind;
    /// use ast_toolkit::{Position, Span, Spanning as _};
    /// 
    /// assert_eq!(Span::new("<example>", "Hello,\nworld!").start(), Some(Position::new1(1, 1)));
    /// assert_eq!(Span::empty("<example>", "Hello,\nworld!").start(), None);
    /// assert_eq!(Span::ranged("<example>", "Hello,\nworld!", 5..=5).start(), Some(Position::new1(1, 6)));
    /// assert_eq!(Span::ranged("<example>", "Hello,\nworld!", 7..12).start(), Some(Position::new1(2, 1)));
    /// assert_eq!(Span::ranged("<example>", "Hello,\nworld!", 12..).start(), Some(Position::new1(2, 6)));
    /// assert_eq!(Span::ranged("<example>", "Hello,\nworld!", 42..=42).start(), None);
    /// ```
    /// These will panic:
    /// ```rust
    /// // Not on a grapheme boundary (i.e., byte-wise, 2 is in the middle of `ÿ`)
    /// assert!(catch_unwind(|| Span::ranged("<example>", "Hÿllo,\nworld!", 2..=2).start()).is_err());
    /// ```
    #[inline]
    #[track_caller]
    fn start(&self) -> Option<Position> {
        self.start_idx().map(|start| if start < self.source().len() { Some(self.pos_of(start)) } else { None }).flatten()
    }

    /// Returns the end position of this range in the source text.
    /// 
    /// Note that not all Spanned values might have sensible end positions. [`Span`]s created with [`Span::empty()`], for example, have no sensible end; nor do Spans over empty sources.
    /// 
    /// # Returns
    /// The end [`Position`] of this range in the source text, or [`None`] if the range doesn't have a sensible end.
    /// 
    /// # Panics
    /// This function may panic if the internal end index is not on a grapheme boundary.
    /// 
    /// # Example
    /// ```rust
    /// # use std::panic::catch_unwind;
    /// use ast_toolkit::{Position, Span, Spanning as _};
    /// 
    /// assert_eq!(Span::new("<example>", "Hello,\nworld!").end(), Some(Position::new1(2, 6)));
    /// assert_eq!(Span::empty("<example>", "Hello,\nworld!").end(), None);
    /// assert_eq!(Span::ranged("<example>", "Hello,\nworld!", 5..=5).end(), Some(Position::new1(1, 6)));
    /// assert_eq!(Span::ranged("<example>", "Hello,\nworld!", 7..12).end(), Some(Position::new1(2, 6)));
    /// assert_eq!(Span::ranged("<example>", "Hello,\nworld!", 12..).end(), Some(Position::new1(2, 6)));
    /// assert_eq!(Span::ranged("<example>", "Hello,\nworld!", 42..=42).end(), None);
    /// ```
    /// These will panic:
    /// ```rust
    /// // Not on a grapheme boundary (i.e., byte-wise, 2 is in the middle of `ÿ`)
    /// assert!(catch_unwind(|| Span::ranged("<example>", "Hÿllo,\nworld!", 2..=2).end()).is_err());
    /// ```
    #[inline]
    #[track_caller]
    fn end(&self) -> Option<Position> {
        self.end_idx().map(|end| if end < self.source().len() { Some(self.pos_of(end)) } else { None }).flatten()
    }



    /// Returns the spanned source text.
    /// 
    /// Very similar to [`Spanning::source()`], except this _does_ take the internal ranges into account.
    /// 
    /// # Returns
    /// A string slice containing the spanned source text.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{Span, SpanningExt as _};
    /// 
    /// assert_eq!(Span::new("<example>", "Hello,\nworld!").text(), "Hello,\nworld!");
    /// assert_eq!(Span::empty("<example>", "Hello,\nworld!").text(), "");
    /// assert_eq!(Span::ranged("<example>", "Hello,\nworld!", 0..5).text(), "Hello");
    /// assert_eq!(Span::ranged("<example>", "Hello,\nworld!", 7..12).text(), "world");
    /// assert_eq!(Span::ranged("<example>", "Hello,\nworld!", 7..).text(), "world!");
    /// assert_eq!(Span::ranged("<example>", "Hello,\nworld!", 7..6).text(), "");
    /// ```
    fn text(&self) -> &str {
        let source: &str = self.source();
        match (self.start_idx(), self.end_idx()) {
            (Some(start), Some(end)) => if start < source.len() && end < source.len() && start <= end { &source[start..=end] } else { "" },
            (None, _) | (_, None)    => "",
        }
    }

    /// Returns the lines of the original source containing the spanned text.
    /// 
    /// This similar to [`SpanningExt::text()`], except that the start and end are more liberal to include their entire lines.
    /// 
    /// # Returns
    /// A string slice containing the spanned source text.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{Span, SpanningExt as _};
    /// 
    /// assert_eq!(Span::new("<example>", "Hello,\nworld!").text(), "Hello,\nworld!");
    /// assert_eq!(Span::empty("<example>", "Hello,\nworld!").text(), "");
    /// assert_eq!(Span::ranged("<example>", "Hello,\nworld!", 0..5).text(), "Hello,\n");
    /// assert_eq!(Span::ranged("<example>", "Hello,\nworld!", 6..12).text(), "Hello,\nworld!");
    /// assert_eq!(Span::ranged("<example>", "Hello,\nworld!", 7..12).text(), "world!");
    /// assert_eq!(Span::ranged("<example>", "Hello,\nworld!", 7..).text(), "world!");
    /// assert_eq!(Span::ranged("<example>", "Hello,\nworld!", 7..6).text(), "");
    /// assert_eq!(Span::ranged("<example>", "Hello,\nworld!", 42..=42).text(), "");
    /// ```
    fn lines(&self) -> &str {
        let source: &str = self.source();
        match (self.start_idx(), self.end_idx()) {
            // If the ranges are defined, then limit by the fact the range has to logically contain elements
            (Some(mut start), Some(mut end)) => if start < source.len() && end < source.len() && start <= end {
                // Move the start backwards to find the nearest line
                let mut found: bool = false;
                let mut prev: usize = start;
                for (i, c) in source[..start].grapheme_indices(true).rev() {
                    // If we find a newline, then move start to include the previous character (which is the start of the newline)
                    if c == "\n" { start = prev; found = true; break; }
                    prev = i;
                }
                if !found { start = 0; }

                // Move the end forwards to find the nearest line
                let mut found: bool = false;
                for (i, c) in source[end..].grapheme_indices(true) {
                    // If we find a newline, then accept the line with it
                    if c == "\n" { end = i; found = true; break; }
                }
                if !found { end = source.len() - 1; }   // NOTE: This `end - 1` is OK because we already asserted end is less than source.len(), which can only be true if the source len() > 0.

                // OK, slice the string like this
                &source[start..=end]
            } else {
                ""
            },

            // Catch explicit empties
            (None, _) | (_, None) => "",
        }
    }

 

    /// Returns if this Span would return a non-empty text.
    /// 
    /// # Returns
    /// True if this Span spans nothing, or false otherwise.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{Span, Spanning as _};
    /// 
    /// assert_eq!(Span::new("<example>", "Hello, world!").is_empty(), false);
    /// assert_eq!(Span::empty("<example>", "Hello, world!").is_empty(), true);
    /// assert_eq!(Span::ranged("<example>", "Hello, world!", 0..=4).is_empty(), false);
    /// assert_eq!(Span::ranged("<example>", "Hello, world!", 6..=5).is_empty(), true);
    /// ```
    #[inline]
    fn is_empty(&self) -> bool { self.len() > 0 }

    /// Returns the length of the spanned area.
    /// 
    /// # Returns
    /// The number of characters (or other logical unit) spanned.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{Span, Spanning as _};
    /// 
    /// assert_eq!(Span::new("<example>", "Hello, world!").len(), 13);
    /// assert_eq!(Span::ranged("<example>", "Hello, world!", 0..=4).len(), 5);
    /// assert_eq!(Span::ranged("<example>", "Hello, world!", 7..=12).len(), 6);
    /// assert_eq!(Span::ranged("<example>", "Hello, world!", 5..=5).len(), 1);
    /// assert_eq!(Span::ranged("<example>", "Hello, world!", 6..=5).len(), 0);
    /// ```
    #[inline]
    fn len(&self) -> usize {
        match (self.start_idx(), self.end_idx()) {
            (Some(start), Some(end)) => if start <= end { 1 + end - start } else { 0 },
            (None, _) | (_, None)    => 0,
        }
    }
}



/// Allows one to span over a string reference.
/// 
/// This [`Spanning`]-capable type is optimised for parsing and keeping in ASTs, such as with [`nom`] (see the `nom`-feature).
/// 
/// # Example
/// ```rust
/// todo!();
/// ```
#[derive(Clone, Copy, Debug, Eq, Hash)]
pub struct Span<'f, 's> {
    /// The filename (or other description) of the file we are spanning.
    pub file   : &'f str,
    /// The entire source text to snippet.
    pub source : &'s str,
    /// The range spanned.
    /// 
    /// Note that the following holds:
    /// - Both are inclusive bounds;
    /// - If either side is [`None`], the range is empty;
    /// - If `.0 > .1`, then the range is empty; and
    /// - Either may be out-of-range of the `source` still.
    pub range  : (Option<usize>, Option<usize>),
}

impl<'f, 's> Span<'f, 's> {
    /// Constructor for the Span that makes it span the entire given source text.
    /// 
    /// # Arguments
    /// - `file`: The filename or other identifier that helps the user distinguish between source texts.
    /// - `source`: The source text to span.
    /// 
    /// # Returns
    /// A new instance of Self that covers the entire given `source`.
    /// 
    /// # Example
    /// ```rust
    /// use std::borrow::Cow;
    /// use std::path::PathBuf;
    /// use ast_toolkit::{Position, Span, Spanning as _, SpanningExt as _};
    /// 
    /// // Create some strings
    /// let file: String = PathBuf::from("/tmp/test").display().to_string();
    /// let bytes: Cow<str> = String::from_utf8_lossy(b"Hello, world!");
    /// 
    /// // Build spans over them!
    /// let span1 = Span::new("<example>", "Hello, world!");
    /// let span2 = Span::new(file.as_str(), "Hello, world!");
    /// let span3 = Span::new("<example>", bytes.as_ref());
    /// 
    /// // Use them!
    /// assert_eq!(span1.text(), "Hello, world!");
    /// assert_eq!(span2.file(), "/tmp/test");
    /// assert_eq!(span3.end(), Position::new1(1, 13));
    /// ```
    #[inline]
    pub fn new(file: &'f str, source: &'s str) -> Self {
        let source_len: usize = source.len();
        Self {
            file,
            source,
            // The range is inclusive in the source _unless_ the source is empty; this is only representable using [`None`].
            range : (Some(0), if source_len > 0 { Some(source_len - 1) } else { None }),
        }
    }

    /// Constructor for a Span that has a source text but spans an empty (and undefined) part of it.
    /// 
    /// # Arguments
    /// - `file`: The filename or other identifier that helps the user distinguish between source texts.
    /// - `source`: The source text to span.
    /// 
    /// # Returns
    /// A new instance of Self that covers none of the given `source`.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{Span, SpanningExt as _};
    /// 
    /// let span1 = Span::empty("<example>", "Hello there!");
    /// assert_eq!(span1.text() == "");
    /// ```
    #[inline]
    pub fn empty(file: &'f str, source: &'s str) -> Self {
        Self {
            file,
            source,
            range : (None, None),
        }
    }

    /// Constructor for a Span that spans a particular range of the given source text.
    /// 
    /// # Arguments
    /// - `file`: The filename or other identifier that helps the user distinguish between source texts.
    /// - `source`: The source text to span.
    /// - `range`: Some [`RangeBounds`]-like range. If the start >= end in this range (or start > end if the end bound is inclusive too), then it's an empty range.
    /// 
    /// # Returns
    /// A new instance of Self that covers the given `source` partially.
    #[inline]
    pub fn ranged(file: &'f str, source: &'s str, range: impl RangeBounds<usize>) -> Self {
        let source_len: usize = source.len();
        Self {
            file,
            source,
            range : match (range.start_bound(), range.end_bound()) {
                (Bound::Excluded(start), Bound::Excluded(end)) => (if *start < usize::MAX { Some(*start + 1) } else { None }, if *end > 0 { Some(*end - 1) } else { None }),
                (Bound::Excluded(start), Bound::Included(end)) => (if *start < usize::MAX { Some(*start + 1) } else { None }, Some(*end)),
                (Bound::Excluded(start), Bound::Unbounded)     => (if *start < usize::MAX { Some(*start + 1) } else { None }, if source_len >0 { Some(source_len - 1) } else { None }),
                (Bound::Included(start), Bound::Excluded(end)) => (Some(*start), if *end > 0 { Some(*end - 1) } else { None }),
                (Bound::Included(start), Bound::Included(end)) => (Some(*start), Some(*end)),
                (Bound::Included(start), Bound::Unbounded)     => (Some(*start), if source_len >0 { Some(source_len - 1) } else { None }),
                (Bound::Unbounded, Bound::Excluded(end))       => (Some(0), if *end > 0 { Some(*end - 1) } else { None }),
                (Bound::Unbounded, Bound::Included(end))       => (Some(0), Some(*end)),
                (Bound::Unbounded, Bound::Unbounded)           => (Some(0), if source_len >0 { Some(source_len - 1) } else { None }),
            },
        }
    }

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
    /// This function panics if the given spans do not have the same `file` or `source`. Note that this goes by *pointer equality*.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::Span;
    /// 
    /// let file: &str = "<example>";
    /// let text: &str = "Hello, world!";
    /// let span1 = Span::ranged(file, text, 0..=4);
    /// let span2 = Span::ranged(file, text, 7..=12);
    /// 
    /// assert_eq!(Span::combined(span1, span2).text(), "Hello, world!");
    /// ```
    #[inline]
    #[track_caller]
    pub fn combined<'f2, 's2>(span1: impl AsRef<Span<'f, 's>>, span2: impl AsRef<Span<'f2, 's2>>) -> Self {
        let (span1, span2): (&Span, &Span) = (span1.as_ref(), span2.as_ref());

        // Assert they talk about the same thing
        if !std::ptr::eq(span1.file as *const str, span2.file as *const str) { panic!("Given spans do not have the same `file`"); }
        if !std::ptr::eq(span1.source as *const str, span2.source as *const str) { panic!("Given spans do not have the same `source`"); }

        // Combine the start bounds into a new one
        let start: Option<usize> = match (span1.range.0, span2.range.0) {
            (Some(start1), Some(start2)) => Some(std::cmp::min(start1, start2)),
            (start1, None)               => start1,
            (None, start2)               => start2,
        };
        // Combine the end bounds into a new one
        let end: Option<usize> = match (span1.range.1, span2.range.1) {
            (Some(end1), Some(end2)) => Some(std::cmp::max(end1, end2)),
            (end1, None)             => end1,
            (None, end2)             => end2,
        };

        // Construct a new self out of it
        Self {
            file   : span1.file,
            source : span1.source,
            range  : ((start, end)),
        }
    }



    /// Will expand the range in this Span to include the given Span.
    /// 
    /// # Arguments
    /// - `other`: The other Span to consume.
    /// 
    /// # Returns
    /// A reference to self for chaining.
    /// 
    /// # Panics
    /// This function panics if the given spans do not have the same `file` or `source`. Note that this goes by *pointer equality*.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::Span;
    /// 
    /// let file: &str = "<example>";
    /// let text: &str = "Hello, world!";
    /// let mut span1 = Span::ranged(file, text, 0..=4);
    /// let span2 = Span::ranged(file, text, 7..=12);
    /// 
    /// span1.consume(span2);
    /// assert_eq!(span1.text(), "Hello, world!");
    /// ```
    #[inline]
    #[track_caller]
    pub fn consume(&mut self, other: impl AsRef<Span<'f, 's>>) -> &mut Self {
        // Define in terms of combined
        *self = Span::combined(*self, other);
        self
    }
}
impl<'f, 's> Spanning for Span<'f, 's> {
    #[inline]
    fn file(&self) -> &str { self.file }
    #[inline]
    fn source(&self) -> &str { self.source }

    #[inline]
    fn start_idx(&self) -> Option<usize> {
        self.range.0.map(|start| if start < self.source.len() { Some(start) } else if !self.source.is_empty() { Some(self.source.len() - 1) } else { None }).flatten()
    }
    #[inline]
    fn end_idx(&self) -> Option<usize> {
        self.range.1.map(|end| if end < self.source.len() { Some(end) } else if !self.source.is_empty() { Some(self.source.len() - 1) } else { None }).flatten()
    }
}
impl<'f, 's> SpanningExt for Span<'f, 's> {}

impl<'f, 's, T: SpanningExt> PartialEq<T> for Span<'f, 's> {
    #[inline]
    fn eq(&self, other: &T) -> bool {
        self.file == other.file() && self.text() == self.text()
    }
}
// impl<'f, 's, S: RangeBounds<usize>> Index<S> for Span<'f, 's> {
//     type Output = Self;

//     fn index(&self, index: S) -> &Self::Output {
//         // Compute the new start bound
//         let start: Option<usize> = match (self.range.0, index.start_bound()) {
//             // NOTE: The part below is allowed because, if the new bound would evaluate higher than usize::MAX, then, because start is inclusive, it will always be empty.
//             (Some(base), Bound::Excluded(idx)) => if *idx < usize::MAX && base <= usize::MAX - (*idx + 1) { Some(base + *idx + 1) } else { None },
//             (Some(base), Bound::Included(idx)) => if base <= usize::MAX - *idx { Some(base + *idx) } else { None },
//             (Some(base), Bound::Unbounded)     => Some(base),
//             // NOTE: We defined either side [`None`] as empty, so we never can index that no matter how hard we tried.
//             (None, _)                          => None,
//         };
//         // Compute the new end bound
//         let end: Option<usize> = match (self.range.1, index.end_bound()) {
//             // NOTE: The part below is allowed because, if the new bound would evaluate higher than usize::MAX, then, because end is inclusive, it will always be empty.
//             (Some(base), Bound::Excluded(idx)) => if *idx > 0 && base <= usize::MAX - (*idx - 1) { Some(base + (*idx - 1)) } else { None },
//             (Some(base), Bound::Included(idx)) => if base <= usize::MAX - *idx { Some(base + *idx) } else { None }, 
//             (Some(base), Bound::Unbounded)     => Some(base),
//             // NOTE: We defined either side [`None`] as empty, so we never can index that no matter how hard we tried.
//             (None, _)                          => None,
//         };

//         // Alright create the new span
//         &Span {
//             file   : self.file,
//             source : self.source,
//             range  : (start, end),
//         }
//     }
// }

impl<'f, 's> AsRef<str> for Span<'f, 's> {
    #[inline]
    fn as_ref(&self) -> &str { self.text() }
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
