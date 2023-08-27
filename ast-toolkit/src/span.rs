//  SPAN.rs
//    by Lut99
// 
//  Created:
//    27 Aug 2023, 12:36:52
//  Last edited:
//    27 Aug 2023, 23:32:19
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines the [`Span`] struct and associates, which we use to keep
//!   track of a node's position in the source text.
// 

use std::ops::Bound;

use num_traits::AsPrimitive;

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
        todo!();
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
        match (self.start_idx(), self.end_idx()) {
            (Some(start), Some(end)) => if start <= end { &self.source()[start..=end] } else { "" },
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
    /// assert_eq!(Span::ranged("<example>", "Hello,\nworld!", 7..12).text(), "world!");
    /// assert_eq!(Span::ranged("<example>", "Hello,\nworld!", 7..).text(), "world!");
    /// assert_eq!(Span::ranged("<example>", "Hello,\nworld!", 7..6).text(), "");
    /// ```
    fn lines(&self) -> &str {
        match (self.start_idx(), self.end_idx()) {
            // If the ranges are defined, then limit by the fact the range has to logically contain elements
            (Some(start), Some(end)) => if start <= end {
                // Move the start backwards to find the nearest line
                todo!();

                // Move the end forwards to find the nearest line

                // OK, done
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

impl<T: Spanning> SpanningExt for T {}



/// Allows one to span over a string reference.
/// 
/// This [`Spanning`]-capable type is optimised for parsing and keeping in ASTs, such as with [`nom`] (see the `nom`-feature).
/// 
/// # Example
/// ```rust
/// todo!();
/// ```
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Span<'f, 's> {
    /// The filename (or other description) of the file we are spanning.
    pub file   : &'f str,
    /// The entire source text to snippet.
    pub source : &'s str,
    /// The range spanned.
    pub range  : (Option<Bound<usize>>, Option<Bound<usize>>),
}

impl<'f, 's> Spanning for Span<'f, 's> {
    #[inline]
    fn file(&self) -> &str { self.file }
    #[inline]
    fn source(&self) -> &str { self.source }
    // fn source(&self) -> &str {
    //     match self.range {
    //         (Some(Bound::Excluded(start)), Some(Bound::Excluded(end))) => if start <= end && start < usize::MAX { &self.source[start + 1..end] } else { "" },   // NOTE: We can get away with the '""' because, if the start is usize::MAX, then the included start would be beyond the source anyway.
    //         (Some(Bound::Excluded(start)), Some(Bound::Included(end))) => if start <= end && start < usize::MAX { &self.source[start + 1..=end] } else { "" },  // NOTE: We can get away with the '""' because, if the start is usize::MAX, then the included start would be beyond the source anyway.
    //         (Some(Bound::Excluded(start)), Some(Bound::Unbounded))     => if start < usize::MAX { &self.source[start + 1..] } else { "" },                      // NOTE: We can get away with the '""' because, if the start is usize::MAX, then the included start would be beyond the source anyway.
    //         (Some(Bound::Included(start)), Some(Bound::Excluded(end))) => if start <= end { &self.source[start..end] } else { "" },
    //         (Some(Bound::Included(start)), Some(Bound::Included(end))) => if start <= end { &self.source[start..=end] } else { "" },
    //         (Some(Bound::Included(start)), Some(Bound::Unbounded))     => &self.source[start..],
    //         (Some(Bound::Unbounded), Some(Bound::Excluded(end)))       => &self.source[..end],
    //         (Some(Bound::Unbounded), Some(Bound::Included(end)))       => &self.source[..=end],
    //         (Some(Bound::Unbounded), Some(Bound::Unbounded))           => &self.source[..],

    //         // Empties are easy
    //         (None, _) | (_, None) => "",
    //     }
    // }

    #[inline]
    fn start_idx(&self) -> Option<usize> {
        self.range.0.map(|start| match start {
            Bound::Excluded(start) => if start < usize::MAX && start + 1 < self.source.len() { Some(start + 1) } else { None },
            Bound::Included(start) => if start < self.source.len() { Some(start) } else { None },
            Bound::Unbounded       => if !self.source.is_empty() { Some(0) } else { None },
        }).flatten()
    }
    #[inline]
    fn end_idx(&self) -> Option<usize> {
        self.range.1.map(|end| match end {
            Bound::Excluded(end) => if end > 0 && end - 1 < self.source.len() { Some(end - 1) } else { None },
            Bound::Included(end) => if end < self.source.len() { Some(end) } else { None },
            Bound::Unbounded     => if !self.source.is_empty() { Some(self.source.len() - 1) } else { None },
        }).flatten()
    }
}
