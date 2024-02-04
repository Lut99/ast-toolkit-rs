//  SPAN.rs
//    by Lut99
//
//  Created:
//    15 Dec 2023, 19:05:00
//  Last edited:
//    04 Feb 2024, 15:35:24
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements a [`Span`], which abstracts over some input to track a particular location in it.
//

use std::fmt::{Display, Formatter, Result as FResult};

use console::Style;
use num_traits::AsPrimitive;
use unicode_segmentation::UnicodeSegmentation;

use super::style::DiagnosticStyle;


/***** CONSTANTS *****/
/// The number of spaces reserver for line widths.
const MAX_LINE_DIGITS: usize = 4;





/***** HELPER MACROS *****/
/// Returns the number of digits in the given number.
macro_rules! n_digits {
    ($n:expr) => {
        (((($n as f64).log10() + 1.0) as f64).floor()) as usize
    };
}





/***** FORMATTERS *****/
/// Defines a formatter that [`Display`]s the given [`Span`] as a snippet of source text.
#[derive(Debug)]
pub struct TextSnippetFormatter<'s, F, I, D> {
    /// The span to format
    span:   &'s Span<F, I>,
    /// The colour to use for highlithing.
    accent: Style,
    /// The style to use for colouring the input.
    style:  D,
}
impl<'s, F, I, D> Display for TextSnippetFormatter<'s, F, I, D>
where
    F: Display,
    I: Spannable,
    I::Char: TextChar,
    D: DiagnosticStyle,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        // let source: &I = self.span.input_ref();
        // let (start, end): ((u64, u64), (u64, u64)) = match (self.span.start(), self.span.end()) {
        //     (Some(start), Some(end)) => (start, end),
        //     _ => {
        //         // Span empty; nothing to do!
        //         return Ok(());
        //     },
        // };
        // println!("source : {source:?}");
        // println!("start  : {start:?}");
        // println!("end    : {end:?}");

        // // Fetch the maximum line number
        // // Note: `end()` returns zero-indexed numbers, but we show one-indexed numbers, so convert to get the actual maximum we show
        // let max_line: u64 = 1 + end.0;

        // // Next, go through the source lines to write them
        // let source: Cow<str> = String::from_utf8_lossy(source);
        // for (l, line) in source.lines().enumerate() {
        //     // Do nothing if the line is not within range
        //     if ((l as u64) < start.0) || ((l as u64) > end.0) {
        //         continue;
        //     }

        //     // Write the line, prefixed with the line number
        //     writeln!(
        //         f,
        //         "{:>width$} {} {}",
        //         self.style.line_number().apply_to(l + 1),
        //         self.style.scaffolding().apply_to('|'),
        //         self.style.source_unaccented().apply_to(line),
        //         width = n_digits!(max_line) - n_digits!(l)
        //     )?;
        // }

        // Ok(())

        /// Writes a line to the given formatter.
        fn write_line(
            f: &mut Formatter<'_>,
            input: &impl Spannable,
            start: usize,
            end: usize,
            accent: &Style,
            style: &impl DiagnosticStyle,
        ) -> FResult {
            // See if we need to write this line at all
            let start_i: usize = i - line_buf.len();
            // NOTE: `end` is exclusive, we treat `i` as inclusive (to match on the newline only)
            if !(start_i < end && start <= i) {
                return Ok(());
            }

            // Write the prefix to the line
            write!(
                f,
                "{:>width$} {} ",
                style.line_number().apply_to(line),
                style.scaffolding().apply_to('|'),
                width = MAX_LINE_DIGITS - n_digits!(line)
            )?;

            // Write the line one-by-one and highlight where necessary
            let col: usize = 0;
            for c in line_buf.drain(..) {
                // Check if we're in range for highlighting
                let x: usize = start_i + col;
                if x >= start && x < end {
                    // Write the character with highlighting
                    write!(f, "{}", accent.apply_to(c))?;
                } else {
                    // Write without highlighting
                    write!(f, "{}", style.source_unaccented().apply_to(c))?;
                }
            }

            // Write end-of-line
            writeln!(f)
        }


        // Define some shorthands
        let Span { from: _, ref input, start, end } = *self.span;
        let accent: &Style = &self.accent;
        let style: &D = &self.style;

        // Search the input until we find that which we need
        let mut line: usize = 0;
        let mut line_buf: Vec<I::Char> = Vec::new();
        for (i, c) in input.chars().enumerate() {
            if c.is_newline() {
                // NOTE: We'll do this here to get one-indexed for free
                line += 1;

                // Write the line
                write_line(f)?;
            } else {
                // Else, keep collecting lines
                line_buf.push(c);
            }
        }

        // Done
        Ok(())
    }
}





/***** AUXILLARY *****/
/// A helper trait for the [`Span`] that can be implemented for anything used as input.
pub trait Spannable {
    /// The type of the "characters" in this `Spannable` (e.g., `u8` for a slice of bytes).
    type Char;
    /// The type of the iterator that is produced by [`Spannable::chars()`].
    type Iter: Iterator<Item = Self::Char>;

    /// Returns an iterator over the internal "characters".
    ///
    /// # Returns
    /// An iterator of type `Self::Iter` that is used to access the characters in-order.
    fn chars(&self) -> Self::Iter;

    /// Returns the number of spannable things (character, bytes, tokens) in this list.
    ///
    /// # Returns
    /// A [`usize`] with the total number of things.
    fn len(&self) -> usize;
}

// Default impls for [`Spannable`]
impl<'b> Spannable for &'b [u8] {
    type Char = u8;
    type Iter = std::iter::Map<std::slice::Iter<'b, u8>, fn(&u8) -> u8>;

    #[inline]
    fn chars(&self) -> Self::Iter { <[u8]>::iter(self).map(|b| *b) }

    #[inline]
    fn len(&self) -> usize { <[u8]>::len(self) }
}
impl<'s> Spannable for &'s str {
    type Char = &'s str;
    type Iter = unicode_segmentation::Graphemes<'s>;

    #[inline]
    fn chars(&self) -> Self::Iter { <str as UnicodeSegmentation>::graphemes(self, true) }

    #[inline]
    fn len(&self) -> usize { self.chars().count() }
}



/// Helper trait that abstracts over [`char`]s or graphemes.
pub trait TextChar: Display {
    /// Returns if this TextChar is a newline.
    fn is_newline(&self) -> bool;
}

// Default impls for [`TextChar`]
impl TextChar for char {
    fn is_newline(&self) -> bool { *self == '\n' }
}
impl<'s> TextChar for &'s str {
    fn is_newline(&self) -> bool { *self == "\n" }
}





/***** LIBRARY *****/
/// Defines a wrapper around some input source text that allows us to track parts of it.
///
/// Built to be [`nom`](https://github.com/rust-bakery/nom)-compatible; see the `nom`-feature.
///
/// # Example
/// ```rust
/// todo!() 
/// ````
#[derive(Clone, Copy, Debug)]
pub struct Span<F, I> {
    /// Something describing the input (e.g., filename).
    from:  F,
    /// An input of \*something\* spannable.
    input: I,
    /// A start position in this input. Inclusive.
    start: usize,
    /// An end position in this input. Exclusive.
    end:   usize,
}
impl<F, I> Span<F, I> {
    /// Constructor for the `Span` that initializes it from a source text, but spanning nothing.
    ///
    /// # Arguments
    /// - `from`: Something describing the input (e.g., filename).
    /// - `input`: The input source (text) to wrap.
    ///
    /// # Returns
    /// A new `Span` that spans none of the given `input`.
    #[inline]
    pub fn empty(from: F, input: I) -> Self { Self { from, input, start: 0, end: 0 } }
}
impl<F, I: Spannable> Span<F, I> {
    /// Constructor for the `Span` that initializes it to Span the entire given range.
    ///
    /// # Arguments
    /// - `input`: The input source (text) to wrap.
    ///
    /// # Returns
    /// A new `Span` that spans the entire given `input`.
    #[inline]
    pub fn new(from: F, input: I) -> Self {
        let len: usize = input.len();
        Self { from, input, start: 0, end: len }
    }

    /// Provides access to the internal `from`-string.
    ///
    /// If `F` implements [`Copy`], you might prefer [`Span::from()`] instead to avoid the lifetime to `self`.
    ///
    /// # Returns
    /// A reference to the internal `from`-string.
    #[inline]
    pub fn from_ref(&self) -> &F { &self.from }

    /// Provides access to the internal `input`-string.
    ///
    /// If `I` implements [`Copy`], you might prefer [`Span::input()`] instead to avoid the lifetime to `self`.
    ///
    /// # Returns
    /// A reference to the internal `input`-string.
    #[inline]
    pub fn input_ref(&self) -> &I { &self.input }

    /// Returns the start position of the `Span` compared to the start of the full source.
    ///
    /// # Returns
    /// A [`usize`] containing the start position (inclusive) of the source text.
    ///
    /// This size is guaranteed to:
    /// - be smaller-or-equal-to `Span::end()`; and
    /// - never be larger-or-equal-to the length of the input _unless_ the length is 0 (then this is 0).
    #[inline]
    pub fn start(&self) -> usize { self.start }

    /// Allows mutation of the `start`-position in this `Span`.
    ///
    /// Some checks are performed before the value is assigned. These checks are:
    /// - assert that the given value is not larger than `Span::end()`;  and
    /// - assert that the given value is not larger-or-equal-to the length of the input _unless_ the length is 0 (this this must be 0).
    ///
    /// If you are sure your value passes these checks, you can use the unsafe `Span::set_start_unchecked()`.
    ///
    /// # Panics
    /// This function panics if the given value does not meet any of the assertions.
    #[track_caller]
    pub fn set_start(&mut self, start: impl AsPrimitive<usize>) {
        let start: usize = start.as_();

        // Assert things
        if start < self.end {
            panic!("Given start position {} is smaller than internal end position {}", start, self.end);
        }
        let input_len: usize = self.input.len();
        if start > input_len || (input_len > 0 && start == input_len) {
            panic!("Given start position {} is larger-than-or-equal-to internal input length {}", start, input_len);
        }

        // Checks out, store
        self.start = start;
    }

    /// Allows mutation of the `start`-position in this `Span`, without having performed checks.
    ///
    /// Ensure that your given `start`-value:
    /// - is not larger than `Span::end()`;  and
    /// - is not larger-or-equal-to the length of the input _unless_ the length is 0 (this this must be 0).
    ///
    /// Use `Span::set_start()` if you're not sure your value matches the above and would like to have checks.
    #[inline]
    pub unsafe fn set_start_unchecked(&mut self, start: impl AsPrimitive<usize>) { self.start = start.as_(); }

    /// Returns the end position of the `Span` compared to the start of the full source.
    ///
    /// # Returns
    /// A [`usize`] containing the end position (exclusive) of the source text.
    ///
    /// This size is guaranteed to:
    /// - be larger-or-equal-to `Span::start()`; and
    /// - never be larger-or-equal-to the length of the input _unless_ the length is 0 (then this is 0).
    #[inline]
    pub fn end(&self) -> usize { self.end }

    /// Allows mutation of the `end`-position in this `Span`.
    ///
    /// Some checks are performed before the value is assigned. These checks are:
    /// - assert that the given value is not smaller than `Span::start()`;  and
    /// - assert that the given value is not larger-or-equal-to the length of the input _unless_ the length is 0 (this this must be 0).
    ///
    /// If you are sure your value passes these checks, you can use the unsafe `Span::set_end_unchecked()`.
    ///
    /// # Panics
    /// This function panics if the given value does not meet any of the assertions.
    #[track_caller]
    pub fn set_end(&mut self, end: impl AsPrimitive<usize>) {
        let end: usize = end.as_();

        // Assert things
        if end > self.start {
            panic!("Given end position {} is smaller than internal start position {}", end, self.start);
        }
        let input_len: usize = self.input.len();
        if end > input_len || (input_len > 0 && end == input_len) {
            panic!("Given end position {} is larger-than-or-equal-to internal input length {}", end, input_len);
        }

        // Checks out, store
        self.end = end;
    }

    /// Allows mutation of the `end`-position in this `Span`, without having performed checks.
    ///
    /// Ensure that your given `end`-value:
    /// - is not smaller than `Span::start()`;  and
    /// - is not larger-or-equal-to the length of the input _unless_ the length is 0 (this this must be 0).
    ///
    /// Use `Span::set_end()` if you're not sure your value matches the above and would like to have checks.
    #[inline]
    pub unsafe fn set_end_unchecked(&mut self, end: impl AsPrimitive<usize>) { self.end = end.as_(); }
}
impl<F: Copy, I> Span<F, I> {
    /// Provides access to the internal `from`-string.
    ///
    /// If `F` does not implement [`Copy`], you might prefer [`Span::from_ref()`] instead.
    ///
    /// # Returns
    /// The internal `from`-string.
    #[inline]
    pub fn from(&self) -> F { self.from }
}
impl<F, I: Copy> Span<F, I> {
    /// Provides access to the internal `input`-string.
    ///
    /// If `F` does not implement [`Copy`], you might prefer [`Span::input_ref()`] instead.
    ///
    /// # Returns
    /// The internal `input`-string.
    #[inline]
    pub fn input(&self) -> I { self.input }
}
impl<F, I> Span<F, I> {
    /// Formats this `Span` to the given formatter as a snippet of UTF-8 input.
    ///
    /// See [`Span::text_snippet_styled()`] to use ANSI-colours while formatting.
    ///
    /// # Returns
    /// A [`SnippetFormatter`] that formats this snippet to a formatter.
    ///
    /// # Example
    /// ```rust
    /// todo!()
    /// ``
    #[inline]
    pub fn text_snippet(&self) -> TextSnippetFormatter<F, I, ()> { TextSnippetFormatter { span: self, accent: Style::new(), style: () } }

    /// Formats this `Span` to the given formatter as a snippet of UTF-8 input, using ANSI-colours.
    ///
    /// See [`Span::text_snippet()`] to disable styling.
    ///
    /// # Arguments
    /// - `accent`: The [`Style`] to write accents with. Usually, this matches the error level (e.g., red for error).
    /// - `style`: A [`DiagnosticStyle`] to use for colouring.
    ///
    /// # Returns
    /// A [`SnippetFormatter`] that formats this snippet to a formatter with the given style.
    ///
    /// # Example
    /// ```rust
    /// todo!()
    /// ``
    #[inline]
    pub fn text_snippet_styled<D>(&self, accent: Style, style: D) -> TextSnippetFormatter<F, I, D> {
        TextSnippetFormatter { span: self, accent, style }
    }
}
