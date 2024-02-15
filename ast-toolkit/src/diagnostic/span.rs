//  SPAN.rs
//    by Lut99
//
//  Created:
//    15 Dec 2023, 19:05:00
//  Last edited:
//    15 Feb 2024, 22:25:32
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements a [`Span`], which abstracts over some input to track a particular location in it.
//

use std::borrow::Cow;
use std::fmt::{Display, Formatter, Result as FResult};

use num_traits::AsPrimitive;
use unicode_segmentation::UnicodeSegmentation;

use super::style::DiagnosticStyle;


/***** CONSTANTS *****/
/// The number of spaces reserver for line widths.
const MAX_LINE_DIGITS: usize = 4;





/***** HELPER MACROS *****/
/// Returns if the given grapheme is a newline.
macro_rules! is_newline {
    ($c:ident) => {
        ($c == "\n" || $c == "\r\n")
    };
}

/// Returns the number of digits in the given number.
macro_rules! n_digits {
    ($n:expr) => {
        (((($n as f64).log10() + 1.0) as f64).floor()) as usize
    };
}





// /***** FORMATTERS *****/
// /// Defines a formatter that [`Display`]s the given [`Span`] as a snippet of source text.
// pub struct TextSnippetFormatter<'s, F, I> {
//     /// The span to format
//     span:  &'s Span<F, I>,
//     /// An _additional_ range (as `[inclusive, exclusive)`) that determines the range to show.
//     ///
//     /// If omitted, defaults to the lines containing the `span`.
//     shown: Option<(usize, usize)>,
//     /// The style to use for colouring the input.
//     style: Box<dyn DiagnosticStyle>,
// }
// impl<'s, F, I> TextSnippetFormatter<'s, F, I> {
//     // /// Factory method that changes which part of the text is shown by the formatter.
//     // ///
//     // /// This is not the highlighted text (that is the span), but instead the text that is shown at all.
//     // ///
//     // /// Defaults to the lines containing the given span.
//     // ///
//     // /// # Arguments
//     // /// - `range`: A range of text (as `[inclusive, exclusive)`) that represents the shown area of the input span's text.
//     // ///
//     // /// # Returns
//     // /// Self for chaining.
//     // pub fn shown(mut self, range: (usize, usize)) -> Self {
//     //     self.shown = Some(range);
//     //     self
//     // }

//     // /// Factory method that changes the style used by the formatter.
//     // ///
//     // /// # Arguments
//     // /// - `style`: A [`DiagnosticStyle`] that is used to find styling for snippets.
//     // ///
//     // /// # Returns
//     // /// Self for chaining.
//     // pub fn style(mut self, style: impl 'static + DiagnosticStyle) -> Self {
//     //     self.style = Box::new(style);
//     //     self
//     // }
// }
// impl<'s, F, I> Display for TextSnippetFormatter<'s, F, I>
// where
//     F: Display,
//     I: TextSpannable,
// {
//     fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
//         // let source: &I = self.span.input_ref();
//         // let (start, end): ((u64, u64), (u64, u64)) = match (self.span.start(), self.span.end()) {
//         //     (Some(start), Some(end)) => (start, end),
//         //     _ => {
//         //         // Span empty; nothing to do!
//         //         return Ok(());
//         //     },
//         // };
//         // println!("source : {source:?}");
//         // println!("start  : {start:?}");
//         // println!("end    : {end:?}");

//         // // Fetch the maximum line number
//         // // Note: `end()` returns zero-indexed numbers, but we show one-indexed numbers, so convert to get the actual maximum we show
//         // let max_line: u64 = 1 + end.0;

//         // // Next, go through the source lines to write them
//         // let source: Cow<str> = String::from_utf8_lossy(source);
//         // for (l, line) in source.lines().enumerate() {
//         //     // Do nothing if the line is not within range
//         //     if ((l as u64) < start.0) || ((l as u64) > end.0) {
//         //         continue;
//         //     }

//         //     // Write the line, prefixed with the line number
//         //     writeln!(
//         //         f,
//         //         "{:>width$} {} {}",
//         //         self.style.line_number().apply_to(l + 1),
//         //         self.style.scaffolding().apply_to('|'),
//         //         self.style.source_unaccented().apply_to(line),
//         //         width = n_digits!(max_line) - n_digits!(l)
//         //     )?;
//         // }

//         // Ok(())

//         /// Translates an index to a line/column pair for the given input.
//         ///
//         /// # Arguments
//         /// - `input`: The [`str`]ing to resolve the line number in.
//         /// - `pos`: The one-dimensional position to translate.
//         ///
//         /// # Returns
//         /// A two-dimensional counterpart to the `pos`, which is a tuple of **one-indexed** line- and column-numbers, respectively.
//         ///
//         /// # Panics
//         /// This function may panic if `pos` is out-of-range for this snippet.
//         #[track_caller]
//         fn index_to_pos(input: &str, mut pos: usize) -> (usize, usize) {
//             // Loop to find
//             for (l, line) in input.lines().enumerate() {
//                 for (c, _) in line.grapheme_indices(true) {
//                     // Check if we found the location indexed by `pos`
//                     if pos == 0 {
//                         return (l + 1, c + 1);
//                     }

//                     // Else, decrement pos to keep searching
//                     pos -= 1;
//                 }
//             }
//             // It was out-of-range
//             panic!("Given index '{pos}' is out-of-range for text snippet of {} graphemes", input.graphemes(true).count());
//         }

//         // /// Writes a line to the given formatter.
//         // fn write_line(
//         //     f: &mut Formatter<'_>,
//         //     line: &mut usize,
//         //     line_buf: &mut Vec<C>,
//         //     i: usize,
//         //     start: usize,
//         //     end: usize,
//         //     style: &impl DiagnosticStyle,
//         // ) -> FResult {
//         //     // NOTE: We'll do this here to get one-indexed for free
//         //     *line += 1;

//         //     // See if we need to write this line at all (either if the range is empty and we're in the start line, or we're in the range)
//         //     let line_buf_len: usize = line_buf.len();
//         //     let start_i: usize = i - line_buf_len;
//         //     // NOTE: `end` is exclusive, we treat `i` as inclusive (to match on the newline only)
//         //     if !((start == end && start >= start_i && start <= i) || (start_i < end && start <= i)) {
//         //         return Ok(());
//         //     }

//         //     // Write the prefix to the line
//         //     write!(
//         //         f,
//         //         "{:>width$} {} ",
//         //         style.line_number().apply_to(*line),
//         //         style.scaffolding().apply_to('|'),
//         //         width = 1 + MAX_LINE_DIGITS - n_digits!(*line)
//         //     )?;

//         //     // Write the line one-by-one and highlight where necessary
//         //     for (col, c) in line_buf.drain(..).enumerate() {
//         //         // Check if we're in range for highlighting
//         //         let x: usize = start_i + col;
//         //         if x >= start && x < end {
//         //             // Write the character with highlighting
//         //             write!(f, "{}", style.source_accented().apply_to(c.display()))?;
//         //         } else {
//         //             // Write without highlighting
//         //             write!(f, "{}", style.source_unaccented().apply_to(c.display()))?;
//         //         }
//         //     }

//         //     // Write end-of-line
//         //     writeln!(f)?;



//         //     // Next, write the accent highlight line, but only if we're accenting something
//         //     if start_i < end && start <= i {
//         //         write!(f, "{:>width$} {} ", "", style.scaffolding().apply_to('|'), width = MAX_LINE_DIGITS)?;
//         //         for col in 0..line_buf_len {
//         //             // Check if we're in range for highlighting
//         //             let x: usize = start_i + col;
//         //             if x >= start && x < end {
//         //                 // Write the character with highlighting
//         //                 write!(f, "{}", style.source_marker().apply_to('^'))?;
//         //             } else {
//         //                 // Write without highlighting
//         //                 write!(f, " ")?;
//         //             }
//         //         }
//         //         writeln!(f)?;
//         //     }

//         //     // Done!
//         //     Ok(())
//         // }


//         // Define some shorthands
//         let Self { span: Span { from, input, start, end }, shown, style } = self;
//         let input: Cow<str> = input.as_str();

//         // Resolve the visible range to either itself or the span (and include that it spans the lines)
//         let mut full_lines: bool = shown.is_none();
//         let (sstart, send): (usize, usize) = shown.unwrap_or((*start, *end));

//         // Iterate over the graphemes to collect lines to write
//         let mut highlighted: bool = false;
//         let (mut line, mut col): (usize, usize) = (1, 1);
//         let mut line_start: usize = 0;
//         for (g, (i, c)) in input.grapheme_indices(true).enumerate() {
//             // Check if we found a line yet
//             if is_newline!(c) {
//                 // NOTE: We take the grapheme index as the [start, end)-range is in this too
//                 let line_end: usize = g;

//                 // Check if this line starts the highlighted area

//                 // Check against the visible range to see if we need to write this line
//                 if sstart <= line_end && line_start < send {
//                     // We do; so search the line for the split point
//                     let mut graphemes: usize = 0;
//                     for (lg, (li, lc)) in input[line_start..=i].grapheme_indices(true).enumerate() {
//                         // If this is within range, then write the accented line
//                         let g: usize = line_start + lg;
//                         if g >= *start && g < *end {
//                             highlighted = true;
//                         }

//                         // Always continue counting
//                         graphemes += 1;
//                     }
//                 }

//                 // Either way, update line count and reset col
//                 line += 1;
//                 col = 1;
//             }

//             // Always increment the column
//             col += 1;
//         }

//         // // Resolve all ranges from grapheme indices to byte indices, for efficient slicing
//         // let (mut highlighted, mut shown): ((Option<usize>, Option<usize>), (Option<usize>, Option<usize>)) = ((None, None), (None, None));
//         // for (g, (i, _)) in input.grapheme_indices(true).enumerate() {
//         //     if g == *start {
//         //         highlighted.0 = Some(i);
//         //     }
//         //     if g + 1 == *end {
//         //         highlighted.1 = Some(i);
//         //     }
//         //     if let Some((start, end)) = range {
//         //         if g == start {
//         //             shown.0 = Some(i);
//         //         }
//         //         if g + 1 == end {
//         //             shown.1 = Some(i);
//         //         }
//         //     }
//         // }
//         // let highlighted: (usize, usize) = if let (Some(start), Some(end)) = highlighted {
//         //     (start, end)
//         // } else {
//         //     panic!("Span range [{},{}) is out-of-range for input of {} graphemes", start, end, input.graphemes(true).count())
//         // };
//         // let shown: Option<(usize, usize)> = if let Some(range) = range {
//         //     if let (Some(start), Some(end)) = shown {
//         //         Some((start, end))
//         //     } else {
//         //         panic!("Visible range [{},{}) is out-of-range for input of {} graphemes", range.0, range.1, input.graphemes(true).count())
//         //     }
//         // } else {
//         //     None
//         // };

//         // // Next, resolve the range if it's [`None`]
//         // let shown: (usize, usize) = if let Some(shown) = shown {
//         //     shown
//         // } else {
//         //     // Assume the span, then find the first newline characters on either side
//         //     // NOTE: Returns `[inclusive, exclusive)`, excluding the left newline but including the right
//         //     (input[..*start].rfind('\n').map(|p| p + 1).unwrap_or(0), input[*end..].find('\n').map(|p| p + 1).unwrap_or(input.len()))
//         // };

//         // // Write the initial from header
//         // let (line, col): (usize, usize) = index_to_pos(input.as_ref(), *start);
//         // writeln!(
//         //     f,
//         //     "{:>width$} {} {}{}{}{}{}",
//         //     "",
//         //     style.scaffolding().apply_to("-->"),
//         //     style.location_from().apply_to(from),
//         //     style.location_colon().apply_to(':'),
//         //     style.location_line().apply_to(line),
//         //     style.location_colon().apply_to(':'),
//         //     style.location_col().apply_to(col),
//         //     width = MAX_LINE_DIGITS - 1,
//         // )?;

//         // // Find the range to show
//         // let shown: &str = &input[shown.0..shown.1];
//         // // Split that into pre, highlighted, and post
//         // let (pre, mid, post): (&str, &str, &str) = (&shown[..highlighted.0], &shown[highlighted.0..highlighted.1], &shown[highlighted.1..]);

//         // // // Write all lines of the first one
//         // // for (l, line) in pre.lines().enumerate() {
//         // //     writeln!(f, "{:>} {line}");
//         // // }

//         // Done
//         Ok(())
//     }
// }





/***** AUXILLARY *****/
/// A helper trait for the [`Span`] that can be implemented for anything used as input.
pub trait Spannable {
    /// Returns the number of spannable things (character, bytes, tokens) in this list.
    ///
    /// # Returns
    /// A [`usize`] with the total number of things.
    fn len(&self) -> usize;
}

// Default binary impls for [`Spannable`]
impl<'b, const LEN: usize> Spannable for &'b [u8; LEN] {
    #[inline]
    fn len(&self) -> usize { LEN }
}
impl<'b> Spannable for &'b [u8] {
    #[inline]
    fn len(&self) -> usize { <[u8]>::len(self) }
}
impl Spannable for Vec<u8> {
    #[inline]
    fn len(&self) -> usize { <Vec<u8>>::len(self) }
}

// Default string impls for [`Spannable`]
impl<'s> Spannable for &'s str {
    #[inline]
    fn len(&self) -> usize { self.chars().count() }
}
impl Spannable for String {
    #[inline]
    fn len(&self) -> usize { self.graphemes(true).count() }
}



/// A helper trait for a [`Spannable`] that allows it to be serialized as a binary snippet.
pub trait BinarySpannable: Spannable {
    /// Returns a binary representation of the whole object.
    ///
    /// Implementations can choose between owned or borrowed through the appropricate [`Cow`]-variant.
    ///
    /// # Returns
    /// A [`Cow`] that contains the binary reference.
    fn as_bytes(&self) -> Cow<[u8]>;
}

// Default binary impls for [`BinarySpannable`]
impl<'b, const LEN: usize> BinarySpannable for &'b [u8; LEN] {
    #[inline]
    fn as_bytes(&self) -> Cow<[u8]> { Cow::Borrowed(self.as_slice()) }
}
impl<'b> BinarySpannable for &'b [u8] {
    #[inline]
    fn as_bytes(&self) -> Cow<[u8]> { Cow::Borrowed(self) }
}
impl BinarySpannable for Vec<u8> {
    #[inline]
    fn as_bytes(&self) -> Cow<[u8]> { Cow::Borrowed(self.as_slice()) }
}

// Default string impls for [`BinarySpannable`]
impl<'s> BinarySpannable for &'s str {
    #[inline]
    fn as_bytes(&self) -> Cow<[u8]> { Cow::Borrowed(<str>::as_bytes(self)) }
}
impl BinarySpannable for String {
    #[inline]
    fn as_bytes(&self) -> Cow<[u8]> { Cow::Borrowed(self.as_bytes()) }
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
    // /// Formats this `Span` to the given formatter as a snippet of UTF-8 input.
    // ///
    // /// See [`Span::text_snippet_styled()`] to use ANSI-colours while formatting.
    // ///
    // /// # Returns
    // /// A [`SnippetFormatter`] that formats this snippet to a formatter.
    // ///
    // /// # Example
    // /// ```rust
    // /// todo!()
    // /// ``
    // #[inline]
    // pub fn text_snippet(&self) -> TextSnippetFormatter<F, I> { TextSnippetFormatter { span: self, shown: None, style: Box::new(()) } }
}
