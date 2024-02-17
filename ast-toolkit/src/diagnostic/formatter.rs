//  FORMATTER.rs
//    by Lut99
//
//  Created:
//    15 Feb 2024, 22:17:53
//  Last edited:
//    17 Feb 2024, 12:40:39
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements a formatter that takes a source text and one or multiple
//!   spans to show them to the user.
//

use std::borrow::Cow;
use std::fmt::{Display, Formatter, Result as FResult};
use std::ops::{Add, Deref, DerefMut, Sub};

use console::Style;
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





/***** HELPER FUNCTIONS *****/
/// Writes a line of source text, with some part of it highlighted by one or more [`HighlightedRange`]s, to the given [`Formatter`].
///
/// Note that this function *may* recurse if the highlighted range contains a `replace` string with newlines.
///
/// # Arguments
/// - `f`: The [`Formatter`] to write to.
/// - `style`: A [`DiagnosticStyle`] that is used for all non-highlighted text.
/// - `highlights`: A list of [`HighlightedRange`]s that may apply to this line.
/// - `current`: The index of the current [`HighlightedRange`]. Since they're ordered, this is an optimization to progressively go through the list instead of searching everytime.
/// - `linen`: The current line number. May be updated in case the `current` highlight has a replace with newlines in it.
/// - `line`: The current line to write.
///
/// # Returns
/// A buffer usable by [`write_marker_line()`] to write the marker line. As such, the caller manually has to do this, e.g.,:
/// ```ignore
/// // This call
/// let marker_line: Vec<_> = write_line(/* ... */)?;
/// // Be sure to add!
/// writeln!(f)?;
/// write_marker_line(f, style, marker_line)?;
/// ```
/// This is necessary because of the recursion that may happen mid-line.
///
/// # Errors
/// This function errors if it failed to write to the given `f`ormatter.
fn write_line(
    f: &mut Formatter<'_>,
    style: &dyn DiagnosticStyle,
    highlights: &[HighlightedRange],
    current: &mut usize,
    linen: &mut usize,
    line: &str,
) -> Result<Vec<(Option<char>, usize, Style, Option<String>)>, std::fmt::Error> {
    /* Analysis */
    // First, go through the source snippet to divide it into snippets with the same highlighted status
    let mut parts: Vec<(Option<&HighlightedRange>, &str, usize)> = Vec::with_capacity(3);
    let mut buf: (Option<&HighlightedRange>, usize, GraphemeIndex) = (None, 0, GraphemeIndex(0));
    'line: for (g, (i, _)) in line.grapheme_indices(true).enumerate().map(|(g, (i, c))| (GraphemeIndex(g), (i, c))) {
        // Switch on whether the current part is highlighted or not
        if let Some(highlight) = buf.0 {
            // It is; wait for the current highlight to end
            if g == highlight.end {
                // Alright; push this as this highlighted range, then update the current highlight
                let glen: usize = *(g - buf.2);
                if glen > 0 {
                    parts.push((Some(highlight), &line[buf.1..i], glen));
                }
                *current += 1;
                if *current >= highlights.len() {
                    // OPTIMIZATION: Rest is always unhighlighted
                    buf = (None, i, g);
                    break 'line;
                }

                // Now search for a highlight that highlights anything outside of the range (as it may be one of them is completed covered by an earlier one)
                while highlights[*current].end < highlight.end {
                    *current += 1;
                    if *current >= highlights.len() {
                        // OPTIMIZATION: Rest is always unhighlighted
                        buf = (None, i, g);
                        break 'line;
                    }
                }

                // Now either continue with nothing, _or_ hit the ground running if the new highlight immediately falls into this range
                if g >= highlights[*current].start {
                    // Write the highlighted immediately
                    buf = (Some(&highlights[*current]), i, g);
                } else {
                    // There's non-highlighted first
                    buf = (None, i, g);
                }
            }
        } else {
            // It's not; wait for the current highlight to be reached
            // NOTE: `start` behaves like an exclusive end index for the part before the highlight
            if g == highlights[*current].start {
                // Alright; push what we have up to now into the `parts`, then continue with this highlight
                let glen: usize = *(g - buf.2);
                if glen > 0 {
                    parts.push((None, &line[buf.1..i], glen));
                }
                buf = (Some(&highlights[*current]), i, g);
            }
        }
    }
    parts.push((buf.0, &line[buf.1..], line[buf.1..].grapheme_indices(true).count()));



    /* Writing */
    // Write the start of the line
    write!(
        f,
        "{:>width$} {} ",
        style.line_number().apply_to(*linen),
        style.scaffolding().apply_to('|'),
        width = n_digits!(MAX_LINE_DIGITS) - n_digits!(*linen)
    )?;

    // Now write the parts of the line one-by-one
    let mut marker_buf: Vec<(Option<char>, usize, Style, Option<String>)> = Vec::new();
    for (highlight, part, glen) in parts {
        if let Some(highlight) = highlight {
            // See if a replace is necessary
            if let Some(replace) = &highlight.replace {
                // Now it gets fun. Check if the replace contains a newline
                if let Some(newline_pos) = replace.find('\n') {
                    // Write _up to_ the newline...
                    write!(f, "{}", highlight.style.apply_to(&replace[..newline_pos]))?;
                    marker_buf.push((
                        Some(highlight.marker),
                        replace[..newline_pos].grapheme_indices(true).count(),
                        highlight.style.clone(),
                        highlight.comment.clone(),
                    ));

                    // ...then finish the line...
                    writeln!(f)?;
                    write_marker_line(f, style, marker_buf)?;

                    // ...write the remainder replace as a new line (which possibly recurses) and then continue with ourselves
                    *linen += 1;
                    marker_buf = write_line(f, style, &[highlight.clone()], &mut 0usize, linen, &replace[newline_pos + 1..])?;
                } else {
                    // Write as uneventful highlighted, replaced source text
                    write!(f, "{}", highlight.style.apply_to(replace))?;
                    marker_buf.push((
                        Some(highlight.marker),
                        replace.grapheme_indices(true).count(),
                        highlight.style.clone(),
                        highlight.comment.clone(),
                    ));
                }
            } else {
                // Write as uneventful highlighted source text
                write!(f, "{}", highlight.style.apply_to(part))?;
                marker_buf.push((Some(highlight.marker), glen, highlight.style.clone(), highlight.comment.clone()));
            }
        } else {
            // Write as uneventful source text
            write!(f, "{}", style.source_unaccented().apply_to(part))?;
            marker_buf.push((None, glen, Style::new(), None));
        }
    }

    // Done, but don't finish the line; the caller has to do that to properly deal with the recursive case
    Ok(marker_buf)
}

/// Writes a marker line (e.g., `^^^^^^`) below a source line.
///
/// # Arguments
/// - `f`: The [`Formatter`] to write to.
/// - `style`: A [`DiagnosticStyle`] that is used for all non-highlighted text.
/// - `parts`: The parts to write markers as. The meaning is:
///   - [`char`]: The character to write for this area, if any (e.g., `^`). If omitted, assumed to be non-highlighted area.
///   - [`usize`]: How many of these characters to write.
///   - [`Style`]: The style to apply to these characters.
///   - [`Option<String>`]: If given, then write an inline comment for this part.
///
/// # Errors
/// This function errors if it failed to write to the given `f`ormatter.
fn write_marker_line(f: &mut Formatter<'_>, style: &dyn DiagnosticStyle, parts: Vec<(Option<char>, usize, Style, Option<String>)>) -> FResult {
    // Write the start of the line
    write!(f, "{:>width$} {} ", "", style.scaffolding().apply_to('|'), width = n_digits!(MAX_LINE_DIGITS))?;

    // Write the parts
    for (c, len, style, comment) in parts {
        // Write the character `len` times
        let actual_c: char = c.unwrap_or(' ');
        write!(f, "{}", style.apply_to(Repeat(actual_c, len)))?;

        // Attempt to write the comment, if any
        if let Some(comment) = comment {
            todo!();
        }
    }

    // Done!
    writeln!(f)
}





/***** HELPERS *****/
/// Disambiguates a character index ([`usize`]) from grapheme indices.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
struct GraphemeIndex(usize);

impl Add<usize> for GraphemeIndex {
    type Output = Self;

    #[inline]
    fn add(self, rhs: usize) -> Self { GraphemeIndex(self.0 + rhs) }
}
impl Sub<GraphemeIndex> for GraphemeIndex {
    type Output = Self;

    #[inline]
    fn sub(self, rhs: Self) -> Self { GraphemeIndex(self.0 - rhs.0) }
}
impl Sub<usize> for GraphemeIndex {
    type Output = Self;

    #[inline]
    fn sub(self, rhs: usize) -> Self { GraphemeIndex(self.0 - rhs) }
}

impl Deref for GraphemeIndex {
    type Target = usize;

    #[inline]
    fn deref(&self) -> &Self::Target { &self.0 }
}
impl DerefMut for GraphemeIndex {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target { &mut self.0 }
}



/// Repeats something [`Display`]able `n` times.
pub struct Repeat<D>(D, usize);
impl<D: Display> Display for Repeat<D> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        for _ in 0..self.1 {
            write!(f, "{}", self.0)?;
        }
        Ok(())
    }
}





/***** AUXILLARY *****/
/// A helper trait that allows some things to be serialized as a UTF-8 source snippet.
pub trait SourceText {
    /// Returns a text representation of the whole object.
    ///
    /// Implementations can choose between owned or borrowed through the appropricate [`Cow`]-variant.
    ///
    /// # Returns
    /// A [`Cow`] that contains the binary reference.
    fn as_str(&self) -> Cow<str>;
}

// Default binary impls for [`SourceText`].
impl<'b, const LEN: usize> SourceText for &'b [u8; LEN] {
    #[inline]
    fn as_str(&self) -> Cow<str> { String::from_utf8_lossy(self.as_slice()) }
}
impl<'b> SourceText for &'b [u8] {
    #[inline]
    fn as_str(&self) -> Cow<str> { String::from_utf8_lossy(self) }
}
impl SourceText for Vec<u8> {
    #[inline]
    fn as_str(&self) -> Cow<str> { String::from_utf8_lossy(self.as_slice()) }
}

// Default string impls for [`SourceText`].
impl<'s> SourceText for &'s str {
    #[inline]
    fn as_str(&self) -> Cow<str> { Cow::Borrowed(self) }
}
impl SourceText for String {
    #[inline]
    fn as_str(&self) -> Cow<str> { Cow::Borrowed(<String>::as_str(self)) }
}



/// Represents a highlighted area, usually directly matching a span.
#[derive(Clone)]
pub struct HighlightedRange {
    /// The start index of the range in the source text. Given as a grapheme index, inclusive.
    start:  GraphemeIndex,
    /// The end index of the range in the source text. Given as a grapheme index, exclusive.
    end:    GraphemeIndex,
    /// The styling to apply to this area.
    style:  Style,
    /// Determines the character to use when writing the highlight markers (e.g., `^`)
    marker: char,

    /// If given, will replace the source text with this text instead.
    replace: Option<String>,
    /// If given, will attach some inline comment to the highlight `marker`s.
    comment: Option<String>,
}





/***** LIBRARY *****/
/// Defines a formatter that implements [`Display`] and that can format one or more Spans in a UTF-8 source text.
pub struct SourceTextFormatter<S> {
    /// The source-string to format.
    source:     S,
    /// A list of areas in the source text to highlight.
    ///
    /// NOTE: Must be sorted on start index (low to high) for the formatter to correctly figure out which source text range to display at all.
    highlights: Vec<HighlightedRange>,
    /// A style that dictates how everything non-highlighted looks.
    style:      Box<dyn DiagnosticStyle>,
}
impl<S: SourceText> Display for SourceTextFormatter<S> {
    #[track_caller]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        let Self { source, highlights, style } = self;
        let source: Cow<str> = source.as_str();

        // Assert at least one highlight is given
        if highlights.is_empty() {
            panic!("Cannot Display a SourceTextFormatter that has no highlights");
        }
        let first: &HighlightedRange = highlights.first().unwrap();
        let last: &HighlightedRange = highlights.last().unwrap();
        let mut current: usize = 0;

        // Iterate over the source's graphemes
        let mut linen: usize = 1;
        let mut gline_start: GraphemeIndex = GraphemeIndex(0);
        let mut iline_start: usize = 0;
        for (g, (i, c)) in source.grapheme_indices(true).enumerate().map(|(g, (i, c))| (GraphemeIndex(g), (i, c))) {
            // Check if this character ends the line
            if is_newline!(c) {
                // It does!
                // NOTE: Inclusive, to take the newline character itself into account
                let gline_end: GraphemeIndex = g;
                let iline_end: usize = i;

                // Decide if we shall write the line
                if gline_start < last.end && first.start <= gline_end {
                    // It's in range of all the highlights, so we shall
                    let line: &str = &source[iline_start..=iline_end];

                    // Write the line now
                    let marker_line: Vec<_> = write_line(f, &**style, highlights, &mut current, &mut linen, line)?;
                    writeln!(f)?;
                    write_marker_line(f, &**style, marker_line)?;
                }

                // Now move the line starts
                gline_start = g + 1;
                iline_start = i + 1;

                // Update the line number too
                linen += 1;
            }
        }
        // Write the last line, if any
        if iline_start < source.len() {
            // NOTE: Inclusive, to take the newline character itself into account
            let gline_end: GraphemeIndex = gline_start + source[iline_start..].graphemes(true).count();
            let iline_end: usize = source.len() - 1;

            // Decide if we shall write the line
            if gline_start < last.end && first.start <= gline_end {
                // It's in range of all the highlights, so we shall
                let line: &str = &source[iline_start..=iline_end];

                // Write the line now
                let marker_line: Vec<_> = write_line(f, &**style, highlights, &mut current, &mut linen, line)?;
                writeln!(f)?;
                write_marker_line(f, &**style, marker_line)?;
            }
        }

        // Done!
        writeln!(f)
    }
}
