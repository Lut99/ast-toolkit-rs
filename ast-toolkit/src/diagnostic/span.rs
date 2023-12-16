//  SPAN.rs
//    by Lut99
//
//  Created:
//    15 Dec 2023, 19:05:00
//  Last edited:
//    16 Dec 2023, 12:40:45
//  Auto updated?
//    Yes
//
//  Description:
//!   Provides the [`DisplaySource`] trait, and various types of
//!   [`Diagnostic`]s that implement it to show advanced source texts.
//

use std::borrow::Cow;
use std::fmt::{Display, Formatter, Result as FResult};

use super::style::DiagnosticStyle;


/***** HELPER MACROS *****/
/// Returns the number of digits in the given number.
macro_rules! n_digits {
    ($n:expr) => {
        (((($n as f64).log10() + 1.0) as f64).floor()) as usize
    };
}





/***** FORMATTER *****/
/// Implements the pretty display for [`DisplaySpan`]-capable types.
pub struct DisplaySpanFormatter<'s, S: ?Sized, T> {
    /// The span to display
    span:  &'s S,
    /// The style to display it in.
    style: T,
}
impl<'s, S: Span, T: DiagnosticStyle> Display for DisplaySpanFormatter<'s, S, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        let source: &[u8] = self.span.source();
        let (start, end): ((u64, u64), (u64, u64)) = match (self.span.start(), self.span.end()) {
            (Some(start), Some(end)) => (start, end),
            _ => {
                // Span empty; nothing to do!
                return Ok(());
            },
        };
        println!("source : {source:?}");
        println!("start  : {start:?}");
        println!("end    : {end:?}");

        // Fetch the maximum line number
        // Note: `end()` returns zero-indexed numbers, but we show one-indexed numbers, so convert to get the actual maximum we show
        let max_line: u64 = 1 + end.0;

        // Next, go through the source lines to write them
        let source: Cow<str> = String::from_utf8_lossy(source);
        for (l, line) in source.lines().enumerate() {
            // Do nothing if the line is not within range
            if ((l as u64) < start.0) || ((l as u64) > end.0) {
                continue;
            }

            // Write the line, prefixed with the line number
            writeln!(
                f,
                "{:>width$} {} {}",
                self.style.line_number().apply_to(l + 1),
                self.style.scaffolding().apply_to('|'),
                self.style.source_unaccented().apply_to(line),
                width = n_digits!(max_line) - n_digits!(l)
            )?;
        }

        Ok(())

        // // Next, we extract only the relevant lines from the source
        // let lines_range: (Option<usize>, Option<usize>) = source.lines_range();
        // let (lines, line, start): (&str, usize, usize) = match lines_range {
        //     (Some(start), Some(end)) => {
        //         if start < source.source.len() && end < source.source.len() && start <= end {
        //             // Fully in bounds
        //             (&source.source[start..=end], 1 + &source.source[..start].chars().filter(|c| *c == '\n').count(), start)
        //         } else if start < source.source.len() && start <= end {
        //             // Now we know the source is non-empty & start is within bounds; clip
        //             (&source.source[start..], 1 + &source.source[..start].chars().filter(|c| *c == '\n').count(), start)
        //         } else {
        //             // Evaluates to an empty slice
        //             ("", 0, start)
        //         }
        //     },

        //     // The rest is always empty but might reveal any start info
        //     (Some(start), _) => ("", 0, start),
        //     (_, _) => ("", 0, 0),
        // };

        // // Write the empty line first
        // writeln!(writer, "{} {}", (0..max_line).map(|_| ' ').collect::<String>(), style('|').bold().blue())?;

        // // Next, write the source lines
        // let mut l: usize = line;
        // let mut first_accent: bool = true;
        // let mut line_buffer: String = String::new();
        // let mut mark_buffer: String = String::new();
        // for (i, c) in lines.grapheme_indices(true) {
        //     // Convert the index to one relative to the start of the source
        //     let i: usize = start + i;

        //     // Decide whether to apply highlighting to this character
        //     let highlight: bool = match (source.start_idx(), source.end_idx()) {
        //         (Some(start), Some(end)) => start <= i && i <= end,
        //         (None, _) => false,
        //         (_, None) => false,
        //     };

        //     // Write it either highlighted or not to the line buffer
        //     if c != "\n" && highlight {
        //         line_buffer.push_str(&accent_colour.apply_to(c).to_string());
        //         if first_accent {
        //             mark_buffer.push_str(&accent_colour.apply_to('^').to_string());
        //             first_accent = false;
        //         } else {
        //             mark_buffer.push_str(&accent_colour.apply_to('~').to_string());
        //         }
        //     } else if c != "\n" {
        //         line_buffer.push_str(c);
        //         mark_buffer.push(' ');
        //     } else {
        //         // Write the entire line (flush the buffer)
        //         writeln!(
        //             writer,
        //             "{}{} {} {}",
        //             (0..max_line - n_digits!(l)).map(|_| ' ').collect::<String>(),
        //             style(l).blue().bold(),
        //             style('|').blue().bold(),
        //             line_buffer
        //         )?;
        //         writeln!(writer, "{} {} {}", (0..max_line).map(|_| ' ').collect::<String>(), style('|').blue().bold(), mark_buffer)?;
        //         line_buffer.clear();
        //         mark_buffer.clear();
        //         l += 1;
        //     }
        // }

        // // If there's still anything in the buffers, flush it
        // if !line_buffer.is_empty() {
        //     writeln!(
        //         writer,
        //         "{}{} {} {}",
        //         (0..max_line - n_digits!(l)).map(|_| ' ').collect::<String>(),
        //         style(l).blue().bold(),
        //         style('|').blue().bold(),
        //         line_buffer
        //     )?;
        //     writeln!(writer, "{} {} {}", (0..max_line).map(|_| ' ').collect::<String>(), style('|').blue().bold(), mark_buffer)?;
        // }

        // // Write the final one _or_ a note and we're done
        // if let Some(note) = note {
        //     writeln!(writer, "{} {} {} {}", (0..max_line).map(|_| ' ').collect::<String>(), style('=').bold().blue(), style("note:").bold(), note)
        // } else {
        //     writeln!(writer, "{} {}", (0..max_line).map(|_| ' ').collect::<String>(), style('|').bold().blue())
        // }
    }
}





/***** LIBRARY *****/
/// Allows something that spans source text to be (prettily!) displayed.
pub trait DisplaySpan: Span {
    /// Returns a formatter that will show the source text highlighted by this span.
    ///
    /// # Arguments
    /// - `style`: A [`DisplaySpanStyle`] that is used to define the styling for accents in the formatted text.
    ///
    /// # Returns
    /// A [`DisplaySpanFormatter`] that implements [`Display`].
    #[inline]
    fn display_span<'s, T>(&'s self, style: T) -> DisplaySpanFormatter<'s, Self, T> { DisplaySpanFormatter { span: self, style } }
}
impl<T: Span> DisplaySpan for T {}

/// The half of the trait that is "private", and contains the child-specific implementation functions.
pub trait Span {
    /// Returns the name of the source file where we span.
    ///
    /// # Returns
    /// A [`&str`] that encodes the name of whatever source the text has.
    fn filename(&self) -> &[u8];
    /// Returns the part of the source text that is spanned by this span.
    ///
    /// # Returns
    /// A [`&str`] that encodes the spanned source text.
    fn source(&self) -> &[u8];

    /// Returns the text position of the first character spanned by this span.
    ///
    /// # Returns
    /// A tuple with first the line number (zero-indexed) and then the column number (zero-indexed). If the area spanned by this span is empty, then this should return [`None`] instead.
    fn start(&self) -> Option<(u64, u64)>;
    /// Returns the text position of the last character spanned by this span.
    ///
    /// # Returns
    /// A tuple with first the line number (zero-indexed) and then the column number (zero-indexed). If the area spanned by this span is empty, then this should return [`None`] instead.
    fn end(&self) -> Option<(u64, u64)>;
}
