//  LOCATED SPAN.rs
//    by Lut99
//
//  Created:
//    16 Dec 2023, 12:08:41
//  Last edited:
//    16 Dec 2023, 12:52:16
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements necessary traits for [`nom_locate`]'s [`LocatedSpan`].
//

use std::str::from_utf8;

use nom::AsBytes;
pub use nom_locate::LocatedSpan;
use unicode_segmentation::UnicodeSegmentation as _;

use super::span::Span;


/***** UNIT TESTS *****/
#[cfg(test)]
mod tests {
    use nom::Slice as _;

    use super::super::span::DisplaySpan as _;
    use super::super::style::PlainStyle;
    use super::*;

    #[test]
    fn test_located_span_empty() {
        // Test if empty formats nothing
        assert_eq!(LocatedSpan::new_extra("", "<example>").display_span(PlainStyle).to_string(), "");
    }

    #[test]
    fn test_located_span_single_line() {
        // Test single line
        assert_eq!(LocatedSpan::new_extra("Hello, world!", "<example>").display_span(PlainStyle).to_string(), "1 | Hello, world!\n");
    }
    #[test]
    fn test_located_span_multi_line() {
        // Test single line
        assert_eq!(
            LocatedSpan::new_extra("Hello, world!\nGeneral Kenobi!", "<example>").display_span(PlainStyle).to_string(),
            "1 | Hello, world!\n2 | General Kenobi!\n"
        );
    }

    #[test]
    fn test_located_span_single_line_partial() {
        // Test a part in a single line
        assert_eq!(LocatedSpan::new_extra("Hello, world!", "<example>").slice(7..12).display_span(PlainStyle).to_string(), "1 | Hello, world!\n");
    }
}





/***** LIBRARY *****/
impl<T, X> Span for LocatedSpan<T, X>
where
    T: AsBytes,
    X: AsRef<[u8]>,
{
    #[inline]
    fn filename(&self) -> &[u8] { self.extra.as_ref() }

    fn source(&self) -> &[u8] {
        // Get a pointer to the start of this line
        let line_start: *const u8 = self.get_line_beginning().as_ptr();

        // Compute the full size of the source
        let source_len: usize = (self.get_column() - 1) + self.fragment().as_bytes().len();

        // Modify the pointer to include that
        // SAFETY: We can do this because we know LocatedSpan just slices into the same string, and its string is valid.
        unsafe { std::slice::from_raw_parts(line_start, source_len) }
    }

    fn start(&self) -> Option<(u64, u64)> {
        // Early quit if there's nothing to show
        let fragment: &[u8] = self.fragment().as_bytes();
        if fragment.is_empty() {
            return None;
        }

        // Else, compute the things
        if from_utf8(fragment).is_ok() {
            Some((self.location_line() as u64 - 1, self.get_utf8_column() as u64 - 1))
        } else {
            Some((self.location_line() as u64 - 1, self.get_column() as u64 - 1))
        }
    }

    fn end(&self) -> Option<(u64, u64)> {
        // Get the start of the line
        let (mut line, mut col): (u64, u64) = self.start()?;

        // Go through the fragment to count things
        let source: &[u8] = self.fragment().as_bytes();
        if let Ok(source) = from_utf8(source) {
            for c in source.graphemes(true) {
                // Simply count
                col += 1;

                // Do magic things if it's a newline
                if c == "\n" {
                    col = 0;
                    line += 1;
                }
            }
        } else {
            for c in source {
                // Simply count
                col += 1;

                // Do magic things if it's a newline
                if *c == b'\n' {
                    col = 0;
                    line += 1;
                }
            }
        }

        // Return the start + other things
        Some((line, col - 1))
    }
}
