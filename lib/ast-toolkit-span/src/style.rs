//  STYLE.rs
//    by Lut99
//
//  Created:
//    16 Dec 2023, 12:39:19
//  Last edited:
//    11 Feb 2024, 22:38:59
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines how to style (and default impls for that) Spans and
//!   Diagnostics.
//

use console::Style;


/***** LIBRARY *****/
/// Defines how to colour the formatting for [`Diagnostic`](super::diagnostic::Diagnostic)s [`Span`](super::span::Span)s.
pub trait DiagnosticStyle {
    /// Returns the colour for the from-name (e.g., filename) of a snippet.
    ///
    /// # Returns
    /// A [`Style`] that determines how to format the from-string in the header of a snippet.
    fn location_from(&self) -> Style;
    /// Returns the colour for the line number in the header of a snippet.
    ///
    /// This is not used as styling for each line's number. See ['DiagnosticStyle::line_number()`] for that.
    ///
    /// # Returns
    /// A [`Style`] that determines how to format the line number in the header of a snippet.
    fn location_line(&self) -> Style;
    /// Returns the colour for the colomn number in the header of a snippet.
    ///
    /// # Returns
    /// A [`Style`] that determines how to format the colomn number in the header of a snippet.
    fn location_col(&self) -> Style;
    /// Returns the colour for the colons in the header of a snippet.
    ///
    /// # Returns
    /// A [`Style`] that determines how to format the colons in the header of a snippet.
    fn location_colon(&self) -> Style;

    /// Returns the colour for the line number for every line.
    ///
    /// This is not used as styling for the initial header. See ['DiagnosticStyle::location_line()`] for that.
    ///
    /// # Returns
    /// A [`Style`] that determines how to format the line number.
    fn line_number(&self) -> Style;
    /// Returns the colour for the scaffolding.
    ///
    /// # Returns
    /// A [`Style`] that determines how to format the scaffolding characters.
    fn scaffolding(&self) -> Style;
    /// Returns the colour for marked source text.
    ///
    /// # Returns
    /// A [`Style`] that determines how to format highlighted source text.
    fn source_accented(&self) -> Style;
    /// Returns the colour for unmarked source text.
    ///
    /// # Returns
    /// A [`Style`] that determines how to format vanilla source text.
    fn source_unaccented(&self) -> Style;
    /// Returns the colour for the marker underneath a highlighted source snippet.
    ///
    /// # Returns
    /// A [`Style`] that determines how to format the marker underneath highlighted source text.
    fn source_marker(&self) -> Style;
}

/// Defines a default style that does not style.
impl DiagnosticStyle for () {
    #[inline]
    fn location_from(&self) -> Style { Style::new() }

    #[inline]
    fn location_line(&self) -> Style { Style::new() }

    #[inline]
    fn location_col(&self) -> Style { Style::new() }

    #[inline]
    fn location_colon(&self) -> Style { Style::new() }

    #[inline]
    fn line_number(&self) -> Style { Style::new() }

    #[inline]
    fn scaffolding(&self) -> Style { Style::new() }

    #[inline]
    fn source_accented(&self) -> Style { Style::new() }

    #[inline]
    fn source_unaccented(&self) -> Style { Style::new() }

    #[inline]
    fn source_marker(&self) -> Style { Style::new() }
}

/// Defines a Rust-like default style.
pub struct RustStyle {
    /// The accent of the style.
    pub accent: Style,
}
impl RustStyle {
    /// Initializes a `RustStyle` for errors.
    ///
    /// # Returns
    /// A new RustStyle that has red as accent colour.
    #[inline]
    pub fn error() -> Self { Self { accent: Style::new().bold().red() } }
}
impl DiagnosticStyle for RustStyle {
    #[inline]
    fn location_from(&self) -> Style { Style::new() }

    #[inline]
    fn location_line(&self) -> Style { Style::new() }

    #[inline]
    fn location_col(&self) -> Style { Style::new() }

    #[inline]
    fn location_colon(&self) -> Style { Style::new() }

    #[inline]
    fn line_number(&self) -> Style { Style::new().blue().bold() }

    #[inline]
    fn scaffolding(&self) -> Style { Style::new().blue().bold() }

    #[inline]
    fn source_accented(&self) -> Style { Style::new() }

    #[inline]
    fn source_unaccented(&self) -> Style { Style::new() }

    #[inline]
    fn source_marker(&self) -> Style { self.accent.clone() }
}