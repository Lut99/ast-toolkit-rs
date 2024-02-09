//  STYLE.rs
//    by Lut99
//
//  Created:
//    16 Dec 2023, 12:39:19
//  Last edited:
//    09 Feb 2024, 17:59:29
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
    /// Returns the colour for the line number.
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
