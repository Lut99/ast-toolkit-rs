//  STYLE.rs
//    by Lut99
//
//  Created:
//    16 Dec 2023, 12:39:19
//  Last edited:
//    16 Dec 2023, 12:40:42
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
    /// Returns the colour for unmarked source text.
    ///
    /// # Returns
    /// A [`Style`] that determines how to format vanilla source text.
    fn source_unaccented(&self) -> Style;
}

/// Defines a default style that does not style.
pub struct PlainStyle;
impl DiagnosticStyle for PlainStyle {
    #[inline]
    fn line_number(&self) -> Style { Style::new() }

    #[inline]
    fn scaffolding(&self) -> Style { Style::new() }

    #[inline]
    fn source_unaccented(&self) -> Style { Style::new() }
}

/// Defines a Rust-like default style.
pub struct RustStyle;
impl DiagnosticStyle for RustStyle {
    #[inline]
    fn line_number(&self) -> Style { Style::new().blue().bold() }

    #[inline]
    fn scaffolding(&self) -> Style { Style::new().blue().bold() }

    #[inline]
    fn source_unaccented(&self) -> Style { Style::new() }
}
