//  LIB.rs
//    by Lut99
//
//  Created:
//    09 Sep 2024, 14:37:40
//  Last edited:
//    24 Mar 2025, 11:59:29
//  Auto updated?
//    Yes
//
//  Description:
//!   Provides tools for defining tokens (i.e., nodes in an AST that
//!   relate to a keyword or punctuation) quickly and ergnomically.
//

// Declare modules
#[cfg(feature = "macros")]
mod macros;
#[cfg(feature = "snack")]
pub mod snack;

// Imports
use ast_toolkit_span::Span;

// Re-exports
pub mod __private {
    #[cfg(feature = "railroad")]
    pub use ast_toolkit_railroad as railroad;
    pub use ast_toolkit_span::{Span, Spannable, Spanning};
}


/***** LIBRARY *****/
/// Abstracts over a keyword or punctuation node in an AST.
///
/// This is practical for representing something that always consists of the same sequence.
pub trait Utf8Token<S>: From<Span<S>> {
    /// The actual sequence of text representing this token.
    const TOKEN: &'static str;
}

/// Abstracts over a delimiting pair of keywords and/or punctuation in an AST.
///
/// This is practical for representing something that always consists of the same sequence of an
/// opening and closing tokens with bits in between (e.g., parenthesis).
pub trait Utf8Delimiter<S>: From<(Span<S>, Span<S>)> {
    /// The actual sequence of text representing the opening delimiter.
    const OPEN_TOKEN: &'static str;
    /// The actual sequence of text representing the closing delimiter.
    const CLOSE_TOKEN: &'static str;
}
