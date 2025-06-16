//  LIB.rs
//    by Lut99
//
//  Created:
//    25 Feb 2024, 11:11:33
//  Last edited:
//    08 May 2025, 10:25:38
//  Auto updated?
//    Yes
//
//  Description:
//!   Provides a [nom](https://github.com/rust-bakery/nom)-compatible Span
//!   that is used to track AST nodes to their location in the source
//!   text.
//!
//!   Used in various other parts of the ast-toolkit.
//

// Declare modules
pub mod range;
mod span;
mod spannable;
mod spanning;

// Bring some of it into the parent namespace
pub use span::Span;
pub use spannable::{Spannable, SpannableBytes};
pub use spanning::Spanning;
