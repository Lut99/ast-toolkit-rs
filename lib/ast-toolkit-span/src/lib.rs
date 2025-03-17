//  LIB.rs
//    by Lut99
//
//  Created:
//    25 Feb 2024, 11:11:33
//  Last edited:
//    17 Mar 2025, 13:06:50
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
pub use spannable::Spannable;
pub use spanning::Spanning;
