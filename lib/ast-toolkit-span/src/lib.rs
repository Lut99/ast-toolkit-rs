//  LIB.rs
//    by Lut99
//
//  Created:
//    25 Feb 2024, 11:11:33
//  Last edited:
//    01 Jul 2024, 16:03:53
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
mod as_str;
mod display;
mod eq;
mod hash;
mod lines;
mod locate;
pub mod range;
mod span;
mod spannable;

// Flatten them all in this namespace
pub use as_str::*;
pub use display::*;
pub use eq::*;
pub use hash::*;
pub use lines::*;
pub use locate::*;
pub use span::*;
pub use spannable::*;
