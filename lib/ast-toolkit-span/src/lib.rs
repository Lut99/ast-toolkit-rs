//  LIB.rs
//    by Lut99
//
//  Created:
//    25 Feb 2024, 11:11:33
//  Last edited:
//    27 Feb 2024, 16:32:09
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
// mod formatter;
// mod formatting;
mod span;
// mod style;

// Flatten them all in this namespace
// pub use formatter::*;
// pub use formatting::*;
pub use span::*;
// pub use style::*;
