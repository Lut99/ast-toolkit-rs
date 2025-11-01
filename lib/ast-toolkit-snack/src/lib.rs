//  LIB.rs
//    by Lut99
//
//  Created:
//    14 Mar 2024, 08:37:24
//  Last edited:
//    08 May 2025, 16:53:50
//  Auto updated?
//    Yes
//
//  Description:
//!   The sun is coming out, the birds are tweeting... It's that time of
//!   the year again! Lut99 makes another re-code of `nom`!
//!
//!   Provides a parser-combinator framework heavily inspired by
//!   [nom](https://github.com/rust-bakery/nom), except that it gives up a
//!   little bit of performance over a more human-friendly debug experience.
//

// Declare submodules
// pub mod ascii;
pub mod asserts;
// pub mod boxed;
// pub mod branch;
// pub mod combinator;
// pub mod debug;
// pub mod error;
// #[cfg(feature = "extra")]
// pub mod extra;
// pub mod fmt;
pub mod auxillary;
pub mod span;
// mod macros;
// pub mod multi;
// pub mod result;
pub mod scan;
// pub mod sequence;
mod spec;
// pub mod utf8;

// Re-exports
pub mod prelude {
    pub use super::auxillary::ResultExt;
}
pub use ast_toolkit_loc::{Loc, Located};
pub use span::*;
// pub use fmt::ExpectsFormatter;
pub use spec::*;
