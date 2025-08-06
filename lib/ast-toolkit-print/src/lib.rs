//  LIB.rs
//    by Lut99
//
//  Description:
//!   Provides utilities for printing ASTs.
//

// Modules
mod display;
mod formatter;
#[cfg(feature = "macros")]
mod macros;
mod spec;

// Use some of it in global
pub use display::Display;
#[cfg(feature = "color")]
pub use formatter::Style;
pub use formatter::{Formatter, INDENT_SIZE};
pub use spec::{DisplayFmt, Displayable};
