//  FRAME.rs
//    by Lut99
//
//  Created:
//    17 Feb 2024, 12:44:42
//  Last edited:
//    17 Feb 2024, 12:50:16
//  Auto updated?
//    Yes
//
//  Description:
//!   Represents an in-memory frame to which source snippets are rendered
//!   before written to an output stream.
//

use std::fmt::{Display, Formatter, Result as FResult};


/***** LIBRARY *****/
/// Represents a "frame" that lives in memory and to which we can conveniently render a source snippet in an object-like style.
///
/// Conceptually, think of it as a window on the source text where we don't worry yet about how to visualize everything. Instead, we're just concerned in appropriately highlighting source text, annotating it with comments, etc.
#[derive(Clone, Debug)]
pub struct Frame {
    /// The maximum terminal width with which we render.
    max_width: usize,
}
impl Frame {
    /// Constructor for the Frame that initializes it with nothing rendered to it yet.
    ///
    /// # Arguments
    /// - `max_width`: The maximum line width that the Frame uses to render. Any line larger will be somehow truncated.
    #[inline]
    pub fn new(max_width: usize) -> Self { Self { max_width } }
}
