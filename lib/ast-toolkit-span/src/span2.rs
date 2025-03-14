//  SPAN 2.rs
//    by Lut99
//
//  Created:
//    14 Mar 2025, 16:51:07
//  Last edited:
//    14 Mar 2025, 16:57:53
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the `ast-toolkit`'s [`Span`], which forms the basis of
//!   many other of the crates.
//

use std::ops::Range;

use crate::range::SpanRange;


/***** LIBRARY *****/
/// Defines an abstraction over a part of a source file.
///
/// A source file is abstracted as a vector of objects, where, for the Span to work, we care as
/// least as possible about what these objects are.
///
/// Examples of vectors are:
/// - Bytes arrays.
/// - Strings (text files).
/// - Vectors of tokens.
#[derive(Clone, Copy)]
pub struct Span<F, S> {
    /// The identifier of the location where the thing was parsed from.
    from:   F,
    /// The source array itself.
    source: S,
    /// The total length of the source array.
    len:    usize,
    /// The current slice of that array.
    ///
    /// Give as a [`SpanRange`] to 1) make it [`Copy`]able, and 2) encode different states (e.g.,
    /// explicitly empty).
    slice:  SpanRange,
}

// Standard impls
