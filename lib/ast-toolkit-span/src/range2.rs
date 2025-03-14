//  RANGE 2.rs
//    by Lut99
//
//  Created:
//    14 Mar 2025, 16:58:17
//  Last edited:
//    14 Mar 2025, 17:04:50
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements [`Range`], an abstraction of a slice of an array.
//


/***** HELPERS *****/
/// Actual implementation of the [`Range`].
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum RangeInner {
    // Fully bounded
    /// Captures a specific slice of the source text.
    ///
    /// Given as `[inclusive, exclusive)`.
    Bounded(usize, usize),

    // Partially bounded
    /// Captures everything of a source text from a specific point onwards.
    ///
    /// Given as `[inclusive, ...`.
    Onwards(usize),
    /// Captures everything of a source text up to a specific point.
    ///
    /// Given as `..., exclusive)`.
    Until(usize),

    // Special cases
    /// Captures the WHOLE source (i.e., open on both ends).
    Full,
    /// Captures NO source.
    Empty,
}





/***** LIBRARY *****/
/// Implements an abstraction of a slice of an array.
///
/// This differs from [`std::ops::Range`] in the following ways:
/// - It implements [`Copy`].
/// - It can encode special states of the range, e.g. [`Empty`](Range::Empty) or open-ended ranges.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Range {
    /// The actual range type, but hidden to avoid manual construction (and preserve properties).
    inner: RangeInner,
}

// Constructors
impl Default for Range {
    #[inline]
    fn default() -> Self { Self { inner: RangeInner::Full } }
}

// Conversion
