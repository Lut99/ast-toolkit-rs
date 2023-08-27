//  SPAN.rs
//    by Lut99
// 
//  Created:
//    27 Aug 2023, 12:36:52
//  Last edited:
//    27 Aug 2023, 12:44:30
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines the [`Span`] struct and associates, which we use to keep
//!   track of a node's position in the source text.
// 


/***** LIBRARY *****/
/// Abstracts over the specific implementation of a span. This allows us to have varying levels of references VS non-references while avoiding lifetime hell.
pub trait Spanning {
    // Child-implemented
    /// Returns the internal source description (e.g., filename or some other user-friendly description to disambiguate different source texts).
    /// 
    /// # Returns
    /// A reference to the filename, as a string.
    fn file(&self) -> &str;
    /// Returns the range spanned by this [`Spanning`] object.
    /// 
    /// Note that:
    /// - The start is inclusive
    /// - The end is inclusive
    /// - `start <= end` must be true
    /// - [`None`] means this span is empty.
    /// 
    /// # Returns
    /// A tuple with the start and end, inclusive, or [`None`] if the spanned area is empty.
    fn range(&self) -> Option<(usize, usize)>;

    // Globally-implemented
    
}



/// Allows one to span over a string reference.
/// 
/// This [`Spanning`]-capable type is optimised for parsing and keeping in ASTs, such as with [`nom`] (see the `nom`-feature).
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Span<'f, 's> {
    /// The filename (or other description) of the file we are spanning.
    pub file   : &'f str,
    /// The entire source text to snippet.
    pub source : &'s str,
}
