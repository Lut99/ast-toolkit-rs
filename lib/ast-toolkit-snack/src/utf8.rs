//  CHARACTER.rs
//    by Lut99
//
//  Created:
//    14 Mar 2024, 09:06:52
//  Last edited:
//    14 Mar 2024, 09:12:45
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines combinators for matching string-like input.
//

use ast_toolkit_span::Span;

use crate::{Combinator, Result};


/***** LIBRARY *****/
/// Matches a specific "tag", i.e., static input.
///
/// Useful for matching keywords.
///
/// # Arguments
/// - `tag`: The tag to match for.
///
/// # Returns
/// A [`Combinator`] that matches the given `tag`.
pub fn tag<T, F, S>(tag: T) -> impl Combinator<F, S, Output = Span<F, S>> { move |input: Span<F, S>| -> Result<Span<F, S>, F, S> {} }
