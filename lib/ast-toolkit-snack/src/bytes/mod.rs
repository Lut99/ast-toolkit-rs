//  MOD.rs
//    by Lut99
//
//  Created:
//    05 Apr 2024, 13:37:49
//  Last edited:
//    25 Apr 2024, 17:53:42
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines value combinators that are matching raw byte sequences.
//

// Submodules
pub mod complete;
pub mod streaming;

// Imports
use ast_toolkit_span::{Span, SpanRange};

use crate::span::OneOfBytes;
use crate::Result;


/***** LIBRARY *****/
/// Will attempt to match as many bytes from the start of a span as possible, as long as those bytes are in the set of to-be-searched-for bytes.
///
/// This version also accepts matching none of them. See [`one_of1()`] to match at least 1.
///
/// # Arguments
/// - `byteset`: A byte array(-like) that defines the set of characters we are looking for.
///
/// # Returns
/// A closure that will perform the actualy match for the given `byteset`. Note that this closure doesn't ever fail, because matching none is OK.
#[inline]
pub fn one_of0<T, F, S>(byteset: T) -> impl FnMut(Span<F, S>) -> Result<Span<F, S>, F, S>
where
    T: AsRef<[u8]>,
    F: Clone,
    S: Clone + OneOfBytes,
{
    move |input: Span<F, S>| -> Result<Span<F, S>, F, S> {
        let match_point: usize = input.one_of_bytes(SpanRange::Open, byteset.as_ref());
        Result::Ok(input.slice(match_point..), input.slice(..match_point))
    }
}
