//  COMPLETE.rs
//    by Lut99
//
//  Created:
//    05 Apr 2024, 13:43:32
//  Last edited:
//    05 Apr 2024, 18:42:08
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines some raw byte-matching value combinators that are complete,
//!   i.e., they consider not enough input a typical [`Failure`].
//

use ast_toolkit_span::{Span, SpanRange};

use crate::fail::{DebugAsRef, Failure};
use crate::span::OneOfBytes;
use crate::Result;


/***** LIBRARY *****/
/// Will attempt to match as many bytes from the start of a span as possible, as long as those bytes are in the set of to-be-searched-for bytes.
///
/// This version does _not_ accept matching none of them. See [`one_of0()`] to also allow finding none.
///
/// # Arguments
/// - `byteset`: A byte array(-like) that defines the set of characters we are looking for.
///
/// # Returns
/// A closure that will perform the actualy match for the given `byteset`.
#[inline]
pub fn one_of1<T, F, S>(byteset: &'static T) -> impl FnMut(Span<F, S>) -> Result<Span<F, S>, F, S>
where
    T: DebugAsRef,
    &'static T: AsRef<[u8]>,
    F: Clone,
    S: Clone + OneOfBytes,
{
    move |input: Span<F, S>| -> Result<Span<F, S>, F, S> {
        let match_point: usize = input.one_of_bytes(SpanRange::Open, byteset.as_ref());
        if match_point > 0 {
            Result::Ok(input.slice(match_point..), input.slice(..match_point))
        } else {
            Result::Fail(Failure::OneOfBytes1 { byteset, span: input.start_onwards() })
        }
    }
}
