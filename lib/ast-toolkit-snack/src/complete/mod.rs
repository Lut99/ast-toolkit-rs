//  MOD.rs
//    by Lut99
//
//  Created:
//    20 Mar 2024, 16:34:14
//  Last edited:
//    20 Mar 2024, 17:22:01
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines complete versions of various combinators, which assume that
//!   the entire input source is known and not having enough is a hard
//!   error.
//

// Declare submodules
pub mod bytes;
pub mod utf8;

// Imports
use std::fmt::Debug;

use ast_toolkit_span::{MatchBytes, Span, Spannable};

use crate::fail::Failure;
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
pub fn tag<T, F, S>(tag: &'static T) -> impl Combinator<F, S, Output = Span<F, S>>
where
    T: Debug,
    &'static T: AsRef<[u8]>,
    F: Clone,
    S: Clone + Spannable,
    for<'s> S::Slice<'s>: MatchBytes,
{
    move |input: Span<F, S>| -> Result<Span<F, S>, F, S> {
        // See if we can parse the input
        let btag: &[u8] = tag.as_ref();
        let match_point: usize = input.match_bytes(btag);
        if match_point < btag.len() {
            // Didn't match the entire tag
            Result::Fail(Failure::Tag { tag })
        } else {
            #[cfg(debug_assertions)]
            assert!(match_point == btag.len());
            Result::Ok((input.slice(..match_point), input.slice(match_point..)))
        }
    }
}
