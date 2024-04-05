//  MOD.rs
//    by Lut99
//
//  Created:
//    05 Apr 2024, 13:31:45
//  Last edited:
//    05 Apr 2024, 13:36:47
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines combinators for matching input by its value.
//!   
//!   This defines all the "hands on" combinators, which actually touch the
//!   input stream instead of only other combinators.
//

// Declare submodules
pub mod bytes;
pub mod utf8;

// Imports
use ast_toolkit_span::{MatchBytes, Span};

use crate::fail::{DebugAsRef, Failure};
use crate::Result;


/***** TESTS *****/
#[cfg(test)]
mod tests {
    use super::tag;
    use crate::fail::Failure;
    use crate::Result;

    type Span = ast_toolkit_span::Span<&'static str, &'static str>;


    #[test]
    fn test_tag() {
        // Some success stories
        let input: Span = Span::new("<test>", "Hello, world!");
        let (rem, res) = tag(&"Hello")(input).unwrap();
        assert_eq!(rem, input.slice(5..));
        assert_eq!(res, input.slice(..5));
        let (rem, res) = tag(&", ")(rem).unwrap();
        assert_eq!(rem, input.slice(7..));
        assert_eq!(res, input.slice(5..7));
        let (rem, res) = tag(&"world!")(rem).unwrap();
        assert_eq!(rem, input.slice(13..));
        assert_eq!(res, input.slice(7..13));

        // Failure
        assert!(matches!(tag(&"Goodbye")(input), Result::Fail(Failure::Tag { .. })));
        assert!(matches!(tag(&"Ho")(input), Result::Fail(Failure::Tag { .. })));
        assert!(matches!(tag(&"hello, world!")(input), Result::Fail(Failure::Tag { .. })));
    }
}





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
pub fn tag<T, F, S>(tag: &'static T) -> impl FnMut(Span<F, S>) -> Result<Span<F, S>, F, S>
where
    T: DebugAsRef,
    &'static T: AsRef<[u8]>,
    F: Clone,
    S: Clone + MatchBytes,
{
    move |input: Span<F, S>| -> Result<Span<F, S>, F, S> {
        // See if we can parse the input
        let btag: &[u8] = tag.as_ref();
        let match_point: usize = input.match_bytes(btag);
        if match_point >= btag.len() {
            // Matched the entire tag
            #[cfg(debug_assertions)]
            assert!(match_point == btag.len());
            Result::Ok(input.slice(match_point..), input.slice(..match_point))
        } else {
            // Didn't match the entire tag
            Result::Fail(Failure::Tag { tag })
        }
    }
}
