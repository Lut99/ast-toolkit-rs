//  MOD.rs
//    by Lut99
//
//  Created:
//    05 Apr 2024, 13:31:45
//  Last edited:
//    07 Apr 2024, 17:54:57
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

use std::fmt::{Debug, Formatter, Result as FResult};
use std::marker::PhantomData;

// Imports
use ast_toolkit_span::{Span, SpanRange};

use crate::fail::{DebugAsRef, Failure};
use crate::span::MatchBytes;
use crate::{Combinator, Expects, Result};


/***** TESTS *****/
#[cfg(test)]
mod tests {
    use super::tag;
    use crate::fail::Failure;
    use crate::{Combinator as _, Result};

    type Span = ast_toolkit_span::Span<&'static str, &'static str>;


    #[test]
    fn test_tag() {
        // Some success stories
        let input: Span = Span::new("<test>", "Hello, world!");
        let (rem, res) = tag(&"Hello").parse(input).unwrap();
        assert_eq!(rem, input.slice(5..));
        assert_eq!(res, input.slice(..5));
        let (rem, res) = tag(&", ").parse(rem).unwrap();
        assert_eq!(rem, input.slice(7..));
        assert_eq!(res, input.slice(5..7));
        let (rem, res) = tag(&"world!").parse(rem).unwrap();
        assert_eq!(rem, input.slice(13..));
        assert_eq!(res, input.slice(7..13));

        // Failure
        assert!(matches!(tag(&"Goodbye").parse(input), Result::Fail(Failure::Tag { .. })));
        assert!(matches!(tag(&"Ho").parse(input), Result::Fail(Failure::Tag { .. })));
        assert!(matches!(tag(&"hello, world!").parse(input), Result::Fail(Failure::Tag { .. })));
    }
}





/***** LIBRARY *****/
/// The concrete combinator returned by `tag()`.
pub struct Tag<F, S, T: 'static> {
    tag: &'static T,
    _f:  PhantomData<F>,
    _s:  PhantomData<S>,
}
impl<F, S, T> Expects for Tag<F, S, T>
where
    T: Debug,
{
    #[inline]
    fn fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "{:?}", self.tag) }
}
impl<F, S, T> Combinator<F, S> for Tag<F, S, T>
where
    F: Clone,
    S: Clone + MatchBytes,
    T: DebugAsRef,
    &'static T: AsRef<[u8]>,
{
    type Output = Span<F, S>;

    fn parse(&mut self, input: Span<F, S>) -> Result<Self::Output, F, S> {
        // See if we can parse the input
        let btag: &[u8] = self.tag.as_ref();
        let match_point: usize = input.match_bytes(SpanRange::Open, btag);
        if match_point >= btag.len() {
            // Matched the entire tag
            #[cfg(debug_assertions)]
            assert!(match_point == btag.len());
            Result::Ok(input.slice(match_point..), input.slice(..match_point))
        } else {
            // Didn't match the entire tag
            Result::Fail(Failure::Tag { tag: self.tag, span: input.start_onwards() })
        }
    }
}



/// Matches a specific "tag", i.e., static input.
///
/// Useful for matching keywords.
///
/// # Arguments
/// - `tag`: The tag to match for.
///
/// # Returns
/// A [`Combinator`] that matches the given `tag`.
pub fn tag<F, S, T>(tag: &'static T) -> Tag<F, S, T>
where
    F: Clone,
    S: Clone + MatchBytes,
    T: DebugAsRef,
    &'static T: AsRef<[u8]>,
{
    Tag { tag, _f: PhantomData::default(), _s: PhantomData::default() }
}
