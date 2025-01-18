//  TAG.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 22:25:00
//  Last edited:
//    18 Jan 2025, 17:41:28
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the (binary) [`tag()`]-combinator.
//

use std::convert::Infallible;
use std::marker::PhantomData;

use ast_toolkit_span::Span;
use ast_toolkit_span::range::SpanRange;

pub use super::super::complete::tag::{ExpectsFormatter, Recoverable};
use crate::Combinator2;
use crate::result::{Result as SResult, SnackError};
use crate::span::{LenBytes, MatchBytes};


/***** COMBINATORS *****/
/// Actual combinator implementing [`tag()`].
pub struct Tag<'t, F, S> {
    /// The actual tag that is being matched for.
    tag: &'t [u8],
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f:  PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:  PhantomData<S>,
}
// NOTE: This lifetime trick will tell Rust that the impl is actually not invariant, but accepts
// any smaller lifetime than `'t`.
impl<'c, 't, F, S> Combinator2<'c, F, S> for Tag<'t, F, S>
where
    't: 'c,
    F: Clone,
    S: Clone + LenBytes + MatchBytes,
{
    type ExpectsFormatter = ExpectsFormatter<'t>;
    type Output = Span<F, S>;
    type Recoverable = Recoverable<'t, F, S>;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { tag: self.tag } }

    fn parse(&mut self, input: Span<F, S>) -> SResult<F, S, Self::Output, Self::Recoverable, Self::Fatal> {
        // See if we can parse the input
        let match_point: usize = input.match_bytes(SpanRange::Open, self.tag);
        if match_point >= self.tag.len() {
            // Matched the entire tag
            #[cfg(debug_assertions)]
            assert!(match_point == self.tag.len());
            Ok((input.slice(match_point..), input.slice(..match_point)))
        } else {
            // Didn't match the entire tag; but is it due to end-of-file or error?
            if match_point == input.len() {
                Err(SnackError::NotEnough { needed: Some(self.tag.len() - match_point), span: input.slice(match_point..) })
            } else {
                Err(SnackError::Recoverable(Recoverable { tag: self.tag, span: input.start_onwards() }))
            }
        }
    }
}





/***** LIBRARY *****/
/// Matches a specific "tag", i.e., a sequence of bytes.
///
/// Useful for matching (binary) keywords.
///
/// # Arguments
/// - `tag`: The tag to match for.
///
/// # Returns
/// A combinator [`Tag`] that will match the prefix of input if it matches `tag`.
///
/// # Fails
/// The returned combinator fails if the prefix of the input was not `tag`. If this was because of
/// an unexpected end-of-file, [`SnackError::NotEnough`] is returned instead.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator2 as _;
/// use ast_toolkit_snack::bytes2::streaming::tag;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::<&str, &[u8]>::new("<example>", b"Hello, world!");
/// let span2 = Span::<&str, &[u8]>::new("<example>", b"Goodbye, world!");
/// let span3 = Span::<&str, &[u8]>::new("<example>", b"Hell");
///
/// let mut comb = tag(b"Hello");
/// assert_eq!(comb.parse(span1), Ok((span1.slice(5..), span1.slice(..5))));
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(tag::Recoverable { tag: b"Hello", span: span2.slice(0..) }))
/// );
/// assert_eq!(
///     comb.parse(span3),
///     Err(SnackError::NotEnough { needed: Some(1), span: span3.slice(4..) })
/// );
/// ```
pub const fn tag<'t, F, S>(tag: &'t [u8]) -> Tag<'t, F, S>
where
    F: Clone,
    S: Clone + LenBytes + MatchBytes,
{
    Tag { tag, _f: PhantomData, _s: PhantomData }
}
