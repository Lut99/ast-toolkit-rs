//  TAG.rs
//    by Lut99
//
//  Created:
//    11 Sep 2024, 17:16:33
//  Last edited:
//    02 Nov 2024, 12:10:09
//  Auto updated?
//    Yes
//
//  Description:
//!   Implemens the [`tag()`]-combinator.
//

use std::convert::Infallible;
use std::marker::PhantomData;

use ast_toolkit_span::Span;
use ast_toolkit_span::range::SpanRange;

pub use super::super::complete::{TagExpectsFormatter, TagRecoverable};
use crate::result::SnackError;
use crate::span::{LenBytes, MatchBytes};
use crate::{Combinator2, Expects};


/***** COMBINATORS *****/
/// Actual combinator implementing [`tag()`].
#[derive(Debug)]
pub struct Tag<'t, F, S> {
    tag: &'t str,
    _f:  PhantomData<F>,
    _s:  PhantomData<S>,
}
impl<'t, F, S> Expects<'t> for Tag<'t, F, S> {
    type Formatter = TagExpectsFormatter<'t>;

    #[inline]
    fn expects(&self) -> Self::Formatter { TagExpectsFormatter { tag: self.tag } }
}
impl<'t, F, S> Combinator2<'t, F, S> for Tag<'t, F, S>
where
    F: Clone,
    S: Clone + LenBytes + MatchBytes,
{
    type Output = Span<F, S>;
    type Recoverable = TagRecoverable<'t, F, S>;
    type Fatal = Infallible;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<(Span<F, S>, Self::Output), SnackError<F, S, Self::Recoverable, Self::Fatal>> {
        // See if we can parse the input
        let tag: &'t [u8] = self.tag.as_bytes();
        let match_point: usize = input.match_bytes(SpanRange::Open, tag);
        if match_point >= tag.len() {
            // Matched the entire tag
            #[cfg(debug_assertions)]
            assert!(match_point == tag.len());
            Ok((input.slice(match_point..), input.slice(..match_point)))
        } else {
            // Didn't match the entire tag; but is it due to end-of-file or error?
            if match_point == input.len() {
                Err(SnackError::NotEnough { needed: Some(self.tag.len() - match_point), span: input.slice(match_point..) })
            } else {
                Err(SnackError::Recoverable(TagRecoverable { tag: self.tag, span: input.start_onwards() }))
            }
        }
    }
}





/***** LIBRARY *****/
/// Matches a specific "tag", i.e., a sequence of UTF-8 characters.
///
/// Useful for matching keywords.
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
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf82::streaming::{TagRecoverable, tag};
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "Hello, world!");
/// let span2 = Span::new("<example>", "Goodbye, world!");
/// let span3 = Span::new("<example>", "Hell");
///
/// let mut comb = tag("Hello");
/// assert_eq!(comb.parse(span1).unwrap(), (span1.slice(5..), span1.slice(..5)));
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(TagRecoverable { tag: "Hello", span: span2.slice(0..) }))
/// );
/// assert_eq!(
///     comb.parse(span3),
///     Err(SnackError::NotEnough { needed: Some(1), span: span3.slice(4..) })
/// );
/// ```
#[inline]
pub const fn tag<'t, F, S>(tag: &'t str) -> Tag<'t, F, S>
where
    F: Clone,
    S: Clone + LenBytes + MatchBytes,
{
    Tag { tag, _f: PhantomData, _s: PhantomData }
}
