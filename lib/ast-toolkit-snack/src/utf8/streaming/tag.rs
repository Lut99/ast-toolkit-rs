//  TAG.rs
//    by Lut99
//
//  Created:
//    11 Sep 2024, 17:16:33
//  Last edited:
//    07 Mar 2025, 14:45:44
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

pub use super::super::complete::tag::{ExpectsFormatter, Recoverable};
use crate::Combinator;
use crate::result::SnackError;
use crate::span::{LenBytes, MatchBytes};


/***** COMBINATORS *****/
/// Actual combinator implementing [`tag()`].
#[derive(Debug)]
pub struct Tag<'t, F, S> {
    tag: &'t str,
    _f:  PhantomData<F>,
    _s:  PhantomData<S>,
}
impl<'t, F, S> Combinator<'t, F, S> for Tag<'t, F, S>
where
    F: Clone,
    S: Clone + LenBytes + MatchBytes,
{
    type ExpectsFormatter = ExpectsFormatter<'t>;
    type Output = Span<F, S>;
    type Recoverable = Recoverable<'t, F, S>;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { tag: self.tag } }

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<(Span<F, S>, Self::Output), SnackError<Self::Recoverable, Self::Fatal, F, S>> {
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
                Err(SnackError::Recoverable(Recoverable { tag: self.tag, span: input.start_onwards() }))
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
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf8::streaming::tag;
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
///     Err(SnackError::Recoverable(tag::Recoverable { tag: "Hello", span: span2.slice(0..) }))
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
