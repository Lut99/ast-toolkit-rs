//  ONE OF 1.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 22:34:02
//  Last edited:
//    17 Mar 2025, 14:30:02
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`one_of1()`]-combinator.
//

use std::convert::Infallible;
use std::marker::PhantomData;

use ast_toolkit_span::Span;

use super::super::complete::one_of1 as one_of1_complete;
pub use super::super::complete::one_of1::{ExpectsFormatter, Recoverable};
use crate::Combinator;
use crate::result::{Result as SResult, SnackError};
use crate::span::BytesParsable;


/***** COMBINATORS *****/
/// Actual implementation of the [`one_of1()`]-combinator.
pub struct OneOf1<'b, S> {
    /// The set of bytes to one of.
    byteset: &'b [u8],
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:      PhantomData<S>,
}
// NOTE: This lifetime trick will tell Rust that the impl is actually not invariant, but accepts
// any smaller lifetime than `'b`.
impl<'c, 'b, S> Combinator<'c, S> for OneOf1<'b, S>
where
    'b: 'c,
    S: Clone + BytesParsable,
{
    type ExpectsFormatter = ExpectsFormatter<'b>;
    type Output = Span<S>;
    type Recoverable = Recoverable<'b, S>;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { byteset: self.byteset } }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        // Check first if there's *any* input to parse.
        if input.is_empty() {
            return Err(SnackError::NotEnough { needed: Some(1), span: input });
        }

        // Otherwise, continue as usual
        one_of1_complete(self.byteset).parse(input)
    }
}





/***** LIBRARY *****/
/// Will attempt to match as many bytes from the start of a span as possible, as long as those
/// bytes are in the set of to-be-searched-for bytes.
///
/// This version does _not_ accept matching none of them. See [`one_of0()`](super::super::one_of0())
/// to also allow finding none.
///
/// # Arguments
/// - `byteset`: A byte array(-like) that defines the set of bytes we are looking for.
///
/// # Returns
/// A combinator [`OneOf1`] that will match the prefix of input as long as those bytes are in
/// `byteset`.
///
/// # Fails
/// The returned combinator fails if it did not match at least one byte. If this match failed
/// because end-of-input was reached, then this fails with a [`SnackError::NotEnough`] instead.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::bytes::streaming::one_of1;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new(b"abcdefg".as_slice());
/// let span2 = Span::new(b"cdefghi".as_slice());
/// let span3 = Span::new("abÿcdef".as_bytes());
/// let span4 = Span::new(b"hijklmn".as_slice());
/// let span5 = Span::new(b"".as_slice());
///
/// // Note: the magic numbers below are the two bytes made up by "ÿ"
/// let mut comb = one_of1(&[b'a', b'b', b'c', 191, 195]);
/// assert_eq!(comb.parse(span1), Ok((span1.slice(3..), span1.slice(..3))));
/// assert_eq!(comb.parse(span2), Ok((span2.slice(1..), span2.slice(..1))));
/// assert_eq!(comb.parse(span3), Ok((span3.slice(5..), span3.slice(..5))));
/// assert_eq!(
///     comb.parse(span4),
///     Err(SnackError::Recoverable(one_of1::Recoverable {
///         byteset: &[b'a', b'b', b'c', 191, 195],
///         span:    span4,
///     }))
/// );
/// assert_eq!(comb.parse(span5), Err(SnackError::NotEnough { needed: Some(1), span: span5 }));
/// ```
#[inline]
pub const fn one_of1<'b, S>(byteset: &'b [u8]) -> OneOf1<'b, S>
where
    S: Clone + BytesParsable,
{
    OneOf1 { byteset, _s: PhantomData }
}
