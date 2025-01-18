//  WHILE 1.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 22:42:41
//  Last edited:
//    18 Jan 2025, 17:42:08
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`while1()`]-combinator.
//

use std::convert::Infallible;
use std::marker::PhantomData;

use ast_toolkit_span::Span;

use super::super::complete::while1 as while1_complete;
pub use super::super::complete::while1::{ExpectsFormatter, Recoverable};
use crate::Combinator2;
use crate::result::{Result as SResult, SnackError};
use crate::span::{LenBytes, WhileBytes};


/***** COMBINATORS *****/
/// Actual implementation of the [`while1()`]-combinator.
pub struct While1<'t, F, S, P> {
    /// The predicate used for matching.
    predicate: P,
    /// A helper provided by the user to describe what is expected.
    what: &'t str,
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f: PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s: PhantomData<S>,
}
// NOTE: This lifetime trick will tell Rust that the impl is actually not invariant, but accepts
// any smaller lifetime than `'b`.
impl<'t, F, S, P> Combinator2<'t, F, S> for While1<'t, F, S, P>
where
    F: Clone,
    S: Clone + LenBytes + WhileBytes,
    P: FnMut(u8) -> bool,
{
    type ExpectsFormatter = ExpectsFormatter<'t>;
    type Output = Span<F, S>;
    type Recoverable = Recoverable<'t, F, S>;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { what: self.what } }

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<F, S, Self::Output, Self::Recoverable, Self::Fatal> {
        // Check first if there's *any* input to parse.
        if input.len() == 0 {
            return Err(SnackError::NotEnough { needed: Some(1), span: input });
        }

        // Otherwise, continue as usual
        while1_complete(self.what, &mut self.predicate).parse(input)
    }
}





/***** LIBRARY *****/
/// Will attempt to match as many bytes from the start of a span as possible, as long as those
/// bytes match a given predicate.
///
/// This version does _not_ accept matching none of them. See [`while0()`](super::super::while0())
/// to also allow finding none.
///
/// # Arguments
/// - `what`: A short string describing what byte is being matched. Should finish the sentence
///   "Expected at least one ...".
/// - `predicate`: A closure that returns true for matching bytes, and false for non-matching
///   bytes. All bytes that are matched are returned up to the first for which
///   `predicate` returns false (if any).
///
/// # Returns
/// A combinator [`While1`] that will match the prefix of input as long as those bytes match
/// the given `predicate`.
///
/// # Fails
/// The returned combinator fails if it did not match at least one byte. If this match failed
/// because end-of-input was reached, then this fails with a [`SnackError::NotEnough`] instead.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator2 as _;
/// use ast_toolkit_snack::bytes2::streaming::while1;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::<&str, &[u8]>::new("<example>", b"abcdefg");
/// let span2 = Span::<&str, &[u8]>::new("<example>", b"cdefghi");
/// let span3 = Span::<&str, &[u8]>::new("<example>", "abÿcdef".as_bytes());
/// let span4 = Span::<&str, &[u8]>::new("<example>", b"hijklmn");
/// let span5 = Span::<&str, &[u8]>::new("<example>", b"");
///
/// let mut comb = while1("'a', 'b', 'c' or 'ÿ'", |b: u8| -> bool {
///     b == b'a' || b == b'b' || b == b'c' || b == 191 || b == 195
/// });
/// assert_eq!(comb.parse(span1), Ok((span1.slice(3..), span1.slice(..3))));
/// assert_eq!(comb.parse(span2), Ok((span2.slice(1..), span2.slice(..1))));
/// assert_eq!(comb.parse(span3), Ok((span3.slice(5..), span3.slice(..5))));
/// assert_eq!(
///     comb.parse(span4),
///     Err(SnackError::Recoverable(while1::Recoverable {
///         what: "'a', 'b', 'c' or 'ÿ'",
///         span: span4,
///     }))
/// );
/// assert_eq!(comb.parse(span5), Err(SnackError::NotEnough { needed: Some(1), span: span5 }));
/// ```
#[inline]
pub const fn while1<'t, F, S, P>(what: &'t str, predicate: P) -> While1<'t, F, S, P>
where
    F: Clone,
    S: Clone + LenBytes + WhileBytes,
    P: FnMut(u8) -> bool,
{
    While1 { predicate, what, _f: PhantomData, _s: PhantomData }
}
