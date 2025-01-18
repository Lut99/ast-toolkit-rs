//  ONE OF 1.rs
//    by Lut99
//
//  Created:
//    02 Nov 2024, 12:19:21
//  Last edited:
//    18 Jan 2025, 18:15:45
//  Auto updated?
//    Yes
//
//  Description:
//!   Implemens the [`one_of1()`]-combinator.
//

use std::convert::Infallible;
use std::fmt::Debug;
use std::marker::PhantomData;

use ast_toolkit_span::Span;

use super::super::complete::one_of1 as one_of1_complete;
pub use super::super::complete::one_of1::{ExpectsFormatter, Recoverable};
use crate::Combinator2;
use crate::result::{Result as SResult, SnackError};
use crate::span::{LenBytes, OneOfUtf8};


/***** COMBINATORS *****/
/// Actual combinator implementing [`one_of1()`].
#[derive(Debug)]
pub struct OneOf1<'t, F, S> {
    charset: &'t [&'t str],
    _f:      PhantomData<F>,
    _s:      PhantomData<S>,
}
impl<'t, F, S> Combinator2<'t, F, S> for OneOf1<'t, F, S>
where
    F: Clone,
    S: Clone + LenBytes + OneOfUtf8,
{
    type ExpectsFormatter = ExpectsFormatter<'t>;
    type Output = Span<F, S>;
    type Recoverable = Recoverable<'t, F, S>;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { charset: self.charset } }

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<F, S, Self::Output, Self::Recoverable, Self::Fatal> {
        // Check first if there's *any* input to parse.
        if input.len() == 0 {
            return Err(SnackError::NotEnough { needed: Some(1), span: input });
        }

        // Otherwise, continue as usual
        one_of1_complete(self.charset).parse(input)
    }
}





/***** LIBRARY *****/
/// Will attempt to match as many characters from the start of a span as possible, as long as those
/// characters are in the set of to-be-searched-for characters.
///
/// This version does _not_ accept matching none of them. See [`one_of0()`](super::super::one_of0())
/// to also allow finding none.
///
/// # Arguments
/// - `charset`: A byte array(-like) that defines the set of characters we are looking for.
///
/// # Returns
/// A combinator [`OneOf1`] that will match the prefix of input as long as those characters are in
/// `charset`.
///
/// # Fails
/// The returned combinator fails if it did not match at least one character. If this match failed
/// because end-of-input was reached, then this fails with a [`SnackError::NotEnough`] instead.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator2 as _;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf82::streaming::one_of1;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "abcdefg");
/// let span2 = Span::new("<example>", "cdefghi");
/// let span3 = Span::new("<example>", "abÿcdef");
/// let span4 = Span::new("<example>", "hijklmn");
/// let span5 = Span::new("<example>", "");
///
/// let mut comb = one_of1(&["a", "b", "c", "ÿ"]);
/// assert_eq!(comb.parse(span1), Ok((span1.slice(3..), span1.slice(..3))));
/// assert_eq!(comb.parse(span2), Ok((span2.slice(1..), span2.slice(..1))));
/// assert_eq!(comb.parse(span3), Ok((span3.slice(5..), span3.slice(..5))));
/// assert_eq!(
///     comb.parse(span4),
///     Err(SnackError::Recoverable(one_of1::Recoverable {
///         charset: &["a", "b", "c", "ÿ"],
///         span:    span4,
///     }))
/// );
/// assert_eq!(comb.parse(span5), Err(SnackError::NotEnough { needed: Some(1), span: span5 }));
/// ```
#[inline]
pub const fn one_of1<'t, F, S>(charset: &'t [&'t str]) -> OneOf1<'t, F, S>
where
    F: Clone,
    S: Clone + LenBytes + OneOfUtf8,
{
    OneOf1 { charset, _f: PhantomData, _s: PhantomData }
}
