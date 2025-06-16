//  DIGIT 1.rs
//    by Lut99
//
//  Created:
//    02 Nov 2024, 11:23:19
//  Last edited:
//    08 May 2025, 11:54:43
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`digit1()`]-combinator.
//

use std::convert::Infallible;
use std::marker::PhantomData;

use ast_toolkit_span::{Span, SpannableBytes};

use crate::Combinator;
use crate::result::{Expected, Result as SResult};
use crate::scan::while1;


/***** TYPE ALIASES *****/
/// The recoverable error returned by [`Digit1`].
pub type Recoverable<S> = Expected<ExpectsFormatter, S>;

/// ExpectsFormatter for the [`digit1()`]-combinator.
pub type ExpectsFormatter = while1::ExpectsFormatter<'static>;





/***** COMBINATORS *****/
/// Actual combinator implementing [`digit1()`].
#[derive(Debug)]
pub struct Digit1<S> {
    _s: PhantomData<S>,
}
impl<'s, S> Combinator<'static, 's, S> for Digit1<S>
where
    S: Clone + SpannableBytes<'s>,
{
    type ExpectsFormatter = ExpectsFormatter;
    type Output = Span<S>;
    type Recoverable = Recoverable<S>;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { what: "digit" } }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        while1("digit", |b| -> bool { *b >= b'0' && *b <= b'9' }).parse(input)
    }
}





/***** LIBRARY *****/
/// Matches as many digits as possible.
///
/// This version does _not_ accept matching none of them. See [`digit0()`](super::super::digit0())
/// to also allow finding none.
///
/// # Returns
/// A combinator [`Digit1`] that matches only digits 0-9.
///
/// # Fails
/// The returned combinator fails if it did not match at least one digit. If this match failed
/// because end-of-input was reached, then this fails with a [`SnackError::NotEnough`] instead.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::ascii::digit1;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("12345six");
/// let span2 = Span::new("one23456");
/// let span3 = Span::new("");
///
/// let mut comb = digit1();
/// assert_eq!(comb.parse(span1), Ok((span1.slice(5..), span1.slice(..5))));
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(digit1::Recoverable {
///         fmt:     digit1::ExpectsFormatter { what: "digit" },
///         fixable: None,
///         span:    span2,
///     }))
/// );
/// assert_eq!(
///     comb.parse(span3),
///     Err(SnackError::Recoverable(digit1::Recoverable {
///         fmt:     digit1::ExpectsFormatter { what: "digit" },
///         fixable: Some(Some(1)),
///         span:    span3,
///     }))
/// );
/// ```
#[inline]
pub const fn digit1<'s, S>() -> Digit1<S>
where
    S: Clone + SpannableBytes<'s>,
{
    Digit1 { _s: PhantomData }
}
