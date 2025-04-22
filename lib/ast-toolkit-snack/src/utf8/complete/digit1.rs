//  DIGIT 1.rs
//    by Lut99
//
//  Created:
//    02 Nov 2024, 11:23:19
//  Last edited:
//    22 Apr 2025, 11:30:31
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`digit1()`]-combinator.
//

use std::convert::Infallible;
use std::fmt::{Debug, Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spannable, Spanning};

use super::while1;
use crate::result::{Expected, Result as SResult, SnackError};
use crate::span::Utf8Parsable;
use crate::{Combinator, ExpectsFormatter as _};


/***** TYPE ALIASES *****/
/// The recoverable error returned by [`Digit1`].
pub type Recoverable<S> = Expected<ExpectsFormatter, S>;





/***** FORMATTERS *****/
/// ExpectsFormatter for the [`digit1()`]-combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct ExpectsFormatter;
impl Display for ExpectsFormatter {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl crate::ExpectsFormatter for ExpectsFormatter {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "at least one digit") }
}





/***** COMBINATORS *****/
/// Actual combinator implementing [`digit1()`].
#[derive(Debug)]
pub struct Digit1<S> {
    _s: PhantomData<S>,
}
impl<'s, S> Combinator<'static, 's, S> for Digit1<S>
where
    S: Clone + Spannable<'s>,
    S::Slice: Utf8Parsable<'s>,
{
    type ExpectsFormatter = ExpectsFormatter;
    type Output = Span<S>;
    type Recoverable = Recoverable<S>;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        match while1("", |c: &str| -> bool {
            c.len() == 1 && {
                let c: char = c.chars().next().unwrap();
                c >= '0' && c <= '9'
            }
        })
        .parse(input)
        {
            Ok(res) => Ok(res),
            Err(SnackError::Recoverable(err)) => Err(SnackError::Recoverable(Recoverable { fmt: ExpectsFormatter, span: err.into_span() })),
            Err(SnackError::Fatal(_) | SnackError::NotEnough { .. }) => unreachable!(),
        }
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
/// The returned combinator fails if it did not match at least one digit.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf8::complete::digit1;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("12345six");
/// let span2 = Span::new("one23456");
/// let span3 = Span::new("5+5");
/// let span4 = Span::new("");
///
/// let mut comb = digit1();
/// assert_eq!(comb.parse(span1), Ok((span1.slice(5..), span1.slice(..5))));
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(digit1::Recoverable {
///         fmt:  digit1::ExpectsFormatter,
///         span: span2,
///     }))
/// );
/// assert_eq!(comb.parse(span3), Ok((span3.slice(1..), span3.slice(..1))));
/// assert_eq!(
///     comb.parse(span4),
///     Err(SnackError::Recoverable(digit1::Recoverable {
///         fmt:  digit1::ExpectsFormatter,
///         span: span4,
///     }))
/// );
/// ```
#[inline]
pub const fn digit1<'s, S>() -> Digit1<S>
where
    S: Clone + Spannable<'s>,
    S::Slice: Utf8Parsable<'s>,
{
    Digit1 { _s: PhantomData }
}
