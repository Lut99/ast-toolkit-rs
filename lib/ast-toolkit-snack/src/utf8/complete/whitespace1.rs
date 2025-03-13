//  DIGIT 1.rs
//    by Lut99
//
//  Created:
//    02 Nov 2024, 11:23:19
//  Last edited:
//    18 Jan 2025, 18:19:57
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`whitespace1()`]-combinator.
//

use std::convert::Infallible;
use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spanning};

use super::one_of1;
use crate::result::{Expected, Result as SResult, SnackError};
use crate::span::OneOfUtf8;
use crate::{Combinator, ExpectsFormatter as _};


/***** TYPE ALIASES *****/
/// The recoverable error returned by [`Whitespace1`].
pub type Recoverable<F, S> = Expected<ExpectsFormatter, F, S>;





/***** FORMATTERS *****/
/// ExpectsFormatter for the [`Whitespace1`]-combinator.
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
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "at least one space, tab, carriage return or newline") }
}





/***** COMBINATORS *****/
/// Actual combinator implementing [`whitespace1()`].
#[derive(Debug)]
pub struct Whitespace1<F, S> {
    _f: PhantomData<F>,
    _s: PhantomData<S>,
}
impl<F, S> Combinator<'static, F, S> for Whitespace1<F, S>
where
    F: Clone,
    S: Clone + OneOfUtf8,
{
    type ExpectsFormatter = ExpectsFormatter;
    type Output = Span<F, S>;
    type Recoverable = Recoverable<F, S>;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter }

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, F, S> {
        match one_of1(&[" ", "\t", "\n", "\r", "\r\n"]).parse(input) {
            Ok(res) => Ok(res),
            Err(SnackError::Recoverable(err)) => Err(SnackError::Recoverable(Recoverable { fmt: self.expects(), span: err.into_span() })),
            Err(SnackError::Fatal(_)) => unreachable!(),
            Err(SnackError::NotEnough { .. }) => unreachable!(),
        }
    }
}





/***** LIBRARY *****/
/// Matches as many whitespace characters as possible.
///
/// Specifically, will match as many as possible from the following set of whitespaces:
/// - A space (` `);
/// - A tab (`\t`);
/// - A carriage return (`\r`); or
/// - A newline (`\n`).
///
/// This version does NOT accept matching none of them. See
/// [`whitespace0()`](super::super::whitespace0) for a version that does.
///
/// # Returns
/// A combinator [`Whitespace1`] that matches only whitespace characters (see above).
///
/// # Fails
/// The returned combinator fails if it did not match at least one whitespace.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf8::complete::whitespace1;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "   \t\n  awesome");
/// let span2 = Span::new("<example>", "cool \n dope");
/// let span3 = Span::new("<example>", "");
///
/// let mut comb = whitespace1();
/// assert_eq!(comb.parse(span1), Ok((span1.slice(7..), span1.slice(..7))));
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(whitespace1::Recoverable {
///         fmt:  whitespace1::ExpectsFormatter,
///         span: span2,
///     }))
/// );
/// assert_eq!(
///     comb.parse(span3),
///     Err(SnackError::Recoverable(whitespace1::Recoverable {
///         fmt:  whitespace1::ExpectsFormatter,
///         span: span3,
///     }))
/// );
/// ```
#[inline]
pub const fn whitespace1<F, S>() -> Whitespace1<F, S>
where
    F: Clone,
    S: Clone + OneOfUtf8,
{
    Whitespace1 { _f: PhantomData, _s: PhantomData }
}
