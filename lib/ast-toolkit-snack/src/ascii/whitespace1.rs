//  DIGIT 1.rs
//    by Lut99
//
//  Created:
//    02 Nov 2024, 11:23:19
//  Last edited:
//    08 May 2025, 11:22:16
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`whitespace1()`]-combinator.
//

use std::convert::Infallible;
use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, SpannableBytes, SpanningInf as _};

use crate::result::{Expected, Result as SResult, SnackError};
use crate::scan::one_of1;
use crate::{Combinator, ParseError as _};


/***** TYPE ALIASES *****/
/// The recoverable error returned by [`Whitespace1`].
pub type Recoverable<S> = Expected<ExpectsFormatter, S>;





/***** FORMATTERS *****/
/// ExpectsFormatter for the [`Whitespace1`]-combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct ExpectsFormatter;
impl Display for ExpectsFormatter {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        <Self as crate::ExpectsFormatter>::expects_fmt(self, f, 0)
    }
}
impl crate::ExpectsFormatter for ExpectsFormatter {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "at least one space, tab, carriage return or newline") }
}





/***** COMBINATORS *****/
/// Actual combinator implementing [`whitespace1()`].
#[derive(Debug)]
pub struct Whitespace1<S> {
    _s: PhantomData<S>,
}
impl<'s, S> Combinator<'static, 's, S> for Whitespace1<S>
where
    S: Clone + SpannableBytes<'s>,
{
    type ExpectsFormatter = ExpectsFormatter;
    type Output = Span<S>;
    type Recoverable = Recoverable<S>;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        match one_of1(&[b' ', b'\t', b'\n', b'\r']).parse(input) {
            Ok(res) => Ok(res),
            Err(SnackError::Recoverable(err)) => {
                let fixable = if err.more_might_fix() { Some(err.needed_to_fix()) } else { None };
                let span = err.into_span();
                Err(SnackError::Recoverable(Recoverable { fmt: self.expects(), fixable, span }))
            },
            Err(SnackError::Fatal(_)) => unreachable!(),
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
/// The returned combinator fails if it did not match at least one whitespace. If this match failed
/// because end-of-input was reached, then this fails with a [`SnackError::NotEnough`] instead.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::ascii::whitespace1;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("   \t\n  awesome");
/// let span2 = Span::new("cool \n dope");
/// let span3 = Span::new("");
///
/// let mut comb = whitespace1();
/// assert_eq!(comb.parse(span1), Ok((span1.slice(7..), span1.slice(..7))));
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(whitespace1::Recoverable {
///         fmt:     whitespace1::ExpectsFormatter,
///         fixable: None,
///         span:    span2,
///     }))
/// );
/// assert_eq!(
///     comb.parse(span3),
///     Err(SnackError::Recoverable(whitespace1::Recoverable {
///         fmt:     whitespace1::ExpectsFormatter,
///         fixable: Some(Some(1)),
///         span:    span3,
///     }))
/// );
/// ```
#[inline]
pub const fn whitespace1<'s, S>() -> Whitespace1<S>
where
    S: Clone + SpannableBytes<'s>,
{
    Whitespace1 { _s: PhantomData }
}
