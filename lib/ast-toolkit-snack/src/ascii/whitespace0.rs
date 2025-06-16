//  DIGIT 1.rs
//    by Lut99
//
//  Created:
//    02 Nov 2024, 11:23:19
//  Last edited:
//    08 May 2025, 11:22:27
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`whitespace0()`]-combinator.
//

use std::convert::Infallible;
use std::fmt::{Debug, Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, SpannableBytes};

use crate::result::{Result as SResult, SnackError};
use crate::scan::one_of0;
use crate::{Combinator, ExpectsFormatter as _};


/***** FORMATTERS *****/
/// ExpectsFormatter for the [`Whitespace0`]-combinator.
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
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "spaces, tabs, carriage returns or newlines") }
}





/***** COMBINATORS *****/
/// Actual combinator implementing [`whitespace0()`].
#[derive(Debug)]
pub struct Whitespace0<S> {
    _s: PhantomData<S>,
}
impl<'s, S> Combinator<'static, 's, S> for Whitespace0<S>
where
    S: Clone + SpannableBytes<'s>,
{
    type ExpectsFormatter = ExpectsFormatter;
    type Output = Span<S>;
    type Recoverable = Infallible;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        match one_of0(&[b' ', b'\t', b'\n', b'\r']).parse(input) {
            Ok(res) => Ok(res),
            Err(SnackError::Recoverable(_)) => unreachable!(),
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
/// This version accepts matching none of them. See [`whitespace1()`](super::whitespace1()) to
/// assert at least something must be matched.
///
/// # Returns
/// A combinator [`Whitespace0`] that matches only whitespace characters (see above).
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::ascii::whitespace0;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("   \t\n  awesome");
/// let span2 = Span::new("cool \n dope");
/// let span3 = Span::new("");
///
/// let mut comb = whitespace0();
/// assert_eq!(comb.parse(span1), Ok((span1.slice(7..), span1.slice(..7))));
/// assert_eq!(comb.parse(span2), Ok((span2.slice(0..), span2.slice(..0))));
/// assert_eq!(comb.parse(span3), Ok((span3.slice(0..), span3.slice(..0))));
/// ```
#[inline]
pub const fn whitespace0<'s, S>() -> Whitespace0<S>
where
    S: Clone + SpannableBytes<'s>,
{
    Whitespace0 { _s: PhantomData }
}
